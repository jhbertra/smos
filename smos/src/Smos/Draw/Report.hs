{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Draw.Report
  ( drawReportCursor,
  )
where

import Brick.Types as B
import Brick.Widgets.Core as B
import Cursor.Brick
import Data.List
import qualified Data.Map as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Lens.Micro
import Path
import Smos.Actions
import Smos.Data
import Smos.Draw.Base
import Smos.Draw.Data
import Smos.Report.Agenda
import Smos.Report.Filter
import Smos.Report.Formatting
import Smos.Report.Stuck
import Smos.Report.Waiting
import Smos.Report.Work
import Smos.Style
import Smos.Types
import Text.Time.Pretty

drawReportCursor :: Select -> ReportCursor -> Drawer
drawReportCursor s = \case
  ReportNextActions narc -> pure $ drawNextActionReportCursor s narc
  ReportWaiting wrc -> drawWaitingReportCursor s wrc
  ReportWork wrc -> drawWorkReportCursor s wrc

drawNextActionReportCursor :: Select -> NextActionReportCursor -> Widget ResourceName
drawNextActionReportCursor s NextActionReportCursor {..} =
  withHeading (str "Next Action Report") $
    vBox
      [ padAll 1 $
          viewport ResourceViewport Vertical $
            case nextActionReportCursorSelectedNextActionEntryCursors of
              Nothing -> txtWrap "Empty next action report"
              Just naecs -> verticalNonEmptyCursorTable (go NotSelected) (go s) (go NotSelected) naecs,
        ( case nextActionReportCursorSelection of
            NextActionReportFilterSelected -> withAttr selectedAttr
            NextActionReportSelected -> id
        )
          $ let ms =
                  case nextActionReportCursorSelection of
                    NextActionReportFilterSelected -> MaybeSelected
                    NextActionReportSelected -> NotSelected
             in hBox [textLineWidget "Filter:", txt " ", drawTextCursor ms nextActionReportCursorFilterBar]
      ]
  where
    go = drawNextActionEntryCursor

drawNextActionEntryCursor :: Select -> NextActionEntryCursor -> [Widget ResourceName]
drawNextActionEntryCursor s naec@NextActionEntryCursor {..} =
  let e@Entry {..} = naec ^. nextActionEntryCursorEntryL
      sel =
        ( case s of
            MaybeSelected -> forceAttr selectedAttr . visible
            NotSelected -> id
        )
   in [ drawFilePath nextActionEntryCursorFilePath,
        maybe emptyWidget drawTodoState $ entryState e,
        sel $ drawHeader entryHeader
      ]

drawWaitingReportCursor :: Select -> WaitingReportCursor -> Drawer
drawWaitingReportCursor s WaitingReportCursor {..} = do
  now <- asks zonedTimeToUTC
  let go = drawWaitingEntryCursor now
  pure $
    withHeading (str "Waiting Report") $
      padAll 1 $
        viewport ResourceViewport Vertical $
          case waitingReportCursorWaitingEntryCursors of
            Nothing -> txtWrap "Empty waiting report"
            Just wecs -> verticalNonEmptyCursorTable (go NotSelected) (go s) (go NotSelected) wecs

drawWorkReportCursor :: Select -> WorkReportCursor -> Drawer
drawWorkReportCursor _ (WorkReportCursor WorkReport {..}) = do
  now <- ask
  let agendaTable :: Widget n
      agendaTable = formatAsTable $ map (drawAgendaEntry now) workReportAgendaEntries
      waitingTable :: Widget n
      waitingTable = formatAsTable $ map (formatWaitingEntry (zonedTimeToUTC now)) workReportOverdueWaiting
      stuckTable :: Widget n
      stuckTable = formatAsTable $ map (formatStuckReportEntry (zonedTimeToUTC now)) workReportOverdueStuck
      ctxs = [] -- Get the contexts from my config
      pieces :: [[[Widget n]]]
      pieces =
        [ unlessNull
            workReportNextBegin
            [ sectionHeading "Next meeting:",
              [formatAsTable $ maybe [] ((: []) . drawAgendaEntry now) workReportNextBegin]
            ],
          unlessNull
            workReportAgendaEntries
            [ sectionHeading "Deadlines:",
              [agendaTable]
            ],
          unlessNull
            workReportResultEntries
            [ sectionHeading "Next actions:",
              [entryTable workReportResultEntries]
            ],
          unlessNull
            workReportOverdueWaiting
            [ warningHeading "Overdue Waiting Entries:",
              [waitingTable]
            ],
          unlessNull
            workReportOverdueStuck
            [ warningHeading "Overdue Stuck Reports:",
              [stuckTable]
            ],
          unlessNull
            ctxs
            $ unlessNull
              workReportEntriesWithoutContext
              [ warningHeading "WARNING, the following Entries don't match any context:",
                [entryTable workReportEntriesWithoutContext]
              ],
          unlessNull
            workReportCheckViolations
            [ warningHeading "WARNING, the following Entries did not pass the checks:",
              concat $
                flip concatMap (M.toList workReportCheckViolations) $
                  \(f, violations) ->
                    unlessNull violations [warningHeading (renderFilter f), [entryTable violations]]
            ]
        ]
  pure $
    withHeading (str "Work Report") $
      padAll 1 $
        vBox $
          concat $
            intercalate [spacer] pieces
  where
    unlessNull :: Foldable l => l e -> [a] -> [a]
    unlessNull l r =
      if null l
        then []
        else r
    spacer = [str " "]
    sectionHeading :: Text -> [Widget n]
    sectionHeading t = [withAttr workReportSectionAttr $ txt t]
    warningHeading :: Text -> [Widget n]
    warningHeading t = [withAttr workReportWarningAttr $ txt t]
    -- Get these thresholds from my config
    formatWaitingEntry :: UTCTime -> WaitingEntry -> [Widget n]
    formatWaitingEntry _ we = [str (show we)]
    formatStuckReportEntry :: UTCTime -> StuckReportEntry -> [Widget n]
    formatStuckReportEntry _ se = [str (show se)]
    formatAsTable :: [[Widget n]] -> Widget n
    formatAsTable = tableWidget
    entryTable = str . show

drawWaitingEntryCursor :: UTCTime -> Select -> WaitingEntryCursor -> [Widget ResourceName]
drawWaitingEntryCursor now s WaitingEntryCursor {..} =
  let sel =
        ( case s of
            MaybeSelected -> forceAttr selectedAttr . visible
            NotSelected -> id
        )
   in [ str $ toFilePath waitingEntryCursorFilePath,
        sel $ drawHeader $ entryHeader $ forestCursorCurrent waitingEntryCursorForestCursor,
        daysSinceWidget 7 now waitingEntryCursorTimestamp
      ]

drawAgendaEntry :: ZonedTime -> AgendaEntry -> [Widget n]
drawAgendaEntry now AgendaEntry {..} =
  let tz = zonedTimeZone now
      d = diffDays (timestampDay agendaEntryTimestamp) (localDay $ zonedTimeToLocalTime now)
      func =
        if
            | d <= 0 && agendaEntryTimestampName == "DEADLINE" -> withAttr agendaEntryDeadlinePast
            | d == 1 && agendaEntryTimestampName == "DEADLINE" -> withAttr agendaEntryDeadlineToday
            | d <= 10 && agendaEntryTimestampName == "DEADLINE" -> withAttr agendaEntryDeadlineSoon
            | d < 0 && agendaEntryTimestampName == "SCHEDULED" -> withAttr agendaEntryScheduledPast
            | d == 0 && agendaEntryTimestampName == "SCHEDULED" -> withAttr agendaEntryScheduledToday
            | otherwise -> id
   in [ func $ txt $ timestampPrettyText agendaEntryTimestamp,
        func $
          txt $
            T.pack $
              renderTimeAgoAuto $
                timeAgo $
                  diffUTCTime
                    (zonedTimeToUTC now)
                    (localTimeToUTC tz $ timestampLocalTime agendaEntryTimestamp),
        drawTimestampName agendaEntryTimestampName,
        maybe emptyWidget drawTodoState agendaEntryTodoState,
        drawHeader agendaEntryHeader,
        func $ drawFilePath agendaEntryFilePath
      ]

daysSinceWidget :: Word -> UTCTime -> UTCTime -> Widget n
daysSinceWidget threshold now t = withAttr style $ str $ show i <> " days"
  where
    th1 = fromIntegral threshold :: Int
    th2 = floor ((fromIntegral threshold :: Double) / 3 * 2) :: Int
    th3 = floor ((fromIntegral threshold :: Double) / 3) :: Int
    style
      | i >= th1 = waitingReportLongWait
      | i >= th2 = waitingReportMidWait
      | i >= th3 = waitingReportShortWait
      | otherwise = waitingReportNoWait
    i = daysSince now t
