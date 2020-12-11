{-# LANGUAGE OverloadedStrings #-}

module Smos.Actions.Report.Work where

import qualified Data.Set as S
import Data.Time
import Path
import Smos.Actions.File
import Smos.Actions.Utils
import Smos.Report.Config
import Smos.Report.Work
import Smos.Types

allPlainReportWorkActions :: [Action]
allPlainReportWorkActions =
  [ reportWork
  ]

allReportWorkUsingActions :: [ActionUsing Char]
allReportWorkUsingActions = []

reportWork :: Action
reportWork =
  Action
    { actionName = "reportWork",
      actionFunc = modifyEditorCursorS $ \ec -> do
        saveCurrentSmosFile
        src <- asks configReportConfig
        wd <- liftIO $ resolveReportWorkflowDir src
        pd <- liftIO $ resolveReportProjectsDir src
        let mpd = stripProperPrefix wd pd
        now <- liftIO getZonedTime
        let workConfig = defaultWorkReportConfig -- TODO get this from my config
        let wrc =
              WorkReportContext
                { workReportContextNow = now,
                  workReportContextProjectsSubdir = mpd,
                  workReportContextBaseFilter = workReportConfigBaseFilter workConfig,
                  workReportContextCurrentContext = Nothing,
                  workReportContextTimeProperty = "timewindow",
                  workReportContextTime = Nothing,
                  workReportContextAdditionalFilter = Nothing,
                  workReportContextContexts = workReportConfigContexts workConfig,
                  workReportContextChecks = S.empty, -- TODO get this from config
                  workReportContextSorter = Nothing,
                  workReportContextWaitingThreshold = 7, -- TODO centralise these defaults
                  workReportContextStuckThreshold = 21
                }
        let dc = smosReportConfigDirectoryConfig src
        iwrc <- liftIO $ produceWorkReportCursor dc wrc
        pure $
          ec
            { editorCursorSelection = ReportSelected,
              editorCursorReportCursor = Just $ ReportWork iwrc
            },
      actionDescription = "Work report"
    }
