module Smos.Draw.Data where

import Brick.Types as B
import Brick.Widgets.Border as B
import Brick.Widgets.Center as B
import Brick.Widgets.Core as B
import Control.Monad.Reader
import Cursor.Brick
import Data.Time
import Smos.Data
import Smos.Draw.Base
import Smos.Style
import Text.Time.Pretty

drawHeader :: Header -> Widget n
drawHeader = withAttr headerAttr . textLineWidget . headerText

drawTodoState :: TodoState -> Widget n
drawTodoState ts =
  withAttr (todoStateSpecificAttr ts <> todoStateAttr) . textLineWidget $ todoStateText ts

drawTimestampName :: TimestampName -> Widget n
drawTimestampName tsn =
  withAttr (timestampNameSpecificAttr tsn <> timestampNameAttr) . textLineWidget $
    timestampNameText tsn

drawTimestamp :: Timestamp -> Drawer
drawTimestamp ts =
  case ts of
    TimestampDay d -> drawDay d
    TimestampLocalTime lt -> drawLocalTime lt

drawDay :: Day -> Drawer
drawDay d = do
  zt <- ask
  pure $
    hBox
      [ str $ formatTimestampDay d,
        str ", ",
        str $ prettyDayAuto (localDay $ zonedTimeToLocalTime zt) d
      ]

drawLocalTime :: LocalTime -> Drawer
drawLocalTime lt = do
  zt@(ZonedTime _ tz) <- ask
  pure $
    hBox
      [ str $ formatTimestampLocalTime lt,
        str ", ",
        str $ prettyTimeAuto (zonedTimeToUTC zt) $ localTimeToUTC tz lt
      ]
