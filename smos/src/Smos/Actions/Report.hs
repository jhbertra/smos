module Smos.Actions.Report where

import Smos.Actions.Report.Next
import Smos.Actions.Report.Waiting
import Smos.Actions.Report.Work
import Smos.Types

allPlainReportActions :: [Action]
allPlainReportActions =
  concat
    [ allPlainReportNextActions,
      allPlainReportWaitingActions,
      allPlainReportWorkActions
    ]

allReportUsingActions :: [ActionUsing Char]
allReportUsingActions =
  concat
    [ allReportNextActionsUsingActions,
      allReportWaitingUsingActions,
      allReportWorkUsingActions
    ]
