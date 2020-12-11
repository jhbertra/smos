{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Smos.Cursor.Report.Work where

import Conduit
import Cursor.Forest
import Cursor.Simple.List.NonEmpty
import Cursor.Simple.Tree
import Cursor.Text
import Cursor.Types
import qualified Data.Conduit.Combinators as C
import Data.Foldable (toList)
import qualified Data.List.NonEmpty as NE
import Data.Validity
import GHC.Generics (Generic)
import Lens.Micro
import Path
import Smos.Cursor.Collapse
import Smos.Cursor.Entry
import Smos.Cursor.Report.Streaming
import Smos.Cursor.SmosFile
import Smos.Data
import Smos.Report.Archive
import Smos.Report.Config
import Smos.Report.Filter
import Smos.Report.ShouldPrint
import Smos.Report.Work

produceWorkReportCursor :: MonadIO m => DirectoryConfig -> WorkReportContext -> m WorkReportCursor
produceWorkReportCursor dc wrc = WorkReportCursor <$> produceWorkReport HideArchive DontPrint dc wrc

newtype WorkReportCursor = WorkReportCursor WorkReport
  deriving (Show, Eq, Generic)

instance Validity WorkReportCursor
