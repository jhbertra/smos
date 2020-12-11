{-# LANGUAGE TypeApplications #-}

module Smos.Cursor.Report.WorkSpec where

import Smos.Cursor.Report.Work
import Smos.Cursor.Report.Work.Gen ()
import Smos.Report.TestUtils
import Smos.Report.Work
import Smos.Report.Work.Gen ()
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Validity
import Test.Validity.Optics

spec :: Spec
spec = do
  genValidSpec @WorkReportCursor
  modifyMaxSuccess (`div` 10) $
    describe "produceWorkActionReportCursor" $
      it "produces valid reports for interesting stores" $
        forAllValid $ \wrc ->
          withInterestingStore $
            \dc -> do
              wrc <- produceWorkReportCursor dc wrc
              shouldBeValid wrc
