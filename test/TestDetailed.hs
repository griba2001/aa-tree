{-# LANGUAGE PackageImports, NamedFieldPuns #-}
module TestDetailed (tests) where

import qualified Test.QuickCheck as Q
import Distribution.TestSuite as TS

import TestSortedSet

toTSResult :: Q.Result -> TS.Result
toTSResult Q.Success {} = TS.Pass
toTSResult Q.GaveUp {} = TS.Fail "GaveUp"
toTSResult Q.Failure {Q.reason} = TS.Fail reason

runQuickCheck :: Q.Testable p => p -> IO TS.Progress
runQuickCheck prop = do
        qres <- Q.quickCheckWithResult Q.stdArgs {Q.maxSuccess = 50, Q.maxSize = 20} prop
        return $ (Finished . toTSResult) qres

tests :: IO [Test]

tests = return [ Test $ TestInstance (runQuickCheck propCheckP0) "propCheckP0" ["AATree"] [] undefined,
                 Test $ TestInstance (runQuickCheck propCheckP1) "propCheckP1" ["AATree"] [] undefined,
                 Test $ TestInstance (runQuickCheck propCheckP2) "propCheckP2" ["AATree"] [] undefined,
                 Test $ TestInstance (runQuickCheck propCheckP3) "propCheckP3" ["AATree"] [] undefined,
                 Test $ TestInstance (runQuickCheck propCheckP4) "propCheckP4" ["AATree"] [] undefined,
                 Test $ TestInstance (runQuickCheck propCheckP5) "propCheckP5" ["AATree"] [] undefined,
                         
                 Test $ TestInstance (runQuickCheck propInsertMember) "propInsertMember" ["set"] [] undefined,
                 Test $ TestInstance (runQuickCheck propDeleteAfterInsertImpliesNotMember) "propDeleteAfterInsertImpliesNotMember" ["set"] [] undefined,
                 Test $ TestInstance (runQuickCheck propSorted) "propSorted" ["set"] [] undefined,
                 Test $ TestInstance (runQuickCheck propYieldsOrigin) "propYieldsOrigin" ["set"] [] undefined,
                 Test $ TestInstance (runQuickCheck propSortedAfterDeletes) "propSortedAfterDeletes" ["set"] [] undefined,
                 Test $ TestInstance (runQuickCheck propMaximum) "propMaximum" ["set"] [] undefined,
                 Test $ TestInstance (runQuickCheck propMinimum) "propMinimum" ["set"] [] undefined
                 ]