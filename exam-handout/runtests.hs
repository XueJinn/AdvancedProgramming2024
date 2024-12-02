import qualified APL.InterpConcurrent_Tests
import qualified APL.InterpPure_Tests
import qualified APL.InterpSim_Tests
import qualified APL.Parser_Tests
import qualified KVDB_Tests
import Test.Tasty (defaultMain, localOption, mkTimeout, testGroup)

main :: IO ()
main =
  defaultMain $
    localOption (mkTimeout 3000000) $
      testGroup
        "Tests"
        [ KVDB_Tests.tests,
          testGroup
            "APL"
            [ APL.InterpPure_Tests.tests,
              APL.InterpSim_Tests.tests,
              APL.InterpConcurrent_Tests.tests,
              APL.Parser_Tests.tests
            ]
        ]
