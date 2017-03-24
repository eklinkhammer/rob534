
import Test.HUnit

import ConstructVisTest

allTests :: Test
allTests = TestList [TestLabel "Construct Vis Tests" constructVisTests]
                    



main :: IO Counts
main = runTestTT allTests

