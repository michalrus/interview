import Test.Tasty
import Test.Tasty.QuickCheck
import Test.Tasty.HUnit

import qualified RunnerSpec

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ RunnerSpec.tests
  ]
