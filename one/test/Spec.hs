import GHC.Base (assert)
import Lib (solver)
import Test.HUnit

main :: IO ()
main = do
  runTestTT solverTest
  return ()

solverTest :: Test
solverTest =
  TestCase
    ( assertEqual
        "Solves the sample problem correctly"
        514579
        ( solver
            [ 1721,
              979,
              366,
              299,
              675,
              1456
            ]
        )
    )