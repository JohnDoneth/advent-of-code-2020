import GHC.Base (assert)
import Lib (solver)
import Test.HUnit

main :: IO ()
main = do
  runTestTT testsStep2
  return ()

tests = TestList [test1, test2, test3]

test1 :: Test
test1 =
  TestCase
    ( assertEqual
        "Finds the first sample problem to be valid"
        True
        (solver "1-3 a: abcde")
    )

test2 :: Test
test2 =
  TestCase
    ( assertEqual
        "Finds the second sample problem to be invalid"
        False
        (solver "1-3 b: cdefg")
    )

test3 :: Test
test3 =
  TestCase
    ( assertEqual
        "Finds the third sample problem to be valid"
        True
        (solver "2-9 c: ccccccccc")
    )

testsStep2 = TestList [test1step2, test2step2, test3step2]

test1step2 :: Test
test1step2 =
  TestCase
    ( assertEqual
        "Finds the first sample problem to be valid"
        True
        (solver "1-3 a: abcde")
    )

test2step2 :: Test
test2step2 =
  TestCase
    ( assertEqual
        "Finds the second sample problem to be invalid"
        False
        (solver "1-3 b: cdefg")
    )

test3step2 :: Test
test3step2 =
  TestCase
    ( assertEqual
        "Finds the third sample problem to be invalid"
        False
        (solver "2-9 c: ccccccccc")
    )