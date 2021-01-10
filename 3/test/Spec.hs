import GHC.Base (assert)
import Lib (TreeMap (..), isTreeAt, rowParser, solver, treeMapParser)
import Test.HUnit
import Text.Parsec

main :: IO ()
main = do
  runTestTT tests
  return ()

tests = TestList [test1, test2, test3, test4, test5, test6]

test1 :: Test
test1 =
  TestCase
    ( assertEqual
        "Parses a row of trees correctly"
        (Right (TreeMap [[False, False, True, False, True]]))
        (parse treeMapParser "" "..#.#")
    )

test2 :: Test
test2 =
  TestCase
    ( assertEqual
        "Parses multiple rows of trees correctly"
        ( Right
            ( TreeMap
                [ [False, False, True, False, True],
                  [False, False, True, False, True]
                ]
            )
        )
        (parse treeMapParser "" "..#.#\n..#.#")
    )

test3 :: Test
test3 =
  TestCase
    ( assertEqual
        "Solver reports the correct number of trees hit"
        2
        ( solver
            ( TreeMap
                [ [True, False],
                  [False, True]
                ]
            )
            1
            1
        )
    )

test4 :: Test
test4 =
  TestCase
    ( assertEqual
        "Solver reports the correct number of trees hit"
        1
        ( solver
            ( TreeMap
                [ [True, False],
                  [False, False]
                ]
            )
            1
            1
        )
    )

test5 :: Test
test5 =
  TestCase
    ( assertEqual
        "Solver reports the correct number of trees hit"
        2
        ( solver
            ( TreeMap
                [ [True, False],
                  [True, False]
                ]
            )
            0
            1
        )
    )

test6 :: Test
test6 =
  TestCase
    ( assertEqual
        "isTreeAt wraps infinitely successfully"
        True
        ( isTreeAt
            ( TreeMap
                [[True, False]]
            )
            2
            0
        )
    )
