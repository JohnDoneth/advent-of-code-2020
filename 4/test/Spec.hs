import GHC.Base (assert)
import Lib
import Test.HUnit
import Text.Parsec

main :: IO ()
main = do
  runTestTT
    ( TestList
        [ tests,
          heightParsingTests,
          birthYearTests,
          hairColorTests,
          eyeColorTests,
          passportIDTests
        ]
    )
  return ()

tests =
  TestList
    [ assertValid "ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm\n\n",
      assertValid "hcl:#ae17e1 iyr:2013\neyr:2024\necl:brn pid:760753108 byr:1931\nhgt:179cm\n\n",
      assertInvalid "iyr:2013 ecl:amb cid:350 eyr:2023 pid:028048884\nhcl:#cfa07d byr:1929\n\n",
      assertInvalid "hcl:#cfa07d eyr:2025 pid:166559648\niyr:2011 ecl:brn hgt:59in\n\n"
    ]

assertValid :: String -> Test
assertValid input =
  TestCase (assertEqual "" True (isValidPassportS input))

assertInvalid :: String -> Test
assertInvalid input =
  TestCase (assertEqual "" False (isValidPassportS input))

birthYearTests :: Test
birthYearTests =
  TestList
    [ TestCase (assertEqual "" True (sValidateBirthYear "2002")),
      TestCase (assertEqual "" False (sValidateBirthYear "2003"))
    ]

heightParsingTests :: Test
heightParsingTests =
  TestList
    [ TestCase (assertEqual "" True (sValidateHeight "60in")),
      TestCase (assertEqual "" True (sValidateHeight "190cm")),
      TestCase (assertEqual "" False (sValidateHeight "190in")),
      TestCase (assertEqual "" False (sValidateHeight "190"))
    ]

hairColorTests :: Test
hairColorTests =
  TestList
    [ TestCase (assertEqual "" True (sValidateBirthYear "#123abc")),
      TestCase (assertEqual "" False (sValidateBirthYear "#123abz")),
      TestCase (assertEqual "" False (sValidateBirthYear "123abc"))
    ]

eyeColorTests :: Test
eyeColorTests =
  TestList
    [ TestCase (assertEqual "" True (sValidateEyeColor "brn")),
      TestCase (assertEqual "" False (sValidateEyeColor "wat"))
    ]

passportIDTests :: Test
passportIDTests =
  TestList
    [ TestCase (assertEqual "" True (sValidatePassportID "000000001")),
      TestCase (assertEqual "" False (sValidatePassportID "0123456789"))
    ]