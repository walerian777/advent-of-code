import Test.HUnit
import InventoryManagementSystem

list1 = ["abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab"]
expectedChecksum = 12
calculateChecksumTest = TestCase (assertEqual "" (calculateChecksum list1) expectedChecksum)

list2 = ["abcde", "fghij", "klmno", "pqrst", "fguij", "axcye", "wvxyz"]
expectedLetters = "fgij"
commonLettersTest = TestCase (assertEqual "" (commonLetters list2) expectedLetters)

tests = TestList [
  TestLabel "Part 1 - Example" calculateChecksumTest,
  TestLabel "Part 2 - Example" commonLettersTest
  ]

