import Test.HUnit
import InventoryManagementSystem

list = ["abcdef", "bababc", "abbcde", "abcccd", "aabcdd", "abcdee", "ababab"]
expectedChecksum = 12
calculateChecksumTest = TestCase (assertEqual "" (calculateChecksum list) expectedChecksum)

tests = TestList [
  TestLabel "Part 1 - Example" calculateChecksumTest
  ]

