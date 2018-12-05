import Test.HUnit
import AlchemicalReduction

polymer = "dabAcCaCBAcCcaDA"
expectedLength = 10
reducedPolymerLengthTest = TestCase (assertEqual "" (reducedPolymerLenght polymer) expectedLength)

tests = TestList [
  TestLabel "Part 1 - Example" reducedPolymerLengthTest
  ]
