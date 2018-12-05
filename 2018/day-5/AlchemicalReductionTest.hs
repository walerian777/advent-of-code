import Test.HUnit
import AlchemicalReduction

polymer = "dabAcCaCBAcCcaDA"

expectedLength = 10
reducedPolymerLengthTest = TestCase (assertEqual "" (reducedPolymerLenght polymer) expectedLength)

expectedLength' = 4
shortestPolymerLengthTest = TestCase (assertEqual "" (shortestPolymerLength polymer) expectedLength')

tests = TestList [
  TestLabel "Part 1 - Example" reducedPolymerLengthTest,
  TestLabel "Part 2 - Example" shortestPolymerLengthTest
  ]
