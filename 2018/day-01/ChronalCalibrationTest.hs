import Test.HUnit
import ChronalCalibration

reachedTwiceTest1 = TestCase (assertEqual "" (reachedTwice [1, -1]) 0)
reachedTwiceTest2 = TestCase (assertEqual "" (reachedTwice [3, 3, 4, -2, -4]) 10)
reachedTwiceTest3 = TestCase (assertEqual "" (reachedTwice [-6, 3, 8, 5, -6]) 5)
reachedTwiceTest4 = TestCase (assertEqual "" (reachedTwice [7, 7, -2, -7, -4]) 14)

tests = TestList [
  TestLabel "+1, -1 = 0" reachedTwiceTest1,
  TestLabel "+3, +3, +4, -2, -4 -> 10" reachedTwiceTest2,
  TestLabel "-6, +3, +8, +5, -6 -> 5" reachedTwiceTest3,
  TestLabel "+7, +7, -2, -7, -4 -> 14" reachedTwiceTest4
  ]
