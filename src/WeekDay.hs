
{-# LANGUAGE NoImplicitPrelude #-}

module WeekDay (
    WeekDay(..),
    nthWeekDayOfMonth
) where

import Data.Time.Calendar (Day, addDays, fromGregorian)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Prelude (Eq, Int, Integer, (.), ($), (-), (*), (+), (==), abs, otherwise, toInteger)

type Month = (Integer, Int)
data WeekDay = Mo | Tu | We | Th | Fr | Sa | Su
    deriving (Eq)

nthWeekDayOfMonth :: Int -> WeekDay -> Month -> Day
nthWeekDayOfMonth n day ym = addDays diff firstDay
    where
        firstDay :: Day
        firstDay = firstDayOfMonth ym

        firstWeekDay :: WeekDay
        (_, _, firstWeekDay) = firstWeekDayOfMonth ym

        diff :: Integer
        diff
            | day == firstWeekDay = weeks + 1
            | otherwise = weeks + (abs . toInteger $ weekDayNumber day - weekDayNumber firstWeekDay)

        weeks :: Integer
        weeks = (toInteger n - 1) * 7

firstWeekDayOfMonth :: Month -> (Integer, Int, WeekDay)
firstWeekDayOfMonth ym = replaceWeekDay . toWeekDate $ firstDayOfMonth ym
    where
        replaceWeekDay :: (Integer, Int, Int) -> (Integer, Int, WeekDay)
        replaceWeekDay (y, m, d) = (y, m, weekDayFromNumber d)

firstDayOfMonth :: Month -> Day
firstDayOfMonth (year, month) = fromGregorian year month 1

weekDayNumber :: WeekDay -> Int
weekDayNumber Mo = 1
weekDayNumber Tu = 2
weekDayNumber We = 3
weekDayNumber Th = 4
weekDayNumber Fr = 5
weekDayNumber Sa = 6
weekDayNumber Su = 7

weekDayFromNumber :: Int -> WeekDay
weekDayFromNumber 1 = Mo
weekDayFromNumber 2 = Tu
weekDayFromNumber 3 = We
weekDayFromNumber 4 = Th
weekDayFromNumber 5 = Fr
weekDayFromNumber 6 = Sa
weekDayFromNumber 7 = Su
