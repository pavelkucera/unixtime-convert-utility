
{-# LANGUAGE NoImplicitPrelude #-}

module PacificTimeZone (
    toPacificTimeZone
) where

import Data.Time (TimeZone(..), ZonedTime, utcToZonedTime, zonedTimeToUTC, UTCTime(..))
import Data.Time.Calendar (Day, toGregorian)
import Prelude(Bool(..), Integer, Int, (.), ($), (<), (>), otherwise)
import WeekDay

-- | Pacific Standard Time
pst :: TimeZone
pst = TimeZone (- 480) False "PST"

-- | Pacific Daylight Time
pdt :: TimeZone
pdt = TimeZone (- 420) True "PDT"

-- | Converts given time to its equivalent in PacificTimeZone
toPacificTimeZone :: ZonedTime -> ZonedTime
toPacificTimeZone time = utcToZonedTime timeZone utcTime
    where
        utcTime :: UTCTime
        utcTime = zonedTimeToUTC time

        timeZone :: TimeZone
        timeZone = pacificTimeZone utcTime

-- | Returns pacific time zone for a given time
pacificTimeZone :: UTCTime -> TimeZone
pacificTimeZone time
    | time < summerTimeStart = pst
    | time > summerTimeEnd = pst
    | otherwise = pdt

    where
        summerTimeStart :: UTCTime
        summerTimeEnd :: UTCTime
        (summerTimeStart, summerTimeEnd) = summerTimeInterval year

        year :: Integer
        (year, _, _) = toGregorian . utctDay $ time

-- | Returns the interval of summer time for a certain year
--
-- Since 2007 time changes on the second Sundy in March at 02:00 PST to 03:00 PDT
-- and back on the first Sunday in November at 02:00 PDT to 01:00 PST
summerTimeInterval :: Integer -> (UTCTime, UTCTime)
summerTimeInterval year = (start, end)
    where
        start = UTCTime (nthSundayOfMonth 2 year 3) 480
        end = UTCTime (nthSundayOfMonth 1 year 11) 420

        nthSundayOfMonth :: Int -> Integer -> Int -> Day
        nthSundayOfMonth n y m = nthWeekDayOfMonth n Su (y, m)
