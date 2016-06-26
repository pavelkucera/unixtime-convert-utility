
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import Control.Monad (when)
import Data.Maybe (isNothing, fromJust)
import Data.Time (ZonedTime, utcToLocalZonedTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.DateTime (fromSeconds)
import Prelude (IO, Integer, String, Maybe(..), (/=), (.), length, head)
import System.Environment (getArgs)
import System.Exit (die)
import System.IO (putStrLn, putStr)
import Text.Read (readMaybe)
import PacificTimeZone

main :: IO ()
main = do
    arguments <- getArgs
    when (length arguments /= 1) (die "Exactly one argument is expected.")

    let seconds = parseSeconds (head arguments)
    when (isNothing seconds) (die "Could not parse input as seconds.")

    localTime <- fromSecondsToLocalTime (fromJust seconds)
    let pstTime = toPacificTimeZone localTime

    putStr "LA: "
    displayTime pstTime

    putStr "Local: "
    displayTime localTime

fromSecondsToLocalTime :: Integer -> IO ZonedTime
fromSecondsToLocalTime = utcToLocalZonedTime . fromSeconds

parseSeconds :: String -> Maybe Integer
parseSeconds = readMaybe

displayTime :: ZonedTime -> IO ()
displayTime = putStrLn . printFormat

printFormat :: ZonedTime -> String
printFormat = formatTime defaultTimeLocale "%F %T"
