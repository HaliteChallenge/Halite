module Halite.Networking
  ( getInit
  , sendInit
  , getFrame
  , sendMoves
  ) where

import Halite.Types

getInit :: IO (ID, Map)
getInit = do
    userID <- read <$> getLine
    [w, h] <- map read . words <$> getLine
    prod <- parseProductionMap (w, h) <$> getLine
    mapc <- parseMapContents (w, h) prod <$> getLine
    return (userID, Map w h mapc)

sendInit :: String -> IO ()
sendInit = undefined

getFrame :: IO Map
getFrame = undefined

sendMoves :: [Move] -> IO ()
sendMoves = undefined

parseProductionMap :: (Int, Int) -> String -> [[Int]]
parseProductionMap = undefined

parseMapContents :: (Int, Int) -> [[Int]] -> String -> [[Site]]
parseMapContents = undefined
