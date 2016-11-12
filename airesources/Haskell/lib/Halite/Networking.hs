module Halite.Networking
  ( getInit
  , sendInit
  , getFrame
  , sendFrame
  ) where

import Halite.Types

getInit :: IO (ID, Map)
getInit = do
    userID <- read <$> getLine
    [w, h] <- map read . words <$> getLine
    prod <- map read . words <$> getLine
    mapc <- parseMapContents (w, h) prod <$> getLine
    return (userID, Map w h mapc)

sendInit :: String -> IO ()
sendInit = putStrLn

getFrame :: Map -> IO Map
getFrame (Map w h cont) =
    Map w h . parseMapContents (w, h) (getProds cont) <$> getLine

sendFrame :: [Move] -> IO ()
sendFrame = putStrLn . unwords . map showMove


parseMapContents :: (Int, Int) -> [Int] -> String -> [[Site]]
parseMapContents (w, h) pds s = splitEvery w $ zipWith3 Site ons sts pds
  where
    (owners, sts) = splitMapContents (read <$> words s) (w * h)
    ons = stretch owners
    stretch (x:y:ys) = replicate x y ++ stretch ys
    stretch [] = []

splitMapContents :: [Int] -> Int -> ([Int], [Int])
splitMapContents xs area = splitAt (length xs - area) xs

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n xs = first : splitEvery n rest
  where
    (first, rest) = splitAt n xs

getProds :: [[Site]] -> [Int]
getProds = map siteProduction . concat

showMove :: Move -> String
showMove (Move (Location x y) d) =
    show x ++ " " ++ show y ++ " " ++ show (fromEnum d)
