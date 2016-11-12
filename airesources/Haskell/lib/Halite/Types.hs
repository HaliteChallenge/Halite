module Halite.Types
  ( Direction(..)
  , Map(..)
  , Location(..)
  , Site(..)
  , Move(..)
  , ID
  ) where

data Direction
    = Still
    | North
    | East
    | South
    | West
     deriving (Show, Eq, Ord, Enum)

data Location = Location
    { locX :: Int
    , locY :: Int
    } deriving (Show, Eq)

data Site = Site
    { siteOwner :: Int
    , siteStrength :: Int
    , siteProduction :: Int
    } deriving (Show, Eq)

data Move = Move
    { moveLocation :: Location
    , moveDirection :: Direction
    } deriving (Show, Eq)

data Map = Map
    { mapWidth :: Int
    , mapHeight :: Int
    , mapContents :: [[Site]]
    } deriving (Show, Eq)

type ID = Int
