module TechnePrelude
    ( -- Module expors
      module BasicPrelude
    , T.pack
    , T.unpack
    -- Classes
    , Semigroup
    -- Functions from modules
    , trace
    , traceId
    , fromJust
    -- Defined functions
    , startsLower
    , startsUpper
    , lookupAll
    , hasNothing
    -- Text stuff
    , tlength
    , tisPrefixOf
    , tnull
    -- Safe
    , headSafe
    , lastSafe
    )
    where

import BasicPrelude
import qualified Data.Text as T
import qualified Data.Char as C
import Data.Semigroup
import Data.Maybe (fromJust)

import Debug.Trace (trace, traceId) -- TODO: Remove in production

startsLower :: Text -> Bool
startsLower = C.isLower . T.head

startsUpper :: Text -> Bool
startsUpper = C.isUpper . T.head

lookupAll :: (Eq a) => a -> [(a,b)] -> [b]
lookupAll _key [] = []
lookupAll  key ((x,y):xys)
    | key == x  =  y : lookupAll key xys
    | otherwise =  lookupAll key xys

hasNothing :: [Maybe a] -> Bool
hasNothing []          = False
hasNothing (Nothing:_) = True
hasNothing (x:xs)      = hasNothing xs

-- ----------------------------------------------------------------------------
-- Text wrappers
-- ----------------------------------------------------------------------------
tlength = T.length
tisPrefixOf = T.isPrefixOf
tnull = T.null

-- ----------------------------------------------------------------------------
-- Safe re-implementations
-- ----------------------------------------------------------------------------
headSafe [] = Nothing
headSafe xs = Just $ head xs

lastSafe [] = Nothing
lastSafe xs = Just $ last xs
