module TechnePrelude
    ( -- Module expors
    module BP
    -- Classes
    , Semigroup
    -- Functions from modules
    , trace
    , traceId
    , fromJust
    , fromRight
    , fromLeft
    , foldlM
    , foldrM
    , groom
    -- Defined functions
    , startsLower
    , startsUpper
    , lookupAll
    , hasNothing
    -- Text stuff
    , tpack
    , tunpack
    , tlength
    , tisPrefixOf
    , tisInfixOf
    , tnull
    , tisUpperFirst
    , tcons
    , tfilter
    , tgroom
    -- String stuff
    , swords
    , sstrip
    -- Safe
    , headSafe
    , lastSafe
    -- Utility
    , fromRight'
    , fromLeft'
    -- bifunctor
    , Bi.bimap
    , Bi.first
    , Bi.second
    )
    where

import BasicPrelude as BP hiding (first, second)
import qualified Data.Text as T
import qualified Data.Char as C
import qualified Data.String as S
import Data.Semigroup
import Data.Maybe (fromJust)
import Data.Either (fromRight, fromLeft)
import Data.Foldable (foldlM, foldrM)
import Text.Groom (groom)
import qualified Data.Bifunctor as Bi
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

tpack = T.pack
tunpack = T.unpack
tlength = T.length
tisPrefixOf = T.isPrefixOf
tisInfixOf = T.isInfixOf
tnull = T.null
tisUpperFirst t = not (tnull t) && C.isUpper (T.head t)
tcons = T.cons
tfilter = T.filter

tgroom :: Show a => a -> Text
tgroom = tpack . groom

-- ----------------------------------------------------------------------------
-- String wrappers
-- ----------------------------------------------------------------------------

swords = S.words
sstrip  = tunpack . T.strip . tpack

-- ----------------------------------------------------------------------------
-- Safe re-implementations
-- ----------------------------------------------------------------------------

headSafe [] = Nothing
headSafe xs = Just $ head xs

lastSafe [] = Nothing
lastSafe xs = Just $ last xs

-- ----------------------------------------------------------------------------
-- Utility functions
-- ----------------------------------------------------------------------------

fromRight' (Right x) = x
fromLeft' (Left x) = x
