module TechnePrelude
    ( -- Module expors
      module BasicPrelude
    , T.pack
    , T.unpack
    -- Functions from modules
    , trace
    , traceId
    -- Defined functions
    , startsLower
    , startsUpper
    , lookupAll
    , tlength
    , tisPrefixOf
    )
    where

import BasicPrelude
import qualified Data.Text as T
import qualified Data.Char as C
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

tlength = T.length
tisPrefixOf = T.isPrefixOf
