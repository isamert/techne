module Frontend
    ( -- Re-exports
    runStateT
    , get
    , put
    -- Types
    , TState (..)
    , TParser (..)
    , TErrorBundle (..)
    -- Functions
    , initTState
    , hasFn
    , getFnSignature
    , getFnReturnType
    ) where

import TechnePrelude
import Frontend.AST

import Data.Void (Void)
import Text.Megaparsec (Parsec, ParseErrorBundle)
import Control.Monad.State.Lazy (StateT, runStateT, get, put)

-- ----------------------------------------------------------------------------
-- Types
-- ----------------------------------------------------------------------------
newtype TState = TState { stateFnDefs :: [FnDef] } deriving (Show, Eq)
type TParser a = StateT TState (Parsec Void Text) a
type TErrorBundle = ParseErrorBundle Text Void

-- ----------------------------------------------------------------------------
-- State related functions
-- ----------------------------------------------------------------------------
initTState = TState []

hasFn :: Name -> TParser Bool
hasFn fnname = do
    state <- get
    return $ fnname `elem` map fnDefName (stateFnDefs state)

getFnSignature :: Name -> TParser (Maybe FnSignature)
getFnSignature fnname = do
    state <- get
    return $ fmap fnDefSignature . headSafe . filter
      (\(FnDef name sig) -> name == fnname) $ stateFnDefs state

getFnReturnType :: Name -> TParser (Maybe Type)
getFnReturnType fnname = (lastSafe =<<) <$> getFnSignature fnname
