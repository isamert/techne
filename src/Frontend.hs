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
    , updateFnDefs
    , getFnSignature
    , getFnReturnType
    -- Helpers
    , testParser
    ) where

import TechnePrelude
import Frontend.AST

import Data.Void (Void)
import Text.Megaparsec (Parsec, ParseErrorBundle, parseTest)
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

-- | Add `def` to function definitions of TState
updateFnDefs :: FnDef -> TParser ()
updateFnDefs def = do
    hasFn <- hasFn (fnDefName def)
    when hasFn (fail "More than one definition for same function.")
    state <- get
    void . put $ state { stateFnDefs = def : stateFnDefs state }

getFnSignature :: Name -> TParser (Maybe FnSignature)
getFnSignature fnname = do
    state <- get
    return $ fmap fnDefSignature . headSafe . filter
      (\(FnDef name sig) -> name == fnname) $ stateFnDefs state

getFnReturnType :: Name -> TParser (Maybe Type)
getFnReturnType fnname = (lastSafe =<<) <$> getFnSignature fnname

-- ----------------------------------------------------------------------------
-- Helper functions
-- ----------------------------------------------------------------------------
testParser p = parseTest (runStateT p initTState)
