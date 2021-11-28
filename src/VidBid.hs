{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module VidBid
    (
    GameStateMachineSchema
    , contract
    ) where

import           Control.Monad                hiding (fmap)
import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Monoid                  (Last (..))
import           Data.Text                    (Text, pack)
import qualified Data.ByteString.Char8        as C

import           GHC.Generics                 (Generic)
import           Ledger                       hiding (singleton)
import           Ledger.Ada                   as Ada
import           Ledger.Constraints           as Constraints
import           Ledger.Typed.Tx
import qualified Ledger.Typed.Scripts         as Scripts
import qualified Ledger.Value                 as V

import           Plutus.Contract              as Contract
import           Plutus.Contract.StateMachine as SM
import qualified PlutusTx
import qualified PlutusTx.Prelude
import           Prelude                      (Semigroup (..), Show (..), String)
import           PlutusTx.Prelude              hiding (pure, (<$>))
import           Data.Aeson                   (FromJSON, ToJSON)
import           GHC.Generics                 (Generic)
import           Schema                       (ToSchema)
import qualified Prelude

newtype HashedString = HashedString BuiltinByteString deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData, Show, FromJSON, ToJSON, ToSchema)

PlutusTx.makeLift ''HashedString

newtype ClearString = ClearString BuiltinByteString deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData, Show, FromJSON, ToJSON, ToSchema)

PlutusTx.makeLift ''ClearString

hashString :: String -> HashedString
hashString = HashedString . sha2_256 . toBuiltin . C.pack

-- create a redeemer script for the guessing game by lifting the
-- string to its on-chain representation
clearString :: String -> ClearString
clearString = ClearString . toBuiltin . C.pack

-- | State of the guessing game
data GameState =
    Initialised HashedString
    -- ^ Initial state. In this state only the 'MintTokens' action is allowed.
    | Locked HashedString
    -- ^ Funds have been locked. In this state only the 'Guess' action is
    --   allowed.
    | Finished
    -- ^ All funds were unlocked.
    deriving stock (Prelude.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)
PlutusTx.unstableMakeIsData ''GameState
PlutusTx.makeLift ''GameState


-- | Inputs (actions)
data GameInput =
    Lock HashedString
    | Guess ClearString
    -- ^ Make a guess, extract the funds, and lock the remaining funds using a
    --   new secret word.
    deriving stock (Prelude.Show, Generic)
    deriving anyclass (ToJSON, FromJSON)
PlutusTx.unstableMakeIsData ''GameInput
PlutusTx.makeLift ''GameInput

{-# INLINABLE transition #-}
transition :: State GameState -> GameInput -> Maybe (TxConstraints Void Void, State GameState)
transition State{stateData=oldData, stateValue=oldValue} input = case (oldData, input) of
    _ -> Nothing


checkGuess :: HashedString -> ClearString -> Bool
checkGuess (HashedString actual) (ClearString guess') = actual == sha2_256 guess'


type GameStateMachine = SM.StateMachine GameState GameInput

{-# INLINABLE machine #-}
machine :: StateMachine GameState GameInput
machine = SM.mkStateMachine Nothing transition isFinal where
    isFinal Finished = True
    isFinal _        = False

{-# INLINABLE mkVidBidValidator #-}
mkVidBidValidator :: Scripts.ValidatorType GameStateMachine
mkVidBidValidator = SM.mkValidator machine

typedVidbidValidator :: Scripts.TypedValidator GameStateMachine
typedVidbidValidator = Scripts.mkTypedValidator @GameStateMachine
    $$(PlutusTx.compile [|| mkVidBidValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator
mintingPolicy :: Scripts.MintingPolicy
mintingPolicy = Scripts.forwardingMintingPolicy typedVidbidValidator

client :: SM.StateMachineClient GameState GameInput
client = SM.mkStateMachineClient $ SM.StateMachineInstance machine typedVidbidValidator

data LockArgs = LockArgs
    { lockArgsSecret :: String
    , lockArgsValue  :: Value
    }
    deriving stock ( Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

data GuessArgs = GuessArgs
    {  guessArgsNewSecret     :: String
    }
    deriving stock ( Show, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)


-- | The @"guess"@ endpoint.
guess :: ( AsContractError e
             , AsSMContractError e
             ) => Promise () GameStateMachineSchema e ()
guess = endpoint @"guess" @GuessArgs $ \(GuessArgs guess ) -> do
    let guessClearString = clearString guess
    void
        $ SM.runStep client
            (Guess guessClearString)

lock :: ( AsContractError e
            , AsSMContractError e
            ) => Promise () GameStateMachineSchema e ()
lock = endpoint @"lock" @LockArgs $ \(LockArgs guess lockValue) -> do
    let secret = hashString guess
    void $ SM.runInitialise client (Initialised secret) mempty


-- | The schema of the contract. It consists of the two endpoints @"lock"@
--   and @"guess"@ with their respective argument types.
type GameStateMachineSchema =
        Endpoint "lock" LockArgs
        .\/ Endpoint "guess" GuessArgs

contract :: ( AsContractError e
                 , AsSMContractError e
                 ) => Contract () GameStateMachineSchema e ()
contract = do
    selectList [lock, guess] >> contract


