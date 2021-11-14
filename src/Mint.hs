{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module Mint ( policy
             , curSymbol
             , mintNFT
             , NFTSchema)
                where

import           Control.Monad          hiding (fmap)
import           Data.Text              (Text)
import           Data.Void              (Void)
import           Plutus.Contract        as Contract
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (mint, singleton)
import           Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Value           as Value
import           Prelude                (IO, Semigroup (..), Show (..), String)
import           Text.Printf            (printf)

{-# INLINABLE mkPolicy #-}
mkPolicy :: TokenName -> () -> ScriptContext -> Bool
mkPolicy tn () ctx =   traceIfFalse "wrong amount minted" checkMintedAmount
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    checkMintedAmount :: Bool
    checkMintedAmount = case flattenValue (txInfoMint  info) of
        [(cs, tn', amt)] -> cs  == ownCurrencySymbol ctx && tn' == tn && amt == 1
        _                -> False

policy :: TokenName -> Scripts.MintingPolicy
policy tn = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \tn' -> Scripts.wrapMintingPolicy $ mkPolicy tn' ||])
    `PlutusTx.applyCode`
    PlutusTx.liftCode tn

curSymbol :: TokenName -> CurrencySymbol
curSymbol tn = scriptCurrencySymbol $ policy tn

type NFTSchema = Endpoint "mint" TokenName

mint :: TokenName -> Contract w NFTSchema Text ()
mint tn = do
    pkh    <- Contract.ownPubKeyHash
    let val     = Value.singleton (curSymbol tn) tn 1
        lookups = Constraints.mintingPolicy (policy tn)
        tx      = Constraints.mustMintValue val
    ledgerTx <- submitTxConstraintsWith @Void lookups tx
    _ <- awaitTxConfirmed (getCardanoTxId ledgerTx)


    Contract.logInfo @String $ printf "forged %s" (show val)

mint' :: Promise () NFTSchema Text ()
mint' = endpoint @"mint" mint

mintNFT :: AsContractError e => Contract () NFTSchema Text e
mintNFT = do
    selectList [mint'] >> mintNFT