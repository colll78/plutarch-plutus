{-# OPTIONS_GHC -Wno-orphans #-}

module Plutarch.LedgerApi.V1 (
  DCert.PDCert (..),
  Credential.PCredential (..),
  Credential.PStakingCredential (..),
  Address.PAddress (..),
  Tx.PTxId (..),
  PTxOut (..),
  PTxInInfo (..),
  Tx.PTxOutRef (..),
  PScriptPurpose (..),
  Crypto.PPubKeyHash (..),
  Time.PPosixTime (..),
  Interval.PExtended (..),
  Interval.PLowerBound (..),
  Interval.PUpperBound (..),
  Interval.PInterval (..),
  Value.PValue (..),
  Value.AmountGuarantees (..),
  Value.PLovelace (..),
  Value.PTokenName (..),
  Value.PCurrencySymbol (..),
  AssocMap.PMap (..),
  AssocMap.KeyGuarantees (..),
  AssocMap.Commutativity (..),
  Scripts.PDatum (..),
  Scripts.PRedeemer (..),
  Scripts.PDatumHash (..),
  Scripts.PRedeemerHash (..),
  Scripts.PScriptHash (..),
) where

import Plutarch.DataRepr (DerivePConstantViaData (DerivePConstantViaData))
import Plutarch.LedgerApi.AssocMap qualified as AssocMap
import Plutarch.LedgerApi.Interval qualified as Interval
import Plutarch.LedgerApi.V1.Address qualified as Address
import Plutarch.LedgerApi.V1.Credential qualified as Credential
import Plutarch.LedgerApi.V1.Crypto qualified as Crypto
import Plutarch.LedgerApi.V1.DCert qualified as DCert
import Plutarch.LedgerApi.V1.Scripts qualified as Scripts
import Plutarch.LedgerApi.V1.Time qualified as Time
import Plutarch.LedgerApi.V1.Tx qualified as Tx
import Plutarch.LedgerApi.Value qualified as Value
import Plutarch.Lift (
  PConstantDecl,
  PUnsafeLiftDecl (PLifted),
 )
import Plutarch.Prelude
import PlutusLedgerApi.V1 qualified as Plutus

-- | @since 3.1.1
newtype PTxOut (s :: S)
  = PTxOut
      ( Term
          s
          ( PDataRecord
              '[ "address" ':= Address.PAddress
               , "value" ':= Value.PValue 'AssocMap.Sorted 'Value.Positive
               , "datumHash" ':= Scripts.PDatumHash
               ]
          )
      )
  deriving stock
    ( -- | @since 3.1.1
      Generic
    )
  deriving anyclass
    ( -- | @since 3.1.1
      PlutusType
    , -- | @since 3.1.1
      PIsData
    , -- | @since 3.1.1
      PEq
    , -- | @since 3.1.1
      PShow
    , -- | @since 3.1.1
      PTryFrom PData
    )

-- | @since 3.1.1
instance DerivePlutusType PTxOut where
  type DPTStrat _ = PlutusTypeData

-- | @since 3.1.1
instance PUnsafeLiftDecl PTxOut where
  type PLifted PTxOut = Plutus.TxOut

-- | @since 3.1.1
deriving via
  (DerivePConstantViaData Plutus.TxOut PTxOut)
  instance
    PConstantDecl Plutus.TxOut

-- | @since 3.1.1
instance PTryFrom PData (PAsData PTxOut)

-- | @since 3.1.1
newtype PTxInInfo (s :: S)
  = PTxInInfo
      ( Term
          s
          ( PDataRecord
              '[ "outRef" ':= Tx.PTxOutRef
               , "resolved" ':= PTxOut
               ]
          )
      )
  deriving stock
    ( -- | @since 3.1.1
      Generic
    )
  deriving anyclass
    ( -- | @since 3.1.1
      PlutusType
    , -- | @since 3.1.1
      PIsData
    , -- | @since 3.1.1
      PEq
    , -- | @since 3.1.1
      PShow
    , -- | @since 3.1.1
      PTryFrom PData
    )

-- | @since 3.1.1
instance DerivePlutusType PTxInInfo where
  type DPTStrat _ = PlutusTypeData

-- | @since 3.1.1
instance PUnsafeLiftDecl PTxInInfo where
  type PLifted PTxInInfo = Plutus.TxInInfo

-- | @since 3.1.1
deriving via
  (DerivePConstantViaData Plutus.TxInInfo PTxInInfo)
  instance
    PConstantDecl Plutus.TxInInfo

-- | @since 3.1.1
instance PTryFrom PData (PAsData PTxInInfo)

-- | @since 3.1.1
data PScriptPurpose (s :: S)
  = PMinting (Term s (PDataRecord '["_0" ':= Value.PCurrencySymbol]))
  | PSpending (Term s (PDataRecord '["_0" ':= Tx.PTxOutRef]))
  | PRewarding (Term s (PDataRecord '["_0" ':= Credential.PStakingCredential]))
  | PCertifying (Term s (PDataRecord '["_0" ':= DCert.PDCert]))
  deriving stock
    ( -- | @since 3.1.1
      Generic
    )
  deriving anyclass
    ( -- | @since 3.1.1
      PlutusType
    , -- | @since 3.1.1
      PIsData
    , -- | @since 3.1.1
      PEq
    , -- | @since 3.1.1
      PShow
    , -- | @since 3.1.1
      PTryFrom PData
    )

-- | @since 3.1.1
instance DerivePlutusType PScriptPurpose where
  type DPTStrat _ = PlutusTypeData

-- | @since 3.1.1
instance PUnsafeLiftDecl PScriptPurpose where
  type PLifted PScriptPurpose = Plutus.ScriptPurpose

-- | @since 3.1.1
deriving via
  (DerivePConstantViaData Plutus.ScriptPurpose PScriptPurpose)
  instance
    PConstantDecl Plutus.ScriptPurpose

-- | @since 3.1.1
instance PTryFrom PData (PAsData PScriptPurpose)
