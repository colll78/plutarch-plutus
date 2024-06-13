{-# OPTIONS_GHC -Wno-orphans #-}

-- Mirrors the equivalent V1 module in plutus-ledger-api
module Plutarch.LedgerApi.Address (
  PAddress (..),
) where

import Plutarch.DataRepr (
  DerivePConstantViaData (DerivePConstantViaData),
  PDataFields,
 )
import Plutarch.LedgerApi.Credential (PCredential, PStakingCredential)
import Plutarch.LedgerApi.Utils (PMaybeData)
import Plutarch.Lift (
  PConstantDecl,
  PUnsafeLiftDecl (PLifted),
 )
import Plutarch.Prelude
import PlutusLedgerApi.V3 qualified as Plutus

-- | @since 2.0.0
newtype PAddress (s :: S)
  = PAddress
      ( Term
          s
          ( PDataRecord
              '[ "credential" ':= PCredential
               , "stakingCredential" ':= PMaybeData PStakingCredential
               ]
          )
      )
  deriving stock
    ( -- | @since 2.0.0
      Generic
    )
  deriving anyclass
    ( -- | @since 2.0.0
      PlutusType
    , -- | @since 2.0.0
      PIsData
    , -- | @since 2.0.0
      PDataFields
    , -- | @since 2.0.0
      PEq
    , -- | @since 2.0.0
      PPartialOrd
    , -- | @since 2.0.0
      POrd
    , -- | @since 2.0.0
      PShow
    , -- | @since 2.0.0
      PTryFrom PData
    )

-- | @since 2.0.0
instance DerivePlutusType PAddress where
  type DPTStrat _ = PlutusTypeData

-- | @since 2.0.0
instance PUnsafeLiftDecl PAddress where
  type PLifted PAddress = Plutus.Address

-- | @since 2.0.0
deriving via
  (DerivePConstantViaData Plutus.Address PAddress)
  instance
    PConstantDecl Plutus.Address

-- | @since 2.0.0
instance PTryFrom PData (PAsData PAddress)
