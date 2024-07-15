{-# OPTIONS_GHC -Wno-orphans #-}

module PlutusLedgerApi.V1.Orphans.Interval () where

import PlutusLedgerApi.V1 qualified as PLA
import PlutusLedgerApi.V1.Orphans.Time ()
import Test.QuickCheck (
  Arbitrary (arbitrary, shrink),
  Arbitrary1 (liftArbitrary, liftShrink),
  CoArbitrary (coarbitrary),
  Function (function),
  elements,
  functionMap,
  oneof,
  variant,
 )

{- | This instance does not bias the constructor choice: it is equally likely to
produce 'PLA.Finite', 'PLA.NegInf' and 'PLA.PosInf'. Bear this in mind when
using: in particular, the instance for 'PLA.Interval' /does not/ make use of
this instance.

@since 1.0.0
-}
instance Arbitrary1 PLA.Extended where
  {-# INLINEABLE liftArbitrary #-}
  liftArbitrary genInner =
    oneof
      [ pure PLA.NegInf
      , PLA.Finite <$> genInner
      , pure PLA.PosInf
      ]
  {-# INLINEABLE liftShrink #-}
  liftShrink shrinkInner = \case
    PLA.NegInf -> []
    PLA.Finite x -> PLA.Finite <$> shrinkInner x
    PLA.PosInf -> []

{- | This makes use of the 'Arbitrary1' instance of 'PLA.Extended' internally,
and thus is subject to the same caveats.

@since 1.0.0
-}
instance Arbitrary a => Arbitrary (PLA.Extended a) where
  {-# INLINEABLE arbitrary #-}
  arbitrary = liftArbitrary arbitrary
  {-# INLINEABLE shrink #-}
  shrink = liftShrink shrink

-- | @since 1.0.0
instance CoArbitrary a => CoArbitrary (PLA.Extended a) where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary = \case
    PLA.NegInf -> variant (0 :: Int)
    PLA.Finite x -> variant (1 :: Int) . coarbitrary x
    PLA.PosInf -> variant (2 :: Int)

-- | @since 1.0.0
instance Function a => Function (PLA.Extended a) where
  {-# INLINEABLE function #-}
  function = functionMap into outOf
    where
      into :: PLA.Extended a -> Maybe (Maybe a)
      into = \case
        PLA.NegInf -> Nothing
        PLA.PosInf -> Just Nothing
        PLA.Finite x -> Just (Just x)
      outOf :: Maybe (Maybe a) -> PLA.Extended a
      outOf = \case
        Nothing -> PLA.NegInf
        Just Nothing -> PLA.PosInf
        Just (Just x) -> PLA.Finite x

{- | This makes use of the 'Arbitrary1' instance of 'PLA.Extended' internally,
and thus is subject to the same caveats. Furthermore, in cases where it makes
sense to talk about open and closed bounds, this instance produces open and
closed bounds with equal probability. Keep these in mind when using this
instance; in particular, the instance for 'PLA.Interval' /does not/ make use
of this instance.

@since 1.0.0
-}
instance Arbitrary (PLA.LowerBound PLA.POSIXTime) where
  {-# INLINEABLE arbitrary #-}
  arbitrary = do
    e <- arbitrary
    case e of
      -- For a finite bound, it makes sense to talk about it being open or
      -- closed.
      PLA.Finite _ -> PLA.LowerBound e <$> arbitrary
      -- If the bound is infinite, it _must_ be open.
      _ -> pure . PLA.LowerBound e $ False
  {-# INLINEABLE shrink #-}
  shrink (PLA.LowerBound e c) = case e of
    PLA.Finite _ -> PLA.LowerBound <$> shrink e <*> shrink c
    -- Negative or positive infinity bounds can't really shrink sensibly
    _ -> []

-- | @since 1.0.0
instance CoArbitrary a => CoArbitrary (PLA.LowerBound a) where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary (PLA.LowerBound e c) = coarbitrary e . coarbitrary c

-- | @since 1.0.0
instance Function a => Function (PLA.LowerBound a) where
  {-# INLINEABLE function #-}
  function = functionMap (\(PLA.LowerBound e c) -> (e, c)) (uncurry PLA.LowerBound)

{- | This makes use of the 'Arbitrary1' instance of 'PLA.Extended' internally,
and thus is subject to the same caveats. Furthermore, in cases where it makes
sense to talk about open and closed bounds, this instance produces open and
closed bounds with equal probability. Keep these in mind when using this
instance; in particular, the instance for 'PLA.Interval' /does not/ make use
of this instance.

@since 1.0.0
-}
instance Arbitrary (PLA.UpperBound PLA.POSIXTime) where
  {-# INLINEABLE arbitrary #-}
  arbitrary = do
    e <- arbitrary
    case e of
      -- For a finite bound, it makes sense to talk about it being open or
      -- closed.
      PLA.Finite _ -> PLA.UpperBound e <$> arbitrary
      -- If the bound is infinite, it _must_ be open.
      _ -> pure . PLA.UpperBound e $ False
  {-# INLINEABLE shrink #-}
  shrink (PLA.UpperBound e c) = case e of
    PLA.Finite _ -> PLA.UpperBound <$> shrink e <*> shrink c
    -- Negative or positive infinity bounds can't really shrink sensibly
    _ -> []

-- | @since 1.0.0
instance CoArbitrary a => CoArbitrary (PLA.UpperBound a) where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary (PLA.UpperBound e c) = coarbitrary e . coarbitrary c

-- | @since 1.0.0
instance Function a => Function (PLA.UpperBound a) where
  {-# INLINEABLE function #-}
  function = functionMap (\(PLA.UpperBound e c) -> (e, c)) (uncurry PLA.UpperBound)

{- | We provide an instance specialized to 'PLA.POSIXTime', rather than a more
general one, as it doesn't make much sense to talk about 'PLA.Interval's of
arbitrary types in general. Furthermore, this is the only instance we
actually use.

This instance aims to fit the following conditions, /in that order/:

1. Ensure that we construct permissible intervals (see
https://github.com/Plutonomicon/cardano-transaction-lib/issues/1041#issuecomment-1280868255);
and
2. The intervals are generated as fairly as possible.

We choose not to shrink intervals, as this is surprisingly complex. In at
least one common case, it's not even possible to write a shrinker that will
ever \'bottom out\', due to infinite bounds being a thing.

@since 1.0.0
-}
instance Arbitrary (PLA.Interval PLA.POSIXTime) where
  {-# INLINEABLE arbitrary #-}
  arbitrary = do
    -- We have four options:
    --
    -- \* [start, +inf)
    -- \* (-inf, end]
    -- \* [start, end)
    -- \* (-inf, +inf)
    lo <-
      oneof
        [ PLA.LowerBound . PLA.Finite <$> arbitrary <*> pure True
        , pure . PLA.LowerBound PLA.NegInf $ False
        ]
    hi <- case lo of
      PLA.LowerBound (PLA.Finite lo') _ -> do
        delta <- arbitrary
        elements
          [ PLA.UpperBound (PLA.Finite (lo' + delta + 1)) False
          , PLA.UpperBound PLA.PosInf False
          ]
      _ ->
        oneof
          [ PLA.UpperBound . PLA.Finite <$> arbitrary <*> pure True
          , pure . PLA.UpperBound PLA.PosInf $ False
          ]
    pure . PLA.Interval lo $ hi

-- | @since 1.0.0
instance CoArbitrary a => CoArbitrary (PLA.Interval a) where
  {-# INLINEABLE coarbitrary #-}
  coarbitrary (PLA.Interval lower upper) = coarbitrary lower . coarbitrary upper

-- | @since 1.0.0
instance Function a => Function (PLA.Interval a) where
  {-# INLINEABLE function #-}
  function = functionMap (\(PLA.Interval lower upper) -> (lower, upper)) (uncurry PLA.Interval)
