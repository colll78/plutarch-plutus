{-# LANGUAGE PolyKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{- | This module is meant to be imported qualified, as some of its identifiers
clash with the Plutarch prelude, as well as other parts of the Plutarch API.

= Important note

While this module suggests that we can construct any intervals we like, Plutus
does not seem to agree (see [this issue](https://github.com/Plutonomicon/cardano-transaction-lib/issues/1041#issuecomment-1280868255)).
 As a result, the /only/ intervals that are guaranteed to work have the following
forms:

* @[start, +inf)@
* @[start, end)@
* @(-inf, end]@
* @(-inf, +inf)@

Construction of, and use of, any intervals other than these is likely to
cause bugs and issues, and must be avoided. The functions in this module
assume that only these kinds of arguments will be given to them, and can
silently produce wrong results if not.

If you want to check if any given 'PInterval' makes sense given the above,
use 'pcheckInterval', which will error if given a 'PInterval' not in one
of the forms above.
-}
module Plutarch.LedgerApi.Interval (
  -- * Types
  PInterval (..),
  PLowerBound (..),
  PUpperBound (..),
  PExtended (..),

  -- * Functions

  -- ** Creation
  psingleton,
  pfrom,
  pto,
  palways,
  pinterval,
  pintervalOpenEnd,

  -- ** Queries
  pcheckInterval,
  pmember,
  pcontains,
  pbefore,
  pafter,

  -- ** Transformation
  phull,
  pintersection,
) where

import Plutarch.Bool (pif')
import Plutarch.DataRepr (
  DerivePConstantViaData (DerivePConstantViaData),
  PDataFields,
 )
import Plutarch.Lift (
  PConstantDecl (PConstanted),
  PUnsafeLiftDecl (PLifted),
 )
import Plutarch.Prelude hiding (psingleton, pto)
import PlutusLedgerApi.V3 qualified as Plutus

-- | @since 2.0.0
newtype PInterval (a :: S -> Type) (s :: S)
  = PInterval
      ( Term
          s
          ( PDataRecord
              '[ "from" ':= PLowerBound a
               , "to" ':= PUpperBound a
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
    )

-- | @since 2.0.0
instance DerivePlutusType (PInterval a) where
  type DPTStrat _ = PlutusTypeData

-- | @since 2.0.0
instance
  PLiftData a =>
  PUnsafeLiftDecl (PInterval a)
  where
  type PLifted (PInterval a) = (Plutus.Interval (PLifted a))

-- | @since 2.0.0
deriving via
  (DerivePConstantViaData (Plutus.Interval a) (PInterval (PConstanted a)))
  instance
    PConstantData a =>
    PConstantDecl (Plutus.Interval a)

-- | @since 3.1.0
instance PTryFrom PData a => PTryFrom PData (PInterval a)

-- | @since 3.1.0
instance PTryFrom PData a => PTryFrom PData (PAsData (PInterval a))

-- | @since 2.0.0
newtype PLowerBound (a :: S -> Type) (s :: S)
  = PLowerBound
      ( Term
          s
          ( PDataRecord
              '[ "_0" ':= PExtended a
               , "_1" ':= PBool
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
    )

-- | @since 2.0.0
instance DerivePlutusType (PLowerBound a) where
  type DPTStrat _ = PlutusTypeData

-- | @since 2.0.0
instance
  PLiftData a =>
  PUnsafeLiftDecl (PLowerBound a)
  where
  type PLifted (PLowerBound a) = (Plutus.LowerBound (PLifted a))

-- | @since 2.0.0
deriving via
  (DerivePConstantViaData (Plutus.LowerBound a) (PLowerBound (PConstanted a)))
  instance
    PConstantData a =>
    PConstantDecl (Plutus.LowerBound a)

-- | @since 3.1.0
instance PTryFrom PData a => PTryFrom PData (PLowerBound a)

-- | @since 3.1.0
instance PTryFrom PData a => PTryFrom PData (PAsData (PLowerBound a))

-- | @since 2.0.0
newtype PUpperBound (a :: S -> Type) (s :: S)
  = PUpperBound
      ( Term
          s
          ( PDataRecord
              '[ "_0" ':= PExtended a
               , "_1" ':= PBool
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
    )

-- | @since 2.0.0
instance DerivePlutusType (PUpperBound a) where
  type DPTStrat _ = PlutusTypeData

-- | @since 2.0.0
instance
  PLiftData a =>
  PUnsafeLiftDecl (PUpperBound a)
  where
  type PLifted (PUpperBound a) = (Plutus.UpperBound (PLifted a))

-- | @since 2.0.0
deriving via
  (DerivePConstantViaData (Plutus.UpperBound a) (PUpperBound (PConstanted a)))
  instance
    PConstantData a =>
    PConstantDecl (Plutus.UpperBound a)

-- | @since 3.1.0
instance PTryFrom PData a => PTryFrom PData (PUpperBound a)

-- | @since 3.1.0
instance PTryFrom PData a => PTryFrom PData (PAsData (PUpperBound a))

-- | @since 2.0.0
data PExtended (a :: S -> Type) (s :: S)
  = PNegInf (Term s (PDataRecord '[]))
  | PFinite (Term s (PDataRecord '["_0" ':= a]))
  | PPosInf (Term s (PDataRecord '[]))
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
      PEq
    , -- | @since 2.0.0
      PPartialOrd
    , -- | @since 2.0.0
      POrd
    , -- | @since 2.0.0
      PShow
    )

-- | @since 2.0.0
instance DerivePlutusType (PExtended a) where
  type DPTStrat _ = PlutusTypeData

-- | @since 3.1.0
instance
  PLiftData a =>
  PUnsafeLiftDecl (PExtended a)
  where
  type PLifted (PExtended a) = Plutus.Extended (PLifted a)

-- | @since 3.1.0
deriving via
  (DerivePConstantViaData (Plutus.Extended a) (PExtended (PConstanted a)))
  instance
    PConstantData a =>
    PConstantDecl (Plutus.Extended a)

-- | @since 3.1.0
instance PTryFrom PData a => PTryFrom PData (PExtended a)

-- | @since 3.1.0
instance PTryFrom PData a => PTryFrom PData (PAsData (PExtended a))

{- | Check if a value is inside the given interval.

@since 2.1.1
-}
pmember ::
  forall (a :: S -> Type) (s :: S).
  (POrd a, PIsData a) =>
  Term
    s
    ( PAsData a
        :--> PInterval a
        :--> PBool
    )
pmember = phoistAcyclic $ plam $ \x i -> pcontains # i # (psingleton # x)

{- | @'pcontains' # x # y@ is true if the interval @y@ is entirely contained in @a@.

@since 2.1.1
-}
pcontains ::
  forall (a :: S -> Type) (s :: S).
  (POrd a, PIsData a) =>
  Term
    s
    ( PInterval a
        :--> PInterval a
        :--> PBool
    )
pcontains = phoistAcyclic $
  plam $ \x' y' -> unTermCont $ do
    x <- tcont $ pletFields @'["from", "to"] x'
    y <- tcont $ pletFields @'["from", "to"] y'
    let lowerX = getField @"from" x
    let upperX = getField @"to" x
    let lowerY = getField @"from" y
    let upperY = getField @"to" y
    pure $ leqP # (lToE # lowerX) # (lToE # lowerY) #&& leqP # (uToE # upperY) # (uToE # upperX)

{- | Given @x@, create the interval @[x, x]@.

@since 2.1.1
-}
psingleton ::
  forall (a :: S -> Type) (s :: S).
  Term s (PAsData a :--> PInterval a)
psingleton = phoistAcyclic $
  plam $ \x ->
    plet (pcon $ PFinite $ pdcons @"_0" # x # pdnil) $ \start ->
      pclosedInterval # start # start

{- | Given @x@, create the interval @[x, +infty)@

@since 2.1.1
-}
pfrom ::
  forall (a :: S -> Type) (s :: S).
  Term s (PAsData a :--> PInterval a)
pfrom = phoistAcyclic $
  plam $ \a ->
    let start = pcon $ PFinite $ pdcons @"_0" # a # pdnil
        end = pcon $ PPosInf pdnil
     in pclosedInterval # start # end

{- | Given @x@, create the interval @(-infty, x]@.

@since 2.1.1
-}
pto ::
  forall (a :: S -> Type) (s :: S).
  Term s (PAsData a :--> PInterval a)
pto = phoistAcyclic $
  plam $ \a ->
    let start = pcon $ PNegInf pdnil
        end = pcon $ PFinite $ pdcons @"_0" # a # pdnil
     in pclosedInterval # start # end

-- TODO: Rename this, as this name is too specific to slots.

{- | Create the interval @(-infty, +infty)@.

@since 2.1.1
-}
palways ::
  forall (a :: S -> Type) (s :: S).
  PLiftData a =>
  Term s (PInterval a)
palways = pconstant Plutus.always

{- | @'phull' i1 i2@ gives the smallest interval that contains both @i1@ and
@i2@.

@since 2.1.1
-}
phull ::
  forall (a :: S -> Type) (s :: S).
  (POrd a, PIsData a) =>
  Term
    s
    ( PInterval a
        :--> PInterval a
        :--> PInterval a
    )
phull = phoistAcyclic $
  plam $ \x' y' -> unTermCont $ do
    x <- tcont $ pletFields @'["from", "to"] x'
    y <- tcont $ pletFields @'["from", "to"] y'
    let lowerX = getField @"from" x
    let upperX = getField @"to" x
    let lowerY = getField @"from" y
    let upperY = getField @"to" y
    let lower = pcon $ PLowerBound $ minP # (lToE # lowerX) # (lToE # lowerY)
    let upper = pcon $ PUpperBound $ maxP # (uToE # upperX) # (uToE # upperY)
    pure $ pinterval' # pdata lower # pdata upper

{- | @'pintersection' i1 i2@ gives the largest interval that is contained in
both @i1@ and @i2@.

@since 2.1.1
-}
pintersection ::
  forall (a :: S -> Type) (s :: S).
  (POrd a, PIsData a) =>
  Term
    s
    ( PInterval a
        :--> PInterval a
        :--> PInterval a
    )
pintersection = phoistAcyclic $
  plam $ \x' y' -> unTermCont $ do
    x <- tcont $ pletFields @'["from", "to"] x'
    y <- tcont $ pletFields @'["from", "to"] y'
    let lowerX = getField @"from" x
    let upperX = getField @"to" x
    let lowerY = getField @"from" y
    let upperY = getField @"to" y
    let lower = pcon $ PLowerBound $ maxP # (lToE # lowerX) # (lToE # lowerY)
    let upper = pcon $ PUpperBound $ minP # (uToE # upperX) # (uToE # upperY)
    pure $ pinterval' # pdata lower # pdata upper

{- | @'before' x i@ is true if @x@ is earlier than the start of @i@.

@since 2.1.1
-}
pbefore ::
  forall (a :: S -> Type) (s :: S).
  (POrd a, PIsData a) =>
  Term
    s
    ( a
        :--> PInterval a
        :--> PBool
    )
pbefore = phoistAcyclic $
  plam $ \a y ->
    let lower = pfield @"from" # y
     in pbefore' # a # (lToE # lower)

{- | @'after' x u@ is true if @x@ is later than the end of @i@.

@since 2.1.1
-}
pafter ::
  forall (a :: S -> Type) (s :: S).
  (POrd a, PIsData a) =>
  Term
    s
    ( a
        :--> PInterval a
        :--> PBool
    )
pafter = phoistAcyclic $
  plam $ \a y ->
    let upper = pfield @"to" # y
     in pafter' # a # (uToE # upper)

{- | @'pinterval' x y@ creates the interval @[x, y]@.

@since 2.1.1
-}
{-# DEPRECATED pinterval "Use pintervalOpenEnd" #-}
pinterval ::
  forall (a :: S -> Type) (s :: S).
  Term
    s
    ( PAsData a
        :--> PAsData a
        :--> PInterval a
    )
pinterval = phoistAcyclic $
  plam $ \x y ->
    let start = pcon $ PFinite $ pdcons @"_0" # x # pdnil
        end = pcon $ PFinite $ pdcons @"_0" # y # pdnil
     in pclosedInterval # start # end

{- | As 'pinterval', except the endpoint is open.

@since 3.2.2
-}
pintervalOpenEnd ::
  forall (a :: S -> Type) (s :: S).
  Term s (PAsData a :--> PAsData a :--> PInterval a)
pintervalOpenEnd = phoistAcyclic $
  plam $ \x y ->
    let start = pcon $ PFinite $ pdcons @"_0" # x # pdnil
        end = pcon $ PFinite $ pdcons @"_0" # y # pdnil
        startClosure = pconstantData True
        endClosure = pconstantData False
        lower = pcon . PLowerBound $ pdcons @"_0" # pdata start #$ pdcons @"_1" # startClosure # pdnil
        upper = pcon . PUpperBound $ pdcons @"_0" # pdata end #$ pdcons @"_1" # endClosure # pdnil
     in pcon . PInterval $ pdcons @"from" # pdata lower #$ pdcons @"to" # pdata upper # pdnil

{- | Checks that we have one of the following:

* `[start, +inf)`
* `(-inf, end]`
* `[start, end)`
* `(-inf, +inf)`

@since 3.2.2
-}
pcheckInterval ::
  forall (a :: S -> Type) (s :: S).
  Term s (PInterval a) ->
  Term s PUnit
pcheckInterval i' = unTermCont $ do
  i <- tcont $ pletFields @'["from", "to"] i'
  start <- tcont $ pletFields @'["_0", "_1"] $ getField @"from" i
  end <- tcont $ pletFields @'["_0", "_1"] $ getField @"to" i
  isCase1 <- tcont $ \k ->
    pif
      (getField @"_1" start)
      -- Case 1: start is closed.
      -- Check that it's finite, yield PTrue
      ( pmatch (getField @"_0" start) $ \case
          PFinite _ -> k (pcon PTrue)
          _ -> ptraceInfoError "interval error: closed start not finite"
      )
      -- Case 2: start is open.
      -- Check that it's -inf, yield PFalse
      ( pmatch (getField @"_0" start) $ \case
          PNegInf _ -> k (pcon PFalse)
          _ -> ptraceInfoError "interval error: open start not -inf"
      )
  tcont $ \k ->
    pif
      isCase1
      -- End must be open, and either finite or +inf
      ( pif
          (getField @"_1" end)
          -- End is closed, fail
          (ptraceInfoError "interval error: closed start and end")
          -- End is open, check for form
          ( pmatch (getField @"_0" end) $ \case
              PNegInf _ -> ptraceInfoError "interval error: -inf end"
              _ -> k (pconstant ())
          )
      )
      -- End must be either closed and finite, or open and +inf
      ( pif
          (getField @"_1" end)
          -- End is closed, check if it's finite
          ( pmatch (getField @"_0" end) $ \case
              PFinite _ -> k (pconstant ())
              _ -> ptraceInfoError "interval error: closed end not finite"
          )
          -- End is open, check if it's +inf
          ( pmatch (getField @"_0" end) $ \case
              PPosInf _ -> k (pconstant ())
              _ -> ptraceInfoError "interval error: open end not +inf"
          )
      )

-- Helpers

-- closed interval from PExtended
pclosedInterval ::
  forall (a :: S -> Type) (s :: S).
  Term
    s
    ( PExtended a
        :--> PExtended a
        :--> PInterval a
    )
pclosedInterval = phoistAcyclic $
  plam $ \start end ->
    let closure = pconstantData True
        upper =
          pcon $
            PUpperBound $
              pdcons @"_0"
                # pdata end
                #$ pdcons @"_1"
                # closure
                # pdnil
        lower =
          pcon $
            PLowerBound $
              pdcons @"_0"
                # pdata start
                #$ pdcons @"_1"
                # closure
                # pdnil
     in pinterval' # pdata lower # pdata upper

--  interval from upper and lower
pinterval' ::
  forall (a :: S -> Type) (s :: S).
  Term
    s
    ( PAsData (PLowerBound a)
        :--> PAsData (PUpperBound a)
        :--> PInterval a
    )
pinterval' = phoistAcyclic $
  plam $ \lower upper ->
    pcon $
      PInterval $
        pdcons @"from"
          # lower
          #$ pdcons @"to"
          # upper
          # pdnil

leqP ::
  forall (a :: S -> Type) (s :: S).
  (PIsData a, POrd a) =>
  Term
    s
    ( PDataRecord '["_0" ':= PExtended a, "_1" ':= PBool]
        :--> PDataRecord '["_0" ':= PExtended a, "_1" ':= PBool]
        :--> PBool
    )
leqP = phoistAcyclic $
  plam $ \x' y' -> unTermCont $ do
    x <- tcont $ pletFields @'["_0", "_1"] x'
    y <- tcont $ pletFields @'["_0", "_1"] y'
    let xt = getField @"_0" x
    let yt = getField @"_0" y
    let xc = getField @"_1" x
    let yc = getField @"_1" y
    pure $
      pif
        (xc #&& yc #|| (pnot # xc) #&& (pnot # yc))
        (leqE # xt # yt)
        (ltE # xt # yt)

leqE ::
  forall (a :: S -> Type) (s :: S).
  (POrd a, PIsData a) =>
  Term
    s
    ( PExtended a
        :--> PExtended a
        :--> PBool
    )
leqE = phoistAcyclic $ plam $ \x y -> ltE # x # y #|| eqE # x # y

ltE ::
  forall (a :: S -> Type) (s :: S).
  (POrd a, PIsData a) =>
  Term
    s
    ( PExtended a
        :--> PExtended a
        :--> PBool
    )
ltE = phoistAcyclic $ plam $ \x y -> pmatch x (cont y)
  where
    cont ::
      forall (s' :: S).
      Term s' (PExtended a) ->
      PExtended a s' ->
      Term s' PBool
    cont y' = \case
      PNegInf _ -> pconstant True
      PPosInf _ -> pmatch y' isPosInf
      PFinite l ->
        let z = pfromData $ pfield @"_0" # l
         in pmatch y' (ltE' z)

isPosInf ::
  forall (a :: S -> Type) (s :: S).
  PExtended a s ->
  Term s PBool
isPosInf = \case
  PPosInf _ -> pconstant True
  _ -> pconstant False

ltE' ::
  forall (a :: S -> Type) (s :: S).
  (POrd a, PIsData a) =>
  Term s a ->
  PExtended a s ->
  Term s PBool
ltE' x = \case
  PNegInf _ -> pconstant False
  PPosInf _ -> pconstant True
  PFinite r -> x #< pfield @"_0" # r

lToE ::
  forall (a :: S -> Type) (s :: S).
  Term s (PLowerBound a :--> PDataRecord '["_0" ':= PExtended a, "_1" ':= PBool])
lToE = phoistAcyclic $ plam $ \x -> pmatch x (\(PLowerBound a) -> a)

uToE ::
  forall (a :: S -> Type) (s :: S).
  Term s (PUpperBound a :--> PDataRecord '["_0" ':= PExtended a, "_1" ':= PBool])
uToE = phoistAcyclic $ plam $ \x -> pmatch x (\(PUpperBound a) -> a)

eqE ::
  forall (a :: S -> Type) (s :: S).
  (PEq a, PIsData a) =>
  Term
    s
    ( PExtended a
        :--> PExtended a
        :--> PBool
    )
eqE = phoistAcyclic $
  plam $ \x y ->
    let cont x' = case x' of
          PNegInf _ -> pmatch y isNegInf
          PPosInf _ -> pmatch y isPosInf
          PFinite l -> pmatch y (eqE' (pfield @"_0" # l))
     in pmatch x cont

isNegInf ::
  forall (a :: S -> Type) (s :: S).
  PExtended a s ->
  Term s PBool
isNegInf = \case
  PNegInf _ -> pconstant True
  _ -> pconstant False

eqE' ::
  forall (a :: S -> Type) (s :: S).
  (PEq a, PIsData a) =>
  Term s a ->
  PExtended a s ->
  Term s PBool
eqE' a y' = case y' of
  PFinite r -> unTermCont $ do
    b <- tcont $ plet $ pfield @"_0" # r
    pure $ a #== b
  _ -> pconstant False

minP ::
  forall (a :: S -> Type) (s :: S).
  (PIsData a, POrd a) =>
  Term
    s
    ( PDataRecord '["_0" ':= PExtended a, "_1" ':= PBool]
        :--> PDataRecord '["_0" ':= PExtended a, "_1" ':= PBool]
        :--> PDataRecord '["_0" ':= PExtended a, "_1" ':= PBool]
    )
minP = phoistAcyclic $ plam $ \x y -> pif' # (leqP # x # y) # x # y

maxP ::
  forall (a :: S -> Type) (s :: S).
  (PIsData a, POrd a) =>
  Term
    s
    ( PDataRecord '["_0" ':= PExtended a, "_1" ':= PBool]
        :--> PDataRecord '["_0" ':= PExtended a, "_1" ':= PBool]
        :--> PDataRecord '["_0" ':= PExtended a, "_1" ':= PBool]
    )
maxP = phoistAcyclic $ plam $ \x y -> pif' # (leqP # x # y) # y # x

-- value < endpoint
pbefore' ::
  forall (a :: S -> Type) (s :: S).
  (PIsData a, POrd a) =>
  Term
    s
    ( a
        :--> PDataRecord '["_0" ':= PExtended a, "_1" ':= PBool]
        :--> PBool
    )
pbefore' = phoistAcyclic $
  plam $ \a y' -> unTermCont $ do
    y <- tcont $ pletFields @'["_0", "_1"] y'
    let yt = getField @"_0" y
    let yc = getField @"_1" y
    pure $
      pif
        yc
        (pmatch yt (ltE' a))
        (pmatch yt (leqE' a))

-- value > endpoint
pafter' ::
  forall (a :: S -> Type) (s :: S).
  (PIsData a, POrd a) =>
  Term
    s
    ( a
        :--> PDataRecord '["_0" ':= PExtended a, "_1" ':= PBool]
        :--> PBool
    )
pafter' = phoistAcyclic $
  plam $ \a y' -> unTermCont $ do
    y <- tcont $ pletFields @'["_0", "_1"] y'
    let yt = getField @"_0" y
    let yc = getField @"_1" y
    pure $
      pif
        yc
        (pmatch yt (gtE' a))
        (pmatch yt (geqE' a))

-- value <= PExtended
leqE' ::
  forall (a :: S -> Type) (s :: S).
  (POrd a, PIsData a) =>
  Term s a ->
  PExtended a s ->
  Term s PBool
leqE' a y = ltE' a y #|| eqE' a y

-- value >= PExtended
geqE' ::
  forall (a :: S -> Type) (s :: S).
  (POrd a, PIsData a) =>
  Term s a ->
  PExtended a s ->
  Term s PBool
geqE' a y = gtE' a y #|| eqE' a y

-- value > PExtended
gtE' ::
  forall (a :: S -> Type) (s :: S).
  (POrd a, PIsData a) =>
  Term s a ->
  PExtended a s ->
  Term s PBool
gtE' x = \case
  PNegInf _ -> pconstant False
  PPosInf _ -> pconstant True
  PFinite r ->
    let y = pfield @"_0" # r
     in y #< x
