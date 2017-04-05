{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE StandaloneDeriving #-}

module ODrive.Fixed where


 --Qn
import GHC.TypeLits
import Ivory.Language
import Ivory.Language.Proxy
import Ivory.Language.Cast
import Ivory.Language.Type
import Ivory.Language.BoundedInteger
import Ivory.Language.IIntegral
import qualified Ivory.Language.Syntax as I

import Data.Bits (shiftR)
import Text.Printf

-- underlying type for fixed point numbers
type FixedRep = Sint32

fixedRep :: I.Type
fixedRep = ivoryType (Proxy :: Proxy FixedRep)

newtype Fixed (n :: Nat) = Fixed { getFixed :: I.Expr }

instance (ANat n) => IvoryType (Fixed n) where
  ivoryType _ = I.TyInt I.Int32

instance (ANat n) => IvoryVar (Fixed n) where
  wrapVar = wrapVarExpr
  unwrapExpr = getFixed

instance (ANat n) => IvoryExpr (Fixed n) where
  wrapExpr = Fixed

instance (ANat n) => IvoryStore (Fixed n)

instance (ANat n) => Num (Fixed n) where
  (*)           = fixedMuls
  (+)           = exprBinop (+)
  (-)           = exprBinop (-)
  abs           = exprUnary abs
  signum        = exprUnary signum
  negate        = exprUnary negate
--  fromInteger   = toFixedi
  fromInteger   = Fixed . fromInteger . (* 2 ^ fromTypeNat (aNat :: NatType n))   -- . (* (2 ** (fromIntegral $ fromTypeNat (aNat :: NatType n))))
--  fromInteger x   = boundedFromFixed Fixed (0 :: Fixed 16) (x * 2 ^ fromTypeNat (aNat :: NatType n))   -- . (* (2 ** (fromIntegral $ fromTypeNat (aNat :: NatType n))))
--  this ones legit. almost
--  fromInteger   = boundedFromFixed -- Fixed  -- (* 2 ^ fromTypeNat (aNat :: NatType n))   -- . (* (2 ** (fromIntegral $ fromTypeNat (aNat :: NatType n))))

instance ( ANat n, IvoryIntegral to, Default to
         ) => SafeCast (Fixed n) to where
  safeCast fx = ivoryCast (fromFixed fx)

instance (ANat n) => IvoryEq  (Fixed n)
instance (ANat n) => IvoryOrd  (Fixed n)

instance ANat len => IvoryInit (Fixed len)
instance (ANat n) => IvoryZeroVal (Fixed n) where
   izeroval = ival 0

-- required so we can do bitshifts even on signed ints
instance IvoryBits Sint64 where
   iBitSize _ = 64

-- seems redundant
--instance ANat n => Bounded (Fixed n) where
--  minBound = -maxBound
--  maxBound = bound --fromInteger (2 ^ (32 - n))
--    where n = fromTypeNat (aNat :: NatType n)
--          m = 32 - n
--          bound = fromInteger $ 2 ^ (m - 1) - 1

-- bounds for integer part
-- defined as 2 ^ (m - 1) - 1 where m is number of decimal points (32 -n)
boundedFromFixed :: forall n. (ANat n) => Integer -> Fixed n
boundedFromFixed i
    | i > bound
    = error $ printf "The constant %d is too large to cast to Fixed %d with bound %d" i n bound
    | i < (-bound) --- XXX: this branch doesn't seem to be called
    = error $ printf "The constant %d is too small to cast to Fixed %d with bound %d" i n (-bound)
    | otherwise
    = Fixed (fromInteger $ fn i)
    where fn = (* 2 ^ n)
          n = fromTypeNat (aNat :: NatType n)
          m =  32 - n
          bound = 2 ^ (m - 1) - 1

--fixedpointCast :: forall n from  .(ANat n, IvoryOrd from, IvoryExpr from) => from -> (Fixed n)
--fixedpointCast f = (f >? 

--toFix :: forall a n. (SafeCast a FixedRep, ANat n) => a -> Fixed n
--toFix = Fixed . wrapExpr . unwrapExpr . (safeCast :: a -> FixedRep)

-- multiply in Uint64s, then shift by N and cast back to Uint32
-- UNSAFE, can overflow
-- need safecast here
fixedMul :: forall n. (ANat n) => Fixed n -> Fixed n -> Fixed n
fixedMul a b = toFix $ (ivoryCast (  (ua  * ub)  `iShiftR` nat :: Sint64) :: Sint32)
  where ua = safeCast a :: Sint64
        ub = safeCast b :: Sint64
        nat = fromIntegral $ fromTypeNat (aNat :: NatType n)

--fixedMuls :: forall n. (ANat n) => Fixed n -> Fixed n -> Fixed n
--fixedMuls a b = (ovfCast ua ub (  (ua  * ub)  `iShiftR` nat :: Sint64)) -- :: Sint32)
--  where ua = safeCast a :: Sint64
--        ub = safeCast b :: Sint64
--        nat = fromIntegral $ fromTypeNat (aNat :: NatType n)
--        -- this is SUB overflow ... hopefuly
--        ovfCast a b sum = ((a .^ b) .& sign ==? 0 .&& ((a .^ sum) .& sign) ==? sign) ? (upbound, toFix $ (ivoryCast :: Sint64 -> Sint32) sum)
--        sign = 1 `iShiftR` 31
--        upbound :: Fixed n
--        --upbound = (toFix :: Sint32 -> Fixed n) $ (2 ^ 31 - 1 :: Sint32)
--        --upbound = (0x80000000 :: Fixed n)
--        upbound = (1337 :: Fixed n)

-- non-asserting, uses 1 if overflows, not correct
--fixedMuls :: forall n. (ANat n) => Fixed n -> Fixed n -> Fixed n
--fixedMuls a b = (ovfCast ua ub) -- :: Sint32)
--  where ua = safeCast a :: Sint64
--        ub = safeCast b :: Sint64
--        sum = ua * ub
--        nat = fromIntegral $ fromTypeNat (aNat :: NatType n)
--        ovfCast a b = (sum <=? 0) ? (
--            (upper ==? 0) ? (upbound, res)
--          , (upper >=? 1) ? (upbound, res))
--        upper = sum `iShiftR` 47
--        res :: Fixed n
--        res = toFix $ (ivoryCast :: Sint64 -> Sint32) (sum `iShiftR` nat :: Sint64)
--        --sign = 1 `iShiftR` 31
--        upbound :: Fixed n
--        --upbound = (toFix :: Sint32 -> Fixed n) $ (2 ^ 31 - 1 :: Sint32)
--        --upbound = (0x80000000 :: Fixed n)
--        upbound = (1 :: Fixed n)

fixedMuls :: forall n. (ANat n) => Fixed n -> Fixed n -> Fixed n
fixedMuls a b = (ovfCast ua ub) -- :: Sint32)
  where ua = safeCast a :: Sint64
        ub = safeCast b :: Sint64
        sum = ua * ub
        nat = fromIntegral $ fromTypeNat (aNat :: NatType n)
        ovfCast a b = (sum <=? 0) ? (
            (upper ==? 0) ? (upbound, res)
          , (upper >=? 1) ? (upbound, res))
        upper = sum `iShiftR` 47
        res :: Fixed n
        res = toFix $ (ivoryCast :: Sint64 -> Sint32) (sum `iShiftR` nat :: Sint64)
        --sign = 1 `iShiftR` 31
        upbound :: Fixed n
        --upbound = (toFix :: Sint32 -> Fixed n) $ (2 ^ 31 - 1 :: Sint32)
        upbound = toFix (0x80000000 :: Sint32)
        --upbound = (1 :: Fixed n)

-- also provide fixedMul32 as a faster variant without intermediate casting to Sint64
-- (maybe) faster but susceptible to intermediate overflows

-- shift to left by N, divide and truncate
fixedDiv :: forall n. (ANat n) => Fixed n -> Fixed n -> Fixed n
fixedDiv a b = toFix $ (ivoryCast ( (ua `iShiftL` nat) `iDiv` ub :: Sint64) :: Sint32)
  where ua = safeCast a :: Sint64
        ub = safeCast b :: Sint64
        nat = fromIntegral $ fromTypeNat (aNat :: NatType n)

toFix :: forall a n. (SafeCast a FixedRep, ANat n) => a -> Fixed n
toFix = mkFix . unwrapExpr . (safeCast :: a -> FixedRep)

mkFix :: forall n. (ANat n) => I.Expr -> Fixed n
mkFix = wrapExpr

fromFixed :: ANat n => Fixed n -> FixedRep
fromFixed = wrapExpr . unwrapExpr

-- ??
fromFixed64 :: ANat n => Fixed n -> Sint64
fromFixed64 = safeCast . fromFixed

-- number of floating points
fixedM :: forall n. (ANat n) => Fixed n -> Integer
fixedM _ = fromTypeNat (aNat :: NatType n)

fixedM' :: forall n. (ANat n) => Fixed n -> Uint8
fixedM' _ = fromIntegral $ fromTypeNat (aNat :: NatType n)

fixedN :: forall n. (ANat n) => Fixed n -> Integer
fixedN _ = 32 - fromTypeNat (aNat :: NatType n)
