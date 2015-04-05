{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Topology.Isometry where

import           Control.Applicative
import           Data.Algebra
import           Data.Complex
import           Data.Function                 (on)
import           Data.Maybe                    (fromJust)
import qualified Numeric.LinearAlgebra.HMatrix as Mx (Matrix (..), ident, inv,
                                                      mul)

import           Topology.Manifold

data IsometryRecord = IsometryRecord {theta :: Angle, z :: Complex Double} deriving (Eq, Show, Read)

class Isometry i where
  iso :: i -> IsometryRecord
  wrap :: IsometryRecord -> i

instance Isometry IsometryRecord where
  iso = id
  wrap = id

newtype Elliptic = Elliptic {unElliptic :: Mx.Matrix Double}

newtype Flat = Flat {unFlat :: IsometryRecord} deriving (Isometry, Eq, Show, Read)
newtype Hyperbolic = Hyperbolic {unHyperbolic :: IsometryRecord} deriving (Isometry, Eq, Show, Read)

instance Group Elliptic where
  zero = Elliptic $ Mx.ident 2
  (^+^) = (Elliptic .) . on ellipticComp unElliptic
  neg = Elliptic . Mx.inv . unElliptic

instance Group Flat where
  zero = Flat $ IsometryRecord 0 0
  (^+^) = (Flat .) . on flatComp iso
  neg = Flat . isoInv . iso

instance Group Hyperbolic where
  zero = Hyperbolic $ IsometryRecord 0 0
  (^+^) = (Hyperbolic .) . on hyperbolicComp iso
  neg = Hyperbolic . isoInv . iso

type Bin x = x -> x -> x

ellipticComp :: Bin (Mx.Matrix Double)
ellipticComp = Mx.mul

flatComp :: Bin IsometryRecord
flatComp a b = uncurry IsometryRecord (part1, part2) where
  [(t1, c1), (t2, c2)] = ((,) <$> theta <*> z) <$> [a, b]
  i = (0:+)
  part1 = t1 + t2
  part2 = c2 + c1 * (exp (negate (i t2)))

hyperbolicComp :: Bin IsometryRecord
hyperbolicComp a b = uncurry IsometryRecord (part1, part2) where
  [(t1, c1), (t2, c2)] = ((,) <$> theta <*> z) <$> [a, b]
  i = (0:+)
  --ADD PI BECAUSE PHASE -> (-pi, pi] AND WE WANT (0, 2pi]
  part1 = (+) (t1 + pi) $ phase $ (exp (i t2) + c1 * conjugate c2) / (1 + exp (i t2) * conjugate c1 * c2)
  part2 = (exp (i t2) * c2 + c1) / (exp (i t2) + c1 * conjugate c2)

isoInv :: IsometryRecord -> IsometryRecord
isoInv a = IsometryRecord (negate t) (negate $ exp (i t) * c) where
  i = (0:+)
  (t, c) = ((,) <$> theta <*> z) a
