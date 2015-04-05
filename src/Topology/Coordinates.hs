module Topology.Coordinates where

import           Control.Applicative
import           Data.Complex
import qualified Data.Map            as M

import           Topology.Disc
import           Topology.Isometry
import           Topology.Manifold

type LocalCoordinate = (Double, Double)

sqNorm :: LocalCoordinate -> Double
sqNorm (a, b) = a^2 + b^2

sub :: LocalCoordinate -> LocalCoordinate -> LocalCoordinate
sub (a, b) (c, d) = (a - c, b -d)

localMetric :: Manifold -> Disc -> LocalCoordinate -> LocalCoordinate -> Double
localMetric m d l1 l2 = (/ sqrt (negate k)) $ acosh $ (1+) $ negate $ (2 * k * sqNorm (l1 `sub` l2)) / ((1+k*(sqNorm l1))*(1+k*(sqNorm l2)))
  where k = curvature m

type GlobalCoordinate = M.Map Disc LocalCoordinate

type TransitionMap = LocalCoordinate -> (Disc, LocalCoordinate)

getIsometry :: (Isometry i) => ManifoldEdge -> i
getIsometry (_, _, info) = wrap $ IsometryRecord (pi + t1 - t2) ((l:+0) * (exp (i t1))) where
  i = (0:+)
  (l, t1, t2) = (len info, a1 info, a2 info)

getTransitionMap :: ManifoldEdge -> TransitionMap
getTransitionMap e@(d1, _, i) = isoFunc (getIsometry e) d1

isoFunc :: Hyperbolic -> Disc -> TransitionMap
isoFunc a d = (,) d . (toPair . complexMap . toComplex) where
  i = (0:+)
  (t, c) = (,) <$> theta <*> z $ iso a
  toComplex = uncurry (:+)
  toPair (x:+y) = (x, y)
  complexMap x = (exp (i t)) * (x + c) / (1 + x * (conjugate c))
