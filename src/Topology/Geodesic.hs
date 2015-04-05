module Topology.Geodesic where

import Topology.Coordinates
import Topology.Manifold

class Geodesic g where
  pointAlong :: g -> Double -> GlobalCoordinate

--Stores a geodesic as the closest point to the origin, in polar
data LocalGeodesic = LocalGeodesic {h :: Double, alpha :: Double} deriving (Eq, Show, Read)

{-
localTrace :: Manifold -> LocalGeodesic -> Double -> LocalGeodesic
localTrace m g t | h g <= 0.01  = if t <= 0.01 then 0 else singularity
                 | otherwise = normal where
  where normal = LocalGeodesic (smallR * cos (th + alpha g)) (smallR * sin (th + alpha g))
        singularity = LocalGeodesic (sSmallR * cos (sTh + alpha g)) (sSmallR * sin (sTh + alpha g))
        r = 1 / (sqrt $ negate $ curvature m)
        smallR = r / sqrt (1 + 2 / (cosh d - 1))
        sSmallR = r / sqrt (1 + 2 / (cosh sD - 1))
        th = atan $ tanh (t / r) / sinh ((h g) / r)
        sTh = pi * (signnum t) / 2
        d = atanh $ tanh ((h g) / r) / cos th
        sD = t / r
-}
