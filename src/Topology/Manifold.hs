module Topology.Manifold where

import           Data.Graph.Inductive.Graph
import qualified Data.Graph.Inductive.PatriciaTree as G

--Helpful synonyms for extensibility
type Angle = Double

data EdgeInfo = EdgeInfo {len :: Double, a1 :: Angle, a2 :: Angle} deriving (Eq, Show, Read)

type NodeLabel = Double
type EdgeLabel = EdgeInfo

type ManifoldNode = LNode NodeLabel
type ManifoldEdge = LEdge EdgeLabel

--Vertex labels: Double
--Edge labels: EdgeInfo
type ManifoldGraph = G.Gr NodeLabel EdgeLabel

--OUR MANIFOLD DATA STRUCTURE
data Manifold = Manifold {curvature :: Double, graph :: ManifoldGraph} deriving (Show, Read)
