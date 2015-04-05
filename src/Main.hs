{-| Renders a pizza-like colour-wheel, with each slice being a unique pre-defined color.
    See http://helm-engine.org/guide/colors/ -}
module Main where

import           Control.Applicative
import           Data.Function (on)
import           FRP.Helm
import qualified FRP.Helm.Graphics    as Graphics
import qualified FRP.Helm.Keyboard    as Keyboard
import qualified FRP.Helm.Text        as Text
import qualified FRP.Helm.Time        as Time
import qualified FRP.Helm.Window      as Window

import           Topology.Coordinates
import           Topology.Disc
import           Topology.Geodesic
import           Topology.Isometry
import           Topology.Manifold

step :: Time -> Double -> Double
step dt n = n + Time.inSeconds dt

screenShow :: (Show s) => s -> Graphics.Form
screenShow x = toForm $ Text.text $ Text.color white $ Text.toText $ show x

render :: Double -> (Int, Int) -> Element
render delta (w, h) = centeredCollage w h
  [
  filled red $ on Graphics.rect fromIntegral w h,
  screenShow $ floor $ 0.5 + 1 / Time.inSeconds delta
  ]

{-| Bootstrap the game. -}
main :: IO ()
main = do
    run config $ render <$> ticker <*> Window.dimensions

  where
    config = EngineConfig {
                          windowDimensions   = (100, 100),
                          windowIsFullscreen = False,
                          windowIsResizable  = True,
                          windowTitle        = "LAHacks! Manifold-fest Destiny"
                          }
    ticker = Time.fps 60
    --stepper = foldp step 0 ticker
