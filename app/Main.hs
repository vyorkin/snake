module Main (main) where

import qualified Snake.Config as Config
import qualified Snake.Game as Game

main :: IO ()
main = Config.load >>= Game.run
