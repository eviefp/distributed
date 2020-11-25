module Main where

import qualified Distributed
import qualified Options.Generic as O
import           Prelude

main :: IO ()
main =
    O.getRecord "distributed"
        >>= Distributed.run
