module CLI where

import Prelude

import qualified Options.Generic as O

data Options
  = Options
      { listen :: Bool
      , port   :: Int
      , name   :: String
      }
  deriving stock (O.Generic, Show)

instance O.ParseRecord Options
