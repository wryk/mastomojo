module Clipboard where

import Effect (Effect)

foreign import writeText :: String -> Effect Boolean
