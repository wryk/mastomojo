module Main where

import Prelude

import Component (component)
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
	body <- HA.awaitBody
	runUI component unit body
