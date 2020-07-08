module Main where

import Prelude

import App.AppM (runAppM)
import App.Component.Router as Router
import App.Data.Route (Route, routeCodec)
import App.Env (Env)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Halogen as H
import Halogen.HTML as HH
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Routing.Duplex as RD
import Routing.PushState as RP

main :: Effect Unit
main = HA.runHalogenAff do
    history <- H.liftEffect RP.makeInterface

    let
        env :: Env
        env =
            { history
            }

        component :: ∀ i o. H.Component HH.HTML Router.Query i o Aff
        component = H.hoist (runAppM env) Router.component

    body <- HA.awaitBody
    halogenIO <- runUI component unit body

    let
        navigation :: Maybe Route -> Route -> Effect Unit
        navigation old new = do
            when (old /= Just new) do
                launchAff_ $ halogenIO.query $ H.tell $ Router.Navigate new

    void $ H.liftEffect $ RP.matchesWith (RD.parse routeCodec) navigation history