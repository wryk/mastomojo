module App.Page.Home where

import Prelude

import App.Capability.Navigate (class Navigate, navigate)
import App.Data.Route as Route
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.Hooks as Hooks
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event as Event

component ::
  ∀ q i o m.
    MonadEffect m =>
    Navigate m =>
    H.Component q i o m
component = Hooks.component \_ _ -> Hooks.do
    domain /\ domainId <- Hooks.useState "eldritch.cafe"

    let
        setDomain = Hooks.put domainId

        handleSubmit event = do
            H.liftEffect $ Event.preventDefault event
            navigate $ Route.Instance domain

    Hooks.pure do
        HH.div_
            [ HH.h1_ [ HH.text "Mastomojo" ]
            , HH.form
                [ HE.onSubmit handleSubmit
                ]
                [ HH.label_
                    [ HH.text "Domain"
                    , HH.input
                        [ HP.required true
                        , HP.value domain
                        , HE.onValueInput setDomain
                        ]
                    ]
                , HH.button
                    [ HP.type_ HP.ButtonSubmit
                    , HP.disabled $ domain == mempty
                    ]
                    [ HH.text "Fetch"
                    ]
                ]
            ]
