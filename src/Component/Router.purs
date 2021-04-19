module App.Component.Router where

import Prelude

import App.Capability.Navigate (class Navigate, navigate)
import App.Component.Utils (OpaqueSlot)
import App.Data.Route (Route(..), routeCodec)
import App.Page.Home as PageHome
import App.Page.Instance as PageInstance
import Data.Either (hush)
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen (liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Routing.Duplex as RD
import Type.Proxy (Proxy(..))
import Web.HTML (window)
import Web.HTML.Location as WL
import Web.HTML.Window as Window

type State =
  { route :: Maybe Route
  }

data Action
  = Initialize

type ChildSlots =
  ( home :: OpaqueSlot Unit
  , instance :: OpaqueSlot Unit
  )

data Query a
  = Navigate Route a

component :: ∀ i o m.
  MonadAff m =>
  Navigate m =>
  H.Component Query i o m
component = H.mkComponent
  { initialState
  , render
  , eval: H.mkEval $ H.defaultEval
    { handleAction = handleAction
    , handleQuery = handleQuery
    , initialize = Just Initialize
    }
  }

  where
    initialState _ = { route: Nothing }

    handleAction :: Action -> H.HalogenM State Action ChildSlots o m Unit
    handleAction = case _ of
      Initialize -> do
        initialRoute <- hush <<< (RD.parse routeCodec) <$> liftEffect getPath
        navigate $ fromMaybe Home initialRoute
          where
            getPath :: Effect String
            getPath = window >>= Window.location >>= WL.pathname

    handleQuery :: ∀ a. Query a -> H.HalogenM State Action ChildSlots o m (Maybe a)
    handleQuery = case _ of
      Navigate destination a -> do
        { route } <- H.get

        when (route /= Just destination) do
          H.modify_ _ { route = Just destination }

        pure $ Just a

    render :: State -> H.ComponentHTML Action ChildSlots m
    render { route } = case route of
      Just r ->
        HH.main_
          [ HH.slot (Proxy :: Proxy "home") unit PageHome.component {} absurd
          , case r of
            Home ->
              HH.text "uwu"
            (Instance domain) ->
              HH.slot (Proxy :: Proxy "instance") unit PageInstance.component { domain } absurd
          ]

      Nothing ->
        HH.text "404"
