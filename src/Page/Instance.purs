module App.Page.Instance where

import Prelude

import Clipboard (writeText)
import App.Data.Emoji (Emoji(..), fetchCustomEmojis)
import Data.Array (filter, length, sort)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Effect.Exception (Error, message)
import Foreign (MultipleErrors)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks

type Input =
    { domain :: String
    }

data Result
    = Fetching String
    | NetworkError String Error
    | DecodeError String MultipleErrors
    | Success String (Array Emoji)

component :: ∀ q o m. MonadAff m => H.Component HH.HTML q Input o m
component = Hooks.component \_ { domain } -> Hooks.do
    animated /\ animatedId <- Hooks.useState false
    displayOnlyVisibleInPicker /\ displayOnlyVisibleInPickerId <- Hooks.useState true
    result /\ resultId <- Hooks.useState Nothing

    Hooks.useLifecycleEffect do
        Hooks.put resultId (Just $ Fetching domain)

        response <- H.liftAff $ fetchCustomEmojis domain

        case response of
            Left error ->
                Hooks.put resultId (Just $ NetworkError domain error)

            Right content ->
                case content of
                    Left errors ->
                        Hooks.put resultId (Just $ DecodeError domain errors)

                    Right emojis ->
                        Hooks.put resultId (Just $ Success domain (sort emojis))

        pure $ Nothing

    let
        copyText text = do
            copyResult <- H.liftEffect $ writeText text
            H.liftEffect $ log $ text <> " " <> if copyResult then "copied" else "not copied"


        emojiList emojis =
            HH.ul_
                $ map emojiItem filteredEmojis
            where
                filteredEmojis = if displayOnlyVisibleInPicker
                    then filter (\(Emoji r) -> r.visibleInPicker) emojis
                    else emojis

        emojiItem (Emoji r) =
            HH.li_
                    [ HH.button
                        [ HP.type_ HP.ButtonButton
                        , HP.title help
                        , HE.onClick \_ -> Just $ copyText shortcode
                        ]
                        [ HH.img
                            [ HP.src imageSource
                            , HP.alt shortcode
                            ]
                        , HH.text shortcode
                        ]
                    ]
            where
                shortcode = ":" <> r.shortcode <> ":"
                help = "Click to copy " <> shortcode <> " to your clipboard"
                imageSource = if animated then r.url else r.staticUrl


    Hooks.pure do
        HH.main_
            [ HH.h1_
                [ HH.text "Mastodon emoji viewer"
                ]
            , HH.label_
                [ HH.text "Animated"
                , HH.input
                    [ HP.type_ HP.InputCheckbox
                    , HP.checked animated
                    , HE.onChecked $ Just <<< (Hooks.put animatedId)
                    ]
                ]
            , HH.label_
                [ HH.text "Show only visible in picker"
                , HH.input
                    [ HP.type_ HP.InputCheckbox
                    , HP.checked displayOnlyVisibleInPicker
                    , HE.onChecked $ Just <<< (Hooks.put displayOnlyVisibleInPickerId)
                    ]
                ]
            , HH.div_
                case result of
                    Nothing ->
                        [ HH.text "BLOBMIOU AF"
                        ]
                    Just (Fetching domain_) ->
                        [ HH.h2_
                            [ HH.text domain_
                            ]
                        , HH.p_
                            [ HH.text "Fetching, please wait ..."
                            ]
                        ]
                    Just (NetworkError domain_ error) ->
                        [ HH.h2_
                            [ HH.text domain_
                            ]
                        , HH.p_
                            [ HH.text "Oups, you got a network error :/"
                            , HH.text "Did you mispell the domain ? Internet is down ?"
                            ]
                        , HH.pre_
                            [ HH.code_
                                [ HH.text $ message error
                                ]
                            ]
                        ]
                    Just (DecodeError domain_ error) ->
                        [ HH.h2_
                            [ HH.text domain_
                            ]
                        , HH.p_
                            [ HH.text "Oups, that instance send us something weird :<"
                            ]
                        , HH.pre_
                            [ HH.code_
                                [ HH.text $ show error
                                ]
                            ]
                        ]
                    Just (Success domain_ emojis) ->
                        [ HH.h2_
                            [ HH.text $ domain_ <> " (" <> (show $ length emojis) <> ")"
                            ]
                        , emojiList emojis
                        ]
            ]

