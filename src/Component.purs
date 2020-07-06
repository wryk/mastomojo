module Component where

import Prelude

import Clipboard (writeText)
import Data.Array (filter, length, sort)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff.Class (class MonadAff)
import Effect.Console (log)
import Effect.Exception (Error, message)
import Emoji (Emoji(..), fetchCustomEmojis)
import Foreign (MultipleErrors)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks as Hooks
import Web.Event.Event as Event

data Result
    = Fetching String
    | NetworkError String Error
    | DecodeError String MultipleErrors
    | Success String (Array Emoji)

component :: ∀ q i o m. MonadAff m => H.Component HH.HTML q i o m
component = Hooks.component \_ _ -> Hooks.do
    domain /\ domainId <- Hooks.useState "eldritch.cafe"
    animated /\ animatedId <- Hooks.useState false
    displayOnlyVisibleInPicker /\ displayOnlyVisibleInPickerId <- Hooks.useState true
    result /\ resultId <- Hooks.useState Nothing

    let
        copyText text = do
            copyResult <- H.liftEffect $ writeText text
            H.liftEffect $ log $ text <> " " <> if copyResult then "copied" else "not copied"

        handleSubmit event = do
            H.liftEffect $ Event.preventDefault event
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
            , HH.form
                [ HE.onSubmit $ Just <<< handleSubmit
                ]
                [ HH.label_
                    [ HH.text "Domain"
                    , HH.input
                        [ HP.required true
                        , HP.value domain
                        , HE.onValueInput $ Just <<< (Hooks.put domainId)
                        ]
                    ]
                , HH.button
                    [ HP.type_ HP.ButtonSubmit
                    , HP.disabled $ domain == mempty
                    ]
                    [ HH.text "Fetch"
                    ]
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

