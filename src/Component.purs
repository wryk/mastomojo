module Component where

import Prelude

import Clipboard (writeText)
import Data.Array (filter, length, sort)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Console (log)
import Effect.Exception (Error, message)
import Emoji (Emoji(..), fetchCustomEmojis)
import Foreign (MultipleErrors)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event (preventDefault)
import Web.Event.Internal.Types (Event)

data Result
	= Fetching String
	| NetworkError String Error
	| DecodeError String MultipleErrors
	| Success String (Array Emoji)

type State =
	{ domain :: String
	, animated :: Boolean
	, displayOnlyVisibleInPicker :: Boolean
	, result :: Maybe Result
	}

data Query a
	= SetDomain String a
	| SetAnimated Boolean a
	| SetDisplayOnlyVisibleInPicker Boolean a
	| CopyText String a
	| SubmitSearch Event a

type Input = Unit

type Message = Void

component :: H.Component HH.HTML Query Input Message Aff
component =
	H.component
		{ initialState: const initialState
		, render
		, eval
		, receiver: const Nothing
		}

	where
		initialState :: State
		initialState =
			{ domain: "eldritch.cafe"
			, animated: false
			, displayOnlyVisibleInPicker: true
			, result: Nothing
			}

		render :: State -> H.ComponentHTML Query
		render state =
			HH.main_
				[ HH.h1_
					[ HH.text "Mastodon emoji viewer"
					]
				, HH.form
					[ HE.onSubmit $ HE.input SubmitSearch
					]
					[ HH.label_
						[ HH.text "Domain"
						, HH.input
							[ HP.required true
							, HP.value state.domain
							, HE.onValueInput $ HE.input SetDomain
							]
						]
					, HH.button
						[ HP.type_ HP.ButtonSubmit
						, HP.disabled $ state.domain == mempty
						]
						[ HH.text "Fetch"
						]
					]
				, HH.label_
					[ HH.text "Animated"
					, HH.input
						[ HP.type_ HP.InputCheckbox
						, HP.checked state.animated
						, HE.onChecked $ HE.input SetAnimated
						]
					]
				, HH.label_
					[ HH.text "Show only visible in picker"
					, HH.input
						[ HP.type_ HP.InputCheckbox
						, HP.checked state.displayOnlyVisibleInPicker
						, HE.onChecked $ HE.input SetDisplayOnlyVisibleInPicker
						]
					]
				, HH.div_
					case state.result of
						Nothing ->
							[ HH.text "BLOBMIOU AF"
							]
						Just (Fetching domain) ->
							[ HH.h2_
								[ HH.text domain
								]
							, HH.p_
								[ HH.text "Fetching, please wait ..."
								]
							]
						Just (NetworkError domain error) ->
							[ HH.h2_
								[ HH.text domain
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
						Just (DecodeError domain error) ->
							[ HH.h2_
								[ HH.text domain
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
						Just (Success domain emojis) ->
							[ HH.h2_
								[ HH.text $ domain <> " (" <> (show $ length emojis) <> ")"
								]
							, emojiList emojis
							]
				]

			where
				emojiList emojis =
					let
						filteredEmojis = if state.displayOnlyVisibleInPicker
							then filter (\(Emoji r) -> r.visibleInPicker) emojis
							else emojis
					in
						HH.ul_
							$ map emojiItem filteredEmojis

				emojiItem (Emoji r) =
					let
						shortcode = ":" <> r.shortcode <> ":"
						imageSource = if state.animated then r.url else r.staticUrl
					in
						HH.li_
							[ HH.button
								[ HP.type_ HP.ButtonButton
								, HP.title $ "Click to copy " <> shortcode <> " to your clipboard"
								, HE.onClick $ HE.input_ $ CopyText shortcode
								]
								[ HH.img
									[ HP.src imageSource
									, HP.alt $ shortcode
									]
								, HH.text $ shortcode
								]
							]

		eval :: Query ~> H.ComponentDSL State Query Message Aff
		eval (SetDomain domain next) = do
			H.modify_ $ _ { domain = domain }
			pure next

		eval (SetAnimated animated next) = do
			H.modify_ $ _ { animated = animated }
			pure next

		eval (SetDisplayOnlyVisibleInPicker displayOnlyVisibleInPicker next) = do
			H.modify_ $ _ { displayOnlyVisibleInPicker = displayOnlyVisibleInPicker }
			pure next

		eval (CopyText text next) = do
			result <- H.liftEffect $ writeText text
			H.liftEffect $ log $ text <> " " <> if result then "copied" else "not copied"
			pure next

		eval (SubmitSearch event next) = do
			H.liftEffect $ preventDefault event

			domain <- H.gets _.domain
			H.modify_ $ _ { result = Just $ Fetching domain }

			response <- H.liftAff $ fetchCustomEmojis domain

			case response of
				Left error ->
					H.modify_ $ _ { result = Just $ NetworkError domain error }

				Right content ->
					case content of
						Left errors ->
							H.modify_ $ _ { result = Just $ DecodeError domain errors }

						Right emojis ->
							H.modify_ $ _ { result = Just $ Success domain (sort emojis) }

			pure next
