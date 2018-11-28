module Emoji where

import Prelude
import Control.Monad.Except (runExcept)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Generic.Rep.Eq (genericEq)
import Data.Generic.Rep.Ord (genericCompare)
import Data.String.Extra (snakeCase)
import Effect.Aff (Aff, attempt)
import Effect.Exception (Error)
import Foreign (MultipleErrors)
import Foreign.Class (class Decode)
import Foreign.Generic (decodeJSON, defaultOptions, genericDecode)
import Milkis as M
import Milkis.Impl.Window as MI

newtype Emoji = Emoji
	{ shortcode :: String
	, url:: String
	, staticUrl:: String
	, visibleInPicker:: Boolean
	}

derive instance genericEmoji :: Generic Emoji _

instance showEmoji :: Show Emoji where
	show = genericShow

instance eqEmoji :: Eq Emoji where
	eq = genericEq

instance ordEmoji :: Ord Emoji where
	compare = genericCompare

instance decodeEmoji :: Decode Emoji where
	decode = genericDecode $ defaultOptions { unwrapSingleConstructors = true, fieldTransform = snakeCase }

fetchCustomEmojis :: String -> Aff (Either Error (Either MultipleErrors (Array Emoji)))
fetchCustomEmojis domain =
	attempt
	$ M.fetch MI.windowFetch (M.URL $ "https://" <> domain <> "/api/v1/custom_emojis") M.defaultFetchOptions
	>>= M.text
	<#> decodeJSON
	>>> runExcept
