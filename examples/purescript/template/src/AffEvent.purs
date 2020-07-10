module AffEvent where

import Prelude

import Effect.Aff (Aff, throwError)
import Effect.Class (liftEffect)
import Control.Promise (Promise, fromAff)
import Control.Monad.Except (runExcept)
import Foreign (F, Foreign, readNumber, readString, renderForeignError, unsafeFromForeign)
import Foreign.Index ((!))
import Effect.Aff.Compat (mkEffectFn1)
import Effect.Uncurried (EffectFn1)
import Effect.Console (log)
import Data.Either (Either(..))
import Effect.Exception (error)

type MyEvent = { id :: Number, text :: String }

readMyEvent :: Foreign -> F MyEvent
readMyEvent value = do
  id <- value ! "id" >>= readNumber
  text <- value ! "text" >>= readString
  pure { id, text }

handleMyEvent :: MyEvent -> Aff String
handleMyEvent event = do
  liftEffect $ log $ "Handing event" <> show event
  pure $ "You said: " <> event.text

runHandler :: forall a b. (Foreign -> F a) -> (a -> Aff b) -> Foreign -> Aff b
runHandler reader handler' value = case runExcept (reader value) of
  Right r -> handler' r
  Left e -> throwError $ error $ show $ renderForeignErrors e
  where
    renderForeignErrors e = show $ renderForeignError <$> e

unsafeRunHandler :: forall a b. (a -> Aff b) -> Foreign -> Aff b
unsafeRunHandler handler' value = do
  handler' $ unsafeFromForeign value

handler :: EffectFn1 Foreign (Promise String)
handler = mkEffectFn1 $ fromAff <<< (runHandler readMyEvent handleMyEvent)

unsafeHandler :: EffectFn1 Foreign (Promise String)
unsafeHandler = mkEffectFn1 $ fromAff <<< (unsafeRunHandler handleMyEvent)
