module Event where

import Prelude

import Control.Monad.Except (runExcept, throwError)
import Effect (Effect)
import Effect.Class (liftEffect)
import Control.Promise (Promise, fromAff)
import Foreign (F, Foreign, readNumber, readString)
import Foreign.Index ((!))
import Effect.Aff.Compat (mkEffectFn1)
import Effect.Uncurried (EffectFn1)
import Effect.Exception (error)
import Data.Either (Either(..))

type Event = { id :: Number, text :: String }

readEvent :: Foreign -> F Event
readEvent value = do
  id <- value ! "id" >>= readNumber
  text <- value ! "text" >>= readString
  pure { id, text }

_handler :: Foreign -> Effect (Promise Foreign)
_handler f = do
  case (runExcept $ readEvent f) of
    Right a ->
      liftEffect $ fromAff $ pure f
    Left e ->
      liftEffect $ fromAff $ throwError $ error $ show e

handler :: EffectFn1 Foreign (Promise Foreign)
handler = mkEffectFn1 _handler
