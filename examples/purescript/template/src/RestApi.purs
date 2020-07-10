module RestApi where

import Prelude
import Effect.Aff (Aff, throwError)
import Effect.Class (liftEffect)
import Control.Promise (Promise, fromAff)
import Foreign (Foreign, renderForeignError)
import Effect.Aff.Compat (mkEffectFn1)
import Effect.Uncurried (EffectFn1)
import Effect.Console (log)
import Data.Either (Either(..))
import Effect.Exception (error)
import Simple.JSON (class ReadForeign, read, write, class WriteForeign)
import Debug.Trace (spy)

runHandler :: forall a b. ReadForeign a => WriteForeign b => (a -> Aff b) -> Foreign -> Aff Foreign
runHandler h event = case read $ spy "event" event of
  Right r -> write <$> h r
  Left e -> throwError $ error $ show $ renderForeignErrors e
  where
  renderForeignErrors e = show $ renderForeignError <$> e

mkHandler :: forall a b. ReadForeign a => WriteForeign b => (a -> Aff b) -> EffectFn1 Foreign (Promise Foreign)
mkHandler h = mkEffectFn1 $ fromAff <<< runHandler h

type APIGatewayProxyEvent
  = { path :: String, httpMethod :: String }

type APIGatewayProxyResult
  = { statusCode :: Int, body :: String, isBase64Encoded :: Boolean }

okResult :: String -> APIGatewayProxyResult
okResult body = { statusCode: 200, body, isBase64Encoded: false }

errorResult :: String -> APIGatewayProxyResult
errorResult body = { statusCode: 500, body, isBase64Encoded: false }

handler' :: APIGatewayProxyEvent -> Aff APIGatewayProxyResult
handler' event = do
  pure result
  where
  result = case event of
    { httpMethod: "GET" } -> okResult "Get method"
    { httpMethod: "POST" } -> okResult "Post method"
    { httpMethod } -> errorResult $ "Unsupported method " <> httpMethod

handler :: EffectFn1 Foreign (Promise Foreign)
handler = mkHandler handler'
