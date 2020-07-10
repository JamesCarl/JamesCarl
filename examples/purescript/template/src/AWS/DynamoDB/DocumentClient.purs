module DynamoDB.DocumentClient where

import Data.Function.Uncurried (Fn1)
import Foreign (Foreign)

foreign import data DocumentClient :: Type

foreign import mkDocumentClient :: String -> DocumentClient

type PutParams
  = { tableName :: String, item :: Foreign }

foreign import putImpl :: Fn1 DocumentClient PutParams
