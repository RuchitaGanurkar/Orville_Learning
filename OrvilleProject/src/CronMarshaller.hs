{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}


module CronMarshaller where

import Data.Int (Int32)
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified Orville.PostgreSQL as O
import Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.Text.Lazy as LT
import qualified Data.Either.Extra as Extra
import Data.Aeson (FromJSON, ToJSON, decode, encode, toJSON)


---------------------------------------------------------------------------------------------------

--CRON TASK : DATE (4th September 2024)

{-

APIs

1. /cron/identifier/format (create - update - delete)
2. /create/identifier
3. /update/identifier
4. /delete//identifier

Tables

For 1. {id , format , status , identifier , data}

Data { name , age }

For 2. 


-}




newtype PersonName = PersonName T.Text deriving (Show, Eq, Generic , FromJSON , ToJSON)
newtype PersonAge = PersonAge Int32 deriving (Show, Eq, Generic , FromJSON , ToJSON)
newtype PersonMobile = PersonMobile Int32 deriving (Show, Eq, Generic , FromJSON , ToJSON)




data Person = Person {
  name :: PersonName ,
  age :: PersonAge ,
  mobile :: PersonMobile
} deriving (Show, Eq, Generic , FromJSON , ToJSON)





newtype Identifier = Identifier T.Text deriving (Show, Eq, Generic, FromJSON, ToJSON)
newtype CronId = CronId Int32 deriving (Show, Eq, Generic, FromJSON, ToJSON)

data Status
    = Pending T.Text
    | Success T.Text
    | Error T.Text
    deriving (Show, Eq, Generic, FromJSON, ToJSON)


data Formats
    = Create T.Text
    | Update T.Text
    | Delete T.Text
    deriving (Show, Eq, Generic, FromJSON, ToJSON)



data Cron = Cron
  { cronId     :: CronId
  , format     :: Formats
  , status     :: Status
  , identifier :: Identifier
  , dataField  :: Person
  } deriving (Show, Generic, FromJSON, ToJSON)





-- Field Definitions
cronIdField :: O.FieldDefinition O.NotNull CronId
cronIdField = O.coerceField (O.integerField "id")

formatField :: O.FieldDefinition O.NotNull Formats
formatField = O.convertField jsonByteStringConversion (O.coerceField (O.jsonbField "format"))

statusField :: O.FieldDefinition O.NotNull Status
statusField = O.convertField jsonByteStringConversion ( O.coerceField (O.unboundedTextField "status"))

identifierField :: O.FieldDefinition O.NotNull Identifier
identifierField = O.convertField  jsonByteStringConversion(O.coerceField (O.unboundedTextField "identifier"))

checkDataField :: O.FieldDefinition O.NotNull Person
checkDataField = O.convertField jsonByteStringConversion (O.jsonbField "data")


jsonByteStringConversion :: (FromJSON a, ToJSON a) => O.SqlType T.Text -> O.SqlType a
jsonByteStringConversion =
  O.tryConvertSqlType (LT.toStrict . decodeUtf8 . encode . toJSON) (Extra.maybeToEither "Error" . decode . encodeUtf8 . LT.fromStrict)



-- Cron Marshaller
cronMarshaller :: O.SqlMarshaller Cron Cron

cronMarshaller =
  Cron
    <$> O.marshallField cronId cronIdField
    <*> O.marshallField format formatField
    <*> O.marshallField status statusField
    <*> O.marshallField identifier identifierField
    <*> O.marshallField dataField checkDataField

cronTable :: O.TableDefinition (O.HasKey CronId) Cron Cron
cronTable =
  O.mkTableDefinition
    "cron"
    (O.primaryKey cronIdField)
    cronMarshaller



data Graph = Graph
  { 
    g_details :: Person 
  } deriving (Show, Generic, FromJSON, ToJSON)


graphDetailsField :: O.FieldDefinition O.NotNull Person 
graphDetailsField = O.convertField jsonByteStringConversion(O.coerceField  $ O.unboundedTextField "g_details") 


--Graph Marshaller || Use marshallReadOnly
graphMarshaller :: O.SqlMarshaller Graph Graph
graphMarshaller =
  Graph
    <$> O.marshallField g_details graphDetailsField
  


graphTable :: O.TableDefinition O.NoKey Graph Graph
graphTable =
  O.mkTableDefinitionWithoutKey
    "graph" 
    graphMarshaller





