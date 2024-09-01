{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}



module Marshaller where



import Data.Aeson (FromJSON, ToJSON, decode, encode, toJSON)
import Data.Int (Int32)
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified Orville.PostgreSQL as O
import Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.Text.Lazy as LT
import qualified Data.Either.Extra as Extra



-- Models and Database Schema
newtype StudentId = StudentId Int32 deriving (Show, Eq, Generic, FromJSON, ToJSON)

type StudentName = T.Text
type StudentAge = Int32

data Student = Student
  { studentId :: StudentId
  , studentName :: StudentName
  , studentAge :: StudentAge
  } deriving (Show, Generic)

studentIdField :: O.FieldDefinition O.NotNull StudentId
studentIdField = O.coerceField (O.integerField "id")

studentNameField :: O.FieldDefinition O.NotNull StudentName
studentNameField = O.unboundedTextField "name"

studentAgeField :: O.FieldDefinition O.NotNull StudentAge
studentAgeField = O.coerceField (O.integerField "age")

studentMarshaller :: O.SqlMarshaller Student Student
studentMarshaller =
  Student
    <$> O.marshallField studentId studentIdField
    <*> O.marshallField studentName studentNameField
    <*> O.marshallField studentAge studentAgeField

studentTable :: O.TableDefinition (O.HasKey StudentId) Student Student
studentTable =
  O.mkTableDefinition
    "students"
    (O.primaryKey studentIdField)
    studentMarshaller




---------------------------------------------------------------------------------------------------

--CRON TASK : DATE (1st September 2024)

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
  , dataField  :: T.Text 
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

checkDataField :: O.FieldDefinition O.NotNull T.Text
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
  { graphId        :: Int32
  , graphData      :: T.Text -- JSON data
  , graphIdentifier :: Identifier
  } deriving (Show, Generic, FromJSON, ToJSON)


graphDataField :: O.FieldDefinition O.NotNull T.Text
graphDataField = O.convertField jsonByteStringConversion $ O.jsonbField "data"

--Graph Marshaller
graphMarshaller :: O.SqlMarshaller Graph Graph
graphMarshaller =
  Graph
    <$> O.marshallField graphId (O.coerceField (O.integerField "id"))
    <*> O.marshallField graphData graphDataField
    <*> O.marshallField graphIdentifier identifierField

graphTable :: O.TableDefinition (O.HasKey Int32) Graph Graph
graphTable =
  O.mkTableDefinition
    "graph"
    (O.primaryKey (O.coerceField (O.integerField "id")))
    graphMarshaller
