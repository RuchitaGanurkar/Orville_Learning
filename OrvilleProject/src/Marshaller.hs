{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Marshaller where



import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int32)
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified Orville.PostgreSQL as O


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