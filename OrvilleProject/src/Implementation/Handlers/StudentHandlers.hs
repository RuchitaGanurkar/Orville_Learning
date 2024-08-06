
module Implementation.Handlers.StudentHandlers where



import Control.Monad.Reader
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except (throwError)
import Servant
import Implementation.Models.Student
import Implementation.Core (AppMonad)
import Implementation.Config (AppConfig)
import Implementation.DB (runDB)
import qualified Orville.PostgreSQL.Execution as Execution
import qualified Orville.PostgreSQL.Plan as Plan
import qualified Data.Text as T
import Data.Int
import qualified Orville.PostgreSQL as O

postStudent :: ApiStudent -> AppMonad NoContent
postStudent apiStudent = do
  let student = Student (apiStudentId apiStudent) (apiStudentName apiStudent) (apiStudentAge apiStudent)
  runDB $ Execution.insertEntity studentTable student
  return NoContent

getStudentById :: Int32 -> AppMonad ApiStudent
getStudentById sid = do
  maybeStudent <- runDB $ Execution.findOne studentTable (Execution.where_ (studentIdField O..== sid))
  case maybeStudent of
    Just student -> return $ ApiStudent (studentId student) (studentName student) (studentAge student)
    Nothing -> throwError err404

getStudentByName :: T.Text -> AppMonad ApiStudent
getStudentByName name = do
  maybeStudent <- runDB $ Plan.findOne studentTable (Execution.where_ (studentNameField O..== name))
  case maybeStudent of
    Just student -> return $ ApiStudent (studentId student) (studentName student) (studentAge student)
    Nothing -> throwError err404



-- iss handler ka type change krna he
-- AppMonad NoContent nahin Server API krna he

-- uske liye Server toh Servant ka he but API humara type he
-- API ka structure change krke return me kuchh dena hoga




