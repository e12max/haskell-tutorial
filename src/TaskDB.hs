{-# LANGUAGE OverloadedStrings #-}

module TaskDB
  ( TaskStatus(Done, NotDone)
  , Task
  , fetchFromDB
  , pushTaskToDB
  , completeTask
  ) where

import HTML (HTML, toHTML)

import qualified Hasql.Connection
import qualified Hasql.Session
import qualified Hasql.Statement
import qualified Hasql.Encoders
import qualified Hasql.Decoders

import qualified Data.Text as T 
import qualified Data.ByteString.UTF8 as StrictUTF8

import Data.Functor.Contravariant (contramap)

import GHC.Int (Int32)



data TaskStatus = Done | NotDone
  deriving (Eq, Ord, Show, Read, Enum)

data Task = Task Int32 String TaskStatus
  deriving (Eq, Ord, Show)

instance HTML Task where
  toHTML task =
    case task of
      Task taskId description status ->
        case status of
          NotDone ->
            "<p><form action=\"/completetask\" method=\"POST\"><input name=\"taskId\" type=\"hidden\" value="
            ++ show taskId
            ++ "></input>"
            ++ description
            ++ "<input type=\"submit\" value=\"complete\"></input></form></p>"
          Done -> "<p><strike>" ++ description ++ "</strike></p>"


selectTasksSession :: Hasql.Session.Session [Task]
selectTasksSession = Hasql.Session.statement () selectTasksStatement

selectTasksStatement :: Hasql.Statement.Statement () [Task]
selectTasksStatement =
  Hasql.Statement.Statement
    "SELECT * FROM todolist_ch8 ORDER BY id;"
    Hasql.Encoders.noParams
    tasksDecoder
    True


tasksDecoder :: Hasql.Decoders.Result [Task]
tasksDecoder = Hasql.Decoders.rowList taskDecoder

taskDecoder :: Hasql.Decoders.Row Task
taskDecoder = do
  taskId <- Hasql.Decoders.column $ Hasql.Decoders.nonNullable Hasql.Decoders.int4
  taskDescription <- Hasql.Decoders.column $ Hasql.Decoders.nonNullable stringDecoder
  taskStatus <- Hasql.Decoders.column $ Hasql.Decoders.nonNullable taskStatusDecoder
  return $ Task taskId taskDescription taskStatus

boolToTaskStatus :: Bool -> TaskStatus 
boolToTaskStatus True = Done 
boolToTaskStatus False = NotDone 

taskStatusDecoder :: Hasql.Decoders.Value TaskStatus 
taskStatusDecoder = fmap boolToTaskStatus Hasql.Decoders.bool 

stringDecoder :: Hasql.Decoders.Value String 
stringDecoder = fmap T.unpack Hasql.Decoders.text 

stringDecoderRow :: Hasql.Decoders.Row String 
stringDecoderRow = Hasql.Decoders.column $ Hasql.Decoders.nonNullable stringDecoder

taskStatusDecoderRow :: Hasql.Decoders.Row TaskStatus
taskStatusDecoderRow = Hasql.Decoders.column $ Hasql.Decoders.nonNullable taskStatusDecoder



connectionSettings :: Hasql.Connection.Settings
connectionSettings =
  Hasql.Connection.settings
    "localhost"
    5555
    "myuser"
    "mypass"
    "todolists"

runSessionAndClose :: Hasql.Session.Session a -> IO a
runSessionAndClose session =
  do
    connectionResult <- Hasql.Connection.acquire connectionSettings
    case connectionResult of
      Left (Just errMsg) -> error $ StrictUTF8.toString errMsg
      Left Nothing -> error "Unspecified connection error"
      Right connection -> do
        sessionResult <- Hasql.Session.run session connection
        Hasql.Connection.release connection
        case sessionResult of
          Right result -> return result
          Left err -> error $ show err

fetchFromDB :: IO [Task]
fetchFromDB = runSessionAndClose selectTasksSession

pushTaskStatement :: Hasql.Statement.Statement String ()
pushTaskStatement =
  Hasql.Statement.Statement
    "INSERT INTO todolist_ch8 ( \
    \  task, done )\
    \VALUES \
    \  ( $1, FALSE );"
    taskNameEncoder
    Hasql.Decoders.noResult
    True

taskNameEncoder :: Hasql.Encoders.Params String
taskNameEncoder = Hasql.Encoders.param $ Hasql.Encoders.nonNullable taskNameEncoderValue

taskNameEncoderValue :: Hasql.Encoders.Value String
taskNameEncoderValue = contramap T.pack Hasql.Encoders.text

pushTaskToDB :: String -> IO ()
pushTaskToDB taskDescription =
  let
    session :: Hasql.Session.Session ()
    session = Hasql.Session.statement taskDescription pushTaskStatement
  in
    runSessionAndClose session

completeTaskStatement :: Hasql.Statement.Statement Int32 ()
completeTaskStatement =
  Hasql.Statement.Statement
    "UPDATE todolist_ch8 \
    \SET done=TRUE \
    \WHERE id=$1;"
    ( Hasql.Encoders.param $ Hasql.Encoders.nonNullable Hasql.Encoders.int4 )
    Hasql.Decoders.noResult
    True

completeTask :: Int32 -> IO ()
completeTask taskId =
  let
    session :: Hasql.Session.Session ()
    session = Hasql.Session.statement taskId completeTaskStatement
  in
    runSessionAndClose session
