{-# LANGUAGE OverloadedStrings #-}

module ServerMain
    ( startServer
    ) where

import qualified Network.Wai.Handler.Warp as Warp (run)
import Network.Wai (Application, Request, Response, ResponseReceived, responseLBS)
import Network.Wai as Wai
import Network.HTTP.Types.Status (status200, status400, status404)
import Network.HTTP.Types.URI (parseQuery)
import qualified Data.ByteString.Lazy.UTF8 as UTF8 (fromString)
import qualified Data.ByteString.Lazy 
import qualified Data.ByteString.UTF8 as StrictUTF8 (fromString, toString)
import qualified Data.Text as T 

import HTML (HTML, toHTML, toHTMLPage)
import TaskDB (Task)
import qualified TaskDB
import GHC.Int (Int32)
import Text.Read (readMaybe)



startServer :: IO ()
startServer = do 
  --TaskDB.pushTaskToDB "implement new feature request for client"
  Warp.run 8080 requestHandler

requestHandler :: Wai.Request 
              -> (Wai.Response -> IO Wai.ResponseReceived) 
              -> IO Wai.ResponseReceived
requestHandler request respond =
  case Wai.pathInfo request of
    [] -> getPage request respond
    "posttask" : ps -> postTask request respond
    "completetask" : ps -> completeTask request respond
    _ -> respond $ responseLBS status404 [] $ "Could not find " 
      <> Data.ByteString.Lazy.fromStrict ( Wai.rawPathInfo request )

getPage :: Application
getPage request respond =
  let
    htmlPage taskList = UTF8.fromString $ toHTMLPage taskList
      <>  "<form action=\"/posttask\" method=\"POST\">\
          \  <input type=\"text\" name=\"taskDescription\"></input>\
          \  <input type=\"submit\" value=\"Create task\"></input>\
          \</form>"
    response tasks = responseLBS status200 [] $ htmlPage tasks
  in
    do
      taskList <- TaskDB.fetchFromDB
      respond $ response taskList

postTask :: Application
postTask request respond =
  let
    getArgs :: IO (Maybe String)
    getArgs = do
      bodyBS <- strictRequestBody request
      case parseQuery $ Data.ByteString.Lazy.toStrict bodyBS of
        [("taskDescription", Just taskDescriptionBS)] ->
          return $ Just $ StrictUTF8.toString taskDescriptionBS
        _ -> return Nothing
  in do
    args <- getArgs
    case args of
      Just taskDescription -> do
        TaskDB.pushTaskToDB taskDescription
        getPage request respond
      Nothing -> do
        respond $ responseLBS status400 [] 
          "Bad arguments, must be of the form \"taskDescription=<string>\""


completeTask :: Application
completeTask request respond =
  let
    getArgs :: IO (Maybe Int32)
    getArgs = do
      bodyBS <- strictRequestBody request
      case parseQuery $ Data.ByteString.Lazy.toStrict bodyBS of
        [("taskId", Just taskIdBS)] ->
          return $ readMaybe $ StrictUTF8.toString taskIdBS
        _ ->
          return Nothing
  in do
    args <- getArgs
    case args of
      Just taskId -> do
        TaskDB.completeTask taskId
        getPage request respond
      Nothing -> do
        print $ queryString request
        respond $ responseLBS status400 [] "Bad arguments, must be of the form \"taskId=<integer>\""
