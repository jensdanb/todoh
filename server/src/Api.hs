{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Api where

import Servant
import Models (State(State, todos), TodoVar, TodoList, Todo(..), UUID,
    postTodo, postTodos, deleteTodo, putTodo, initialize, insertMocks, TodoVariable, updateRecord)
import Network (runServerWithCors)
import Control.Concurrent.STM (readTVarIO)
import Control.Monad.Trans.Reader  (ReaderT, ask, runReaderT)
import Control.Monad.Reader (liftIO)
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)

---
--- Server 
--- 

type AppM = ReaderT State Handler

stmApp :: State -> Application
stmApp state = serve stmAPI $ hoistServer stmAPI (nt state) serveSTM

nt :: State -> AppM a -> Handler a
nt state x = runReaderT x state

runStmServer :: Int -> IO ()
runStmServer port = do
    startState <- initialize
    runServerWithCors (stmApp (State startState)) port


runStmServerWithMocks :: Int -> IO ()
runStmServerWithMocks port = do
    startState <- initialize
    liftIO $ insertMocks startState
    runServerWithCors (stmApp (State startState)) port

---
--- API toplevel 
--- 

type STMAPI = EPmeta
        :<|> PostTodo
        :<|> PostTodos
        :<|> GetTodos
        :<|> DelTodo
        :<|> PutTodo
        :<|> PutTodoVariable

serveSTM :: ServerT STMAPI AppM
serveSTM = handleStatusMessage
        :<|> handlePostTodo
        :<|> handlePostTodos
        :<|> handleGetTodos
        :<|> handleDelTodo
        :<|> handlePutTodo
        :<|> handlePutTodoVariable

stmAPI :: Proxy STMAPI
stmAPI = Proxy

--- 
--- API endpoints
--- 

type EPmeta = "serverConnected" :> Get '[JSON] Bool

handleStatusMessage :: AppM Bool
handleStatusMessage = return True

type GetTodos = "getTodos" :> Get '[JSON] TodoList

handleGetTodos :: AppM TodoList
handleGetTodos = do
    State{todos = todoVar} <- ask
    liftIO $ reverse <$> readTVarIO todoVar


genericHandler :: a -> (a -> TodoVar -> IO (Either ServerError a)) -> AppM a
genericHandler var f = do
    State{todos = todoVar} <- ask
    response <- liftIO $ f var todoVar
    case response of 
        Right result -> return result
        Left err -> throwError err


type PostTodo = "postTodo" :> ReqBody '[JSON] Todo :> PostCreated '[JSON] Todo

handlePostTodo :: Todo -> AppM Todo
handlePostTodo newTodo = genericHandler newTodo postTodo

type PostTodos = "postTodos" :> ReqBody '[JSON] [Todo] :> PostCreated '[JSON] [Todo]

handlePostTodos :: [Todo] -> AppM [Todo]
handlePostTodos newTodos = genericHandler newTodos postTodos

type DelTodo = "delTodo" :> ReqBody '[JSON] UUID :> Delete '[JSON] UUID

handleDelTodo :: UUID -> AppM UUID
handleDelTodo uuid = genericHandler uuid deleteTodo
    

type PutTodo = "putTodo" :> ReqBody '[JSON] Todo :> Put '[JSON] Todo

handlePutTodo :: Todo -> AppM Todo
handlePutTodo newTodo = genericHandler newTodo putTodo

type PutTodoVariable = "putTodoVar" :> ReqBody '[JSON] TodoVariableRequest :> Put '[JSON] Todo

handlePutTodoVariable :: TodoVariableRequest -> AppM Todo
handlePutTodoVariable (TodoVariableRequest reqId reqData) = do
    State{todos = todoVar} <- ask 
    response <- liftIO $ updateRecord reqId reqData todoVar
    case response of 
        Right result -> return result
        Left err -> throwError err

data TodoVariableRequest = TodoVariableRequest
  { reqId    :: UUID
  , reqData :: TodoVariable
  } deriving (Show, Generic)

instance FromJSON TodoVariableRequest
instance ToJSON TodoVariableRequest