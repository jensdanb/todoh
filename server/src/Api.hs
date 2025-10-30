{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Api where

import Servant
import Models (State(State, todos), TodoVar, TodoList, Todo(..), UUID,
    postTodo, postTodos, deleteTodo, putTodo, initialize, insertMocks)
import Network (runServerWithCors)
import Control.Concurrent.STM (readTVarIO)
import Control.Monad.Trans.Reader  (ReaderT, ask, runReaderT)
import Control.Monad.Reader (liftIO)
import qualified Data.Text.Lazy as L
import Data.Text.Lazy.Encoding (encodeUtf8)

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

serveSTM :: ServerT STMAPI AppM
serveSTM = handleStatusMessage
        :<|> handlePostTodo
        :<|> handlePostTodos
        :<|> handleGetTodos
        :<|> handleDelTodo
        :<|> handlePutTodo

stmAPI :: Proxy STMAPI
stmAPI = Proxy

--- 
--- API endpoints
--- 

type EPmeta = "serverConnected" :> Get '[JSON] Bool

handleStatusMessage :: AppM Bool
handleStatusMessage = return True

handleGeneric :: a -> (a -> TodoVar -> IO (Either L.Text a)) -> AppM a
handleGeneric var f = do
    State{todos = todoVar} <- ask
    response <- liftIO $ f var todoVar
    case response of 
        Right result -> return result
        Left msg -> throwError err503 { errBody = encodeUtf8 msg }

type PostTodo = "postTodo" :> ReqBody '[JSON] Todo :> PostCreated '[JSON] Todo

handlePostTodo :: Todo -> AppM Todo
handlePostTodo newTodo = handleGeneric newTodo postTodo


type PostTodos = "postTodos" :> ReqBody '[JSON] [Todo] :> PostCreated '[JSON] [Todo]

handlePostTodos :: [Todo] -> AppM [Todo]
handlePostTodos newTodos = handleGeneric newTodos postTodos

type GetTodos = "getTodos" :> Get '[JSON] TodoList

handleGetTodos :: AppM TodoList
handleGetTodos = do
    State{todos = todoVar} <- ask
    liftIO $ reverse <$> readTVarIO todoVar

type DelTodo = "delTodo" :> ReqBody '[JSON] UUID :> Delete '[JSON] UUID

handleDelTodo :: UUID -> AppM UUID
handleDelTodo uuid = do
    State{todos = todoVar} <- ask
    liftIO $ deleteTodo uuid todoVar
    return uuid

type PutTodo = "putTodo" :> ReqBody '[JSON] Todo :> Put '[JSON] Todo

handlePutTodo :: Todo -> AppM Todo
handlePutTodo newTodo = do
    State{todos = todoVar} <- ask
    liftIO $ putTodo newTodo todoVar
    return newTodo

