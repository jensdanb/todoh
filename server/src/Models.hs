{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module Models (State(State, todos), TodoVar, TodoList, Todo(..), TodoVariable(..), UUID,
    initialize, postTodo, insertMocks, deleteTodo, putTodo, postTodos, updateRecord) where

import Data.Aeson (ToJSON (toJSON), FromJSON (parseJSON), sumEncoding, genericToJSON, defaultOptions, SumEncoding (TaggedObject), genericParseJSON)
import GHC.Generics (Generic)
import qualified Data.Text.Lazy as L
import Data.Text.Lazy.Encoding (encodeUtf8)
import Data.List (find)
import Control.Concurrent.STM (TVar, newTVarIO, atomically, modifyTVar, readTVarIO, readTVar, STM, writeTVar)
import Control.Monad (void)
import Data.Maybe (isJust)
import Servant (ServerError (errBody), err409)

--- 
--- Definitions
---
-- type TodoKeyValue = (UUID, Todo)

data Todo = Todo
    { id :: UUID
    , name :: Name
    , completed :: Bool
    , syncStatus :: SyncStatus
    } deriving (Eq, Show, Generic)

instance ToJSON Todo
instance FromJSON Todo


type UUID = L.Text
type Name = L.Text
type TodoList = [Todo]


data SyncStatus = FromServer | FromServerAndEdited | FreshFish | Posted
  deriving (Eq, Show, Generic)

instance ToJSON SyncStatus
instance FromJSON SyncStatus


data TodoVariable = Name Name | Completed Bool | SyncStatus SyncStatus
  deriving (Eq, Show, Generic)


instance ToJSON TodoVariable where
  toJSON = genericToJSON defaultOptions
    { sumEncoding = TaggedObject "label" "value" }
instance FromJSON TodoVariable where 
  parseJSON = genericParseJSON defaultOptions
    { sumEncoding = TaggedObject "label" "value" }


--- 
--- State
--- 

type TodoVar = TVar TodoList
newtype State = State { 
  todos :: TodoVar
  } deriving (Generic)

initialize :: IO TodoVar
initialize = newTVarIO []


--- 
--- Logic
--- 

modTodo :: TodoVariable -> Todo -> Todo
modTodo (Name newVal) todo = todo {name=newVal}
modTodo (Completed newVal) todo = todo {completed=newVal}
modTodo (SyncStatus newVal) todo = todo {syncStatus=newVal}


markSynced :: Todo -> Todo 
markSynced = modTodo (SyncStatus FromServer)


addTodo :: Todo -> (TodoList -> TodoList)
addTodo todo = (markSynced todo : )


addTodos :: [Todo] -> (TodoList -> TodoList)
addTodos todos = (reverse (map markSynced todos) <> )


rmTodo :: UUID -> (TodoList -> TodoList)
rmTodo uuid = (filter (\todo -> todo.id /= uuid))


matchingId :: UUID -> Todo -> Bool
matchingId uuid todo = uuid == todo.id


findById :: UUID -> TodoList -> Maybe Todo
findById uuid = find (matchingId uuid)


overlap :: [Todo] -> [Todo] -> Bool
overlap todos todos' = any id [x.id == y.id | x <- todos , y <- todos']


--- 
--- Defaults and templates
--- 

data TodoTemplate = TodoTemplate 
  { completed' :: Bool
  , syncStatus' :: SyncStatus} 
  deriving (Eq, Show, Generic)


baseTodo :: TodoTemplate
baseTodo = TodoTemplate {completed'=False, syncStatus'=FromServer}


todoFromTemplate :: TodoTemplate -> UUID -> Name -> Todo
todoFromTemplate temp uuid name = Todo{id=uuid, name=name, completed=temp.completed', syncStatus=temp.syncStatus'}


mock1, mock2, mock3, mock4 :: Todo
mock1 = modTodo (Completed True) $ todoFromTemplate baseTodo "todo-1sgsgerjkg" "Eat"
mock2 = todoFromTemplate baseTodo "todo-2sigisgoel" "Sleep"
mock3 = todoFromTemplate baseTodo "todo-3efkiffieu" "Repeat"
mock4 = todoFromTemplate baseTodo "todo-efwpekkgwm" "Repeat"


insertMocks :: TodoVar -> STM ()
insertMocks todoVar = do
  void $ postTodos [mock1, mock2] todoVar
  void $ postTodos [mock3, mock4] todoVar

--- 
--- TVar interface
---

modifyTodoList :: (TodoList -> TodoList) -> TodoVar -> STM ()
modifyTodoList f tVar = modifyTVar tVar f


updateById :: UUID -> (Todo -> Todo) -> TodoVar -> STM (Either ServerError Todo)
updateById uuid updateFn stateTVar = do
  records <- readTVar stateTVar
  let (before, rest) = break (\r -> r.id == uuid) records
  case rest of
    [] -> return $ Left err409 {errBody = ("No Todo with id " <> encodeUtf8 uuid)}
    (r:after) -> do
      let updatedRecord = updateFn r
          newRecords = before <> (updatedRecord : after)
      writeTVar stateTVar newRecords
      return $ Right updatedRecord


updateRecord :: UUID -> TodoVariable -> TodoVar -> STM (Either ServerError Todo)
updateRecord uuid val tVar =  updateById uuid (modTodo val) tVar


postTodo :: Todo -> TodoVar-> STM (Either ServerError Todo)
postTodo todo tVar = do 
  alreadyExists <- todoExists tVar todo
  case alreadyExists of 
    True -> return $ Left err409 {errBody = ("Todo with id " <> encodeUtf8 todo.id <> " already Exists")}
    False -> do 
      modifyTodoList (addTodo todo) tVar
      return $ Right todo


postTodos :: [Todo] -> TodoVar -> STM (Either ServerError [Todo])
postTodos todos tVar = do 
  overlaps <- overlap' tVar todos
  case overlaps of 
    True -> return $ Left err409 {errBody = ("A posted Todo's ID is already in use")}
    False -> do 
      modifyTodoList (addTodos todos) tVar
      return $ Right todos


deleteTodo :: UUID -> TodoVar -> STM (Either ServerError UUID)
deleteTodo uuid tVar = do 
  tList <- readTVar tVar
  exists <- return $ findById uuid tList
  case exists of 
    Nothing -> return $ Left err409 {errBody = ("No Todo with ID " <> encodeUtf8 uuid)}
    Just _ -> do
      modifyTodoList (rmTodo uuid) tVar
      return $ Right uuid


putTodo :: Todo -> TodoVar -> STM (Either ServerError Todo)
putTodo newTodo tVar = do 
  alreadyExists <- todoExists tVar newTodo
  case alreadyExists of 
    False -> return $ Left err409 {errBody = ("No Todo with ID " <> encodeUtf8 newTodo.id)}
    True -> do 
      modifyTodoList (map putter) tVar
      return $ Right newTodo
      where 
        putter :: Todo -> Todo 
        putter oldTodo = 
          if oldTodo.id == newTodo.id 
            then newTodo { syncStatus=FromServer } 
            else oldTodo


overlap' :: TodoVar -> [Todo] -> STM Bool
overlap' tVar todos = do 
    tList <- readTVar tVar
    return $ overlap tList todos 


todoExists :: TodoVar -> Todo -> STM (Bool)
todoExists tVar todo = do 
    tList <- readTVar tVar
    let foundMatch = findById todo.id tList
    return $ isJust foundMatch
