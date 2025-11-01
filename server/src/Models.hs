{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}

module Models (State(State, todos), TodoVar, TodoList, Todo(..), TodoVariable(..), UUID, ErrorMsg(..),
    initialize, postTodo, insertMocks, deleteTodo, putTodo, postTodos, updateRecord) where

import Data.Aeson (ToJSON (toJSON), FromJSON (parseJSON), sumEncoding, genericToJSON, defaultOptions, SumEncoding (TaggedObject), genericParseJSON)
import GHC.Generics (Generic)
import qualified Data.Text.Lazy as L
import Data.List (find)
import Control.Concurrent.STM (TVar, newTVarIO, atomically, modifyTVar, readTVarIO, readTVar, STM, writeTVar)
import Control.Monad (void)
import Data.Maybe (isJust)

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

insertMocks :: TodoVar -> IO ()
insertMocks todoVar = do
  void $ postTodos [mock1, mock2] todoVar
  void $ postTodos [mock3, mock4] todoVar

--- 
--- TVar interface
---

data ErrorMsg = E404 L.Text | E409 L.Text | E410 L.Text
  deriving (Eq, Generic)

instance Show ErrorMsg where 
  show :: ErrorMsg -> String
  show (E404 t) = "404 Not Found Error: " <> show t
  show (E409 t) = "409 Conflict Error: " <> show t
  show (E410 t) = "410 Gone Error: " <> show t

instance ToJSON ErrorMsg
instance FromJSON ErrorMsg

modifyTodoList :: (TodoList -> TodoList) -> TodoVar -> IO ()
modifyTodoList f tVar = atomically $ modifyTVar tVar f


updateById :: UUID -> (Todo -> Todo) -> TodoVar -> STM (Either ErrorMsg Todo)
updateById uuid updateFn stateTVar = do
  records <- readTVar stateTVar
  let (before, rest) = break (\r -> r.id == uuid) records
  case rest of
    [] -> return $ Left $ E409 ("No Todo with id " <> uuid)  -- No match, do nothing
    (r:after) -> do
      let updatedRecord = updateFn r
          newRecords = before <> (updatedRecord : after)
      writeTVar stateTVar newRecords
      return $ Right updatedRecord

updateRecord :: UUID -> TodoVariable -> TodoVar -> IO (Either ErrorMsg Todo)
updateRecord uuid val tVar =  atomically $ updateById uuid (modTodo val) tVar



postTodo :: Todo -> TodoVar-> IO (Either ErrorMsg Todo)
postTodo todo tVar = do 
  alreadyExists <- todoExists tVar todo
  case alreadyExists of 
    True -> return $ Left $ E409 ("Todo with id " <> todo.id <> " already Exists")
    False -> do 
      modifyTodoList (addTodo todo) tVar
      return $ Right todo

postTodos :: [Todo] -> TodoVar -> IO (Either ErrorMsg [Todo])
postTodos todos tVar = do 
  overlaps <- overlap' tVar todos
  case overlaps of 
    True -> return $ Left $ E409 ("A posted Todo's ID is already in use")
    False -> do 
      modifyTodoList (addTodos todos) tVar
      return $ Right todos

deleteTodo :: UUID -> TodoVar -> IO (Either ErrorMsg UUID)
deleteTodo uuid tVar = do 
  tList <- readTVarIO tVar
  exists <- return $ findById uuid tList
  case exists of 
    Nothing -> return $ Left $ E409 ("No Todo with ID " <> uuid)
    Just _ -> do
      modifyTodoList (rmTodo uuid) tVar
      return $ Right uuid

putTodo :: Todo -> TodoVar -> IO (Either ErrorMsg Todo)
putTodo newTodo tVar = do 
  alreadyExists <- todoExists tVar newTodo
  case alreadyExists of 
    False -> return $ Left $ E409 ("No Todo with ID " <> newTodo.id)
    True -> do 
      modifyTodoList (map putter) tVar
      return $ Right newTodo
      where 
        putter :: Todo -> Todo 
        putter oldTodo = 
          if oldTodo.id == newTodo.id 
            then newTodo { syncStatus=FromServer } 
            else oldTodo


overlap' :: TodoVar -> [Todo] -> IO Bool
overlap' tVar todos = do 
    tList <- readTVarIO tVar
    return $ overlap tList todos 

todoExists :: TodoVar -> Todo -> IO (Bool)
todoExists tVar todo = do 
    tList <- readTVarIO tVar
    let foundMatch = findById todo.id tList
    return $ isJust foundMatch
