{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Models (State(State, todos), TodoVar, TodoList, Todo(..), UUID,
    initialize, postTodo, insertMocks, deleteTodo, putTodo, postTodos, todoExists, overlap') where

import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import qualified Data.Text.Lazy as L
import Data.List (find, intersect, null)
import Control.Concurrent.STM (TVar, newTVarIO, atomically, modifyTVar, readTVarIO)
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

data TodoValue = Name Name | Completed Bool | SyncStatus SyncStatus
  deriving (Eq, Show, Generic)

instance ToJSON TodoValue
instance FromJSON TodoValue

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

modTodo :: TodoValue -> Todo -> Todo
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

--- 
--- TVar interface
---

modifyTodoList :: (TodoList -> TodoList) -> TodoVar -> IO ()
modifyTodoList f tVar = atomically $ modifyTVar tVar f

insertMocks :: TodoVar -> IO ()
insertMocks todoVar = do
  void $ postTodos [mock1, mock2] todoVar
  void $ postTodos [mock3] todoVar
  

postTodo :: Todo -> TodoVar-> IO (Either L.Text Todo)
postTodo todo tVar = do 
  alreadyExists <- todoExists tVar todo
  case alreadyExists of 
    True -> return $ Left "Already Exists"
    False -> do 
      modifyTodoList (addTodo todo) tVar
      return $ Right todo

postTodos :: [Todo] -> TodoVar -> IO (Either L.Text [Todo])
postTodos todos tVar = do 
  overlaps <- overlap' tVar todos
  case overlaps of 
    True -> return $ Left "ID of a posted Todos is already in use."
    False -> do 
      modifyTodoList (addTodos todos) tVar
      return $ Right todos

deleteTodo :: UUID -> TodoVar -> IO (Either L.Text UUID)
deleteTodo uuid tVar = do 
  tList <- readTVarIO tVar
  exists <- return $ findById uuid tList
  case exists of 
    Nothing -> return $ Left $ "No Todo with ID " <> uuid <> " to delete"
    Just _ -> do
      modifyTodoList (rmTodo uuid) tVar
      return $ Right uuid

putTodo :: Todo -> TodoVar -> IO (Either L.Text Todo)
putTodo newTodo tVar = do 
  alreadyExists <- todoExists tVar newTodo
  case alreadyExists of 
    False -> return $ Left $ "No Todo with ID " <> newTodo.id <> " to update"
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
