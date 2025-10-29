{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Models where

import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import qualified Data.Text.Lazy as L
import Data.List (find, intersect, null)
import Control.Concurrent.STM (TVar, newTVarIO, atomically, modifyTVar, readTVarIO)
import Data.Maybe (isJust)

--- 
--- Definitions
---
-- type TodoKeyValue = (UUID, Todo)
type TodoList = [Todo]

type UUID = L.Text
type Name = L.Text

data SyncStatus = FromServer | FromServerAndEdited | FreshFish | Posted
  deriving (Eq, Show, Generic)

instance ToJSON SyncStatus
instance FromJSON SyncStatus

data Todo = Todo
    { id :: UUID
    , name :: Name
    , completed :: Bool
    , syncStatus :: SyncStatus
    } deriving (Eq, Show, Generic)


instance ToJSON Todo
instance FromJSON Todo

data TodoValue = Name Name | Completed Bool | SyncStatus SyncStatus
  deriving (Eq, Show, Generic)

instance ToJSON TodoValue
instance FromJSON TodoValue

--- 
--- State
--- 

type TodoVar = TVar TodoList
newtype State = State { 
  todos :: TVar TodoList
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

modifyTodoList :: (TodoList -> TodoList) -> TodoVar -> IO ()
modifyTodoList f tVar = atomically $ modifyTVar tVar f

insertTodo :: Todo -> TodoVar -> IO ()
insertTodo newTodo = modifyTodoList (markSynced newTodo : )

insertTodos :: [Todo] -> TodoVar -> IO ()
insertTodos newTodos = modifyTodoList $ (reverse newTodos' <>)
  where newTodos' = map markSynced newTodos

deleteTodo :: UUID -> TodoVar -> IO ()
deleteTodo uuid = modifyTodoList (filter (\todo -> todo.id /= uuid))

putTodo :: Todo -> TodoVar -> IO ()
putTodo newTodo = modifyTodoList (map putter)
  where 
    putter :: Todo -> Todo 
    putter oldTodo = 
      if oldTodo.id == newTodo.id 
        then newTodo { syncStatus=FromServer } 
        else oldTodo

matchingId :: UUID -> Todo -> Bool
matchingId uuid todo = uuid == todo.id

findById :: UUID -> TodoList -> Maybe Todo
findById uuid = find (matchingId uuid)

overlap :: [Todo] -> [Todo] -> Bool
overlap todos todos' = not . null $ ids `intersect` ids'
  where 
    ids = map (.id) todos
    ids' = map (.id) todos'

overlap' :: TodoVar -> [Todo] -> IO Bool
overlap' tVar todos = do 
    tList <- readTVarIO tVar
    return $ overlap tList todos 

todoExists :: TodoVar -> Todo -> IO (Bool)
todoExists tVar todo = do 
    tList <- readTVarIO tVar
    let foundMatch = findById todo.id tList
    return $ isJust foundMatch

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
  insertTodos [mock1, mock2] todoVar
  insertTodos [mock3] todoVar
