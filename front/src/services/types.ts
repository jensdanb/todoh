
export type BackEndPoints = "serverConnected" | "postTodo" | "postTodos" | "getTodos" | "delTodo" | "putTodo"

export interface Todo {
    completed : boolean, 
    id : string, 
    name : string, 
    knownUnSynced : boolean
}