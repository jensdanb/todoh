import { nanoid } from "nanoid";

export type BackEndPoint = "serverConnected" | "postTodo" | "postTodos" | "getTodos" | "delTodo" | "putTodo"

export class Todo {
    id: string; 
    name: string;
    completed: boolean;
    knownUnSynced : boolean

    constructor(name: string, id?: string, completed: boolean = false, knownUnSynced: boolean = true) {
        this.id = id ?? `todo-${nanoid()}`; 
        this.name = name; 
        this.completed = completed; 
        this.knownUnSynced = knownUnSynced;
      }
} 
