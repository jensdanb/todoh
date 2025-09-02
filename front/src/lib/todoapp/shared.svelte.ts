import { nanoid } from "nanoid";

// Core

export type BackEndPoint = "serverConnected" | "postTodo" | "postTodos" | "getTodos" | "delTodo" | "putTodo";

export type SyncStatus = "From server" | "From server and edited" | "Fresh fish" | "Posted"

export class Task {
    id: string; 
    name: string;
    completed: boolean;
    syncStatus : SyncStatus

    constructor(name: string, other?: {id?: string, completed?: boolean, syncStatus?: SyncStatus} ) {
        this.name = name; 
        this.id = other?.id ?? `todo-${nanoid()}`; 
        this.completed = other?.completed ?? false; 
        this.syncStatus = other?.syncStatus ?? "Fresh fish";
      };
};

export const todo_list: {todos: Task[]} = $state({todos: [new Task("Dummy")]});

// Local functions

export function local_put(id: string, new_task: Task) {
    todo_list.todos = todo_list.todos.map((task) => {
        if (task.id == id) {
            return {...new_task, id: id}
        }
        else {
            return task
        }
    })
}

export const in_work: {new_task_name: string} = $state({new_task_name: ""});

// UI
export type TodoFilter = "All" | "Pending" | "Completed";

export const todo_filter: {selected: TodoFilter} = $state({
    selected: "All"
});
