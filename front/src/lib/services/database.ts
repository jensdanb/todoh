// In a real app, this data would live in a database,
// rather than in memory. But for now, we cheat.
import { Todo } from "./types";

const db = new Map<string, [Todo]>();

export function getTodos(userid: string): [Todo] {
	if (!db.get(userid)) {
		db.set(userid, [new Todo("Learn Svelte")]);
	}
	return db.get(userid);
}

export function createTodo(userid: string, description: string) {
	const todos: [Todo] = db.get(userid);

	todos.push(new Todo(description));
    console.log('Todos: ' + todos);
}

export function deleteTodo(userid: string, todoid: string) {
	const todos = db.get(userid);
	const index = todos.findIndex((todo: Todo) => todo.id === todoid);

	if (index !== -1) {
		todos.splice(index, 1);
	}
}


export function localPut(userid: string, id: string, new_task: Todo) {
    const todos = db.get(userid);
    todos.map((task: Todo) => {
        if (task.id == id) {
            return {...new_task, id: id}
        }
        else {
            return task
        }
    })
}