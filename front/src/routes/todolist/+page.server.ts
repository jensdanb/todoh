import type { Cookies, Actions } from '@sveltejs/kit';
import { Todo } from '$lib/services/types';
import { apiBaseUrl, getJSON, requestErrorResponse } from "$lib/services/network";

// ------- //
// Network //

type BackEndPoint = "/serverConnected" | "/postTodo" | "/postTodos" | "/getTodos" | "/delTodo" | "/putTodo" | "/putTodoVar";

function getMethod (address: BackEndPoint) {
    switch (address) {
        case "/postTodo":
            return "POST";
        case "/postTodos":
            return "POST";
        case "/putTodo":
            return "PUT";
        case "/putTodoVar":
            return "PUT";
        case "/delTodo":
            return "DELETE";
        default:
            return requestErrorResponse(address);
    }
}

async function todoQuery (address: BackEndPoint, clientTodo: Todo | [Todo]) {
    var method = getMethod(address);
    if (method instanceof Response) {
        return method;
    }
    const response = await fetch(apiBaseUrl + address, {
                method: method,
                body: JSON.stringify(clientTodo),
                headers: {"Content-type": "application/json; charset=UTF-8"}
        })
    if (!response.ok) throw new Error('Network response was not ok')
    return response.json();
};

async function todoIdQuery(address: BackEndPoint, targetId: string, data?: any) {
    
}

async function netPostTodo(newTodo: Todo) {
    return await todoQuery('/postTodo', newTodo);
};

async function netPostTodos(newTodos: [Todo]) {
    return await todoQuery('/postTodos', newTodos);
};

async function netPutTodo(newTodo: Todo) {
    await todoQuery('/putTodo', newTodo);
};

async function netDelTodo(id: string) {
    await todoQuery('/delTodo', id);
};

// network  //
// local     //

export async function load({ cookies }: {cookies: Cookies}) {
    let id: string | undefined = cookies.get('userid');
    const todoList = await getJSON('/getTodos');

    if (!id) {
        id = crypto.randomUUID();
        cookies.set('userid', id, { path: '/'})
    }

    return {
        todos: todoList
    };
}



export const actions: Actions = {
	create: async ({ cookies, request }) => {
		const formData = await request.formData();
        await netPostTodo(new Todo(formData.get('description') as string));
	}, 
    toggle: async ({cookies, request}) => {
        const formData = await request.formData();
        const todo: Todo = JSON.parse(formData.get('rename-id') as string);
        await netPutTodo({...todo, completed: !todo.completed});
    }, 
    rename: async ({cookies, request}) => {
        const formData = await request.formData();
        const todo: Todo = JSON.parse(formData.get('rename-id') as string);
        const newDescription = formData.get('new name') as string;
        await netPutTodo({...todo, name: newDescription});
    }, 
    delete: async ({cookies, request}) => {
        const formData = await request.formData();
        const todoId: string = formData.get('rename-id') as string;
        await netDelTodo(todoId);
    }
};

