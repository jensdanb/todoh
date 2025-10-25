import type { Cookies, Actions } from '@sveltejs/kit';
import { Todo } from '$lib/services/types';
import { hsUrl, getJSON, requestErrorResponse } from "$lib/services/network";

// ------- //
// Network //

type BackEndPoint = "serverConnected" | "postTodo" | "postTodos" | "getTodos" | "delTodo" | "putTodo";

async function modifyingQuery (address: BackEndPoint, clientTodo: Todo | [Todo] | string) {
    var method = "";
    if (["postTodo", "postTodos"].includes(address)) {
        method = "POST"
    }
    else if (["putTodo"].includes(address)) {
        method = "PUT"
    }
    else if (["delTodo"].includes(address)) {
        method = "DELETE"
    }
    else {return requestErrorResponse(address)};

    const response = await fetch(hsUrl + address, {
                method: method,
                body: JSON.stringify(clientTodo),
                headers: {"Content-type": "application/json; charset=UTF-8"}
        })
    if (!response.ok) throw new Error('Network response was not ok')
    return response.json();
};

async function netPostTodo(newTodo: Todo) {
    return await modifyingQuery('postTodo', newTodo);
};

async function netPostTodos(newTodos: [Todo]) {
    return await modifyingQuery('postTodos', newTodos);
};

async function netPutTodo(newTodo: Todo) {
    await modifyingQuery('putTodo', newTodo);
};

async function netDelTodo(id: string) {
    await modifyingQuery('delTodo', id);
};

// network  //
// local     //

export async function load({ cookies }: {cookies: Cookies}) {
    let id: string | undefined = cookies.get('userid');
    const todoList = await getJSON('getTodos');

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
        console.log(formData.get('description'));
        await netPostTodo(new Todo(formData.get('description') as string));
	}, 
    rename: async ({cookies, request}) => {
        const formData = await request.formData();
        const todo: Todo = JSON.parse(formData.get('rename-id') as string);
        const newDescription = formData.get('new name') as string;
        await netPutTodo({...todo, name: newDescription});
    }
};

