import { type Cookies, type Actions, fail } from '@sveltejs/kit';
import { Todo, type SyncStatus, CustomError } from '$lib/services/types';
import { apiBaseUrl, getJSON, requestErrorResponse } from "$lib/services/network";

// ------- //
// Network //

type BackEndPoint = "/serverConnected" | "/getTodos" | "/postTodo" | "/postTodos" | "/delTodo" | "/putTodo" | "/putTodoVar";

const METHOD_MAP = new Map<BackEndPoint, string>();
METHOD_MAP.set("/serverConnected", "GET");
METHOD_MAP.set("/getTodos", "GET");
METHOD_MAP.set("/postTodo", "POST");
METHOD_MAP.set("/postTodos", "POST");
METHOD_MAP.set("/delTodo", "DELETE");
METHOD_MAP.set("/putTodo", "PUT");
METHOD_MAP.set("/putTodoVar", "PUT");

async function handleError(response: Response) {
    let errorMessage = 'Network response was not ok';
        try {
            const errorData = await response.json();
            errorMessage = errorData.message || errorData.error || errorMessage;
        } catch (e) {
            // If parsing fails, use the default message
        }
        throw new Error(errorMessage);
        
}

async function todoQuery (address: BackEndPoint, clientTodo: Todo | [Todo] | string) {
    const method = METHOD_MAP.get(address);

    console.log(clientTodo);
    const response = await fetch(apiBaseUrl + address, {
                method: method,
                body: JSON.stringify(clientTodo),
                headers: {"Content-type": "application/json; charset=UTF-8"}
        })
    
    console.log(response);
    if (!response.ok) {
        const errorData = await response.json();
        throw new CustomError(errorData.message || 'Network response was not ok', response.status);
    }
    return response.json();
};

async function todoIdQuery(address: BackEndPoint, 
                            targetId: string, 
                            dataField?: "Name" | "Completed" | "SyncStatus" , 
                            data?: string | boolean | SyncStatus) {
    const method = METHOD_MAP.get(address);

    var reqBody
    if (dataField && data !== undefined) {
        reqBody = {
            "reqId": targetId, 
            "reqData": {
                "label": dataField,
                "value": data
            }};
    }
    else {
        reqBody = {"reqId": targetId}
    }
    console.log(reqBody);

    const response = await fetch(apiBaseUrl + address, {
        method: method,
        body: JSON.stringify(reqBody),
        headers: {"Content-type": "application/json; charset=UTF-8"}
        })
    console.log(response);
    if (!response.ok) {
        const errorData = await response.json();
        throw new CustomError(errorData.message || 'Network response was not ok', response.status);
    }
    return response.json();
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

async function netToggleTodo(id:string, toggleTo: boolean) {
    await todoIdQuery('/putTodoVar', id, "Completed", toggleTo);
}

async function netRenameTodo(id:string, newName: string) {
    await todoIdQuery('/putTodoVar', id, "Name", newName);
}

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
        await netToggleTodo(todo.id, !todo.completed);
    }, 
    rename: async ({cookies, request}) => {
        const formData = await request.formData();
        const todo: Todo = JSON.parse(formData.get('rename-id') as string);
        const newDescription = formData.get('new name') as string;
        await netRenameTodo(todo.id, newDescription);
    }, 
    delete: async ({cookies, request}) => {
        const formData = await request.formData();
        const todoId: string = formData.get('rename-id') as string;
        try {
            await netDelTodo(todoId);
            return { success: true};
        } catch (error) {
            console.log(error);
            if (error instanceof CustomError) {
                return fail(error.status, {error: error.message});
            }
            return fail(500, {error: 'An unknown error occurred'});
        }
    }
};

