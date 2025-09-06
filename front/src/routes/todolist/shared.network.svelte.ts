import { BackEndPoint } from "./shared.svelte";
import { Todo } from '$lib/services/database';
import { hsUrl, getJSON } from "$lib/services/network";

async function netGetTodos() {
    return (await getJSON('getTodos'));
};

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
    if (!response.ok){
        throw new Error('Network response was not ok')
    } 
    else return response.json();
};

async function netPostTodo(newTodo: Todo) {
    await modifyingQuery('postTodo', newTodo);
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

export { netGetTodos, netPostTodo, netPostTodos, netPutTodo, netDelTodo };