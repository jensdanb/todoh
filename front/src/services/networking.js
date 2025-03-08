
const hsUrl = 'http://localhost:8080';

const networkErrorResponse = (response) => {
    return new Response("Network error happened: " + response.status + ' ' + response.statusText, {
        status: 408,
        headers: { "Content-Type": "text/plain" },
    });
};

const requestErrorResponse = (suspect='') => {
    return new Response("Invalid request: " + suspect, {
        status: 400,
        headers: { "Content-Type": "text/plain" },
    });
};

async function getJSON (address) {
    const response = await fetch(hsUrl + address);
    if (!response.ok){
        throw new Error('Network response was not ok')
    } 
    else return response.json();
};

async function netGetTodos() {
    return (await getJSON('/getTodos'));
};

async function modifyingQuery (address, clientTodo) {
    var method = "";
    if (["/postTodo", "/postTodos"].includes(address)) {
        method = "POST"
    }
    else if (["/putTodo"].includes(address)) {
        method = "PUT"
    }
    else if (["/delTodo"].includes(address)) {
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

async function netPostTodo(newTodo) {
    await modifyingQuery('/postTodo', newTodo);
};

async function netPostTodos(newTodos) { // Array
    return await modifyingQuery('/postTodos', newTodos);
};

async function netPutTodo(newTodo) {
    await modifyingQuery('/putTodo', newTodo);
};

async function netDelTodo(id) {
    await modifyingQuery('/delTodo', id);
};

function setServerData(address, setter) {
    fetch(hsUrl + address)
        .then(response => response.json())
        .then(data => setter( data ))
        .catch(error => {
        console.error('Error: ', error)
        }
        );
};

export { getJSON, hsUrl, netGetTodos, netPostTodo, netPostTodos, netPutTodo, netDelTodo };