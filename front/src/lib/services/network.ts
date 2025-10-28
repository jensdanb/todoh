import { browser } from "$app/environment";

/* Old way
const hsLocal = 'http://localhost:8080/';

const hsProxy = 'http://localhost/api/';
// const hsLocalIp = 'http://192.168.1.86:80/api/';
// const hsServer = 'http://46.62.152.102:80/api/';
const hsUrl = hsProxy;
*/

const hsDockerExt = 'http://hsserver:8080/'
const hsProd = 'https://todo.jensdanbolt.no/api/'
const hsDockerHost = 'http://host.docker.internal:80/api/'


// New way
// For server-side requests
const backendUrl = process.env.BACKEND_URL || 'http://hsserver:8080';

// For browser-side requests
const apiBaseUrl = browser ? '/api' : backendUrl;


/*
const networkErrorResponse = (response: Response) => {
    return new Response("Network error happened: " + response.status + ' ' + response.statusText, {
        status: 408,
        headers: { "Content-Type": "text/plain" },
    });
};
*/

const requestErrorResponse = (suspect='') => {
    return new Response("Invalid request: " + suspect, {
        status: 400,
        headers: { "Content-Type": "text/plain" },
    });
};

async function getJSON (address: string) {
    const response = await fetch(apiBaseUrl + address);
    if (!response.ok) throw new Error('Network response was not ok');
    return response.json();
};


export { getJSON, apiBaseUrl, requestErrorResponse };