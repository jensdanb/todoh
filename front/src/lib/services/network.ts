
const hsLocal = 'http://localhost:80/api/';
// const hsLocalIp = 'http://192.168.1.86:80/api/'
// const hsServer = 'http://46.62.152.102:80/api/';
// const hsProd = 'https://todo.jensdanbolt.no/api/'

const hsUrl = hsLocal;

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
    const response = await fetch(hsUrl + address);
    if (!response.ok){
        throw new Error('Network response was not ok')
    } 
    else return response.json();
};


export { getJSON, hsUrl };