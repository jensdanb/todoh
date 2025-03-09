import { Todo, BackEndPoint } from './types';
import { netGetTodos } from "./networking";
import { cacheTodoList } from "./databasing";

const mutationFunction = async (todo: Todo, netFunction: { (newTodo: Todo): Promise<void>; (arg0: any): Promise<any>; }, dbFunction: { (todo: Todo): Promise<void>; (arg0: any): any; }) => {
    return await netFunction(todo)
        .then(async () => {await dbFunction({...todo, knownUnSynced: false})})
        .catch(async (error: any) => {await dbFunction(todo)})
};

const cacheServerTodos = async () => {
    const serverTodos = await netGetTodos()
        .then((responseJson: any) => {
            cacheTodoList(responseJson);
        })
}

export {mutationFunction, cacheServerTodos};