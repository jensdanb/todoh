import { Todo, BackEndPoint } from './types';
/*
import { netGetTodos } from "./networking";
import { cacheTodoList } from "./databasing";
*/

const mutationFunction = async (todo: Todo, 
    netFunction: { (newTodo: Todo): Promise<void>; (arg0: Todo): Promise<any>; }, 
    dbFunction: { (todo: Todo): Promise<void>; (arg0: Todo): any; }
) => {
    return await netFunction(todo)
        .then(async () => {await dbFunction({...todo, knownUnSynced: false})})
        .catch(async (error: Error) => {await dbFunction(todo)})
};
/*
const cacheServerTodos = async () => {
    await netGetTodos()
        .then((responseJson: JSON) => {
            cacheTodoList(responseJson);
        })
}
*/

export {mutationFunction};