import { useState, JSX, FC } from "react"; 
import { useMutation, useQuery, useQueryClient } from '@tanstack/react-query';

import { Todo } from "../../services/types";
import { netGetTodos, netPostTodo, netDelTodo, } from "../../services/network";
import { dbAddTodo, dbDelTodo } from "../../services/databasing";
import { mutationFunction } from "../../services/common";
import { TodoC, Form, FilterButton, PwaController } from "./Components";


const FILTER_NAMES = ["All", "Active", "Completed"];

interface TodoAppProps { initialFilter: string}
const TodoApp:FC<TodoAppProps> = ({initialFilter}) => {

    const [onlineMode, setOnlineMode] = useState(true);

    const queryClient = useQueryClient()

    const {isPending: getTodosPending, isError: getTodosErrored, data: todos} = useQuery({ 
        queryKey: ['todos'], 
        queryFn: netGetTodos, 
        networkMode: "always"
    });

    const invalidateTodos = () => {queryClient.invalidateQueries({ queryKey: ['todos'] })}
    

    const addTodoMutation = useMutation({
        mutationFn: async (name: string) => {
            const newTask = new Todo(name); 
            return await mutationFunction(newTask, netPostTodo, dbAddTodo);
        },
        onSettled: invalidateTodos,
    });

    const delTodoMutation = useMutation({
        mutationFn: async (id: string) => {
            return await netDelTodo(id)
                .then(() => {dbDelTodo(id)})
        },
        onSettled: invalidateTodos,
      })
    
    const pwaController = 
        <PwaController 
            intendedOnline={onlineMode} 
            toggleOnline={() => {
                if (!onlineMode) {invalidateTodos()}
                setOnlineMode(onlineMode ? false : true)
            }}
            netPending={getTodosPending}
            netErrored={getTodosErrored}
            // netError={getTodosError}
        />    
      
        
    const [taskFilter, setTaskFilter] = useState(initialFilter);
    
    function todoComponent(todoData: Todo): JSX.Element { 
        return <TodoC
                    id={todoData.id}
                    key={todoData.id}
                    name={todoData.name}
                    completed={todoData.completed}
                    knownUnSynced={todoData.knownUnSynced}
                    todoData={todoData}
                    invalidateTodoList={invalidateTodos}
                    deleteTask={delTodoMutation.mutate}
                />
    }

    const filterTaskCompleted = (task: Todo, chosenFilter: string) => {
        if (chosenFilter=="All") {return true}
        else if (chosenFilter=="Active") {return !task.completed}
        else if (chosenFilter=="Completed") {return task.completed}
    }

    const taskList = todos
        ?.filter((todo: Todo) => filterTaskCompleted(todo, taskFilter))
        ?.map(todoComponent);
    
    const filterButtons = FILTER_NAMES.map((name) => 
        <FilterButton 
            key={name} 
            name={name}
            isPressed={name === taskFilter}
            setTaskFilter={setTaskFilter}
            />
    );

    const headingText = `${todos?.length} tasks, ${todos?.filter((todo: Todo) => filterTaskCompleted(todo, "Active")).length} remaining`;
    
    return (
        <>
        <div className="todoapp stack-large content">
            <h1>TodoMatic v2</h1>
            {pwaController}

            <Form onSubmit={addTodoMutation.mutate}/>

            <div className="filters btn-group stack-exception">
                {filterButtons}
            </div>
            
            <h2 id="list-heading">{headingText}</h2>
            <ul
                role="list"
                className="todo-list stack-large stack-exception"
                aria-labelledby="list-heading">
                {taskList}
            </ul>
        </div>
        </>
    );
  }
  
  export default TodoApp;
  