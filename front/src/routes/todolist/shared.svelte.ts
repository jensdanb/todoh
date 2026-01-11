
// Core


export const in_work: {new_task_name: string} = $state({new_task_name: ""});

// UI
export type TodoFilter = "All" | "Pending" | "Completed";

export const todo_filter: {selected: TodoFilter} = $state({
    selected: "All" as TodoFilter
});