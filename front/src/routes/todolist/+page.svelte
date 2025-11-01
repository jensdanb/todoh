<script lang="ts">
    import { Todo } from '$lib/services/types';
    import {todo_filter} from './shared.svelte';
    import Form from "./TaskForm.svelte"
    import TaskList from "./TaskList.svelte";

    let { data } = $props();
    $inspect(data);

    let filterTaskCompleted = $derived((task: Todo) => {
        if (todo_filter.selected=="All") {return true}
        else if (todo_filter.selected=="Pending") {return !task.completed}
        else if (todo_filter.selected=="Completed") {return task.completed}
    })

</script>

<div class="container">
    <h1>Todo-List</h1>
    <Form />
    <TaskList todos={data.todos} {filterTaskCompleted}/>
</div>

<style>
	.container {
        background-color: rgb(221, 253, 242);
	}
    h1 {
        align-self: center;
        margin-top: 0rem;
        margin-bottom: var(--s5);
    }
</style>