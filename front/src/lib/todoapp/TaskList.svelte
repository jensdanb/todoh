<script lang="ts">
    import { todo_filter, todo_list, Task, in_work } from "./shared.svelte";
    import TaskC from './TaskC.svelte';

    let filterTaskCompleted = $derived((task: Task) => {
        if (todo_filter.selected=="All") {return true}
        else if (todo_filter.selected=="Pending") {return !task.completed}
        else if (todo_filter.selected=="Completed") {return task.completed}
    })

    let taskList = $derived(todo_list.todos
        .filter((todo: Task) => filterTaskCompleted(todo)));

</script>

<div class="card">
    <div class="group-horizontal-btns">
        <button 
            onmousedown={() => todo_filter.selected = "All"}
            aria-current={todo_filter.selected === "All"}
            class="aria-btn">
            All
        </button>
        <button 
            onmousedown={() => todo_filter.selected = "Pending"}
            aria-current={todo_filter.selected === "Pending"}
            class="aria-btn">
            Pending
        </button>
        <button 
            onmousedown={() => todo_filter.selected = "Completed"}
            aria-current={todo_filter.selected === "Completed"}
            class="aria-btn">
            Completed
        </button>
    </div>
    <div>{todo_filter.selected} tasks:</div>
    <ul class="card2">
		{#each taskList as todo (todo.id)}
			<TaskC {...todo} />
		{/each}
	</ul>
</div>

<style>
	.card {
        background-color: hsl(62, 83%, 91%);
	}
    .card2 {
        background-color: hsl(62, 90%, 91%);
        box-shadow: var(--shadow-size) var(--shadow-light-color);
	}
</style>