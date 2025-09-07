<script lang="ts">
    import { todo_filter } from "./shared.svelte.ts";
    import TaskC from './TaskC.svelte';

    let { todos, filterTaskCompleted } = $props();
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
		{#each todos as todo (todo.id)}
            {#if filterTaskCompleted(todo)}
			    <TaskC {todo} />
            {/if}
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