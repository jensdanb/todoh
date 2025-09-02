<script lang="ts" >
    import { Task, todo_list, in_work } from "./shared.svelte";

    function handleSubmit() {
        if (in_work.new_task_name != ""){
            todo_list.todos.push(new Task(in_work.new_task_name))
            in_work.new_task_name = ""
        };
    };
</script>

<div>
    <form onsubmit={handleSubmit}>
        <label class="card">
            New todo: 
            <input 
                type="text"
                id="new-todo-input"
                name="task description"
                autoComplete="on"
                bind:value={in_work.new_task_name}
                autofocus
            />
            <button onmousedown={handleSubmit} >
                Add
            </button>
        </label>
    </form>
    
</div>

<style>
	.card {
        background-color: antiquewhite;
	}
</style>