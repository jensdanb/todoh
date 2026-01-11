<script lang="ts" >
    import { enhance } from '$app/forms';
    import { in_work } from "./shared.svelte";


    /*function handleSubmit() {
        if (in_work.new_task_name != ""){
            data.todos.push(new Todo(in_work.new_task_name))
            in_work.new_task_name = ""
        };
    };*/
</script>

<div>
    <form method="POST" action="?/create" use:enhance>
        <label class="card">
            New todo: 
            <input 
                type="text"
                id="new-todo-input"
                name="description"
                autoComplete="on"
                bind:value={in_work.new_task_name}
                autofocus
                required
            />
            <button>
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