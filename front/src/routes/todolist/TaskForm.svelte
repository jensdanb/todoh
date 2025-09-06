<script lang="ts" >
    import { in_work } from "./shared.svelte";

    let { data } = $props();

    function handleSubmit() {
        if (in_work.new_task_name != ""){
            data.todos.push(new Todo(in_work.new_task_name))
            in_work.new_task_name = ""
        };
    };
</script>

<div>
    <form method="POST">
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