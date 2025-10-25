<script lang="ts">
    import { Todo } from '$lib/services/types.ts';
    import { enhance } from '$app/forms';
    import { localPut, deleteTodo } from '$lib/services/database';
    let { todo } = $props();
    let todoS = $derived(JSON.stringify(todo));

    type Mode = "Overview" | "Edit";
    let editMode: Mode = $state("Overview");
    let editName: string = $state(todo.name);


    function onCheck() {
        localPut(todo.id, {...todo, completed: !todo.completed});
    };

    function onDelete() {
        deleteTodo()
    };

    /*function handleSubmit() {
        local_put(todo.id, {...todo, name: editName});
        editMode = "Overview";
    };*/
</script>

<div class="card-small">
    {#if editMode=="Overview"}
        <form>
            <input
                id={todo.id}
                type="checkbox"
                checked={todo.completed}
                onchange={onCheck} 
                class="checkbox"
            />
            {todo.name}
        </form>
        <div>
            <button
                id={todo.id}
                onmousedown={() => editMode = "Edit"}>
                Edit
            </button>
            <button
                id={todo.id}
                onmousedown={onDelete}>
                Delete
            </button>
        </div>
    {:else} 
        <form id="renameForm" method="POST" action="?/rename" 
        use:enhance={() => {editMode = "Overview"}}>
            <input 
                type="text"
                id="new-todo-input"
                name="new name"
                autoComplete="on"
                bind:value={editName}
            />
            <input type="hidden" name="rename-id" value={todoS}/>
            <div>
                <button type="button" 
                    onmousedown={() => editMode = "Overview"} >
                    Cancel
                </button>
                <button type="submit">
                    Save
                </button>
            </div>
        </form>

    {/if}

</div>

<style>
    .card-small {
        background-color: hsl(62, 83%, 91%);
        box-shadow: var(--shadow-size) var(--shadow-mid-color);
	}
</style>