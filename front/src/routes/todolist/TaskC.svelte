<script lang="ts">
    import { Todo } from '$lib/services/types.ts';
    import { enhance } from '$app/forms';
    import { localPut, deleteTodo } from '$lib/services/database';
    let { todo } = $props();
    let todoS: string = $derived(JSON.stringify(todo));
    let isCompleted: boolean = $derived(todo.completed)

    let myForm: HTMLFormElement;
    type Mode = "Overview" | "Edit";
    let editMode: Mode = $state("Overview");
    let editName: string = $state(todo.name);

    /*function handleSubmit() {
        local_put(todo.id, {...todo, name: editName});
        editMode = "Overview";
    };*/
</script>

<div class="card-small">
    {#if editMode=="Overview"}
        <form bind:this={myForm} method="POST" action="?/toggle" use:enhance>
            <input type="hidden" name="rename-id" value={todoS}/>
            <input
                id={todo.id}
                type="checkbox"
                bind:checked={isCompleted}
                onchange={() => {myForm.requestSubmit()}}
                class="checkbox"
            />
            {todo.name}
        </form>

        <form method="POST" action="?/delete" use:enhance>
            <input type="hidden" name="rename-id" value={todo.id}/>
            <button type="button"
                onmousedown={() => editMode = "Edit"}>
                Edit
            </button>
            <button type="submit">
                Delete
            </button>
        </form>
    {:else} 
        <form method="POST" action="?/rename" 
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