<script lang="ts">
    import { todo_list, local_put } from "./shared.svelte";
    let props = $props();

    type Mode = "Overview" | "Edit";
    let editMode: Mode = $state("Overview");
    let editName: string = $state(props.name);


    function onCheck() {
        local_put(props.id, {...props, completed: !props.completed});
    };

    function onDelete() {
        todo_list.todos = todo_list.todos.filter(
            (task) => task.id !== props.id
        );
    };

    function handleSubmit() {
        local_put(props.id, {...props, name: editName});
        editMode = "Overview";
    };
</script>

<div class="card-small">
    {#if editMode=="Overview"}
    <form>
        <input
            id={props.id}
            type="checkbox"
            checked={props.completed}
            onchange={onCheck} 
            class="checkbox"
        />
        {props.name}
    </form>
    <div>
        <button
            id={props.id}
            onmousedown={() => editMode = "Edit"}>
            Edit
        </button>
        <button
            id={props.id}
            onmousedown={onDelete}>
            Delete
        </button>
    </div>
    {:else} 
        <form onsubmit={handleSubmit}>
            <input 
                type="text"
                id="new-todo-input"
                name="new description"
                autoComplete="on"
                bind:value={editName}
            />
        </form>
        <div>
            <button onmousedown={() => editMode = "Overview"} >
                Cancel
            </button>
            <button onmousedown={handleSubmit} >
                Save
            </button>
        </div>
    {/if}

</div>

<style>
    .card-small {
        background-color: hsl(62, 83%, 91%);
        box-shadow: var(--shadow-size) var(--shadow-mid-color);
	}
</style>