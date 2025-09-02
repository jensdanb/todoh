<script lang="ts">
    import Form from "./TaskForm.svelte"
    import TaskList from "./TaskList.svelte";
</script>

<div class="container">
    <h1>Todo-List</h1>
    <Form />
    <TaskList />
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