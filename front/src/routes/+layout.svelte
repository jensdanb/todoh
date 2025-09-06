<script lang="ts">
	import favicon from '$lib/assets/favicon.svg';
	import '$lib/styles.css'

	let { children } = $props();
</script>

<svelte:head>
	<link rel="icon" href={favicon} />
</svelte:head>

<nav>
	<a href="/">home</a>
	<a href="/todolist">todo-list</a>
</nav>

{@render children?.()}
