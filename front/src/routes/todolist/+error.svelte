<script>
  export let error;
</script>

<h1>Error</h1>
<p>{error.message}</p>
<p>It looks like the item was modified or deleted by another user. Please refresh the page.</p>
<button on:click={() => window.location.reload()}>Refresh</button>
