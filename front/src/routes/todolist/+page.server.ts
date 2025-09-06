import type { Cookies } from '@sveltejs/kit';
import { getJSON, hsUrl } from '$lib/services/network.ts';
import { netGetTodos } from './shared.network.svelte';

export function load({ cookies }: {cookies: Cookies}) {
    let id: string | undefined = cookies.get('userid');

    if (!id) {
        id = crypto.randomUUID();
        cookies.set('userid', id, { path: '/'})
    }

    return {
        todos: netGetTodos()
    };
}