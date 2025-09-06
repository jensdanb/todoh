import type { Cookies } from '@sveltejs/kit';
import * as db from '$lib/services/database.ts';

export function load({ cookies }: {cookies: Cookies}) {
    let id: string | undefined = cookies.get('userid');

    if (!id) {
        id = crypto.randomUUID();
        cookies.set('userid', id, { path: '/'})
    }

    return {
        id: id,
        todos: db.getTodos(id)
    };
}

export const actions = {
	default: async ({ cookies, request }) => {
		const data = await request.formData();
		db.createTodo(cookies.get('userid'), data.get('description'));
	}
};

