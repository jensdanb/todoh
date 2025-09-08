import type { Cookies, Actions } from '@sveltejs/kit';
import * as db from '$lib/services/database.ts';

export function load({ cookies }: {cookies: Cookies}) {
    let id: string | undefined = cookies.get('userid');

    if (!id) {
        id = crypto.randomUUID();
        cookies.set('userid', id, { path: '/'})
    }

    return {
        todos: db.getTodos(id)
    };
}

export const actions: Actions = {
	create: async ({ cookies, request }) => {
		const formData = await request.formData();
		db.createTodo(cookies.get('userid') as string, formData.get('description') as string);
	}, 
    rename: async ({cookies, request}) => {
        const formData = await request.formData();
        db.putTodo(cookies.get('userid') as string, formData.get('todo-id') as string, formData.get('new description') as string);
    }
};

