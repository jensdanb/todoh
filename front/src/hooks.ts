import { SyncStatus, Todo } from "./lib/services/types";
import type { Transport } from '@sveltejs/kit';

export const transport: Transport = {
    Todo: {
        encode: (value) => value instanceof Todo && [value.id, value.name, value.completed, value.syncStatus],
        decode: ([id, name, completed, syncStatus]: [string, string, boolean, SyncStatus]) => new Todo(name, {id: id, completed: completed, syncStatus: syncStatus})
    }
};