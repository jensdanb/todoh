export type SyncStatus = "From server" | "From server and edited" | "Fresh fish" | "Posted"

export class Todo {
    id: string; 
    name: string;
    completed: boolean;
    syncStatus : SyncStatus;

    constructor(name: string, other?: {id?: string, completed?: boolean, syncStatus?: SyncStatus} ) {
        this.name = name; 
        this.id = other?.id ?? `todo-${crypto.randomUUID()}`; 
        this.completed = other?.completed ?? false; 
        this.syncStatus = other?.syncStatus ?? "Fresh fish";
      };
};