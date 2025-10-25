export type SyncStatus = "FromServer" | "FromServerAndEdited" | "FreshFish" | "Posted"

export class Todo {
    id: string; 
    name: string;
    completed: boolean;
    syncStatus : SyncStatus;

    constructor(name: string, other?: {id?: string, completed?: boolean, syncStatus?: SyncStatus} ) {
        this.name = name; 
        this.id = other?.id ?? `todo-${crypto.randomUUID()}`; 
        this.completed = other?.completed ?? false; 
        this.syncStatus = other?.syncStatus ?? "FreshFish";
      };
};