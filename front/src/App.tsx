import {QueryClient, QueryClientProvider,} from '@tanstack/react-query'
import { NavBar } from "./components";
import { TodoApp } from "./components";

const queryClient = new QueryClient()

let initial_filter: "All" | "Active" | "Completed";

function App() {
  initial_filter = "All"
  return (
    <QueryClientProvider client={queryClient}>
      <>
        <NavBar />
        <TodoApp
          initialFilter={initial_filter}/>
      </>
    </QueryClientProvider>
  )
}

export default App
