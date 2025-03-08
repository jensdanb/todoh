import { StrictMode } from 'react'
import { createRoot } from 'react-dom/client'

import App from './App';
import './index.css';
import './color.css';
import { createTodoDB } from "./services/databasing";
// import { cacheServerTodos } from './services/common';

createTodoDB();
// cacheServerTodos();

createRoot(document.getElementById('root')!).render(
  <StrictMode>
    <App/>
  </StrictMode>,
)
