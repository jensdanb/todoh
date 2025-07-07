import { UseMutateFunction } from "@tanstack/react-query";
import {FC, useState} from "react"; 

// Child component of TodoApp

interface FormProps {
    onSubmit: UseMutateFunction<void, Error, string, unknown>
}

const Form:FC<FormProps> = (props) => {
    
    const [name, setName] = useState("");

    function handleTyping(event: { target: { value: string; }; }) {
        setName(event.target.value);
    }

    function handleSubmit(event: { preventDefault: () => void; }) {
        event.preventDefault();
        if (name != "") {
            props.onSubmit(name);
            setName("");
        };
    }

    return (
        <form onSubmit={handleSubmit}>
            <h2 className="label-wrapper">
                <label htmlFor="new-todo-input" className="label__lg">
                    What more to do?
                </label>
            </h2>

            <input
                type="text"
                id="new-todo-input"
                className="input input__lg"
                name="text"
                autoComplete="on"
                value={name}
                onChange={handleTyping}
            />

            <button type="submit" className="btn btn__strong btn__lg">
              Add
            </button>
        </form>
    );
  }
  
  export default Form;
  
