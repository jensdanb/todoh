import { FC } from "react";

interface FilterButtonProps {
    key: string; 
    name: string;
    isPressed: Boolean;
    setTaskFilter: (arg0: any) => void;
}

const FilterButton:FC<FilterButtonProps> = (props) => {
    return (
      <button 
        type="button" 
        className="btn toggle-btn" 
        aria-pressed="true"
        onMouseDown={() => props.setTaskFilter(props.name)}>
        <span className="visually-hidden">Show </span>
        <span>{props.name} </span>
        <span className="visually-hidden"> tasks</span>
      </button>
    );
  }
  
  export default FilterButton;
  