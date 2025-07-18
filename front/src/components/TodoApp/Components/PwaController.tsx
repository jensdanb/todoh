import { FC } from "react";

interface PWAProps {
    intendedOnline: boolean;
    toggleOnline: any;
    netPending: boolean;
    netErrored: boolean;
}

const PwaController:FC<PWAProps> =  ({intendedOnline, toggleOnline, netPending, netErrored, /*netError*/ }) => {
    const msgOnlineMode = intendedOnline ? "Online mode" : "Offline mode";

    function networkMsg () {
        if (netPending) {return "Backend: Waiting for response."};
        if (netErrored) {return "Backend: No response. Check internet connection and authentication, retry and contact support"};
        return "Backend: Got response.";
    }

    const msgError = networkMsg()
    
    return (
        <>
            <p>{msgOnlineMode + ". " + msgError}</p>
            <button 
                type="button" 
                className="btn toggle-btn" 
                aria-pressed="true"
                onMouseDown={toggleOnline}>
                <span>Toggle online </span>
            </button>
        </>);
};

export default PwaController;

// if (!props.actualOnline) return <div>Tried to connect, but failed</div>;
