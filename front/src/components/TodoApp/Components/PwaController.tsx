
function PwaController ({intendedOnline, toggleOnline, netPending, netErrored, netError}) {
    const msgOnlineMode = intendedOnline ? "Online mode" : "Offline mode";

    function networkMsg () {
        if (netPending) {return "Waiting for network response"};
        if (netErrored) {return "Tried to connect, but failed"};
        return "Success";
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
