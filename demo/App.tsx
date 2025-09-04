import ReactDOM from "react-dom/client";
import { REPL } from "./components/REPL";
import "./App.css";

function App() {
    return (
        <div className="App">
            <REPL />
        </div>
    );
}

const root = ReactDOM.createRoot(document.getElementById("root")!);
root.render(<App />);
