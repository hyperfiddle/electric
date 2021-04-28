function ws_uri(path : string ){
    let loc = window.location;
    let uri : string;
    if (loc.protocol === "https:") {
        uri = "wss:";
    } else {
        uri = "ws:";
    }
    uri += "//" + loc.host;
    uri += loc.pathname + path;
    return uri;
}

let ws = new WebSocket(ws_uri("ws"));
