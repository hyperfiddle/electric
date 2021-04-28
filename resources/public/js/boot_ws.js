function ws_uri(path) {
    var loc = window.location;
    var uri;
    if (loc.protocol === "https:") {
        uri = "wss:";
    }
    else {
        uri = "ws:";
    }
    uri += "//" + loc.host;
    uri += loc.pathname + path;
    return uri;
}
var ws = new WebSocket(ws_uri("ws"));
