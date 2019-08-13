var ws = new WebSocket(host);

ws.onmessage = function(msg) {
  document.getElementsByTagName("code")[0].innerHTML = msg.data;
};

