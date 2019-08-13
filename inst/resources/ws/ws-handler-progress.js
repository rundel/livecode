var ws = new WebSocket(host);

draw_pb = function(interval) {
  var pb = document.getElementById('progressbar');
  while (pb.lastChild) {
    pb.removeChild(pb.lastChild);
  }

  new ProgressBar.Circle(progressbar, {
    strokeWidth: 50,
    easing: 'easeInOut',
    duration: interval*1000,
    color: '#6499D3',
    svgStyle: null
  }).animate(1.0);
};

ws.onmessage = function(msg) {
  document.getElementsByTagName("code")[0].innerHTML = msg.data;
  draw_pb(2);
};

