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
  var obj = JSON.parse(msg.data);

  draw_pb(obj.interval);

  if (obj.content) {
    document.getElementsByTagName("code")[0].innerHTML = obj.content;
  }
};

