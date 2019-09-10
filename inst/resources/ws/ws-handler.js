var ws = new WebSocket("ws://"+window.location.host);

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
  var need_update = false;

  draw_pb(obj.interval);

  if (obj.selection) {
    if (typeof Prism != "undefined") {
      var cur = document.getElementsByTagName("pre")[0].getAttribute("data-line");
      if (cur != obj.selection) {
        document.getElementsByTagName("pre")[0].setAttribute("data-line", obj.selection);
        need_update = true;
      }
    }
  }

  if (obj.content) {
    var code = obj.content.replace(/</g,"&lt;");
    document.getElementsByTagName("code")[0].innerHTML = code;

    need_update = true;
  }

  if (need_update) {
    document.querySelectorAll('pre code').forEach((block) => {
      if (typeof hljs  != "undefined") {
        hljs.highlightBlock(block);
        hljs.lineNumbersBlock(block);
      }
      if (typeof Prism != "undefined") {
        Prism.highlightElement(block);
      }
    });
  }
};

