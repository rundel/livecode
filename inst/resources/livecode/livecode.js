var ws = new WebSocket("ws://"+window.location.host);

draw_pb = function(interval) {
  var pb = document.getElementById('progressbar');
  while (pb.lastChild) {
    pb.removeChild(pb.lastChild);
  }

  new ProgressBar.Circle(progressbar, {
    strokeWidth: 50,
    easing: 'linear',
    duration: interval*1000,
    color: '#6499D3',
    svgStyle: null
  }).animate(1.0);
};

draw_timer = function(duration, color) {

  new ProgressBar.Line(container, {
    strokeWidth: 10,
    easing: 'linear',
    duration: duration * 1000,
    color: color,
    trailColor: '#eee',
    svgStyle: {width: '100%'},
    text: {
      style: {
        color: '#999',
        position: 'absolute',
        left: '50%',
        top: '10%',
        padding: 0,
        margin: 0,
        transform: null
      }
    },
    step: (state, bar) => {
    	var sec_left = duration - duration * bar.value();
      var sec = Math.round(sec_left % 60).toString().padStart(2,"0");
      var min = Math.floor( sec_left / 60 ).toString().padStart(2,"0");

      bar.setText(min + ":" + sec);
    }
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

  if (obj.messages) {
    //document.getElementById("debug").innerHTML += JSON.stringify( obj.messages );
    //document.getElementById("debug").innerHTML += "\n\n";
    for(var m of obj.messages) {
      new Noty(m).show();
    }
  }
  
  if (obj.filename) {
    document.querySelector("#file span").innerHTML = obj.filename;
  }
  
  if (obj.url) {
    document.querySelector("#url span").innerHTML = obj.url;
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

