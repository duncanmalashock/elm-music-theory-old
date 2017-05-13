var connect = require('connect');
var serveStatic = require('serve-static');
var socketIO = require('socket.io').listen(1025);
connect().use(serveStatic(__dirname)).listen(8080, function(){
    console.log('Web server running at localhost:8080');
});

var noteOn = 144;
var noteOff = 128

var Midi = require('midi');

var output = new Midi.output();

output.getPortCount();

var playNote = function (port, note, duration) {
  output.openPort(port);
  output.sendMessage([noteOn, note, 127]);
  setTimeout(
    function() {
      output.openPort(port);
      output.sendMessage([noteOff, note, 127]);
      output.closePort(port);
    }, duration
  );
  output.closePort(port);
};

socketIO.sockets.on('connection', function (socket) {
  console.log('Socket connection open');
  socket.on('sendNote', function (data) {
    playNote(data[0], data[1], data[2]);
  });
});
