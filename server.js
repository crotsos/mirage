var http = require('http');
var fs = require('fs');
var path = require('path');
var WebSocketServer = require('websocket').server;

http.createServer(function (request, response) {
 
    console.log('request starting...' + request.url);
    if(request.url == "/topo.json") {
      console.log("got a request for tree.json");
    }
    var filePath = '.' + request.url;
    if (filePath == './')
        filePath = './index.html';
         
    var extname = path.extname(filePath);
    var contentType = 'text/html';
    switch (extname) {
        case '.js':
            contentType = 'text/javascript';
            break;
        case '.css':
            contentType = 'text/css';
            break;
    }
     
    path.exists(filePath, function(exists) {
     
        if (exists) {
            fs.readFile(filePath, function(error, content) {
                if (error) {
                    response.writeHead(500);
                    response.end();
                }
                else {
                    response.writeHead(200, { 'Content-Type': contentType });
                    response.end(content, 'utf-8');
                }
            });
        }
        else {
            response.writeHead(404);
            response.end();
        }
    });
     
}).listen(8125);
 
console.log('Server running at http://127.0.0.1:8125/');

var server = http.createServer(function(request, response) {
    // process HTTP request. Since we're writing just WebSockets server
    // we don't have to implement anything.
});
server.listen(1337, function() { });

// create the server
wsServer = new WebSocketServer({
    httpServer: server
});

// WebSocket server
wsServer.on('request', function(request) {
    var connection = request.accept(null, request.origin);

    console.log("hello");
    function fire_event(i) {
      if(i > 10) return;
      console.log("event "+(i++)+" fired!!");
      connection.send(i);
      setTimeout(
        function () {fire_event(i)}, 10000);
    }
    fire_event(0);

    // This is the most important callback for us, we'll handle
    // all messages from users here.
    connection.on('message', function(message) {
        if (message.type === 'utf8') {
            // process WebSocket message
        }
    });

    connection.on('close', function(connection) {
        // close user connection
    });

});


