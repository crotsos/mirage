//http://mbostock.github.com/d3/ex/miserables.json
//
var width = 768,
    height = 512;

var tree = d3.layout.tree()
    .size([height, width - 160]);

var diagonal = d3.svg.diagonal()
    .projection(function(d) { return [d.y, d.x]; });

var vis = d3.select("#chart").append("svg")
    .attr("width", width)
    .attr("height", height)
  .append("g")
    .attr("transform", "translate(40, 0)");

d3.json("topo.json", function(json) {
  var nodes = tree.nodes(json);

  var link = vis.selectAll("path.link")
      .data(tree.links(nodes))
    .enter().append("path")
      .attr("class", "link")
      .attr("d", diagonal);

  var node = vis.selectAll("g.node")
      .data(nodes)
    .enter().append("g")
      .attr("class", "node")
      .attr("transform", function(d) { return "translate(" + d.y + "," + d.x + ")"; })

  node.append("circle")
      .attr("r", 4.5);
 
  node.on("click", function(d) {output_flow_table(d)});
  
  node.append("text")
      .attr("dx", function(d) { return d.children ? -8 : 8; })
      .attr("dy", 3)
      .attr("text-anchor", function(d) { return d.children ? "end" : "start"; })
      .text(function(d) { return d.name; });
} );

function output_flow_table(d) {
  d3.select("#flow_table")
    .html("<tr/>")
    .html("<tr><td>"+d.size+"</td></tr>");
}

// if user is running mozilla then use it's built-in WebSocket
window.WebSocket = window.WebSocket || window.MozWebSocket;

var connection = new WebSocket('ws://127.0.0.1:1337');
connection.onopen = function () {
  alert("connected");
};

connection.onmessage = 
function(m) {
  alert("receive " + m.data);
};



