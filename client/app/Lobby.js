var Observable = require("FuseJS/Observable");
var hamHands = require("HamHands");

var serverIp = Observable("127.0.0.1");
var port = Observable("1234");

var debugInfo = Observable("");

var connectToServer = function() {
	console.log("connectToServer");

	hamHands.connect(serverIp.value, port.value).then(function(result) {
		console.log("Connected!");
		debugInfo.value = "Connected";
		router.goto("controls");
	}).catch(function(e) {
		console.log("Ah balls connect failed: " + e);
		debugInfo.value = "Connect Failed:\n" + e;
	});
};

module.exports = {
	connectToServer: connectToServer,
	debugInfo: debugInfo,
	serverIp: serverIp,
	port: port
};
