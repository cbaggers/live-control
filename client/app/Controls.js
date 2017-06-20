var HamHands = require("HamHands");

HamHands.on("lostConnection", function() {
	router.goto("lobby");
});
