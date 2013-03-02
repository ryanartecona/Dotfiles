// Ryan Artecona
// config for Slate app (https://github.com/jigish/slate)

slate.configAll({
	'nudgePercentOf': 'screenSize',
	'resizePercentOf': 'screenSize',
	'repeatOnHoldOps': 'move,resize,nudge',
	'secondsBeforeRepeat': 0.3,
	'secondsBetweenRepeat': 0.08
});


var numCols = 12;
var numRows = 8;

var menuBarHeight = 20;


// Reusable ops

var pushLeft = slate.operation('push', {'direction': 'left'});
var pushRight = slate.operation('push', {'direction': 'right'});
var pushUp = slate.operation('push', {'direction': 'up'});
var pushDown = slate.operation('push', {'direction': 'down'});

var nudgeLeft = function(window) {
	var winX = window.rect().x;
	var screenWidth = window.screen().visibleRect().width;
	var colWidth = screenWidth / numCols;

	var nudgeByX = Math.floor(Math.max(0, winX % colWidth)) || Math.ceil(colWidth);

	window.doop(S.op('nudge', {
		'x': '-'+nudgeByX,
		'y': '+0',
	}));
};

var nudgeRight = function(window) {
	var winX = window.rect().x;
	var screenWidth = window.screen().visibleRect().width;
	var colWidth = screenWidth / numCols;

	var nudgeByX = Math.floor(Math.max(0, colWidth-(winX % colWidth))) || Math.ceil(colWidth);

	window.doop(S.op('nudge', {
		'x': '+'+nudgeByX,
		'y': '+0'
	}));
};

var nudgeUp = function(window) {
	var winY = window.rect().y - menuBarHeight;
	var screenHeight = window.screen().visibleRect().height;
	var rowHeight = screenHeight / numRows;

	var nudgeByY = Math.floor(Math.max(0, winY % rowHeight)) || Math.ceil(rowHeight);

	window.doop(S.op('nudge', {
		'x': '+0',
		'y': '-'+nudgeByY
	}));
};

var nudgeDown = function(window) {
	var winY = window.rect().y - menuBarHeight;
	var screenHeight = window.screen().visibleRect().height;
	var rowHeight = screenHeight / numRows;

	var nudgeByY = Math.floor(Math.max(0, rowHeight-(winY % rowHeight))) || Math.ceil(rowHeight);

	window.doop(S.op('nudge', {
		'x': '+0',
		'y': '+'+nudgeByY
	}));
};


var resizeBy = function(amtByX, amtByY) {
	return function(window) {
		var winWidth = window.rect().width;
		var winHeight = window.rect().height;
		var screenWidth = window.screen().visibleRect().width;
		var screenHeight = window.screen().visibleRect().height;
		var colWidth = screenWidth / numCols;
		var rowHeight = screenHeight / numRows;

		var widthOffset = (colWidth * (amtByX>0)) + ((winWidth % colWidth) * -amtByX);
		widthOffset = amtByX && ((widthOffset > 15 ? widthOffset : 0) || Math.ceil(colWidth + widthOffset));

		var heightOffset = (rowHeight * (amtByY>0)) + ((winHeight % rowHeight) * -amtByY);
		heightOffset = amtByY && ((heightOffset > 15 ? heightOffset : 0) || Math.ceil(rowHeight + heightOffset));

		window.resize({
			'width': winWidth + (amtByX * widthOffset),
			'height': winHeight + (amtByY * heightOffset)
		});
	}
};

var extendWidth = resizeBy(1, 0);
var contractWidth = resizeBy(-1, 0);
var extendHeight = resizeBy(0, 1);
var contractHeight = resizeBy(0, -1);


// Key Bindings
slate.bindAll({
	// 'key:mod1,mod2': [operation, isRepeatable]

	// growing/shrinking current window
	'left:ctrl,alt': [contractWidth, true],
	'right:ctrl,alt': [extendWidth, true],
	'up:ctrl,alt': [contractHeight, true],
	'down:ctrl,alt': [extendHeight, true],

	// moving current window
	'left:ctrl': [nudgeLeft, true], 
	'right:ctrl': [nudgeRight, true],
	'up:ctrl': [nudgeUp, true],
	'down:ctrl': [nudgeDown, true],

	// shooting current window to sides/corners
	'left:shift,ctrl': [pushLeft, true],
	'right:shift,ctrl': [pushRight, true],
	'up:shift,ctrl': [pushUp, true],
	'down:shift,ctrl': [pushDown, true]
});