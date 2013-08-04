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

// factory for directional nudge functions
var nudgeBy = function(factorX, factorY) {
    return function (window) {
        var winX = window.rect().x;
        var winY = window.rect().y - menuBarHeight;
        var screenWidth = window.screen().visibleRect().width;
        var screenHeight = window.screen().visibleRect().height;
        var colWidth = screenWidth / numCols;
        var rowHeight = screenHeight / numRows;
        
        var offsetX = (colWidth * (factorX>0)) + ((winX % colWidth) * -factorX)
        offsetX = factorX && ((offsetX > 15 ? offsetX : 0) || Math.ceil(colWidth + offsetX));
        
        var offsetY = (rowHeight * (factorY>0)) + ((winY % rowHeight) * -factorY)
        offsetY = factorY && ((offsetY > 15 ? offsetY : 0) || Math.ceil(rowHeight + offsetY));
        
        var signX = factorX < 0 ? '-' : '+';
        var signY = factorY < 0 ? '-' : '+';

        window.doop(S.op('nudge', {
        	'x': signX + offsetX,
        	'y': signY + offsetY
        }));
    };
};

var nudgeLeft  = nudgeBy(-1, 0);
var nudgeRight = nudgeBy(1, 0);
var nudgeUp    = nudgeBy(0, -1);
var nudgeDown  = nudgeBy(0, 1);

// factory for directional resize functions
var resizeBy = function(amtByX, amtByY, nudgeBack) {
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

		var doNudge = function() {
			window.doop(S.op('nudge', {
				'x': (amtByX > 0 ? '-' : '+') + (widthOffset * Math.abs(amtByX)),
				'y': (amtByY > 0 ? '-' : '+') + (heightOffset * Math.abs(amtByY))
			}));
		};

		if (nudgeBack) {doNudge()}

		window.resize({
			'width': winWidth + (amtByX * widthOffset),
			'height': winHeight + (amtByY * heightOffset)
		});

	};
};

var extendWidthRight = resizeBy(1, 0);
var contractWidthRight = resizeBy(-1, 0);
var extendHeightDown = resizeBy(0, 1);
var contractHeightDown = resizeBy(0, -1);

var extendWidthLeft = resizeBy(1, 0, true);
var contractWidthLeft = resizeBy(-1, 0, true);
var extendHeightUp = resizeBy(0, 1, true);
var contractHeightUp = resizeBy(0, -1, true);


// Key Bindings
slate.bindAll({
	// 'key:mod1,mod2': [operation, isRepeatable]

	// growing/shrinking current window
	'left:ctrl,alt': [contractWidthRight, true],
	'right:ctrl,alt': [extendWidthRight, true],
	'up:ctrl,alt': [contractHeightDown, true],
	'down:ctrl,alt': [extendHeightDown, true],

	// growing/shrinking current window, in up/left direction
	'left:ctrl,alt,cmd': [extendWidthLeft, true],
	'right:ctrl,alt,cmd': [contractWidthLeft, true],
	'up:ctrl,alt,cmd': [extendHeightUp, true],
	'down:ctrl,alt,cmd': [contractHeightUp, true],

	// growing current window
	// 'left:ctrl,alt': [extendWidthLeft, true],
	// 'right:ctrl,alt': [extendWidthRight, true],
	// 'up:ctrl,alt': [extendHeightUp, true],
	// 'down:ctrl,alt': [extendHeightDown, true],

	// shrinking current window
	// 'left:ctrl,alt,cmd': [contractWidthRight, true],
	// 'right:ctrl,alt,cmd': [contractWidthLeft, true],
	// 'up:ctrl,alt,cmd': [contractHeightDown, true],
	// 'down:ctrl,alt,cmd': [contractHeightUp, true],

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