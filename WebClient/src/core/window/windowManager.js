/**
 * WindowManager controls the placement and appearance of windows within a given DOM element.
 */

/* globals L */ 

/* exported WindowManager */
var WindowManager = L.Evented.extend({

    initialize: function (opts) {
        if (!opts) throw new Error('options object was not provided. The object is mandatory.');
        if (!opts.element) throw new Error('options.element is not provided.');

        this._windowContainer = opts.element;

        this._windowStack = [];
        this._initialZIndex = this._windowContainer.style.zIndex ? 
            parseInt(this._windowContainer.style.zIndex, 10) : 999;

        this._nextPosition = {
            x: 0,
            y: 0
        };
    },

    addWindow: function (window) {
        window.on('focus', this._onWindowFocus, this);
        this._windowContainer.appendChild(window.element());
        window.move(this._nextPosition.x, this._nextPosition.y);
        this._addWindowToStack(window);
        window.onAdd(this);

        this._incrementNextPostion();
    },

    removeWindow: function (window) {
        window.off('focus', this._onWindowFocus, this);
        this._removeWindowFromStack(window);
        window.remove();
    },

    _incrementNextPostion: function () {
        var offsetY = 200;
        var offsetX = 300;

        this._nextPosition.y += offsetY;

        if (this._nextPosition.y + 50 > this._windowContainer.getBoundingClientRect().height) {
            this._nextPosition.y = 0;
            this._nextPosition.x += offsetX;
        }

        if (this._nextPosition.x + 50 > this._windowContainer.getBoundingClientRect().width) {
            this._nextPosition.y = 0;
            this._nextPosition.x = 0;
        }
    },

    _addWindowToStack: function (window) {
        if (this._windowStack.indexOf(window) >= 0) return;

        window.element().style.zIndex = '' + (this._initialZIndex + this._windowStack.length);
        this._windowStack.push(window);        
    },

    _removeWindowFromStack: function (window) {
        var index = this._windowStack.indexOf(window);
        if (index < 0) return;

        this._windowStack.splice(index, 1);
        this._buildZIndexes();
    },

    _buildZIndexes: function () {
        var initialZIndex = this._initialZIndex;
        this._windowStack.forEach(function(window, index) {
            window.element().style.zIndex = '' + (initialZIndex + index + 1);
        });
    },

    _onWindowFocus: function (data) {
        var window = data.windowView;
        var index = this._windowStack.indexOf(window);
        if (index < 0) return;

        this._windowStack.splice(index, 1);
        this._windowStack.push(window);
        
        this._buildZIndexes();
    }

});
