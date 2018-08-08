/**
 * WindowView base window class.
 */

/* globals L */ 

/* exported WindowView */
var WindowView = L.Evented.extend({

    initialize: function (opts) {
        var options = opts || {};

        this._minWidth = options.minWidth || 350;
        this._minHeight = options.minHeight || 200;
        this._maxWidth = options.maxWidth || null;
        this._maxHeight = options.maxHeight || null;

        this._width = options.width || this._minWidth;
        this._height = options.height || this._minHeight;
        this._windowClass = options.class || '';

        if (options.resizable === undefined) {
            options.resizable = true;
        }
        this._resizable = !!options.resizable;

        if (options.movable === undefined) {
            options.movable = true;
        }
        this._movable = !!options.movable;

        this._bindEventHandlers();

        this.render();
    },

    render: function () {
        if (this._rootElement) return; // already rendered

        this._rootElement = L.DomUtil.create('div', 'window-view ' + this._windowClass);
        this._rootElement.style.width = '' + this._width + 'px';
        this._rootElement.style.height = '' + this._height + 'px';
        
        var titleBar = L.DomUtil.create('div', 'window-titlebar', this._rootElement);
        this._title = L.DomUtil.create('span', 'window-title', titleBar);        
        var closeButton = L.DomUtil.create('a', 'window-close', titleBar);
        closeButton.href = '#';
        closeButton.addEventListener('click', this._onCloseBtnClicked);
        closeButton.addEventListener('touchend', this._onCloseBtnClicked);

        this._viewportElement = L.DomUtil.create('div', 'window-viewport', this._rootElement);

        if (this._resizable) {
            this._makeResizable();
        }

        if (this._movable) {
            this._makeMovable();
        }

        this.onRender(this._viewportElement);
    },

    remove: function () {
        if (!this._rootElement) return; // already removed

        this.onRemove();

        var parent = this._rootElement.parentNode;
        if (parent) {
            parent.removeChild(this._rootElement);
        }

        this._rootElement = null;
        this._windowManager = null;

        this._notifyRemove();
    },

    close: function () {
        if (this._windowManager) {
            this._windowManager.removeWindow(this);
        } else {
            this.remove();
        }
    },

    isOpen: function () {
        return !!this._rootElement;
    },

    onAdd: function (windowManager) {
        this._windowManager = windowManager;
    },

    element: function () {
        return this._rootElement;
    },

    viewportElement: function () {
        return this._viewportElement;
    },

    setTitle: function (titleText) {
        this._title.textContent = titleText;
    },

    move: function (left, top) {
        this._rootElement.style.transform = 'translate(' + left + 'px, ' + top +  'px)';
    },

    resize: function (width, height) {
        if (this._width === width && this._height === height) return;

        var newWidth = width;
        var newHeight = height;

        if (this._maxWidth) {
            newWidth = Math.min(newWidth, this._maxWidth);
        }
        newWidth = Math.max(newWidth, this._minWidth);

        if (this._maxHeight) {
            newHeight = Math.min(newHeight, this._maxHeight);
        }
        newHeight = Math.max(newHeight, this._minHeight);
        
        this._width = newWidth;
        this._height = newHeight;

        this._rootElement.style.width = '' + this._width + 'px';
        this._rootElement.style.height = '' + this._height + 'px';
        
        this.onResize();

        return true;
    },

    show: function () {
        L.DomUtil.removeClass(this._rootElement, 'hidden');
    },

    hide: function () {
        L.DomUtil.addClass(this._rootElement, 'hidden');        
    },

    onRender: function (/* jshint unused:false */ viewport) {
        // override in child classes
    },

    onRemove: function () {
        // override in child classes
    },

    onResize: function () {
        // override in child classes
    },

    _notifyFocus: function () {
        this.fire('focus', { windowView: this });
    },

    _notifyRemove: function () {
        this.fire('remove', { windowView: this });
    },

    _makeResizable: function () {
        var resizeArea = L.DomUtil.create('div', 'window-resize', this._rootElement);
        resizeArea.addEventListener('mousedown', this._onResizeMousedown);
        resizeArea.addEventListener('touchstart', this._onResizeTouchstart);
    },

    _makeMovable: function () {
        this._rootElement.addEventListener('mousedown', this._onMoveMousedown);
        this._rootElement.addEventListener('touchstart', this._onMoveTouchstart);
    },

    _bindEventHandlers: function () {
        // close button event handlers
        this._onCloseBtnClicked = this._onCloseBtnClicked.bind(this);

        // resize events handlers
        this._onResizeMousedown = this._onResizeMousedown.bind(this);
        this._onResizeTouchstart = this._onResizeTouchstart.bind(this);
        this._onResizeMousemove = this._onResizeMousemove.bind(this);
        this._onResizeTouchmove = this._onResizeTouchmove.bind(this);
        this._onResizeMouseup = this._onResizeMouseup.bind(this);
        this._onResizeTouchend = this._onResizeTouchend.bind(this);

        // move events handlers
        this._onMoveMousedown = this._onMoveMousedown.bind(this);
        this._onMoveTouchstart = this._onMoveTouchstart.bind(this);
        this._onMoveMousemove = this._onMoveMousemove.bind(this);
        this._onMoveTouchmove = this._onMoveTouchmove.bind(this);
        this._onMoveMouseup = this._onMoveMouseup.bind(this);
        this._onMoveTouchend = this._onMoveTouchend.bind(this);
    },

    _onCloseBtnClicked: function (e) {
        e.preventDefault();

        this.close();
    },

    _onResizeMousedown: function (e) {
        e.preventDefault();
        e.stopPropagation();

        document.addEventListener('mousemove', this._onResizeMousemove);
        document.addEventListener('mouseup', this._onResizeMouseup);
        
        this._onStartResize({ 
            x: e.clientX, 
            y: e.clientY 
        });
    },

    _onResizeTouchstart: function (e) {
        e.preventDefault();
        e.stopPropagation();

        document.addEventListener('touchmove', this._onResizeTouchmove);
        document.addEventListener('touchend', this._onResizeTouchend);

        this._onStartResize({ 
            x: e.changedTouches[0].clientX, 
            y: e.changedTouches[0].clientY 
        });
    },

    _onResizeMousemove: function (e) {
        e.preventDefault();
        e.stopPropagation();

        this._onResizing({ 
            x: e.clientX, 
            y: e.clientY 
        });
    },

    _onResizeTouchmove: function (e) {
        this._onResizing({ 
            x: e.changedTouches[0].clientX, 
            y: e.changedTouches[0].clientY 
        });
    },

    _onResizeMouseup: function (e) {
        e.preventDefault();
        e.stopPropagation();

        document.removeEventListener('mousemove', this._onResizeMousemove);
        document.removeEventListener('mouseup', this._onResizeMouseup);

        this._onEndResize();
    },

    _onResizeTouchend: function () {
        document.removeEventListener('touchmove', this._onResizeTouchmove);
        document.removeEventListener('touchend', this._onResizeTouchend);

        this._onEndResize();
    },

    _onStartResize: function (point) {
        this._resizeState = {
            startPoint: point,
            startSize: {
                width: this._width,
                height: this._height
            }
        };

        this._notifyFocus();
    },

    _onResizing: function (point) {
        if (!this._resizeState) return;
        
        var deltaX = point.x - this._resizeState.startPoint.x;
        var deltaY = point.y - this._resizeState.startPoint.y;

        this.resize(
            this._resizeState.startSize.width + deltaX,
            this._resizeState.startSize.height + deltaY
        );
    },

    _onEndResize: function () {
        this._resizeState = null;
    },

    _onMoveMousedown: function (e) {
        e.preventDefault();
        e.stopPropagation();

        document.addEventListener('mousemove', this._onMoveMousemove);
        document.addEventListener('mouseup', this._onMoveMouseup);
        
        this._onStartMove({ 
            x: e.clientX, 
            y: e.clientY 
        });
    },

    _onMoveTouchstart: function (e) {
        e.preventDefault();
        e.stopPropagation();

        document.addEventListener('touchmove', this._onMoveTouchmove);
        document.addEventListener('touchend', this._onMoveTouchend);

        this._onStartMove({ 
            x: e.changedTouches[0].clientX, 
            y: e.changedTouches[0].clientY 
        });
    },

    _onMoveMousemove: function (e) {
        e.preventDefault();
        e.stopPropagation();

        this._onMoving({ 
            x: e.clientX,
            y: e.clientY 
        });
    },

    _onMoveTouchmove: function (e) {
        this._onMoving({ 
            x: e.changedTouches[0].clientX, 
            y: e.changedTouches[0].clientY 
        });
    },

    _onMoveMouseup: function (e) {
        e.preventDefault();
        e.stopPropagation();

        document.removeEventListener('mousemove', this._onMoveMousemove);
        document.removeEventListener('mouseup', this._onMoveMouseup);

        this._onEndMove();
    },

    _onMoveTouchend: function () {
        document.removeEventListener('touchmove', this._onMoveTouchmove);
        document.removeEventListener('touchend', this._onMoveTouchend);

        this._onEndMove();
    },

    _onStartMove: function (point) {
        var windowRect = this._rootElement.getBoundingClientRect();
        var parentRect = this._rootElement.parentNode.getBoundingClientRect();
        var left = windowRect.left - parentRect.left;
        var top = windowRect.top - parentRect.top;

        this._moveState = {
            startPoint: point,
            startPosition: {
                left: left,
                top: top
            }
        };

        this._notifyFocus();
    },

    _onMoving: function (point) {
        if (!this._moveState) return;
        
        var deltaX = point.x - this._moveState.startPoint.x;
        var deltaY = point.y - this._moveState.startPoint.y;

        this.move(
            this._moveState.startPosition.left + deltaX,
            this._moveState.startPosition.top + deltaY
        );
    },

    _onEndMove: function () {
        this._moveState = null;
    }

});
