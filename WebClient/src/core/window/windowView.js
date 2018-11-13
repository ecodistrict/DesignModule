/**
 * WindowView base window class.
 */

/* globals L */

import './window.css';

import View from '../view';
import DomUtil from '../../utils/DomUtil';
import DockButtonViewModel from '../widgets/dockButton/dockButtonViewModel';
import DockButtonView from '../widgets/dockButton/dockButtonView';
import CloseButtonView from '../widgets/closeButton/closeButtonView';

var WindowView = View.extend({

    onInitialize: function (opts) {
        var options = opts || {};

        this._minWidth = options.minWidth || 350;
        this._minHeight = options.minHeight || 200;
        this._maxWidth = options.maxWidth || null;
        this._maxHeight = options.maxHeight || null;

        this._persistedLeft = null;
        this._persistedTop = null;
        this._currentLeft = 0;
        this._currentTop = 0;
        this._width = options.width || this._minWidth;
        this._height = options.height || this._minHeight;
        this._windowClass = options.class || '';
        this._initialWindowTitle = options.title || '';

        if (options.resizable === undefined) {
            options.resizable = true;
        }
        this._resizable = !!options.resizable;

        if (options.movable === undefined) {
            options.movable = true;
        }
        this._movable = !!options.movable;

        if (options.dockable === undefined) {
            options.dockable = true;
        }
        this._dockable = !!options.dockable;

        this._dockState = new DockButtonViewModel({
            docked: false
        });

        var windowId = 0;
        Object.defineProperty(this, 'windowId', {
            get: function () { return windowId; },
            set: function (neWwindowId) { windowId = neWwindowId; }
        });

        this._bindEventHandlers();
    },

    onRender: function () {
        this._rootElement = L.DomUtil.create('div', 'window-view ' + this._windowClass);
        this._rootElement.style.width = '' + this._width + 'px';
        this._rootElement.style.height = '' + this._height + 'px';
        this._rootElement.addEventListener('transitionend', this._onTransitionEnd.bind(this));
        
        var titleBar = L.DomUtil.create('div', 'window-titlebar', this._rootElement);
        this._title = L.DomUtil.create('span', 'window-title', titleBar);
        this.setTitle(this._initialWindowTitle);

        var buttonsContainer = L.DomUtil.create('div', 'window-buttons-container', titleBar);

        this._dockButton = new DockButtonView({ dockButtonViewModel: this._dockState });
        this._dockButton.on('dockClicked', this._onDockClicked, this);
        this._dockButton.on('undockClicked', this._onUndockClicked, this);
        this._dockButton.on('focus', this._notifyFocus, this);
        buttonsContainer.appendChild(this._dockButton.element());
        if (!this.isDockable()) {
            this._dockButton.hide();
        }

        this._closeButton = new CloseButtonView();
        this._closeButton.on('closeClicked', this._onCloseBtnClicked, this);
        this._closeButton.on('focus', this._notifyFocus, this);
        buttonsContainer.appendChild(this._closeButton.element());

        this._viewportElement = L.DomUtil.create('div', 'window-viewport', this._rootElement);

        if (this._resizable) {
            this._makeResizable();
        }

        if (this._movable) {
            this._makeMovable();
        }

        this.onRenderWindow(this._viewportElement);

        return this._rootElement;
    },

    onRemove: function () {
        this.onRemoveWindow();

        document.removeEventListener('mousemove', this._onResizeMousemove);
        document.removeEventListener('mouseup', this._onResizeMouseup);
        document.removeEventListener('touchmove', this._onResizeTouchmove);
        document.removeEventListener('touchend', this._onResizeTouchend);
        document.removeEventListener('mousemove', this._onMoveMousemove);
        document.removeEventListener('mouseup', this._onMoveMouseup);
        document.removeEventListener('touchmove', this._onMoveTouchmove);
        document.removeEventListener('touchend', this._onMoveTouchend);

        if (this._windowManager) {
            this._windowManager.off('dockingAvailabilityStatus', 
                this._onDockingAvailabilityChanged, this);
        }
        this._windowManager = null;
    },

    close: function () {
        if (this._windowManager) {
            this._windowManager.removeWindow(this);
        } else {
            this.remove();
        }
    },

    show: function () {
        View.prototype.show.call(this);
        if (this._persistedDockState) {
            this.dock();
            this._persistedDockState = null;
        }
    },

    hide: function () {
        View.prototype.hide.call(this);
        this._persistedDockState = this.docked();
        this.undock();
    },

    isOpen: function () {
        return !!this._rootElement;
    },

    onAdd: function (windowManager) {
        this._windowManager = windowManager;

        this._processDockingAvailability(
            this._windowManager.dockingAvailabilityStatus);
        this._windowManager.on('dockingAvailabilityStatus', 
            this._onDockingAvailabilityChanged, this);
    },

    viewportElement: function () {
        return this._viewportElement;
    },

    setTitle: function (titleText) {
        this._title.textContent = titleText;
    },

    move: function (left, top) {
        // Don't persist new position while window is docked.
        if (!this.docked()) {
            this._persistedLeft = left;
            this._persistedTop = top;
        }

        this._currentLeft = left;
        this._currentTop = top;        

        this._rootElement.style.transform = 'translate(' + left + 'px, ' + top +  'px)';
        this.onMove();
        this._notifyMoved();
    },

    resize: function (width, height) {
        var newWidth = width;
        var newHeight = height;
        
        // Don't persist new size while window is docked.
        // Also ignore window size limitations when docked.
        if (!this.docked()) {
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
        }        

        this._rootElement.style.width = '' + newWidth + 'px';
        this._rootElement.style.height = '' + newHeight + 'px';
        
        this.onResize();
        this._notifyResized();
    },

    fillSpace: function (spaceGeometry) {
        this.move(spaceGeometry.x, spaceGeometry.y);
        this.resize(spaceGeometry.width, spaceGeometry.height);
    },

    restoreGeometry: function (doneCallback) {
        if (this._persistedLeft === null) {
            this._persistedLeft = this._currentLeft;
        }
        if (this._persistedTop === null) {
            this._persistedTop = this._currentTop;
        }

        var positionChanged = this._persistedLeft !== this._currentLeft || this._persistedTop !== this._currentTop;

        this.move(this._persistedLeft, this._persistedTop);
        this.resize(this._width, this._height);

        if (doneCallback) {
            if (this.docked() && positionChanged && this.isVisible()) {
                DomUtil.singleShotEventListener(this._rootElement, 'transitionend', doneCallback);
            } else {
                doneCallback();
            }
        }
    },

    onRenderWindow: function (/* jshint unused:false */ viewport) {
        // override in child classes
    },

    onRemoveWindow: function () {
        // override in child classes
    },

    onMove: function () {
        // override in child classes
    },

    onResize: function () {
        // override in child classes
    },

    setDockState: function (docked) {
        // this method should be called only by the owner of this window
        
        if (docked && !this.isDockable) return;
        
        this._dockState.docked = docked;
        this._restoreDockStyle();
        this._notifyDockState(this._dockState.docked);
    },

    isDockable: function () {
        return this._dockable;
    },

    dock: function () {
        if (!this._windowManager) return;
        if (!this.isDockable()) return;
        if (this.docked()) return;

        this._windowManager.dockWindow(this);
    },

    undock: function () {
        if (!this._windowManager) return;
        if (!this.isDockable()) return;
        if (!this.docked()) return;

        this._windowManager.undockWindow(this);
    },

    docked: function () {
        return this._dockState.docked;
    },

    getBoundingRect: function () {
        var windowRect = this._rootElement.getBoundingClientRect();
        
        return {
            x: this._currentLeft,
            y: this._currentTop,
            width: windowRect.width,
            height: windowRect.height
        };
    },

    // private methods

    _notifyMoveStart: function () {
        this.fire('moveStart', { view: this });
    },

    _notifyMoving: function () {
        this.fire('moving', { view: this });
    },

    _notifyMoveFinish: function () {
        this.fire('moveFinish', { view: this });
    },

    _notifyResizeStart: function () {
        this.fire('resizeStart', { view: this });
    },

    _notifyResizing: function () {
        this.fire('resizing', { view: this });
    },

    _notifyResizeFinish: function () {
        this.fire('resizeFinish', { view: this });
    },

    _notifyDockState: function (docked) {
        this.fire('resizeFinish', { view: this, docked:docked });
    },

    _notifyMoved: function () {
        this._fireAsync('moved', { view: this });
    },

    _notifyResized: function () {
        this._fireAsync('resized', { view: this });
    },

    _processDockingAvailability: function (dockingAvailable) {
        var docked = this.docked();

        if (this.isDockable() && (dockingAvailable || docked)) {
            this._dockButton.show();
        } else {
            this._dockButton.hide();
        }
    },

    _clearDockStyle: function () {
        L.DomUtil.removeClass(this._rootElement, 'docked');
    },

    _restoreDockStyle: function () {
        if (this.docked()) {
            L.DomUtil.addClass(this._rootElement, 'docked');
        } else {
            L.DomUtil.removeClass(this._rootElement, 'docked');
        }
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

    _onCloseBtnClicked: function () {
        this.close();
    },

    _onDockClicked: function () {
        this.dock();
    },

    _onUndockClicked: function () {
        this.undock();
    },

    _onDockingAvailabilityChanged: function (data) {
        this._processDockingAvailability(data.dockingAvailabilityStatus);
    },

    _onTransitionEnd: function (e) {
        if (e.propertyName !== 'transform' ||
            e.target !== this._rootElement) {
            return;
        }
        
        this.onMove();
        this._notifyMoved();
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
        var windowRect = this.getBoundingRect();

        this._resizeState = {
            startPoint: point,
            startSize: {
                width: windowRect.width,
                height: windowRect.height
            }
        };

        this._notifyFocus();
        this._notifyResizeStart();
    },

    _onResizing: function (point) {
        if (!this._resizeState) return;
        
        var deltaX = point.x - this._resizeState.startPoint.x;
        var deltaY = point.y - this._resizeState.startPoint.y;

        this.resize(
            this._resizeState.startSize.width + deltaX,
            this._resizeState.startSize.height + deltaY
        );

        this._notifyResizing();
    },

    _onEndResize: function () {
        this._resizeState = null;
        this._notifyResizeFinish();
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

        this._clearDockStyle();
        this._notifyFocus();
        this._notifyMoveStart();
    },

    _onMoving: function (point) {
        if (!this._moveState) return;
        
        var deltaX = point.x - this._moveState.startPoint.x;
        var deltaY = point.y - this._moveState.startPoint.y;

        this.move(
            this._moveState.startPosition.left + deltaX,
            this._moveState.startPosition.top + deltaY
        );

        this._notifyMoving();
    },

    _onEndMove: function () {
        this._moveState = null;
        this._restoreDockStyle();
        this._notifyMoveFinish();
    },

    _fireAsync: function (eventType, data, propagate) {
        setTimeout(function () {
            this.fire(eventType, data, !!propagate);
        }.bind(this), 0);
    }

});

export default WindowView;
