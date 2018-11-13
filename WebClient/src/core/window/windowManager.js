/**
 * WindowManager controls the placement and appearance of windows within a given DOM element.
 */

/* globals L */ 

import './windowManager.css';

var WindowManager = L.Evented.extend({

    initialize: function (opts) {
        if (!opts) throw new Error('options object was not provided. The object is mandatory.');
        if (!opts.element) throw new Error('options.element is not provided.');
        if (!opts.layout) throw new Error('options.layout is not provided.');

        var dockingAvailabilityStatus = false;
        Object.defineProperty(this, 'dockingAvailabilityStatus', {
            get: function () { return dockingAvailabilityStatus; },
            set: function (newDockingAvailabilityStatus) {
                dockingAvailabilityStatus = !!newDockingAvailabilityStatus;
                this.fire('dockingAvailabilityStatus', { 
                    dockingAvailabilityStatus: dockingAvailabilityStatus 
                });
            }
        });

        this._windowContainer = opts.element;
        this._layout = opts.layout;
        this._layout.on('freeSpaceStatus', this._onLayoutfreeSpaceStatusChanged, this);
        this.dockingAvailabilityStatus = this._layout.hasFreeSpace();

        this._windowStack = [];
        this._initialZIndex = this._windowContainer.style.zIndex ? 
            parseInt(this._windowContainer.style.zIndex, 10) : 999;
        this._windowIdCounter = 0;
        this._windowMetaInfoMap = {};

        this._nextPosition = {
            x: 0,
            y: 0
        };

        this._onResize();
        window.addEventListener('resize', this._onResize.bind(this));

        this._createDockHighlightArea();
    },

    addWindow: function (window, opts) {
        if (!window) throw this._createNoWindowError();

        var options = opts || {};

        window.on('focus', this._onWindowFocus, this);
        window.on('moving', this._onWindowMoving, this);
        window.on('moveFinish', this._onWindowMoveFinish, this);
        window.on('resizeStart', this._onWindowResizeStart, this);

        this._addWindowToStack(window);
        window.windowId= ++this._windowIdCounter;
        this._windowMetaInfoMap[window.windowId] = {};
        
        var docked = false;
        if (window.isDockable() && !options.position) {
            docked = this.dockWindow(window);
        }

        this._windowContainer.appendChild(window.element());
        window.onAdd(this);

        if (!docked) {

            if (options.position) {
                this._positionWindow(window, options.position);
            } else {
                window.move(this._nextPosition.x, this._nextPosition.y);
                this._incrementNextPostion();
            }
            
        }
    },

    removeWindow: function (window) {
        if (!window) throw this._createNoWindowError();

        window.off('focus', this._onWindowFocus, this);
        window.off('moving', this._onWindowMoving, this);
        window.off('moveFinish', this._onWindowMoveFinish, this);
        window.off('resizeStart', this._onWindowResizeStart, this);
        
        delete this._windowMetaInfoMap[window.windowId];
        this._removeWindowFromStack(window);
        this._layout.remove(window);
        window.remove();        
    },

    dockWindow: function (window) {
        if (!window) throw this._createNoWindowError();
        
        return this._dockWindowImpl(window);
    },

    undockWindow: function (window) {
        if (!window) throw this._createNoWindowError();

        this._undockWindowImpl(window);
    },

    _createDockHighlightArea: function () {
        this._dockHighlightArea = L.DomUtil.create(
            'div', 'window-manager-dock-highlight hidden',
            this._windowContainer
        );
    },

    _showDockHighlightArea: function (areaGeometry) {
        var initialZIndex = this._initialZIndex;
        this._dockHighlightArea.style.zIndex = initialZIndex + this._windowStack.length;
        this._dockHighlightArea.style.width = '' + areaGeometry.width + 'px';
        this._dockHighlightArea.style.height = '' + areaGeometry.height + 'px';
        this._dockHighlightArea.style.transform = 
            'translate(' + areaGeometry.x + 'px, ' + areaGeometry.y +  'px)';
        L.DomUtil.removeClass(this._dockHighlightArea, 'hidden');
    },

    _hideDockHighlightArea: function () {
        L.DomUtil.addClass(this._dockHighlightArea, 'hidden');
    },

    _positionWindow: function (window, position) {
        if (position === 'center') {
            var containerRect = this._windowContainer.getBoundingClientRect();            
            var windowRect = window.getBoundingRect();

            var x = (containerRect.width - windowRect.width) / 2;
            var y = (containerRect.height - windowRect.height) / 2;

            window.move(x, y);
        }
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

    _onLayoutfreeSpaceStatusChanged: function (data) {
        this.dockingAvailabilityStatus = data.freeSpaceAvailable;
    },

    _onWindowFocus: function (data) {
        var window = data.view;
        var index = this._windowStack.indexOf(window);
        if (index < 0) return;

        this._windowStack.splice(index, 1);
        this._windowStack.push(window);
        
        this._buildZIndexes();
    },

    _onWindowMoving: function (data) {
        var window = data.view;
        if (!window.docked()) return;

        var newSpace = this._layout.findNearestSpace(window);
        if (newSpace) {
            this._showDockHighlightArea(newSpace.geometry);
        } else {
            this._hideDockHighlightArea();
        }
    },

    _onWindowMoveFinish: function (data) {
        var window = data.view;
        if (!window.docked()) return;

        var dockToSpace = function (window, space) {
            if (!window) return false;
            if (!this._dockWindowImpl(window, space)) {
                console.warn('Failed to insert a window to a found space.', window, space);
                this._undockWindowImpl(window);
                return false;
            }
            return true;
        }.bind(this);

        this._hideDockHighlightArea();

        var currentSpace = this._layout.getLayoutItemSpace(window);
        var newSpace = this._layout.findNearestSpace(window);
        if (!newSpace) {
            return dockToSpace(window, currentSpace);
        }

        var newSpaceWindow = this._layout.getLayoutItem(newSpace);
        if (window === newSpaceWindow) {            
            return dockToSpace(window, currentSpace);
        }

        if (dockToSpace(window, newSpace)) {
            dockToSpace(newSpaceWindow, currentSpace);
        }        
    },

    _onWindowResizeStart: function (data) {
        var window = data.view;
        if (!window.docked()) return;

        this._layout.remove(window);
        window.setDockState(false);

        var windowRect = window.getBoundingRect();
        window.move(windowRect.x, windowRect.y);
        window.resize(windowRect.width, windowRect.height);
    },
    
    _onResize: function () {
        var rect = this._windowContainer.getBoundingClientRect();
        this._layout.resize(rect);
    },

    _dockWindowImpl: function (window, space) {
        if (!window) return false;

        var windowMetaInfo = this._windowMetaInfoMap[window.windowId];
        var newSpace = space;
        var previousSpace = windowMetaInfo.dockSpace;
        var windowOnPreviousSpace = this._layout.getLayoutItem(previousSpace);
        if (!newSpace && 
            previousSpace && 
            (!windowOnPreviousSpace || windowOnPreviousSpace === window)) {

            newSpace = previousSpace;
        }
        
        this._layout.remove(window);
        window.setDockState(true);
        if (!this._layout.insert(window, newSpace)) {
            window.setDockState(false);
            return false;
        }

        var dockSpace = newSpace || this._layout.getLayoutItemSpace(window);        
        windowMetaInfo.dockSpace = dockSpace;
        
        return true;        
    },

    _undockWindowImpl: function (window) {
        if (!window) return;

        function onWindowGeometryRestored() {
            window.setDockState(false);
        }

        this._layout.remove(window);
        window.restoreGeometry(onWindowGeometryRestored);        
    }

});

export default WindowManager;
