/**
 * WindowManager controls the placement and appearance of windows within a given DOM element.
 */

/* globals L */ 

/* exported WindowManager */
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

        this._nextPosition = {
            x: 0,
            y: 0
        };

        this._onResize();
        window.addEventListener('resize', this._onResize.bind(this));

        this._createDockHighlightArea();
    },

    addWindow: function (window) {
        window.on('focus', this._onWindowFocus, this);
        window.on('moving', this._onWindowMoving, this);
        window.on('moveFinish', this._onWindowMoveFinish, this);
        window.on('resizeStart', this._onWindowResizeStart, this);

        this._addWindowToStack(window);
        var docked = this.dockWindow(window);
        this._windowContainer.appendChild(window.element());
        window.onAdd(this);

        if (!docked) {
            window.move(this._nextPosition.x, this._nextPosition.y);
            this._incrementNextPostion();
        }
    },

    removeWindow: function (window) {
        window.off('focus', this._onWindowFocus, this);
        this._removeWindowFromStack(window);
        this.undockWindow(window);
        window.remove();        
    },

    dockWindow: function (window) {
        if (window.docked()) return true;        
        
        window.setDockState(true);
        if (!this._layout.insert(window)) {
            window.setDockState(false);
            return false;
        }
        
        return true;        
    },

    undockWindow: function (window) {
        if (!window.docked()) return;

        function onWindowGeometryRestored() {
            window.setDockState(false);
        }

        this._layout.remove(window);
        window.restoreGeometry(onWindowGeometryRestored);        
    },

    _createDockHighlightArea: function () {
        this._dockHighlightArea = L.DomUtil.create(
            'div', 'window-manager-dock-highligh hidden',
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
        var window = data.windowView;
        var index = this._windowStack.indexOf(window);
        if (index < 0) return;

        this._windowStack.splice(index, 1);
        this._windowStack.push(window);
        
        this._buildZIndexes();
    },

    _onWindowMoving: function (data) {
        var window = data.windowView;
        if (!window.docked()) return;

        var newSpace = this._layout.findNearestSpace(window);
        if (newSpace) {
            this._showDockHighlightArea(newSpace.geometry);
        } else {
            this._hideDockHighlightArea();
        }
    },

    _onWindowMoveFinish: function (data) {
        var window = data.windowView;
        if (!window.docked()) return;

        var dockToSpace = function (window, space) {
            if (!window) return false;

            this._layout.remove(window);
            if (!this._layout.insert(window, space)) {
                console.warn('Failed to insert a window to a found space.', window, space);
                this.undockWindow(window);
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
        var window = data.windowView;
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
    }

});
