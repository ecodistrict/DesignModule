/**
 * Dock button view.
 */

/* globals L */ 

/* exported DockButtonView */
var DockButtonView = L.Evented.extend({

    initialize: function (opts) {
        if (!opts) throw new Error('No arguments are provided to the View');
        if (!opts.dockButtonViewModel) throw new Error('dockButtonViewModel is not provided');

        this._parent = opts.parent;
        this.dockButtonViewModel = opts.dockButtonViewModel;
        this.dockButtonViewModel.on('docked', this._updateState, this);

        this.render();
    },

    render: function () {
        if (this._rootElement) return;

        this._rootElement = L.DomUtil.create('span', 'dock-btn', this._parent);
        this._rootElement.addEventListener('click', this._onClick.bind(this));
        this._rootElement.addEventListener('touchend', this._onClick.bind(this));
        this._rootElement.addEventListener('mousedown', this._onPress.bind(this));
        this._rootElement.addEventListener('touchstart', this._onPress.bind(this));
    },

    remove: function () {
        if (!this._rootElement) return;

        this.dockButtonViewModel.off('docked', this.updateState, this);

        var parent = this._rootElement.parentNode;
        if (parent) {
            parent.removeChild(this._rootElement);
        }

        this._rootElement = null;
    },

    element: function () {
        return this._rootElement;
    },

    show: function () {
        L.DomUtil.removeClass(this._rootElement, 'hidden');
    },

    hide: function () {
        L.DomUtil.addClass(this._rootElement, 'hidden');
    },

    _updateState: function () {
        if (this.dockButtonViewModel.docked) {
            L.DomUtil.removeClass(this._rootElement, 'fas');
            L.DomUtil.removeClass(this._rootElement, 'fa-thumbtack');
            L.DomUtil.addClass(this._rootElement, 'fas');
            L.DomUtil.addClass(this._rootElement, 'fa-window-restore');
        } else {
            L.DomUtil.removeClass(this._rootElement, 'fas');
            L.DomUtil.removeClass(this._rootElement, 'fa-window-restore');
            L.DomUtil.addClass(this._rootElement, 'fas');
            L.DomUtil.addClass(this._rootElement, 'fa-thumbtack');
        }        
    },

    _onClick: function (e) {
        e.preventDefault();
        e.stopPropagation();

        if (this.dockButtonViewModel.docked) {
            this._notifyUndockClicked();
        } else {
            this._notifyDockClicked();
        }
    },

    _onPress: function (e) {
        e.stopPropagation();
        this._notifyFocus();
    },

    _notifyDockClicked: function () {
        this.fire('dockClicked', { button: this });
    },

    _notifyUndockClicked: function () {
        this.fire('undockClicked', { button: this });
    },

    _notifyFocus: function () {
        this.fire('focus', { button: this });
    },

});
