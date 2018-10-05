/**
 * Dock button view.
 */

/* globals L */ 

import './dockButton.css';
import View from '../../view';

var DockButtonView = View.extend({

    onInitialize: function (opts) {
        if (!opts) throw new Error('No arguments are provided to the View');
        if (!opts.dockButtonViewModel) throw new Error('dockButtonViewModel is not provided');

        this.dockButtonViewModel = opts.dockButtonViewModel;
        this.dockButtonViewModel.on('docked', this._updateState, this);
    },

    onRender: function () {
        var rootElement = L.DomUtil.create('span', 'dock-btn', this._parent);
        rootElement.addEventListener('click', this._onClick.bind(this));
        rootElement.addEventListener('touchend', this._onClick.bind(this));
        rootElement.addEventListener('mousedown', this._onPress.bind(this));
        rootElement.addEventListener('touchstart', this._onPress.bind(this));
        return rootElement;
    },

    onRemove: function () {
        this.dockButtonViewModel.off('docked', this.updateState, this);
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
    }

});

export default DockButtonView;
