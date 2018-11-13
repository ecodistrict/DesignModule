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
    },

    onRender: function () {
        this._buttonElement = L.DomUtil.create('span', 'dock-btn', this._parent);        
        this._buttonElement.addEventListener('click', this._onClick.bind(this));
        this._buttonElement.addEventListener('touchend', this._onClick.bind(this));
        this._buttonElement.addEventListener('mousedown', this._onPress.bind(this));
        this._buttonElement.addEventListener('touchstart', this._onPress.bind(this));

        this._updateState();
        this.dockButtonViewModel.on('docked', this._updateState, this);
        
        return this._buttonElement;
    },

    onRemove: function () {
        this.dockButtonViewModel.off('docked', this.updateState, this);
    },

    _updateState: function () {
        if (this.dockButtonViewModel.docked) {
            L.DomUtil.removeClass(this._buttonElement, 'fas');
            L.DomUtil.removeClass(this._buttonElement, 'fa-thumbtack');
            L.DomUtil.addClass(this._buttonElement, 'fas');
            L.DomUtil.addClass(this._buttonElement, 'fa-window-restore');
        } else {
            L.DomUtil.removeClass(this._buttonElement, 'fas');
            L.DomUtil.removeClass(this._buttonElement, 'fa-window-restore');
            L.DomUtil.addClass(this._buttonElement, 'fas');
            L.DomUtil.addClass(this._buttonElement, 'fa-thumbtack');
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
