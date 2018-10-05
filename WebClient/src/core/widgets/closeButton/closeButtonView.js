/**
 * Close button view.
 */

/* globals L */ 

import './closeButton.css';
import View from '../../view';

var CloseButtonView = View.extend({

    onRender: function () {
        var rootElement = L.DomUtil.create('span', 'close-btn fas fa-times', this._parent);
        rootElement.addEventListener('click', this._onClick.bind(this));
        rootElement.addEventListener('touchend', this._onClick.bind(this));
        rootElement.addEventListener('mousedown', this._onPress.bind(this));
        rootElement.addEventListener('touchstart', this._onPress.bind(this));
        return rootElement;
    },

    _onClick: function (e) {
        e.preventDefault();
        e.stopPropagation();

        this._notifyCloseClicked();
    },

    _onPress: function (e) {
        e.stopPropagation();
        this._notifyFocus();
    },

    _notifyCloseClicked: function () {
        this.fire('closeClicked', { view: this });
    }

});

export default CloseButtonView;
