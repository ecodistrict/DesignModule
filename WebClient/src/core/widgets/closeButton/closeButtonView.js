/**
 * Close button view.
 */

/* globals L */ 

/* exported CloseButtonView */
var CloseButtonView = L.Evented.extend({

    initialize: function (opts) {
        var options = opts || {};        

        this._parent = options.parent;
        
        this.render();
    },

    render: function () {
        if (this._rootElement) return;

        this._rootElement = L.DomUtil.create('span', 'close-btn fas fa-times', this._parent);
        this._rootElement.addEventListener('click', this._onClick.bind(this));
        this._rootElement.addEventListener('touchend', this._onClick.bind(this));
        this._rootElement.addEventListener('mousedown', this._onPress.bind(this));
        this._rootElement.addEventListener('touchstart', this._onPress.bind(this));
    },

    remove: function () {
        if (!this._rootElement) return;

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
        this.fire('closeClicked', { button: this });
    },

    _notifyFocus: function () {
        this.fire('focus', { button: this });
    },

});
