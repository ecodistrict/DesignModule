/**
 * View. Base class for views.
 */

/* globals L */ 

var View = L.Evented.extend({

    initialize: function (opts) {
        var options = opts || {};
        this._parent = options.parent;
        
        this.onInitialize(opts);        
        this.render();
    },

    render: function () {
        if (this._rootElement) return;

        this._rootElement = this.onRender();
    },

    remove: function () {
        if (!this._rootElement) return;

        this.onRemove();
        if (!this._rootElement) {
            return this._notifyRemove();
        } 

        var parent = this._rootElement.parentNode;
        if (parent) {
            parent.removeChild(this._rootElement);
        }
        this._rootElement = null;

        this._notifyRemove();
    },

    element: function () {
        return this._rootElement;
    },

    show: function () {
        if (!this._rootElement) return;

        L.DomUtil.removeClass(this._rootElement, 'hidden');
    },

    hide: function () {
        if (!this._rootElement) return;

        L.DomUtil.addClass(this._rootElement, 'hidden');
    },

    onInitialize: function (/* jshint unused:false */ opts) {
        // override in child classes
    },

    onRender: function () {
        // override in child classes
    },

    onRemove: function () {
        // override in child classes
    },

    _notifyRemove: function () {
        this.fire('remove', { view: this });
    },

    _notifyFocus: function () {
        this.fire('focus', { view: this });
    },

});

export default View;
