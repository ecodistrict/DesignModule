/**
 * DetailsItemView implements view of a single details item that could be graph, kpi or layer.
 */

import './detailsItem.css';
import detailsItemHtml from './detailsItem.html';

/* globals L, d3 */

var DetailsItemView = L.Evented.extend({

    initialize: function (opts) {
        if (!opts) throw new Error('No arguments are provided the constructor');
        if (!opts.model) throw new Error('model is not provided');
        if (!opts.preview) throw new Error('preview is not provided');        

        this._width = opts.width || 150;
        this._height = opts.height || 80;

        this._model = opts.model;
        this._preview = opts.preview;
        this._parent = opts.parent;

        this.render();        
    },

    render: function () {
        if (this._rootElement) return; 

        this._rootElement = L.DomUtil.create('div', 'details-item-view', this._parent);
        d3.select(this._rootElement)
            .html(detailsItemHtml)
            .style('width', this._width + 'px')
            .style('height', this._height + 'px');
        
        L.DomEvent
            .on(this._rootElement, 'click', L.DomEvent.stop)
            .on(this._rootElement, 'click', this._notifyClicked, this);

        this._updateTitle();
        this._model.on('title', this._updateTitle, this);

        this._updateSelection();
        this._model.on('selected', this._updateSelection, this);

        var previewViewport = d3.select(this._rootElement).select('.preview-viewport').node();

        this._preview.render(previewViewport);
    },

    remove: function () {
        if (!this._rootElement) return;

        this._model.off('title', this._updateTitle, this);
        this._model.off('slected', this._updateSelection, this);

        L.DomUtil.remove(this._rootElement);
        this._rootElement = null;
    },

    show: function () {
        L.DomUtil.removeClass(this._rootElement, 'hidden');
        this._preview.show();
    },

    hide: function () {
        L.DomUtil.addClass(this._rootElement, 'hidden');
        this._preview.hide();
    },

    element: function () {
        return this._rootElement;
    },

    _updateTitle: function () {
        d3.select(this._rootElement).select('.title').text(this._model.title);
    },

    _updateSelection: function () {
        if (this._model.selected) {
            this._select();
        } else {
            this._unselect();
        }
    },

    _select: function () {
        L.DomUtil.addClass(this._rootElement, 'selected');
    },

    _unselect: function () {
        L.DomUtil.removeClass(this._rootElement, 'selected');
    },

    _notifyClicked: function () {
        this.fire('clicked', { view: this, model: this._model });
    }

});

export default DetailsItemView;
