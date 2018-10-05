/**
 * DetailsItemView implements view of a single details item that could be graph, kpi or layer.
 */

import './detailsItem.css';
import View from '../../core/view';
import detailsItemHtml from './detailsItem.html';
import TooltipView from '../../core/widgets/tooltip/tooltipView';

/* globals L, d3 */

var DetailsItemView = View.extend({

    onInitialize: function (opts) {
        if (!opts) throw new Error('No arguments are provided the constructor');
        if (!opts.model) throw new Error('model is not provided');
        if (!opts.previewCreator) throw new Error('previewCreator is not provided');        

        this._width = opts.width || 150;
        this._height = opts.height || 80;

        this._model = opts.model;
        this._previewCreator = opts.previewCreator;
    },

    onRender: function () {
        this._rootElement = L.DomUtil.create('div', 'details-item-view', this._parent);

        this._tooltip = new TooltipView({
            html: function () { return this._model.title; }.bind(this)
        });

        d3.select(this._rootElement)
            .html(detailsItemHtml)
            .style('width', this._width + 'px')
            .style('height', this._height + 'px')
            .on('mouseover', this._tooltip.show)
            .on('touchstart', this._tooltip.show)
            .on('mouseout', this._tooltip.hide)
            .on('touchend', this._tooltip.hide);
        
        L.DomEvent
            .on(this._rootElement, 'click', L.DomEvent.stop)
            .on(this._rootElement, 'click', this._notifyClicked, this);

        this._updateTitle();
        this._model.on('title', this._updateTitle, this);

        this._updateSelection();
        this._model.on('selected', this._updateSelection, this);

        var previewViewport = d3.select(this._rootElement).select('.preview-viewport').node();

        this._preview = this._previewCreator(previewViewport);

        return this._rootElement;
    },

    onRemove: function () {
        this._model.off('title', this._updateTitle, this);
        this._model.off('selected', this._updateSelection, this);

        this._tooltip.remove();
    },

    show: function () {
        View.prototype.show.call(this);
        this._preview.show();
    },

    hide: function () {
        View.prototype.hide.call(this);
        this._preview.hide();
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
