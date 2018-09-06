/**
 * DetailsView implements panel that allows selecting graph, kpi or layout.
 */

import './details.css';
import detailsHtml from './details.html';
import ItemsContainer from './itemsContainer';
import DetailsItemView from './detailsItemView';

/* globals L, d3 */

var DetailsView = L.Control.extend({
    options: {
        collapsed: true,
        position: 'topright',
        autoZIndex: true
    },

    initialize: function (opts) {
        if (!opts) throw new Error('No arguments are provided the constructor');
        if (!opts.graphItemsModel) throw new Error('graphItemsModel is not provided');
        if (!opts.kpiItemsModel) throw new Error('kpiItemsModel is not provided');
        if (!opts.layersItemsModel) throw new Error('layersItemsModel is not provided');

        this._graphItemsModel = opts.graphItemsModel;
        this._kpiItemsModel = opts.kpiItemsModel;
        this._layersItemsModel = opts.layersItemsModel;
        this._parent = opts.parent;
        this._itemSize = L.extend({ width: 220, height: 150}, opts.itemSize);

        if (this._parent) {
            this.render();
        }        
    },

    render: function () {
        if (this._rootElement) return; 

        this._rootElement = L.DomUtil.create('div', 'details-view', this._parent);
        this._rootElement.innerHTML = detailsHtml;
        
        this._graphsSection = d3.select(this._rootElement).select('.graphs').node();
        this._kpisSection = d3.select(this._rootElement).select('.kpis').node();
        this._layersSection = d3.select(this._rootElement).select('.layers').node();

        this._graphsContainer = new ItemsContainer({
            element: d3.select(this._graphsSection).select('.container').node(),
            createView: this._createGraphItemView.bind(this),
            modelCollection: this._graphItemsModel,
            equalityPredicate: function (modelLeft, modelRight) {
                return modelLeft.originalModel === modelRight.originalModel;
            }
        });
        this._graphsContainer.on('modelRemoved', this._onGraphItemRemoved, this);

        this._kpisContainer = new ItemsContainer({
            element: d3.select(this._kpisSection).select('.container').node(),
            createView: this._createKpiItemView.bind(this),
            modelCollection: this._kpiItemsModel
        });
        this._kpisContainer.on('modelRemoved', this._onKpiItemRemoved, this);

        this._layersContainer = new ItemsContainer({
            element: d3.select(this._layersSection).select('.container').node(),
            createView: this._createLayerItemView.bind(this),
            modelCollection: this._layersItemsModel
        });
        this._layersContainer.on('modelRemoved', this._onLayersItemRemoved, this);

        L.DomEvent
            .on(this._rootElement, 'click', L.DomEvent.stop)
            .on(this._rootElement, 'click', this._expand, this)
            .on(document, 'click', this._collapse, this);
        
        if (!L.Browser.android) {
            L.DomEvent
                .on(this._rootElement, 'mouseenter', this._expand, this)
                .on(this._rootElement, 'mouseleave', this._collapse, this);
        }

        this._updateWidth();
        this._graphItemsModel.on('change', this._updateWidth, this);
        this._kpiItemsModel.on('change', this._updateWidth, this);
        this._layersItemsModel.on('change', this._updateWidth, this);

        this._updateGraphsSectionVisibility();
        this._graphItemsModel.on('change', this._updateGraphsSectionVisibility, this);

        this._updateKpisSectionVisibility();        
        this._kpiItemsModel.on('change', this._updateKpisSectionVisibility, this);

        this._updateLayersSectionVisibility();
        this._layersItemsModel.on('change', this._updateLayersSectionVisibility, this);

        if (this.options.collapsed) {
            this._collapse();
        } else {
            this._expand();
        }

        L.DomEvent.disableClickPropagation(this._rootElement);
    },

    remove: function () {
        if (!this._rootElement) return;

        L.DomEvent.off(document, 'click', this._collapse, this);
        this._graphItemsModel.off('change', this._updateWidth, this);
        this._kpiItemsModel.off('change', this._updateWidth, this);
        this._layersItemsModel.off('change', this._updateWidth, this);
        this._graphItemsModel.off('change', this._updateGraphsSectionVisibility, this);
        this._kpiItemsModel.off('change', this._updateKpisSectionVisibility, this);
        this._layersItemsModel.off('change', this._updateLayersSectionVisibility, this);

        this._graphsContainer.remove();
        this._kpisContainer.remove();
        this._layersContainer.remove();

        L.DomUtil.remove(this._rootElement);
        this._rootElement = null;
    },

    onAdd: function () {
        this.render();
        return this._rootElement;
    },

    _canExpand: function () {
        return !this._graphItemsModel.empty()  || 
               !this._kpiItemsModel.empty()    || 
               !this._layersItemsModel.empty();
    },

    _collapse: function () {
        L.DomUtil.addClass(this._rootElement, 'collapsed');
    },

    _expand: function () {
        if (!this._canExpand()) return;

        L.DomUtil.removeClass(this._rootElement, 'collapsed');
    },

    _createGraphItemView: function (graphItemModel) {
        var itemView = new DetailsItemView({
            model: graphItemModel,
            width: this._itemSize.width,
            height: this._itemSize.height
        });
        itemView.on('clicked', this._onGraphItemClicked, this);

        return itemView;
    },

    _createKpiItemView: function () {
        return null;
    },

    _createLayerItemView: function () {
        return null;
    },

    _onGraphItemClicked: function (data) {
        this._notifyGraphItemClicked(data.model);
    },

    _onGraphItemRemoved: function (data) {
        this._notifyGraphItemRemoved(data.model);
    },

    _onKpiItemRemoved: function (data) {
        this._notifyKpiItemRemoved(data.model);
    },

    _onLayerItemRemoved: function (data) {
        this._notifyLayerItemRemoved(data.model);
    },

    _updateWidth: function () {        
        var maxElementCount = Math.max(
            this._graphItemsModel.models.length,
            this._kpiItemsModel.models.length,
            this._layersItemsModel.models.length
        );
        var elementsPerRow = maxElementCount > 0 ? Math.ceil(Math.sqrt(maxElementCount)) : 1;

        var sectionMargin = 5;
        var itemMargin = 5;
        var itemWidth = this._itemSize.width;
        var rowWidth = itemMargin + (itemWidth + itemMargin + 2) * elementsPerRow + 2*sectionMargin;

        d3.select(this._rootElement).select('.details-body')
            .style('width', rowWidth + 'px');
    },

    _updateGraphsSectionVisibility: function() {
        if (this._graphItemsModel.empty()) {
            L.DomUtil.addClass(this._graphsSection, 'hidden');
        } else {
            L.DomUtil.removeClass(this._graphsSection, 'hidden');
        }
    },

    _updateKpisSectionVisibility: function () {
        if (this._kpiItemsModel.empty()) {
            L.DomUtil.addClass(this._kpisSection, 'hidden');
        } else {
            L.DomUtil.removeClass(this._kpisSection, 'hidden');
        }
    },

    _updateLayersSectionVisibility: function () {
        if (this._layersItemsModel.empty()) {
            L.DomUtil.addClass(this._layersSection, 'hidden');
        } else {
            L.DomUtil.removeClass(this._layersSection, 'hidden');
        }
    },

    _notifyGraphItemClicked: function (graphItemModel) {
        this.fire('graphItemClicked', { detailsItemModel: graphItemModel });
    },

    _notifyKpiItemClicked: function (kpiItemModel) {
        this.fire('kpiItemClicked', { detailsItemModel: kpiItemModel });
    },

    _notifyLayerItemClicked: function (layerItemModel) {
        this.fire('layerItemClicked', { detailsItemModel: layerItemModel });
    },

    _notifyGraphItemRemoved: function (graphItemModel) {
        this.fire('graphItemRemoved', { detailsItemModel: graphItemModel });
    },

    _notifyKpiItemRemoved: function (kpiItemModel) {
        this.fire('kpiItemRemoved', { detailsItemModel: kpiItemModel });
    },

    _notifyLayerItemRemoved: function (layerItemModel) {
        this.fire('layerItemRemoved', { detailsItemModel: layerItemModel });
    }

});
L.extend(DetailsView.prototype, L.Evented.prototype);

export default DetailsView;
