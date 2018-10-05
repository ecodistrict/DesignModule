/**
 * DetailsViewController. Manages details models and details view.
  */

/* globals L */

import DetailsView from './detailsView';
import ModelCollection from '../../core/modelCollection';
import DetailsItemViewModel from './detailsItemViewModel';

var DetailsViewController = L.Evented.extend({

    initialize: function (opts) {
        if (!opts) throw new Error('No arguments are provided the constructor');
        if (!opts.graphService) throw new Error('graphService is not provided');
        if (!opts.graphViewManager) throw new Error('graphViewManager is not provided');
        if (!opts.graphPreviewFactory) throw new Error('graphPreviewFactory is not provided');

        this._graphService = opts.graphService;
        this._graphViewManager = opts.graphViewManager;

        this._graphItemsModel = new ModelCollection();
        this._updateGraphItemsModel();
        this._graphService.graphsModel.on('change', this._updateGraphItemsModel, this);
        this._graphService.graphsModel.on('remove', this._onGraphModelRemoved, this);
        this._graphViewManager.on('graphShown', this._onGraphShown, this);
        this._graphViewManager.on('graphHidden', this._onGraphHidden, this);

        this._kpiItemsModel = new ModelCollection();
        this._layersItemsModel = new ModelCollection();

        this._detailsView = new DetailsView({
            graphItemsModel: this._graphItemsModel,
            kpiItemsModel: this._kpiItemsModel,
            layersItemsModel: this._layersItemsModel,
            graphPreviewFactory: opts.graphPreviewFactory
        });

        this._detailsView.on('graphItemClicked', this._onGraphItemClicked, this);
        this._detailsView.on('kpiItemClicked', this._onKpiItemClicked, this);
        this._detailsView.on('layerItemClicked', this._onLayerItemClicked, this);
    },

    view: function () {
        return this._detailsView;
    },

    remove: function() {
        if (!this._detailsView) return;

        this._graphService.graphsModel.off('change', this._updateGraphItemsModel, this);
        this._graphViewManager.off('graphShown', this._selectGraph, this);
        this._graphViewManager.off('graphHidden', this._unselectGraph, this);

        this._detailsView.remove();
        this._detailsView = null;
    },

    _createGraphItemModel: function (graphModel) {
        return new DetailsItemViewModel({
            id: graphModel.id,
            selected: this._graphViewManager.isGraphShown(graphModel),
            originalModel: graphModel
        });
    },

    _updateGraphItemsModel: function() {
        this._graphItemsModel.set(
            this._graphService.graphsModel.models.map(this._createGraphItemModel.bind(this))
        );
    },

    _onGraphShown: function (data) {
        var graphItemModel = this._graphItemsModel.getById(data.graphModel.id);
        if (graphItemModel) {
            graphItemModel.selected = true;
        }
    },

    _onGraphHidden: function (data) {
        var graphItemModel = this._graphItemsModel.getById(data.graphModel.id);
        if (graphItemModel) {
            graphItemModel.selected = false;
        }
    },

    _onGraphItemClicked: function (data) {
        var graphModel = data.detailsItemModel.originalModel;

        if (this._graphViewManager.isGraphShown(graphModel)) {
            this._graphViewManager.hideGraph(graphModel);
        } else {
            this._graphViewManager.showGraph(graphModel);
        }
    },

    _onKpiItemClicked: function (/* jshint unused:false */ data) {

    },

    _onLayerItemClicked: function (/* jshint unused:false */ data) {

    },

    _onGraphModelRemoved: function (data) {
        var graphModels = data.models;
        var graphViewManager = this._graphViewManager;
        
        function hideGraph(graphModel) {
            graphViewManager.hideGraph(graphModel);
        }
        
        graphModels.forEach(hideGraph);
    }

});

export default DetailsViewController;
