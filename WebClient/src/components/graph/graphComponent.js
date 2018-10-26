/**
 * GraphComponent. This is a entry point for the entire graph component.
 * This class creates and manages all graph related objects.
 */

/* globals L */

import GraphViewControllerFactory from './graphViewControllerFactory';
import GraphPreviewFactory from './graphPreviewFactory';
import GraphService from './graphService';
import GraphViewManager from './graphViewManager';

var graphViewOptions;        
if (window.innerWidth < 500) { // Mobile
    graphViewOptions = {
        minWidth: 200,
        minHeight: 150,
        maxWidth: 900,
        maxHeight: 600,
        width: 250,
        height: 200
    };
} else {
    graphViewOptions = {
        minWidth: 300,
        minHeight: 200,
        maxWidth: 900,
        maxHeight: 600,
        width: 350,
        height: 250
    };
}

var GraphComponent = L.Evented.extend({

    initialize: function (opts) {
        if (!opts) throw new Error('Arguments are not provided');
        if (!opts.windowManager) throw new Error('windowManager is not provided');

        var windowManager = opts.windowManager;

        this._graphService = new GraphService();
        this._graphService.on('showGraphs', this._showGraphs, this);

        this._graphViewControllerFactory = new GraphViewControllerFactory();
        this._graphPreviewFactory = new GraphPreviewFactory();
        
        this._graphViewManager = new GraphViewManager({
            windowManager: windowManager,
            graphViewOptions: graphViewOptions,
            graphViewControllerFactory: this._graphViewControllerFactory
        });
        this._graphViewManager.on('graphCategoryClicked', this._reportGraphCategorySelected, this);
    },

    destroy: function () {
        if (!this._graphService) return;

        this._graphService.off('showGraphs', this._showGraphs, this);
        this._graphViewManager.off('graphCategoryClicked', this._reportGraphCategorySelected, this);

        this._graphService.destroy();
        this._graphService = null;

        this._graphViewManager.destroy();
        this._graphViewManager = null;

        this._graphViewControllerFactory.destroy();
        this._graphViewControllerFactory = null;

        this._graphPreviewFactory.destroy();
        this._graphPreviewFactory = null;
    },

    service: function () {
        return this._graphService;
    },

    graphViewControllerFactory: function () {
        return this._graphViewControllerFactory;
    },

    graphPreviewFactory: function () {
        return this._graphPreviewFactory;
    },

    graphViewManager: function () {
        return this._graphViewManager;
    },

    _showGraphs: function (data) {
        if (!data) return;
        if (!data.graphModels) return;

        var graphViewManager = this._graphViewManager;

        function showGraph(graphModel) {
            graphViewManager.showGraph(graphModel);
        }

        var graphModels = data.graphModels;
        graphModels.forEach(showGraph);
    },

    _reportGraphCategorySelected: function (data) {        
        if (!data) return;
        if (!data.graphModel) return;
        if (!data.categoryId) return;

        this._graphService.reportGraphCategorySelected(data.graphModel, data.categoryId);
    }

});

export default GraphComponent;
