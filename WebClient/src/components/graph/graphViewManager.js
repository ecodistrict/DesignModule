/**
 * Graph view manager. Manages graphs views in the application.
 * Responsible for displaying the graphs on the screen and maintaining
 * their visual state.
 */

/* globals L */

import GraphViewControllerFactory from './graphViewControllerFactory';

var GraphViewManager = L.Evented.extend({

    initialize: function (opts) {
        if (!opts.windowManager) throw new Error('windowManager is not provided');
        this._options = opts;

        this._windowManager = this._options.windowManager;
        this._graphViewControllers = {};
        this._graphViewControllerFactory = new GraphViewControllerFactory();
    },

    showGraph: function (graphModel) {
        if (this._graphViewControllers[graphModel.id]) return;
        
        var graphViewController = this._graphViewControllerFactory
            .create(graphModel, this._windowManager, this._options.graphViewOptions);

        this._addGraphViewController(graphViewController);
        this._notifyGraphShown(graphModel);
    },

    hideGraph: function (graphModel) {
        var graphViewController = this._graphViewControllers[graphModel.id];
        if (!graphViewController) return;

        graphViewController.closeView();
    },

    isGraphShown: function (graphModel) {
        var graphViewController = this._graphViewControllers[graphModel.id];
        if (!graphViewController) return false;
        
        return graphViewController.isViewOpen();
    },

    _addGraphViewController: function (graphViewController) {
        var id = graphViewController.graphModel.id;
        this._graphViewControllers[id] = graphViewController;
        graphViewController.on('viewClosed', this._onGraphViewClosed, this);
    },

    _removeGraphViewController: function (graphViewController) {
        delete this._graphViewControllers[graphViewController.graphModel.id];
        graphViewController.off('viewClosed', this._onGraphViewClosed, this);
        graphViewController.remove();
    },

    _onGraphViewClosed: function (eventData) {
        var graphViewController = eventData.graphViewController;
        this._notifyGraphHidden(graphViewController.graphModel);
        this._removeGraphViewController(graphViewController);
    },

    _notifyGraphShown: function (graphModel) {
        this.fire('graphShown', { graphModel: graphModel });
    },

    _notifyGraphHidden: function (graphModel) {
        this.fire('graphHidden', { graphModel: graphModel });
    }

});

export default GraphViewManager;
