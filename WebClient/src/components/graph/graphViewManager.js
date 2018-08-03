/**
 * Graph view manager. Manages graphs views in the application.
 * Responsible for displaying the graphs on the screen and maintaining
 * their visual state.
 */

var GraphViewManager = L.Evented.extend({

    initialize: function (opts) {
        if (!opts.windowManager) throw new Error('windowManager is not provided');
        this._options = opts;

        this._windowManager = this._options._windowManager;
        this._graphViewControllers = {};
        this._graphViewControllerFactory = new GraphViewControllerFactory();
    },

    showGraph: function (graphModel) {
        if (this._graphViewControllers[graphModel.id]) return;
        
        var grpahViewController = this._graphViewControllerFactory
            .create(graphModel, windowManager, this._options.graphViewOptions);

        this._addGraphViewController(grpahViewController);
        this._notifyGraphShown(graphModel);
    },

    hideGraph: function (graphModel) {
        var grpahViewController = this._graphViewControllers[graphModel.id];
        if (!grpahViewController) return;

        grpahViewController.closeView();
    },

    isGraphShown: function (graphModel) {
        var grpahViewController = this._graphViewControllers[graphModel.id];
        if (!grpahViewController) return false;
        
        return grpahViewController.isViewOpen();
    },

    _addGraphViewController: function (grpahViewController) {
        var id = grpahViewController.originalGraphModel.id;
        this._graphViewControllers[id] = grpahViewController;
        grpahViewController.on('viewClosed', this._onGraphViewClosed, this);
    },

    _removeGraphViewController: function (grpahViewController) {
        delete this._graphViewControllers[grpahViewController.originalGraphModel.id];
        grpahViewController.off('viewClosed', this._onGraphViewClosed, this);
        grpahViewController.remove();
    },

    _onGraphViewClosed: function (eventData) {
        var grpahViewController = eventData.grpahViewController;
        this._notifyGraphHidden(grpahViewController.originalGraphModel);
        this._removeGraphViewController(grpahViewController);
    },

    _notifyGraphShown: function (graphModel) {
        this.fire('graphShown', { graphModel: graphModel });
    },

    _notifyGraphHidden: function (graphModel) {
        this.fire('graphHidden', { graphModel: graphModel });
    }

});
