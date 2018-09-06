/**
 * Graph view controller factory. Creates view controllers
 * depending on a graph.
 */

/* globals L */ 
/* globals GraphViewController */ 
/* globals ContinuousGraphViewController */ 

/* exported GraphViewControllerFactory */

 var GraphViewControllerFactory = L.Evented.extend({

    initialize: function (opts) {
        this._viewControllerMap = {
            continuous: ContinuousGraphViewController
        };
    },

    create: function (graphModel, windowManager, graphViewOptions) {
        var ViewController = this._viewControllerMap[graphModel.type] || GraphViewController;

        return new ViewController({
            graphModel: graphModel,
            windowManager: windowManager,
            graphViewOptions: graphViewOptions
        });
    }
});
