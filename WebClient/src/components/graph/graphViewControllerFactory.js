/**
 * Graph view controller factory. Creates view controllers
 * depending on a graph.
 */

 var GraphViewControllerFactory = L.Evented.extend({

    initialize: function (opts) {
        
    },

    create: function (graphModel, windowManager, viewOptions) {
        return new GraphViewController(L.extend({
            graphModel: graphModel,
	        windowManager: windowManager
        }, viewOptions));
    }
});
