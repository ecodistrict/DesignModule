/**
 * Graph view controller factory. Creates view controllers
 * depending on a graph.
 */

/* globals L */

import GraphViewController from './graphViewController';
import CategoryGraphViewController from './categoryGraph/categoryGraphViewController';

var GraphViewControllerFactory = L.Evented.extend({

    initialize: function () {
        this._viewControllerMap = {
            category: CategoryGraphViewController
        };
    },

    create: function (graphModel, graphViewOptions) {
        var ViewController = this._viewControllerMap[graphModel.type] || GraphViewController;

        return new ViewController({
            graphModel: graphModel,
            graphViewOptions: graphViewOptions
        });
    }
});

export default GraphViewControllerFactory;
