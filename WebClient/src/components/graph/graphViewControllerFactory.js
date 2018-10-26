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

    destroy: function () {

    },

    create: function (graphModel, graphViewOptions) {
        // In case type is unknown a generic graph view controller will be returned
        // in order to show at least an empty graph window and not to crash.
        var ViewController = this._viewControllerMap[graphModel.type] || GraphViewController;

        return new ViewController({
            graphModel: graphModel,
            graphViewOptions: graphViewOptions
        });
    }
});

export default GraphViewControllerFactory;
