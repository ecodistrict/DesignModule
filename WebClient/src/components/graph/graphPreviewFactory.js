/**
 * GraphPreviewFactory. Graph preview factory creates preview depending on a graph.
 */

/* globals L */

import GraphPreview from './graphPreview';
import CategoryGraphPreview from './categoryGraph/categoryGraphPreview';

var GraphPreviewFactory = L.Evented.extend({

    initialize: function () {
        this._previewMap = {
            category: CategoryGraphPreview
        };
    },

    create: function (graphModel) {
        var Preview = this._previewMap[graphModel.type] || GraphPreview;

        return new Preview({
            graphModel: graphModel
        });
    }
});

export default GraphPreviewFactory;
