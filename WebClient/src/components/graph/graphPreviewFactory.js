/**
 * GraphPreviewFactory. Graph preview factory creates preview depending on a graph.
 */

/* globals L */

import View from '../../core/view';
import CategoryGraphPreview from './categoryGraph/categoryGraphPreview';

var GraphPreviewFactory = L.Evented.extend({

    initialize: function () {
        this._previewMap = {
            category: CategoryGraphPreview
        };
    },

    destroy: function () {

    },

    create: function (graphModel, previewViewportElement) {
        var PreviewClass = this._previewMap[graphModel.type] || View;

        return new PreviewClass({
            graphModel: graphModel,
            parent: previewViewportElement
        });
    }
});

export default GraphPreviewFactory;
