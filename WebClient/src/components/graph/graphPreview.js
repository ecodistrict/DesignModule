/**
 * GraphPreview is a base class for all concrete graphs previews.
 */

/* globals L */

var GraphPreview = L.Evented.extend({

    initialize: function () { },

    render: function () { },

    remove: function () { },

    show: function () { },

    hide: function () { },

    element: function () { 
        return null;
    }

});

export default GraphPreview;
