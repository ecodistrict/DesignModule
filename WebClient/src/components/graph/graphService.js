/**
 * Graph service. Provides graph models to the application.
 * Responsible for retrieving graphs from server side and 
 * maintaining them in sync with the server.
 */

/* globals L */

import './graphModel';
import GraphsModel from './graphsModel';

var GraphService = L.Evented.extend({

    initialize: function () {
        this._graphsModel = new GraphsModel();
        
        Object.defineProperty(this, 'graphsModel', {
            get: function () { return this._graphsModel; }
        });
    }

});

export default GraphService;
