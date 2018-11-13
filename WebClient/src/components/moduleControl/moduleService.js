/**
 * ModuleService. Provides models list to the application.
 * Responsible for retrieving modules from server side and 
 * maintaining them in sync with the server.
 */

/* globals L */

import ModuleModel from './moduleModel';
import ModelCollection from '../../core/modelCollection';

var ModuleService = L.Evented.extend({

    initialize: function () {
        this._modules = new ModelCollection();
        
        Object.defineProperty(this, 'modules', {
            get: function () { return this._modules; }
        });

        var enabled = false;
        Object.defineProperty(this, 'enabled', {
            get: function () { return enabled; },
            set: function (newEnabled) {
                var newEnabledValue = !!newEnabled;
                if (enabled === newEnabledValue) return;
                
                enabled = newEnabledValue;
                this.fire('enabled', { enabled: enabled });
            }
        });
    },

    remove: function () {
        this._modules.off();
        this._modules.set([]);
    },

    processServerMessage: function (messagePayload) {
        if (!Array.isArray(messagePayload.status)) return;

        var messages = messagePayload.status;

        messages.forEach(function (message) {
            if (!message) return;

            if (typeof message.new !== 'undefined') {
                this._addModule(message.new);
            }                
            else if (typeof message.change !== 'undefined') {
                this._updateModule(message.change);
            }                
            else if (typeof message.delete !== 'undefined') {
                this._removeModule(message.delete);
            }                
            else if (typeof message.reset !== 'undefined') {
                this._resetModules();
            }                
        }.bind(this));
    },

    _addModule: function (module) {
        if (typeof module !== 'object') return;
        if (!module.id) return;
        
        var moduleModel = this._modules.getById(module.id);
        
        try {
            if (moduleModel) {
                moduleModel.set(module);
            } else {
                moduleModel= new ModuleModel(module);
                this._modules.add(moduleModel);
            }
        } catch (error) {
            console.error('Failed to add new module model with id ' + module.id + '. ' + error);
        }
    },

    _updateModule: function (module) {
        if (typeof module !== 'object') return;
        if (!module.id) return;

        var moduleModel = this._modules.getById(module.id);
        if (!moduleModel) return;

        try {
            moduleModel.set(module);
        } catch (error) {
            console.error('Failed to update the module model with id ' + moduleModel.id + '. ' + error);
        }
    },

    _removeModule: function (module) {
        if (typeof module !== 'object') return;
        if (!module.id) return;

        var moduleModel = this._modules.getById(module.id);
        if (moduleModel) {
            this._modules.remove(moduleModel);
        }
    },

    _resetModules: function () {
        this._modules.set([]);
    }

});

export default ModuleService;
