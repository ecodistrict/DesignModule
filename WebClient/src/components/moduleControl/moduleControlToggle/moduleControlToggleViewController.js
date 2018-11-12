/**
 * ModuleControlToggleViewController. This is a controller for ModuleControlToggleView.
 */

/* globals L */ 

import ModuleControlToggleView from './moduleControlToggleView';
import ModuleControlToggleViewModel from './moduleControlToggleViewModel';
import { ModuleStatus } from '../moduleModel';

var ModuleControlToggleViewController = L.Evented.extend({

    initialize: function (opts) {
        if (!opts) throw new Error('No arguments are provided to the View');
        if (!opts.modules) throw new Error('opts.modules property is not provided');

        this._modules = opts.modules;
        this._subscribeToModules({ models: this._modules.models });
        this._modules.on('add', this._subscribeToModules, this);
        this._modules.on('remove', this._unsubscribeFromModules, this);
        this._modules.on('change', this._calculateIsLoadingFlagValue, this);

        this._moduleControlToggleViewModel = new ModuleControlToggleViewModel();
        this._moduleControlToggleView = new ModuleControlToggleView({ toggleViewModel: this._moduleControlToggleViewModel });
        this._moduleControlToggleView.on('clicked', this._notifyModuleControlToggle, this);

        this._calculateIsLoadingFlagValue();
    },

    remove: function () {
        if (!this._moduleControlToggleView) return;

        this._unsubscribeFromModules({ models: this._modules.models });
        this._modules = null;

        this._moduleControlToggleView.off('clicked', this._notifyModuleControlToggle, this);
        this.off();

        this._moduleControlToggleView.remove();
        this._moduleControlToggleView = null;
    },

    view: function () {
        return this._moduleControlToggleView;
    },

    model: function () {
        return this._moduleControlToggleViewModel;
    },

    _calculateIsLoadingFlagValue: function () {
        var loading = this._modules.models.reduce(function (loading, module) {
            return loading || 
                   (module.status === ModuleStatus.CALCULATING) || 
                   (module.status === ModuleStatus.BUSY);
        }, false);
        
        this._moduleControlToggleViewModel.loading = loading;
    },

    _subscribeToModules: function (data) {
        if (!data) return;
        if (!Array.isArray(data.models)) return;

        var modules = data.models;
        modules.forEach(function (module) {
            module.on('status', this._calculateIsLoadingFlagValue, this);
        }.bind(this));
    },

    _unsubscribeFromModules: function (data) {
        if (!data) return;
        if (!Array.isArray(data.models)) return;

        var modules = data.models;
        modules.forEach(function (module) {
            module.off('status', this._calculateIsLoadingFlagValue, this);
        }.bind(this));
    },

    _notifyModuleControlToggle: function () {
        this.fire('moduleControlToggle');
    }

}); 

export default ModuleControlToggleViewController;
