/**
 * ModuleToastsController. Shows toasts depending on modules state.
 */

/* globals L */

import { ModuleStatus } from './moduleModel';

var ModuleToastsController = L.Evented.extend({

    initialize: function (opts) {
        if (!opts) throw new Error('No arguments are provided');
        if (!opts.modules) throw new Error('opts.modules property is not provided');
        if (!opts.toastFunction) throw new Error('toastFunction is not provided');

        this._actions = {};
        this._actions[ModuleStatus.CALCULATING] = this._showModuleIsBusyToast.bind(this);
        this._actions[ModuleStatus.BUSY] = this._showModuleIsBusyToast.bind(this);
        this._actions[ModuleStatus.READY] = this._showModuleIsReadyToast.bind(this);

        this._modules = opts.modules;
        this._showToast = opts.toastFunction;

        this._subscribeToModules({ models: this._modules.models });
        this._modules.on('add', this._subscribeToModules, this);
        this._modules.on('remove', this._unsubscribeFromModules, this);
    },

    remove: function () {
        if (!this._actions) return;

        this._actions = null;

        this._modules.off('add', this._subscribeToModules, this);
        this._modules.off('remove', this._unsubscribeFromModules, this);
        this._unsubscribeFromModules({ models: this._modules.models });
        this._modules = null;
    },

    _subscribeToModules: function (data) {
        if (!data) return;
        if (!Array.isArray(data.models)) return;

        var modules = data.models;
        modules.forEach(function (module) {
            this._onModuleStatusChange({
                module: module,
                status: module.status
            });
            module.on('status', this._onModuleStatusChange, this);
        }.bind(this));
    },

    _unsubscribeFromModules: function (data) {
        if (!data) return;
        if (!Array.isArray(data.models)) return;

        var modules = data.models;
        modules.forEach(function (module) {
            module.off('status', this._onModuleStatusChange, this);
        }.bind(this));
    },

    _onModuleStatusChange: function (data) {
        if (!data) return;

        var action = this._actions[data.status];
        if (!action) return;
        
        action(data.module);
    },

    _showModuleIsReadyToast: function (module) {
        this._showToast('' + module.name + ' module is READY', 'succes', 5000);
    },

    _showModuleIsBusyToast: function (module) {
        this._showToast('' + module.name + ' module is BUSY', 'succes', 5000);
    },

});

export default ModuleToastsController;
