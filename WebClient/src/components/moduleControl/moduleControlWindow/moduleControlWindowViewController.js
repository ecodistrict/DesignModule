/**
 * ModuleControlWindowViewController. This is a controller for ModuleControlWindowView.
 */

/* globals L */ 

import ModuleControlWindowView from './moduleControlWindowView';
import ModuleControlWindowViewModel from './moduleControlWindowViewModel';
import ModuleControlItemModel from './moduleControlItem/moduleControlItemModel';
import { ModuleStatus } from '../moduleModel';
import ModelCollection from '../../../core/modelCollection';

var moduleStatusToStringMap = {};
moduleStatusToStringMap[ModuleStatus.READY] = 'ready';
moduleStatusToStringMap[ModuleStatus.CALCULATING] = 'busy...';
moduleStatusToStringMap[ModuleStatus.BUSY] = 'busy...';

function moduleStatusToString(moduleStatus) {
    var statusText = moduleStatusToStringMap[moduleStatus] || '';
    return statusText;
}

function isModuleBusy(moduleStatus) {
    return moduleStatus === ModuleStatus.CALCULATING ||
           moduleStatus === ModuleStatus.BUSY;
}

function isModuleReady(moduleStatus) {
    return moduleStatus === ModuleStatus.READY;
}

function moduleStatusToLoadingFlag(moduleStatus) {
    return isModuleBusy(moduleStatus);
}

function buildModuleControlItemModelData(moduleModel) {
    return {
        id: moduleModel.id,
        name: moduleModel.name,
        status: moduleStatusToString(moduleModel.status),
        loading: moduleStatusToLoadingFlag(moduleModel.status)
    };
}

var ModuleControlWindowViewController = L.Evented.extend({

    initialize: function (opts) {
        if (!opts) throw new Error('No arguments are provided to the View');
        if (!opts.modules) throw new Error('opts.modules property is not provided');

        this._modules = opts.modules;
        this._moduleControlItemModelsCache = new ModelCollection();
        this._updateModuleControlItemModelsCache();

        this._moduleControlWindowViewModel = new ModuleControlWindowViewModel();
        this._moduleControlWindowView = new ModuleControlWindowView({
            moduleControlWindowViewModel: this._moduleControlWindowViewModel
        });
        
        this._moduleControlWindowView.on('show', this._subscribe, this);
        this._moduleControlWindowView.on('hide', this._unsubscribe, this);
    },

    remove: function () {
        if (!this._moduleControlWindowView) return;

        this._unsubscribe();

        this._moduleControlWindowView.close();
        this._moduleControlWindowView = null;
    },

    view: function () {
        return this._moduleControlWindowView;        
    },

    model: function () {
        return this._moduleControlWindowViewModel;
    },

    hideModuleControlWindowView: function () {
        this._moduleControlWindowView.hide();
    },

    showModuleControlWindowView: function () {
        this._moduleControlWindowView.show();
    },

    toggleModuleControlWindowView: function () {
        if (this._moduleControlWindowView.isVisible()) {
            this._moduleControlWindowView.hide();
        } else {
            this._moduleControlWindowView.show();
        }
    },

    _subscribe: function () {
        this._subscribeOnModuleModels({ models: this._modules.models });
        this._updateModuleControlWindowViewModel();

        this._modules.on('add', this._subscribeOnModuleModels, this);
        this._modules.on('remove', this._unsubscribeFromModuleModels, this);
        this._modules.on('change', this._updateModuleControlWindowViewModel, this);
    },

    _unsubscribe: function () {
        this._modules.off('add', this._subscribeOnModuleModels, this);
        this._modules.off('remove', this._unsubscribeFromModuleModels, this);
        this._modules.off('change', this._updateModuleControlWindowViewModel, this);

        this._unsubscribeFromModuleModels({ models: this._modules.models });
    },

    _updateModuleControlItemModelsCache: function () {
        var moduleControlItemModelsCache = this._moduleControlItemModelsCache;
        var moduleModels = this._modules.models;

        function toModuleControlItemModel(moduleModel) {
            var moduleControlItemModelData = buildModuleControlItemModelData(moduleModel);
            var moduleControlItemModel = moduleControlItemModelsCache.getById(moduleModel.id);
            
            if (moduleControlItemModel) {
                moduleControlItemModel.set(moduleControlItemModelData);
            } else {
                moduleControlItemModel = new ModuleControlItemModel(moduleControlItemModelData);
            }

            return moduleControlItemModel;
        }
        
        var moduleControlItemModels = moduleModels.map(toModuleControlItemModel);
        moduleControlItemModelsCache.set(moduleControlItemModels);
    },

    _updateModuleControlWindowViewModel: function () {
        this._updateModuleControlItemModelsCache();

        var moduleControlItemModelsCache = this._moduleControlItemModelsCache;

        function getModuleControlItemModel(moduleModel) {
            return moduleControlItemModelsCache.getById(moduleModel.id);
        }

        function filterReadyModels(moduleModel) {
            if (!moduleModel) return false;
            return isModuleReady(moduleModel.status);
        }

        function filterBusyModels(moduleModel) {
            if (!moduleModel) return false;
            return isModuleBusy(moduleModel.status);
        }

        function filterNotEmpty(item) {
            return !!item;
        }

        var moduleModels = this._modules.models;

        var readyModuleControlItemModels = moduleModels
            .filter(filterReadyModels)
            .map(getModuleControlItemModel)
            .filter(filterNotEmpty);        

        var busyModuleControlItemModels = moduleModels
            .filter(filterBusyModels)
            .map(getModuleControlItemModel)
            .filter(filterNotEmpty);

        this._moduleControlWindowViewModel.readyModules.set(readyModuleControlItemModels);
        this._moduleControlWindowViewModel.busyModules.set(busyModuleControlItemModels);
    },

    _subscribeOnModuleModels: function (data) {
        if (!data) return;
        if (!Array.isArray(data.models)) return;

        var modules = data.models;
        modules.forEach(function (module) {
            module.on('change', this._updateModuleControlItemModel, this);
            module.on('status', this._updateModuleControlWindowViewModel, this);
        }.bind(this));
    },

    _unsubscribeFromModuleModels: function (data) {
        if (!data) return;
        if (!Array.isArray(data.models)) return;

        var modules = data.models;
        modules.forEach(function (module) {
            module.off('change', this._updateModuleControlItemModel, this);
            module.off('status', this._updateModuleControlWindowViewModel, this);
        }.bind(this));
    },

    _updateModuleControlItemModel: function (data) {
        var moduleModel = data.module;
        if (!moduleModel) return;

        var moduleControlItem = 
            this._moduleControlWindowViewModel.readyModules.getById(moduleModel.id) || 
            this._moduleControlWindowViewModel.busyModules.getById(moduleModel.id);        
        
        if (moduleControlItem) {
            var moduleControlItemData = buildModuleControlItemModelData(moduleModel);
            moduleControlItem.set(moduleControlItemData);
        }
    }
    
}); 

export default ModuleControlWindowViewController;
