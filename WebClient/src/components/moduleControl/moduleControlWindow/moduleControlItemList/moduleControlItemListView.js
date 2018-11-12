/**
 * ModuleControlItemListView. View for displaying a list of modules.
 */

/* globals L */ 

import './moduleControlItemList.css';
import View from '../../../../core/view';
import ModuleControlItemView from '../moduleControlItem/moduleControlItemView';

var ModuleControlItemListView = View.extend({

    onInitialize: function (opts) {
        if (!opts) throw new Error('No arguments are provided to the View');
        if (!opts.modules) throw new Error('opts.modules property is not provided');

        this._moduleItems = {};
        this._modules = opts.modules;
    },

    onRender: function () {
        this._rootElement = L.DomUtil.create('div', 'module-control-item-list', this._parent);
        
        this._addModuleItems({ models: this._modules.models });
        
        this._modules.on('add', this._addModuleItems, this);
        this._modules.on('remove', this._removeModuleItems, this);

        return this._rootElement;
    },

    onRemove: function () {
        this._modules.off('add', this._addModuleItems, this);
        this._modules.off('remove', this._removeModuleItems, this);

        this._removeReadyModuleItems({ models: this._moduleControlWindowViewModel.readyModules.models });
    },

    _addModuleItems: function (data) {
        if (!data || !Array.isArray(data.models)) return;

        var modules = data.models;        
        var moduleControlList = this._rootElement;
        var moduleItems = this._moduleItems;
        
        modules.forEach(function (module) {
            if (moduleItems[module.id]) return;

            var moduleItemView = new ModuleControlItemView({
                parent: moduleControlList,
                moduleControlItemModel: module
            });

            moduleItems[module.id] = moduleItemView;
        });
    },

    _removeModuleItems: function (data) {
        if (!data || !data.models) return;

        var modules = data.models;
        var moduleItems = this._moduleItems;
        
        modules.forEach(function (module) {
            var moduleItemView = moduleItems[module.id];
            if (!moduleItemView) return;

            delete moduleItems[module.id];
            moduleItemView.remove();
        });
    }

});

export default ModuleControlItemListView;
