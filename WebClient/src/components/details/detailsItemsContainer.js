/**
 * DetailsItemsContainer is a container to store details items as models and their views.
 * View will be created for model. Views are added to a DOM element associated with the container.
 */

/* globals L */

import { not } from '../../utils/functionUtils';

var DetailsItemsContainer = L.Evented.extend({

    initialize: function (opts) {
        if (!opts) throw new Error('No arguments are provided the constructor');
        if (!opts.element) throw new Error('element is not provided');
        if (!opts.createView) throw new Error('createView is not provided');
        
        this._element = opts.element;
        this._createView = opts.createView;
        this._equalityPredicate = 
            opts.equalityPredicate || function (leftArg, rightArg) { return leftArg === rightArg; };

        this._views = [];

        this.listenTo(opts.modelCollection);
    },

    remove: function () {
        if (this._modelCollection) {
            this._modelCollection.off('change', this._onModelCollectionChange, this);
            this._modelCollection = null;
        }

        var removeView = this._removeView.bind(this);
        this._views.forEach(removeView);
        this._views = [];
    },

    containsModel: function (model) {
        var isModelEqual = this._equalityPredicate;
        function viewWithModel(viewInfo) {
            return isModelEqual(viewInfo.model, model);
        }
        return !!this._views.find(viewWithModel);
    },

    setModels: function (models) {
        var modelsToSet = Array.isArray(models) ? models : [models];

        var isModelEqual = this._equalityPredicate;
        
        function isViewToKeep(viewInfo) { 
            return !!modelsToSet.find(function (modelToKeep) {
                return isModelEqual(modelToKeep, viewInfo.model);
            });
        }

        var removeView = this._removeView.bind(this);
        
        function softRemoveView(viewInfo) {
            viewInfo.view.remove();
        }
        
        // remove views with a model missing in the 'models' array.
        this._views.filter(not(isViewToKeep)).forEach(removeView);
        this._views = this._views.filter(isViewToKeep);

        this._views.forEach(softRemoveView);
        this._views = [];

        this.addModels(modelsToSet);
    },

    addModels: function (models) {
        var containsModel = this.containsModel.bind(this);
        var createViewInfo = this._createViewInfo.bind(this);

        var modelsToAdd = Array.isArray(models) ? models : [models];        
        modelsToAdd = modelsToAdd.filter(function (model) { return !containsModel(model); });

        var newViews = modelsToAdd
            .map(createViewInfo)
            .filter(function (viewInfo) { return viewInfo !== null; });
        
        this._views = this._views.concat(newViews);

        newViews.forEach(function (viewInfo) {
            this._element.appendChild(viewInfo.view.element());
            
            if (this.isVisisble()) {
                viewInfo.view.show();
            }
        }.bind(this));
    },
    
    removeModels: function (models) {
        var modelsToRemove = Array.isArray(models) ? models : [models];

        var isModelEqual = this._equalityPredicate;
        
        function isViewToRemove(viewInfo) { 
            return !!modelsToRemove.find(function (modelToRemove) {
                return isModelEqual(modelToRemove, viewInfo.model);
            });
        }

        var removeView = this._removeView.bind(this);

        this._views.filter(isViewToRemove).forEach(removeView);
        this._views = this._views.filter(not(isViewToRemove));
    },

    element: function () {
        return this._element;
    },

    listenTo: function (modelCollection) {
        if (!modelCollection && this._modelCollection) {
            this._modelCollection.off('change', this._onModelCollectionChange, this);
            this._modelCollection = null;
            return;
        }

        this._modelCollection = modelCollection;
        if (modelCollection) {
            this._onModelCollectionChange({ models: modelCollection.models });
            this._modelCollection.on('change', this._onModelCollectionChange, this);
        }
    },

    show: function () {
        L.DomUtil.removeClass(this._element, 'hidden');
        
        this._views.forEach(function (viewInfo) {
            viewInfo.view.show();
        });
    },

    hide: function () {
        L.DomUtil.addClass(this._element, 'hidden');
        
        this._views.forEach(function (viewInfo) {
            viewInfo.view.hide();
        });
    },

    isVisisble: function () {
        return !L.DomUtil.hasClass(this._element, 'hidden');        
    },

    _onModelCollectionChange: function (data) {
        this.setModels(data.models);
    },

    _createViewInfo: function (model) {
        var view;

        try {
            view = this._createView(model);
        } catch (e) {
            console.warn('Failed to create a view for the model', model, e);
            return null;
        }

        return {
            model: model,
            view: view
        };
    },

    _removeView: function (viewInfo) {
        viewInfo.view.remove();
    }

});

export default DetailsItemsContainer;
