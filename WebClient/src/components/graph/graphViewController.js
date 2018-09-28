/**
 * Graph view controller. Manages graph model anf graph view.
 * This is a base class for all child graph view controllers.
 */

/* globals L */

import GraphView from './graphView';
import GraphViewModel from './graphViewModel';
import GraphLegendViewModel from './graphLegendViewModel';

var GraphViewController = L.Evented.extend({

    initialize: function (opts) {
        if (!opts.graphModel) throw new Error('graphModel is not provided');
        if (!opts.windowManager) throw new Error('windowManager is not provided');

        this._graphViewOptions = L.extend({
            minWidth: 150,
            minHeight: 100,
            maxWidth: 800,
            maxHeight: 600,
            width: 300,
            height: 200
        }, opts.graphViewOptions);

        this.windowManager = opts.windowManager;

        this.graphModel = opts.graphModel;
        this.graphModel.on('title', this._updateGraphViewModelTitle, this);
        this.graphModel.on('series', this._updateGraphViewModelLegend, this);
        this.graphModel.on('axes', this._updateGraphViewModelAxes, this);
        this.graphModel.on('categories', this._updateGraphViewModelCategories, this);

        this.graphLegendViewModel = new GraphLegendViewModel({
            entries: this._constructLegendFromSeries(this.graphModel.series)
        });
        this.graphLegendViewModel.on('entries', this._updateGraphViewModelSeries, this);

        this.graphViewModel = new GraphViewModel({
            title: this.graphModel.title,
            series: this._filterSeriesByLegend(
                this.graphModel.series, 
                this.graphLegendViewModel.entries
            ),
            axes: this.graphModel.axes,
            categories: this.graphModel.categories
        });

        this.onInitialize();

        this.graphView = this.createGraphView();
        this.graphView.on('entryClicked', this._toggleLegendEntry, this);
        this.graphView.on('remove', this._notifyViewClosed, this);
        
        this.windowManager.addWindow(this.graphView);
    },

    closeView: function () {
        this.graphView.close();
    },

    isViewOpen: function () {
        return this.graphView.isOpen();
    },

    remove: function () {
        if (!this.graphModel) return;

        this.onRemove();
        
        this.graphModel.off('title', this._updateGraphViewModelTitle, this);
        this.graphModel.off('series', this._updateGraphViewModelLegend, this);
        this.graphModel.off('axes', this._updateGraphViewModelAxes, this);
        this.graphModel.off('categories', this._updateGraphViewModelCategories, this);
        this.graphModel = null;

        this.graphLegendViewModel.off('entries', this._updateGraphViewModelSeries, this);
        this.graphLegendViewModel = null;

        this.graphViewModel = null;

        this.graphView.close();
        this.graphView.off('entryClicked', this._toggleLegendEntry, this);
        this.graphView.off('remove', this._notifyViewRemoved, this);
        this.graphView = null;
        
        this.windowManager = null;
    },

    onInitialize: function () {
        // can be overriden in child class
    },

    onRemove: function () {
        // can be overriden in child class
    },

    createGraphView: function () {
        // can be overriden in child class
        
        return new GraphView(L.extend({
            graphViewModel: this.graphViewModel,
            graphLegendModel: this.graphLegendViewModel,            
        }, this._graphViewOptions));
    },

    processAxes: function (axes) {
        // can be overriden in child class
        return axes;
    },

    processCategories: function (categories) {
        // can be overriden in child class
        return categories;
    },

    processSeries: function (series) {
        // can be overriden in child class
        return series;
    },

    _notifyViewClosed: function (eventData) {
        this.fire('viewClosed', {
            graphViewController: this,
            view: eventData.windowView
        });
    },

    _toggleLegendEntry: function (eventData) {
        var legendEntry = eventData.entry;
        if (legendEntry.enabled && this.graphLegendViewModel.enabledEntriesCount() === 1) return;

        this.graphLegendViewModel.toggleEntry(legendEntry);
    },

    _filterSeriesByLegend: function(series, legendEntries) {
        var enabledEntries = legendEntries
            .reduce(function (result, entry) {
                result[entry.id] = !!entry.enabled;
                return result;
            }, {});

        return series.filter(function (s) {
            return enabledEntries[s.id];
        });
    },

    _updateGraphViewModelTitle: function () {
        this.graphViewModel.title = this.graphModel.title;
    },

    _updateGraphViewModelSeries: function () {
        var filteredSeries = this._filterSeriesByLegend(
            this.graphModel.series, 
            this.graphLegendViewModel.entries
        );

        this.graphViewModel.series = this.processSeries(filteredSeries);
    },

    _updateGraphViewModelLegend: function () {
        var legendEntries = this._constructLegendFromSeries(this.graphModel.series);
        this.graphLegendViewModel.merge(legendEntries);
    },

    _updateGraphViewModelAxes: function () {
        this.graphViewModel.axes = this.processAxes(this.graphModel.axes);
    },

    _updateGraphViewModelCategories: function () {
        this.graphViewModel.categories = this.processCategories(this.graphModel.categories);
    },

    _constructLegendFromSeries: function (series) {
        return series.map(function (s) {
            return {
                id: s.id,
                title: s.title,
                color: s.color
            };
        });
    }
});

export default GraphViewController;
