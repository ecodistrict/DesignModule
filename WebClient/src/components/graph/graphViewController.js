/**
 * Graph view controller. Manages graph model anf graph view.
 * This is a base class for all child graph view controllers.
 */

var GraphViewController = L.Evented.extend({

    initialize: function (opts) {
        if (!opts.graphModel) throw new Error('graphModel is not provided');
        if (!opts.windowManager) throw new Error('windowManager is not provided');

        this._viewOptions = L.extend({
            minWidth: 150,
            minHeight: 100,
            maxWidth: 800,
            maxHeight: 600,
            width: 300,
            height: 200
        }, opts.viewOptions);

        this.windowManager = opts.windowManager;

        this.originalGraphModel = opts.graphModel;
        this.originalGraphModel.on('title', this._updateGraphViewModelTitle, this);
        this.originalGraphModel.on('series', this._updateGraphViewModelLegend, this);        

        this.graphLegendViewModel = new GraphLegendViewModel({
            entries: this._constructLegendFromSeries(this.originalGraphModel.series)
        });
        this.graphLegendViewModel.on('entries', this._updateGraphViewModelSeries, this);

        this.graphViewModel = new GraphViewModel({
            title: this.originalGraphModel.title,
            series: this._filterSeriesByLegend(this.originalGraphModel.series, this.graphLegendViewModel.entries)
        });        

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
        if (!this.originalGraphModel) return;
        
        this.originalGraphModel.off('title', this._updateGraphViewModelTitle, this);
        this.originalGraphModel.off('series', this._updateGraphViewModelLegend, this);
        this.originalGraphModel = null;

        this.graphLegendViewModel.off('entries', this._updateGraphViewModelSeries, this);
        this.graphLegendViewModel = null;

        this.graphViewModel = null;

        this.graphView.close();
        this.graphView.off('entryClicked', this._toggleLegendEntry, this);
        this.graphView.off('remove', this._notifyViewRemoved, this);
        this.graphView = null;
        
        this.windowManager = null;
    },

    createGraphView: function () {
        // can be overriden in child class
        
        return new GraphView(L.extend({
            graphModel: this.graphViewModel,
            graphLegendModel: this.graphLegendViewModel,            
        }, this._viewOptions));
    },

    _notifyViewClosed: function (eventData) {
        this.fire('viewClosed', {
            grpahViewController: this,
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
        this.graphViewModel.title = this.originalGraphModel.title;
    },

    _updateGraphViewModelSeries: function () {
        this.graphViewModel.series = this._filterSeriesByLegend(this.originalGraphModel.series, this.graphLegendViewModel.entries);
    },

    _updateGraphViewModelLegend: function () {
        var legendEntries = this._constructLegendFromSeries(this.originalGraphModel.series);
        this.graphLegendViewModel.merge(legendEntries);
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
