/**
 * GraphLegendView represents a legend attached to a graph.
 */

var GraphLegendView = L.Evented.extend({

    initialize: function (opts) {
        if (!opts.element) throw new Error('element is not provided');
        if (!opts.model) throw new Error('model is not provided');

        this.element = opts.element;
        this.model = opts.model;

        this.render();
    },   

    render: function () {        
        L.DomEvent.disableClickPropagation(this.element);

        this._redraw();
        this._adjustVisibility();

        this.model.on('entries', this._redraw, this);
        this.model.on('entries', this._adjustVisibility, this);
    },

    remove: function () {
        this.model.off('entries', this._redraw, this);
        this.model.off('entries', this._adjustVisibility, this);

        while (this.element.firstChild) {
            this.element.removeChild(this.element.firstChild);
        }        
    },

    show: function () {
        L.DomUtil.removeClass(this.element, 'hidden');
    },

    hide: function () {
        L.DomUtil.addClass(this.element, 'hidden');        
    },

    _redraw: function () {
        var entries = d3.select(this.element).selectAll('div.graph-legend-entry')
            .data(this.model.entries);

        entries.exit().remove();

        var newEntry = entries.enter().append('div')
            .attr('class', 'graph-legend-entry')
            .on('click', this._notifyEntryClicked.bind(this));

        newEntry.append('div')
            .attr('class', 'graph-legend-entry-marker')
            .style('border-color', function (d) { return d.color; })
            .style('background-color', entryMarkerColor);

        newEntry.append('span')
            .attr('class', 'graph-legend-entry-text')
            .text(function (d) { return d.title; });

        entries.merge(newEntry).select('.graph-legend-entry-marker')
            .style('border-color', function (d) { return d.color; })
            .style('background-color', entryMarkerColor);

        entries.merge(newEntry).select('.graph-legend-entry-text')
            .text(function (d) { return d.title; });

        function entryMarkerColor(d) { 
            return d.enabled ? d.color : 'rgba(0, 0, 0, 0)';
        }
    },

    _adjustVisibility: function () {
        if (this.model.entries.length > 1) {
            this.show();
        } else {
            this.hide();
        }
    },

    _notifyEntryClicked: function (entry) {
        this.fire('entryClicked', { entry: entry });
    }

});
