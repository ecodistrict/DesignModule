
var ScaleSliderView = L.Control.extend({

    initialize: function (opts) {
        if (!opts) throw new Error('No arguments are provided to the View');
        if (!opts.element) throw new Error('No element is provided to the View');
        if (!opts.model) throw new Error('No model is provided to the View');

        L.setOptions(this, opts.leafletOptions);

        this.element = opts.element;
        this.width = this.element.offsetWidth;
        this.height = this.element.offsetHeight;

        this.model = opts.model;
        this.model.on('value', this._modelValueChanged.bind(this));
        this.model.on('brush', this._modelBrushChanged.bind(this));
        this.model.on('events', this._modelEventsChanged.bind(this));

        this.modelValueDecorator = opts.modelValueDecorator || function (value) { return '' + value; };
        this.modelValueScaleCreator = opts.modelValueScaleCreator || function () { return d3.scaleLinear(); };

        this.padding = L.extend({ left: 20, top: 0, right: 20, bottom: 0 }, opts.padding || {});
        this.xAxisMargin = L.extend({ top: 19, left: 0, right: 0, bottom: 0 }, opts.xAxisMargin || {});

        this._render();
    },

    resize: function () {
        this.width = this.element.offsetWidth;
        this.height = this.element.offsetHeight;

        this._buildInnerSpaceGeometry();
        this._resizeViewport();
        this._resizeUnderlay();
        this._resizeBrush();
        this._redraw();
    },

    setZoomLevel: function (zoomLevel) {
        this.innerSpaceTransform = d3.zoomIdentity.translate(0, 0).scale(zoomLevel);
        this._redraw();
    },

    _render: function () {
        this._buildViewport();
        this._buildInnerSpaceGeometry();
        this._buildInnerSpace();
        this._buildInnerSpaceLayoutSkeleton();
        this._buildUnderlay();
        this._buildZoom();
        this._buildScale();
        this._buildXAxis();
        this._buildPointer();
        this._buildBrush();
        this._buildValueLabel();
        this._buildTooltip();

        this._updateEvents();

        this._registerUserEventsHandlers();
    },    

    _buildViewport: function () {
        this.svgViewport = d3.select(this.element)
            .append('svg')
            .attr('class', 'scale-slider-viewport')
            .attr('width', this.width)
            .attr('height', this.height);

        L.DomEvent.disableClickPropagation(this.svgViewport.node());
    },

    _buildInnerSpaceGeometry: function () {
        this.innerSpaceGeometry = {
            left: this.padding.left,
            top: this.padding.top,
            width: this.width - this.padding.left - this.padding.right,
            height: this.height - this.padding.top - this.padding.bottom
        };
        
        if (this.innerSpaceGeometry.width < 0) this.innerSpaceGeometry.width = 0;
        if (this.innerSpaceGeometry.height < 0) this.innerSpaceGeometry.height = 0;
    },

    _buildInnerSpace: function () {
        this.innerSpace = this.svgViewport.append("g")
            .attr("transform", "translate(" + this.innerSpaceGeometry.left + ", " + this.innerSpaceGeometry.top + ")");
    },

    _buildInnerSpaceLayoutSkeleton: function () {
        this.innerSpace.append("g").attr("class", "axis-container");
        this.innerSpace.append("g").attr("class", "pointer-container");
        this.innerSpace.append("g").attr("class", "underlay-container");
        this.innerSpace.append("g").attr("class", "events-container");
        this.innerSpace.append("g").attr("class", "brush-container");
        this.innerSpace.append("g").attr("class", "value-label-container");
    },

    _buildUnderlay: function () {
        this.innerSpace.select('.underlay-container').append("rect")
            .attr('class', 'underlay')
            .attr('x', 0)
            .attr('y', 0)
            .attr('width', this.innerSpaceGeometry.width)
            .attr('height', this.innerSpaceGeometry.height)
            .attr('opacity', 0);
    },

    _buildZoom: function () {
        this.zoom = d3.zoom()
            .on("zoom", this._zoomed.bind(this));

        this.innerSpace.select('.underlay-container .underlay')
            .call(this.zoom);
        this.innerSpaceTransform = d3.zoomIdentity.translate(0, 0).scale(1);
    },

    _buildScale: function () {
        this.baseScale = this.modelValueScaleCreator(this.model.value)
            .range([0, this.innerSpaceGeometry.width]);
    },

    _buildXAxis: function () {
        this.xAxis = d3.axisBottom(this.baseScale);

        this.innerSpace.select('.axis-container').append("g")
            .attr("class", "axis axis-x")
            .attr("transform", "translate(" + this.xAxisMargin.left + ", " + this.xAxisMargin.top + ")");

        this._updateXAxis();
    },

    _buildBrush: function () {
        var brushTopY = this.xAxisMargin.top + 2;
        var brushHeight = this.innerSpaceGeometry.height > brushTopY ? 
            this.innerSpaceGeometry.height - brushTopY : 0;

        this.brush = d3.brushX()
            .extent([[0, brushTopY], [this.innerSpaceGeometry.width, brushTopY + brushHeight]])
            .on('brush', this._brushed.bind(this));        

        var g = this.innerSpace.select('.brush-container').append("g")
            .attr("class", "brush")
            .call(this.brush);        

        g.selectAll("rect.overlay").remove();
        g.selectAll('.brush rect').on('contextmenu', this._brushMousedownHandler.bind(this));

        this._updateBrush();
    },

    _buildPointer: function () {
        this.innerSpace.select('.pointer-container').append("line")
            .attr('class', 'pointer')
            .attr("x1", this._pointerX())
            .attr("y1", this.xAxisMargin.top + 1)
            .attr("x2", this._pointerX())
            .attr("y2", this.innerSpaceGeometry.height);
    },

    _buildValueLabel: function () {
        var position = this._valueLabelPosition();

        this.valueLabel = this.innerSpace.select('.value-label-container').append('text')
            .attr('class', 'value-label noselect')
            .attr('x', position.x)
            .attr('y', position.y)
            .on('click', this._modelValueLabelClicked.bind(this));

        this._updateValueLabel();
    },

    _buildTooltip: function () {
        this.tip = d3.tip()
            .attr('class', 'd3-tip')
            .style('z-index', 999)
            .offset(function () {
                // default tip is shown on x at middle of rect
                // translate to show tip at mouse x
                // else we risk showing the tip off screen and creating a scrollbar
                var r = this.getBoundingClientRect();
                var m = (r.left + r.right) / 2;
                return [-10, d3.event.clientX - m];
            })
            .html(function (d) { return (d && d.tooltip) ? d.tooltip : ""; });

        this.svgViewport.call(this.tip);
    },    

    _resizeViewport: function () {
        this.svgViewport.attr('width', this.width).attr('height', this.height);
    },

    _resizeUnderlay: function () {
        this.innerSpace.select(".underlay-container .underlay")
            .attr('width', this.innerSpaceGeometry.width)
            .attr('height', this.innerSpaceGeometry.height);
    },

    _resizeBrush: function () {
        // completetly rebuild the brush on resize
        this.innerSpace.select('.brush').remove();
        this._buildBrush();
    },

    _redraw: function () {
        this._buildScale();
        this._updateZoom();
        this._updateXAxis();
        this._updatePointer();
        this._updateBrush();
        this._updateValueLabel();
        this._updateEvents();
    },

    _updateZoom: function () {
        var underlay = this.svgViewport.select('.underlay-container .underlay');

        var valuePosition = this.baseScale(this.model.value);
        this.innerSpaceTransform = d3.zoomIdentity.translate(valuePosition * (1 - this.innerSpaceTransform.k), 0)
            .scale(this.innerSpaceTransform.k);

        this.zoom.transform(underlay, this.innerSpaceTransform);
    },

    _updateXAxis: function () {
        // Create a new scale with the current value (let's call it x) in the center.
        // This scale should be zoomed to current level (k coefficient).
        // After such zooming a position of the current value will be moved according 
        // to the zoom coefficient k.
        // In order to place the current value into the middle of the zoomed scale 
        // we need to shift everything by x*(1 - k). 
        // In such case x will be center of the zoomed scale.    
        var modelValueScale = this.modelValueScaleCreator(this.model.value)
            .range([0, this.innerSpaceGeometry.width]);
        var valuePosition = modelValueScale(this.model.value);
        var transform = d3.zoomIdentity.translate(valuePosition * (1 - this.innerSpaceTransform.k), 0)
            .scale(this.innerSpaceTransform.k);
        var scale = transform.rescaleX(modelValueScale);

        var gXAxis = this.svgViewport.select('.axis-x');
        gXAxis.call(this.xAxis.scale(scale));
        this.svgViewport.selectAll('.axis-x .tick').classed('noselect', true);        
    },

    _updateBrush: function () {
        if (d3.event && d3.event.type === 'brush') return; // if we are already updating the brush then skip.

        var gBrush = this.svgViewport.select('.brush');

        if (this.model.brush) {
            var selection = [this.model.brush.start, this.model.brush.end];
            gBrush.call(this.brush.move, selection.map(this._currentScale()));
        } else {
            gBrush.call(this.brush.move, null);
        }
    },

    _updateValueLabel: function () {
        var position = this._valueLabelPosition();

        this.svgViewport.select('.value-label')
            .attr('x', position.x)
            .attr('y', position.y)
            .text(this.modelValueDecorator(this.model.value));
    },

    _updatePointer: function () {
        this.svgViewport.select('.pointer').remove();
        this._buildPointer();
    },

    _updateEvents: function () {
        var eventsContainer = this.innerSpace.select('.events-container');
        var events = eventsContainer.selectAll("rect.event").data(this.model.events);
        var scale = this._currentScale();

        function constructEventLevelClass(level) {
            if (typeof level !== 'number') return '';

            return 'event-level-' + level;
        }

        events.enter()
            .append("rect")
            .attr("class", function (d) { return "event " + constructEventLevelClass(d.level); })
            .attr("x", function (d) { return scale(d.start); })
            .attr("width", function (d) { return scale(d.end) - scale(d.start); })
            .on('mouseover', this.tip.show)
            .on('mouseout', this.tip.hide)
            .on('click', this._modelEventClicked.bind(this))
            .style("fill", function (d) { return d.color; });
        events.transition()
            .attr("x", function (d) { return scale(d.start); })
            .attr("width", function (d) { return scale(d.end) - scale(d.start); })
            .style("fill", function (d) { return d.color; });
        events.exit()
            .remove();
    },

    _zoomed: function () {
        if (!d3.event.sourceEvent) return; // if zoom change occured not from UI then ignore this event

        this.innerSpaceTransform = d3.event.transform;

        var zoomedScale = this.innerSpaceTransform.rescaleX(this.baseScale);
        this.model.value = zoomedScale.invert(this._pointerX());
    },

    _brushed: function () {
        if (!d3.event.sourceEvent) return; // if brush change occured not from UI then ignore this event

        var currentScale = this._currentScale();
        var selection = d3.event.selection;

        if (selection) {
            var modelSelection = selection.map(currentScale.invert, currentScale);

            this.model.brush = {
                start: modelSelection[0],
                end: modelSelection[1]
            };
        } else {
            this.model.brush = null;
        }
    },

    _modelValueChanged: function (data) {
        if (d3.event && d3.event.type === 'zoom') {
            this._updateXAxis();
            this._updateValueLabel();
            this._updateBrush();
            this._updateEvents();
        } else {
            this._redraw();
        }
    },

    _modelBrushChanged: function () {
        this._updateBrush();
    },

    _modelEventsChanged: function () {
        this._updateEvents();
    },

    _pointerX: function () {
        return this.innerSpaceGeometry.width / 2;
    },

    _valueLabelPosition: function () {
        return {
            x: this.innerSpaceGeometry.width / 2 + 8,
            y: this.innerSpaceGeometry.height - 6
        };
    },

    _currentScale: function () {
        return this.xAxis.scale();
    },

    _registerUserEventsHandlers: function () {
        this.svgViewport.on('contextmenu', function () { d3.event.preventDefault(); });
        this.innerSpace.select('.underlay-container .underlay')
            .on('contextmenu', this._underlayMousedownHandler.bind(this), false);
    },

    _underlayMousedownHandler: function () {        
        // stop default handling
        d3.event.preventDefault();
        d3.event.stopImmediatePropagation();

        var x = d3.event.offsetX - this.innerSpaceGeometry.left;
        var initialBrushHalfWidthInPixels = 20;

        this.model.brush = {
            start: this._currentScale().invert(x - initialBrushHalfWidthInPixels),
            end: this._currentScale().invert(x + initialBrushHalfWidthInPixels)
        };
    },

    _brushMousedownHandler: function () {
        // stop default handling
        d3.event.preventDefault();
        d3.event.stopImmediatePropagation();

        this.model.brush = null;
    },

    _modelValueLabelClicked: function () {
        this.fire('valueClicked');
    },

    _modelEventClicked: function (d) {
        this.fire('eventSelected', { event: d });
    }

});
L.extend(ScaleSliderView.prototype, L.Evented.prototype);