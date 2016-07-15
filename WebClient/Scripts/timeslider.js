// http://jsfiddle.net/HF57g/2/

// D3 timeslider, aParent is div

function addTimeSlider(aParent, options) {
    var yMargin = 8;
    var xMargin = 32;
    var xSelTextOffset = 8;
    var ySelTextOffsetBottom = 6;

    var selectedDateTimeFormat = d3.time.format("%Y-%m-%d %H:%M");

    var rectParent = aParent.getBoundingClientRect();

    var svg = d3.select(aParent).append("svg");

    var range = function () {
        return [xMargin, rectParent.width - 1 - xMargin];
    };

    var scale = d3.time.scale()
                    .domain([options.startRange, options.endRange])
                    .range([xMargin, rectParent.width - 1 - xMargin]);


    var xaxis = d3.svg.axis()
        .scale(scale)
        .orient("bottom");

    aParent.sendSelectedTime = function (aSelectedTimeText) {

        var selectedTimeCommand = {
            selectedTime: aSelectedTimeText
        };
        wsSend(selectedTimeCommand);
    };

    // todo: set selectedTime by moving scale
    // todo: implement

    function rescale() {
        svg.select("g").call(xaxis).selectAll("text").style("font-size", "10px").style("pointer-events", "none");
        var selectedTimeTime = scale.invert(selectedTime.node().getAttribute("x") - xSelTextOffset);
        var selectedTimeText = selectedDateTimeFormat(selectedTimeTime);
        selectedTime.text(selectedTimeText);
        aParent.sendSelectedTime(selectedTimeText);
    }

    // zoom handler (scroll wheel)
    var zoom = d3.behavior.zoom().on("zoom", rescale).x(scale); //.scaleExtent([options.minScale, options.maxScale]); todo: does not work after resize!

    // zoom handler rect
    var rect = svg.append("rect")
                    .attr("x", 0)
                    .attr("y", 0)
                    .attr("width", rectParent.width)
                    .attr("height", rectParent.height)
                    .attr("opacity", 0)
                    .call(zoom);
    
    svg.append("g")
        .attr("class", "xaxis")
        .attr("transform", "translate(0,"+yMargin+")")
        .call(xaxis)
        .selectAll("text")
            .style("pointer-events", "none")
            .style("font-size", "10px");

    var line = svg.append("line")
        .attr("x1", rectParent.width / 2)
        .attr("x2", rectParent.width / 2)
        .attr("y1", yMargin)
        .attr("y2", rectParent.height)
        .style("pointer-events", "none")
        .style("stroke", "rgb(6,120,255)");

    var selectedTime = svg.append("text")
        .attr("x", xSelTextOffset + rectParent.width / 2)
        .attr("y", rectParent.height - ySelTextOffsetBottom)
        .attr("text-anchor", "start")
        .style("pointer-events", "none") // todo: remove and add popup to select date/time specifically
        .style("font-size", "14px");

    rescale();

    var close = L.DomUtil.create('div', 'timeslider-close');
    close.innerHTML = '&#x2715;';
    close.onclick = function (e) {
        aParent._control._collapse();
    };
    aParent.appendChild(close);

    var resize = function () {
        rectParent = aParent.getBoundingClientRect();
        scale.range([xMargin, rectParent.width - 1 - xMargin]);
        rect.attr("width", rectParent.width);
        zoom.x(scale); // rebind, zoom makes copy! http://stackoverflow.com/questions/27204907/d3-reset-zoom-when-chart-is-brushed
        line
            .attr("x1", rectParent.width / 2)
            .attr("x2", rectParent.width / 2);
        selectedTime.attr("x", xSelTextOffset + rectParent.width / 2);
        rescale();
    };

    // resize handler
    window.addEventListener('resize', resize);

    aParent.resetTimeSlider = function () {
        resize();
    };
}

// leaflet button

L.Control.TimeSlider = L.Control.extend({
    options: {
        collapsed: true,
        position: 'bottomright',
        autoZIndex: true,
        hideSingleBase: false
    },

    initialize: function (timeslider, options) {
        L.setOptions(this, options);

        this._timeslider = timeslider;
        this._timeslider._control = this; // link back control on time slider
        this._lastZIndex = 0;
        this._handlingClick = false;
    },

    onAdd: function (map) {
        this._initLayout();
        this._map = map;
        return this._container;
    },

    onRemove: function () {
        
    },

    _initLayout: function () {
        var className = 'leaflet-control-timeslider',
            container = this._container = L.DomUtil.create('div', className+' '+className+'-expanded'); // initially hidden ie expanded

        // makes this work on IE touch devices by stopping it from firing a mouseout event when the touch is released
        container.setAttribute('aria-haspopup', true);

        L.DomEvent.disableClickPropagation(container);
        if (!L.Browser.touch) {
            L.DomEvent.disableScrollPropagation(container);
        }

        var form = this._form = L.DomUtil.create('form', className + '-list');

        L.DomEvent
            .on(container, 'click', L.DomEvent.stop)
            .on(container, 'click', this._expand, this);

        var link = this._categoriesLink = L.DomUtil.create('a', className + '-toggle', container);
        link.href = '#';
        link.title = 'Timeslider';

        L.DomEvent
            .on(link, 'click', L.DomEvent.stop)
            .on(link, 'click', this._expand, this);

        container.appendChild(form);
    },

    _expand: function () {
        L.DomUtil.addClass(this._container, 'leaflet-control-timeslider-expanded');
        L.DomUtil.removeClass(this._timeslider, 'leaflet-control-timeslider-expanded');
        this._timeslider.resetTimeSlider();
    },

    _collapse: function () {
        L.DomUtil.removeClass(this._container, 'leaflet-control-timeslider-expanded');
        L.DomUtil.addClass(this._timeslider, 'leaflet-control-timeslider-expanded');
        this._timeslider.sendSelectedTime('now');
    }
});

L.control.timeslider = function (timeslider, options) {
    return new L.Control.TimeSlider(timeslider, options);
};


