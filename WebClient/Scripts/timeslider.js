// http://jsfiddle.net/HF57g/2/

// D3 timeslider, aParent is div
// time slider



sendSelectedTime = function (aSelectedTimeText) {

    var selectedTimeCommand = {
        selectedTime: aSelectedTimeText
    };
    wsSend(selectedTimeCommand);
};


timeslider.setDatetimeTimeSlider = function (date, difference) {

    if (!date) {
        var tsStart = new Date();
        var tsEnd = new Date();
    } else {
        var tsStart = new Date(date);
        var tsEnd = new Date(date);
    }

    if (difference) {
        var tsStart = new Date(tsStart.getTime() + difference);
        var tsEnd = new Date(tsEnd.getTime() + difference);
    }


    var isDate = function (date) {
        return ((new Date(date) !== "Invalid Date" && !isNaN(new Date(date))));
    }



    if (isDate(date)) {

        var diff = (timeslider.scale.domain()[1] - timeslider.scale.domain()[0]) / 2;

        var rectParent = timeslider.getBoundingClientRect();

        tsStart.setTime(tsStart.getTime() - diff);
        tsEnd.setTime(tsEnd.getTime() + diff);


        var range = function () {
            return [timeslider.xMargin, rectParent.width - 1 - timeslider.xMargin];
        };

        var scale = d3.time.scale()
        .domain([tsStart, tsEnd])
        .range([timeslider.xMargin, rectParent.width - 1 - timeslider.xMargin]);

        var xaxis = d3.svg.axis()
        .scale(scale)
        .orient('bottom');

        var selectedDateTimeFormat = d3.time.format('%Y-%m-%d %H:%M');
        d3.select(timeslider).select('svg').select('g').call(xaxis).selectAll('text').style('font-size', '10px').style('pointer-events', 'none');
        var selectedTimeTime = scale.invert(d3.select(timeslider).select('.selectedTime').node().getAttribute('x') - timeslider.xSelTextOffset);
        var selectedTimeText = selectedDateTimeFormat(selectedTimeTime);
        d3.select(timeslider).select('.selectedTime').text(selectedTimeText).style('cursor', 'pointer');
        sendSelectedTime(selectedTimeText);
        timeslider.scale = scale;
    } else {
        if (date !== '') {
            alert('date is not valid!');
        }

    }

}


// leaflet button

L.Control.TimeSlider = L.Control.extend({
    options: {
        collapsed: true,
        position: 'bottomright',
        autoZIndex: true,
        hideSingleBase: false
    },
    buildTimeSlider: function (timeslider) {

        var tsStart = new Date();
        tsStart.setTime(tsStart.getTime() - 86400000);
        var tsEnd = new Date();
        tsEnd.setTime(tsEnd.getTime() + 86400000);
        var selectedDateTimeFormat = d3.time.format('%Y-%m-%d %H:%M');
        var rectParent = timeslider.getBoundingClientRect();
        var range = function () {
            return [timeslider.xMargin, rectParent.width - 1 - timeslider.xMargin];
        };
        var scale = d3.time.scale()
        .domain([tsStart, tsEnd])
        .range([timeslider.xMargin, rectParent.width - 1 - timeslider.xMargin]);

        var xaxis = d3.svg.axis()
        .scale(scale)
        .orient('bottom');



        d3.select(timeslider).select('svg').remove();

        var svg = d3.select(timeslider).append('svg');
        svg.attr("width", "100%").attr("height", "48px").style("position", "absolute").style("bottom", "0px");


        // zoom handler (scroll wheel)
        var zoom = d3.behavior.zoom().on('zoom', rescale).x(scale); //.scaleExtent([options.minScale, options.maxScale]); todo: does not work after resize!

        // zoom handler rect
        var rect = svg.append('rect')
        .attr('x', 0)
        .attr('y', 0)
        .attr('width', rectParent.width)
        .attr('height', rectParent.height)
        .attr('opacity', 0)
        .style('cursor', 'col-resize')
        .call(zoom);

        svg.append('g')
        .attr('class', 'xaxis')
        .attr('transform', 'translate(0,' + timeslider.yMargin + ')')
        .call(xaxis)
        .selectAll('text')
        .style('pointer-events', 'none')
        .style('font-size', '10px');

        var line = svg.append('line')
        .classed("timeSliderLine", true)
        .attr('x1', rectParent.width / 2)
        .attr('x2', rectParent.width / 2)
        .attr('y1', timeslider.yMargin)
        .attr('y2', rectParent.height)
        .style('pointer-events', 'auto')
        .style('stroke', 'rgb(6,120,255)')
        .style('stroke-width', '4');

        var selectedTime = svg.append('text')
        .attr('x', timeslider.xSelTextOffset + rectParent.width / 2)
        .attr('y', rectParent.height - timeslider.ySelTextOffsetBottom)
        .attr('text-anchor', 'start')
        .attr('class', 'selectedTime')
        .style('pointer-events', 'auto')
        .style('font-size', '14px');

        selectedTime.on('click', function () {
            timeslider._control._expand();
        });

        rescale();



        var close = L.DomUtil.create('div', 'timeslider-close');
        close.innerHTML = '&#x2715;';
        close.onclick = function (e) {
            timeslider._control._collapse();
        };

        timeslider.appendChild(close);
        var resize = function () {
            rectParent = timeslider.getBoundingClientRect();
            scale.range([timeslider.xMargin, rectParent.width - 1 - timeslider.xMargin]);
            rect.attr('width', rectParent.width);
            zoom.x(scale); // rebind, zoom makes copy! http://stackoverflow.com/questions/27204907/d3-reset-zoom-when-chart-is-brushed
            timeslider.scale = scale;
            line
            .attr('x1', rectParent.width / 2)
            .attr('x2', rectParent.width / 2);
            selectedTime.attr('x', timeslider.xSelTextOffset + rectParent.width / 2);
            rescale();
        };

        // resize handler
        window.addEventListener('resize', resize);

        timeslider.resetTimeSlider = function () {
            resize();
        };


        function rescale() {
            svg.select('g').call(xaxis).selectAll('text').style('font-size', '10px').style('pointer-events', 'none');
            var selectedTimeTime = scale.invert(selectedTime.node().getAttribute('x') - timeslider.xSelTextOffset);
            var selectedTimeText = selectedDateTimeFormat(selectedTimeTime);
            selectedTime.text(selectedTimeText).style('cursor', 'pointer');
            sendSelectedTime(selectedTimeText);
            timeslider.scale = scale;
        }
    },
    initialize: function (timeslider, options) {
        L.setOptions(this, options);

        this._timeslider = timeslider;
        this._timeslider._control = this; // link back control on time slider
        this._lastZIndex = 0;
        this._handlingClick = false;
        timeslider.yMargin = 8;
        timeslider.xMargin = 32;
        timeslider.xSelTextOffset = 8;
        timeslider.ySelTextOffsetBottom = 6;

        this.buildTimeSlider(timeslider);

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
        container = this._container = L.DomUtil.create('div', className + ' ' + className + '-expanded'); // initially hidden ie expanded

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
        // this._timeslider.resetTimeSlider();
        var definition = {};
        definition.grid = {};
        definition.title = 'Settings';
        definition.grid.title = 'Settings';
        definition.settings = [];
        L.control.timeslidersettings.createSettings(definition, 'settings')
    },

    _collapse: function () {

        L.DomUtil.removeClass(this._container, 'leaflet-control-timeslider-expanded');
        L.DomUtil.addClass(this._timeslider, 'leaflet-control-timeslider-expanded');

    }
});

L.control.timeslider = function (timeslider, options) {
    return new L.Control.TimeSlider(timeslider, options);
};
