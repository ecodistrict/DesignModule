// time slider

// http://jsfiddle.net/HF57g/2/


function sendSelectedTime(aSelectedTimeText) {
    wsSend({
        type: "timeslider",
        payload: { selectedTime: aSelectedTimeText }
    });
}

function isValidDate(date) {
    return new Date(date) !== "Invalid Date" && !isNaN(new Date(date));
}

// timeslider control

L.Control.TimeSlider = L.Control.extend({

    options: {
        collapsed: true,
        position: 'bottomright',
        autoZIndex: true,
        hideSingleBase: false,
        showSettings: true,
        settingsShowing: false
    },

    buildTimeSlider: function (aTimesliderDiv) {
        
        var tsStart = new Date();
        tsStart.setTime(tsStart.getTime() - 86400000);
        var tsEnd = new Date();
        tsEnd.setTime(tsEnd.getTime() + 86400000);
        var selectedDateTimeFormat = d3.time.format('%Y-%m-%d %H:%M');
        var rectParent = aTimesliderDiv.getBoundingClientRect();
        var range = function () {
            return [aTimesliderDiv.xMargin, rectParent.width - 1 - aTimesliderDiv.xMargin];
        };
        var scale = d3.time.scale()
            .domain([tsStart, tsEnd])
            .range([aTimesliderDiv.xMargin, rectParent.width - 1 - aTimesliderDiv.xMargin]);
        var xaxis = d3.svg.axis()
            .scale(scale)
            .orient('bottom');
        d3.select(aTimesliderDiv).select('svg').remove();
        var svg = d3.select(aTimesliderDiv).append('svg');
        svg.attr("width", "100%").attr("height", "48px").style("position", "absolute").style("bottom", "0px");
        // zoom handler (scroll wheel)
        var zoom = d3.behavior.zoom().on('zoom', rescale).x(scale);
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
            .attr('transform', 'translate(0,' + aTimesliderDiv.yMargin + ')')
            .call(xaxis)
            .selectAll('text')
            .style('pointer-events', 'none')
            .style('font-size', '10px');
        var line = svg.append('line')
            .classed("timeSliderLine", true)
            .attr('x1', rectParent.width / 2)
            .attr('x2', rectParent.width / 2)
            .attr('y1', aTimesliderDiv.yMargin)
            .attr('y2', rectParent.height)
            .style('pointer-events', 'none')
            .style('stroke', 'rgb(6,120,255)')
            .style('stroke-width', '4');
        var selectedTime = svg.append('text')
            .attr('x', aTimesliderDiv.xSelTextOffset + rectParent.width / 2)
            .attr('y', rectParent.height - aTimesliderDiv.ySelTextOffsetBottom)
            .attr('text-anchor', 'start')
            .attr('class', 'selectedTime')
            .style('pointer-events', 'auto')
            .style('font-size', '14px');
        selectedTime.on('click', (function () {
            if (this.options.showSettings) {
                this.options.settingsShowing = true;
                this._expand();
            }
        }).bind(this));
        rescale();

        aTimesliderDiv.setEvents = function (aEvents) {
            var events = svg.selectAll("rect.item").data(aEvents.map(function (d) {
                var res = {};
                res.start = selectedDateTimeFormat.parse(d.start);
                res.end = selectedDateTimeFormat.parse(d.end); // d3.time.format(selectedDateTimeFormat)
                res.color = d.color;
                return res;
            }));
            events.enter()
                .append("rect")
                .attr("class", "item")
                .attr("x", function (d) { return scale(d.start); })
                .attr("y", 2)
                .attr("width", function (d) { return scale(d.end) - scale(d.start); })
                .attr("height", 4)
                .style('pointer-events', 'none')
                .style("fill", function (d) { return d.color; });
            events.transition()
                .attr("x", function (d) { return scale(d.start); })
                .attr("width", function (d) { return scale(d.end) - scale(d.start); })
                .style("fill", function (d) { return d.color; });
            events.exit()
                .remove();
        };

        aTimesliderDiv.addEvents = function (aEvents) {
            var _existing = svg.selectAll("rect.item").data();
            var _new = aEvents.map(function (d) {
                var res = {};
                res.start = selectedDateTimeFormat.parse(d.start);
                res.end = selectedDateTimeFormat.parse(d.end); // d3.time.format(selectedDateTimeFormat)
                res.color = d.color;
                return res;
            });
            var events = svg.selectAll("rect.item").data(d3.merge([_existing, _new]));
            events.enter()
                .append("rect")
                .attr("class", "item")
                .attr("x", function (d) { return scale(d.start); })
                .attr("y", 2)
                .attr("width", function (d) { return scale(d.end) - scale(d.start); })
                .attr("height", 4)
                .style('pointer-events', 'none')
                .style("fill", function (d) { return d.color; });
            events.transition()
                .attr("x", function (d) { return scale(d.start); })
                .attr("width", function (d) { return scale(d.end) - scale(d.start); })
                .style("fill", function (d) { return d.color; });
            events.exit()
                .remove();
        };

        aTimesliderDiv.HandleEvents = function (payload) {
            if (payload.setEvents) {
                aTimesliderDiv.setEvents(payload.setEvents);
            }
            else if (payload.addEvents) {
                aTimesliderDiv.addEvents(payload.addEvents);
            }
        };

        /*
        flights = [
            { start: "2017-01-01 00:00", end: "2017-01-02 00:00", color: "#FF0000" },
            { start: "2017-01-03 00:00", end: "2017-01-04 00:00", color: "#FF0000" },
            { start: "2017-01-05 00:00", end: "2017-01-06 00:00", color: "#FF0000" },
            { start: "2017-01-07 00:00", end: "2017-01-08 00:00", color: "#FF0000" },
            { start: "2017-01-09 00:00", end: "2017-01-10 00:00", color: "#FF0000" },
            { start: "2017-01-11 00:00", end: "2017-01-12 00:00", color: "#FF0000" },
            { start: "2017-01-13 00:00", end: "2017-01-14 00:00", color: "#FF0000" },
            { start: "2017-01-15 00:00", end: "2017-01-16 00:00", color: "#FF0000" },
            { start: "2017-01-17 00:00", end: "2017-01-18 00:00", color: "#FF0000" },
            { start: "2017-01-31 00:00", end: "2017-02-01 00:00", color: "#FF0000" },
            { start: "2017-02-02 00:00", end: "2017-02-03 00:00", color: "#FF0000" },
            { start: "2017-02-04 00:00", end: "2017-02-05 00:00", color: "#FF0000" },
            { start: "2017-02-06 00:00", end: "2017-02-07 00:00", color: "#FF0000" },
            { start: "2017-02-08 00:00", end: "2017-02-09 00:00", color: "#FF0000" },
            { start: "2017-02-10 00:00", end: "2017-02-11 00:00", color: "#FF0000" },
            { start: "2017-02-12 00:00", end: "2017-02-13 00:00", color: "#FF0000" },
            { start: "2017-02-14 00:00", end: "2017-02-15 00:00", color: "#FF0000" },
            { start: "2017-02-16 00:00", end: "2017-02-17 00:00", color: "#FF0000" },
            { start: "2017-02-18 00:00", end: "2017-02-19 00:00", color: "#FF0000" }
        ];

        aTimesliderDiv.setEvents(flights);

        flights2 = [
            { start: "2017-03-01 00:00", end: "2017-03-02 00:00", color: "#00FF00" },
            { start: "2017-03-03 00:00", end: "2017-03-04 00:00", color: "#00FF00" },
            { start: "2017-03-05 00:00", end: "2017-03-06 00:00", color: "#00FF00" },
            { start: "2017-03-07 00:00", end: "2017-03-08 00:00", color: "#00FF00" },
            { start: "2017-03-09 00:00", end: "2017-03-10 00:00", color: "#00FF00" },
            { start: "2017-03-11 00:00", end: "2017-03-12 00:00", color: "#00FF00" },
            { start: "2017-03-13 00:00", end: "2017-03-14 00:00", color: "#00FF00" },
            { start: "2017-03-15 00:00", end: "2017-03-16 00:00", color: "#00FF00" },
            { start: "2017-03-17 00:00", end: "2017-03-18 00:00", color: "#00FF00" },
            { start: "2017-03-31 00:00", end: "2017-02-01 00:00", color: "#00FF00" }
        ];

        aTimesliderDiv.addEvents(flights2);
        */

        var close = L.DomUtil.create('div', 'timeslider-close');
        close.innerHTML = '&#x2715;';
        close.onclick = (function (e) {
            this._collapse();
            L.control.timeslidersettings.clearSettings();
        }).bind(this);
        aTimesliderDiv.appendChild(close);
        var resize = function () {
            rectParent = aTimesliderDiv.getBoundingClientRect();
            scale.range([aTimesliderDiv.xMargin, rectParent.width - 1 - aTimesliderDiv.xMargin]);
            rect.attr('width', rectParent.width);
            zoom.x(scale); // rebind, zoom makes copy! http://stackoverflow.com/questions/27204907/d3-reset-zoom-when-chart-is-brushed
            aTimesliderDiv.scale = scale;
            line
                .attr('x1', rectParent.width / 2)
                .attr('x2', rectParent.width / 2);
            selectedTime.attr('x', aTimesliderDiv.xSelTextOffset + rectParent.width / 2);
            rescale();
        };
        // resize handler
        window.addEventListener('resize', resize);
        aTimesliderDiv.resetTimeSlider = function () {
            resize();
        };

        aTimesliderDiv.setDatetimeTimeSlider = function (date, difference) {
            if (typeof date === "string") {
                date = selectedDateTimeFormat.parse(date);
            }
            var tsStart = date ? new Date(date) : new Date();
            var tsEnd = date ? new Date(date) : new Date();

            if (difference) {
                tsStart = new Date(tsStart.getTime() + difference);
                tsEnd = new Date(tsEnd.getTime() + difference);
            }

            if (isValidDate(date)) {
                // todo: change ref to timeslider
                var diff = (aTimesliderDiv.scale.domain()[1] - aTimesliderDiv.scale.domain()[0]) / 2;
                var rectParent = aTimesliderDiv.getBoundingClientRect();
                tsStart.setTime(tsStart.getTime() - diff);
                tsEnd.setTime(tsEnd.getTime() + diff);
                var range = function () {
                    return [aTimesliderDiv.xMargin, rectParent.width - 1 - aTimesliderDiv.xMargin];
                };
                var scale = d3.time.scale()
                    .domain([tsStart, tsEnd])
                    .range([aTimesliderDiv.xMargin, rectParent.width - 1 - aTimesliderDiv.xMargin]);
                var xaxis = d3.svg.axis()
                    .scale(scale)
                    .orient('bottom');
                //var selectedDateTimeFormat = d3.time.format('%Y-%m-%d %H:%M');
                d3.select(aTimesliderDiv).select('svg').select('g').call(xaxis).selectAll('text').style('font-size', '10px').style('pointer-events', 'none');
                var selectedTimeTime = scale.invert(d3.select(aTimesliderDiv).select('.selectedTime').node().getAttribute('x') - aTimesliderDiv.xSelTextOffset);
                var selectedTimeText = selectedDateTimeFormat(selectedTimeTime);
                d3.select(aTimesliderDiv).select('.selectedTime').text(selectedTimeText).style('cursor', 'pointer');
                sendSelectedTime(selectedTimeText);
                aTimesliderDiv.scale = scale;
            }
        };

        function updateEvents() {
            return svg.selectAll("rect.item")
                .attr("x", function (d) { return scale(d.start); })
                .attr("width", function (d) { return scale(d.end) - scale(d.start); });
        }

        function rescale(e) {
            svg.select('g').call(xaxis).selectAll('text').style('font-size', '10px').style('pointer-events', 'none');
            var selectedTimeTime = scale.invert(selectedTime.node().getAttribute('x') - aTimesliderDiv.xSelTextOffset);
            var selectedTimeText = selectedDateTimeFormat(selectedTimeTime);
            selectedTime.text(selectedTimeText).style('cursor', 'pointer');
            updateEvents();
            sendSelectedTime(selectedTimeText);
            aTimesliderDiv.scale = scale;
        }
    },

    initialize: function (aTimesliderDiv, options) {
        L.setOptions(this, options);
        this._timesliderDiv = aTimesliderDiv;
        //this._timesliderDiv._settingsControl = this; // link back control on time slider
        this._lastZIndex = 0;
        this._handlingClick = false;
        aTimesliderDiv.yMargin = 8;
        aTimesliderDiv.xMargin = 32;
        aTimesliderDiv.xSelTextOffset = 8;
        aTimesliderDiv.ySelTextOffsetBottom = 6;
        this.buildTimeSlider(aTimesliderDiv);
    },

    onAdd: function (map) {
        this._initLayout();
        this._map = map;
        return this._container;
    },

    onRemove: function () {
        // todo: why is this defined?
    },

    _initLayout: function () {
        var className = 'leaflet-control-timeslider',
        container = this._container = L.DomUtil.create('div', className + ' ' + className + '-expanded'); // initially hidden ie expanded
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
        L.DomUtil.removeClass(this._timesliderDiv, 'leaflet-control-timeslider-expanded');
        if (this.options.showSettings && this.options.settingsShowing) {
            var definition = {};
            definition.grid = {};
            definition.title = 'Settings';
            definition.grid.title = 'Settings';
            definition.settings = [];
            L.control.timeslidersettings.createSettings(definition, 'settings', this);
        }
        // send state back to publisher
        wsSend({
            type: "timeslider",
            payload: { active: true }
        });
    },

    _collapse: function () {
        L.DomUtil.removeClass(this._container, 'leaflet-control-timeslider-expanded');
        L.DomUtil.addClass(this._timesliderDiv, 'leaflet-control-timeslider-expanded');

        wsSend({
            type: "timeslider",
            payload: { active: false }
        });
    }
});

L.control.timeslider = function (aTimesliderDiv, options) {
    return new L.Control.TimeSlider(aTimesliderDiv, options);
};


// timeslider settings

var settings = null;

// create settings control
L.control.timeslidersettings = L.control();

// handle onAdd and addTo
L.control.timeslidersettings.onAdd = function (map) {
    // main div
    this._div = L.DomUtil.create('div', 'settings');
    this._div.addEventListener('mousedown', this._startmove);
    this._div.addEventListener('touchstart', this._startmove);
    L.DomEvent.disableClickPropagation(this._div);
    //this.update();
    return this._div;
};

L.control.timeslidersettings._moveit = function (e) {
    if (e.type === 'touchmove') {
        if (settings) {
            settings.style.left = (settings._mdx + e.changedTouches[0].clientX) + 'px';
            settings.style.top = (settings._mdy + e.changedTouches[0].clientY) + 'px';
            settings.style.bottom = 'auto';
        }
    } else {
        if (settings) {
            settings.style.left = (settings._mdx + e.clientX) + 'px';
            settings.style.top = (settings._mdy + e.clientY) + 'px';
            settings.style.bottom = 'auto';
        }
    }
};

L.control.timeslidersettings._endmove = function (e) {
    window.removeEventListener('mouseup', L.control.timeslidersettings._endmove, true);
    window.removeEventListener('touchend', L.control.timeslidersettings._endmove, true);
    window.removeEventListener('mousemove', L.control.timeslidersettings._moveit, true);
    window.removeEventListener('touchmove', L.control.timeslidersettings._moveit, true);

    settings = null;
};

L.control.timeslidersettings._startmove = function (e) {
    settings = e.target;
    while (settings && !settings.classList.contains('settings'))
        settings = settings.parentNode;
    if (settings) {
        window.addEventListener('mouseup', L.control.timeslidersettings._endmove, true);
        window.addEventListener('touchend', L.control.timeslidersettings._endmove, true);
        window.addEventListener('mousemove', L.control.timeslidersettings._moveit, true);
        window.addEventListener('touchmove', L.control.timeslidersettings._moveit, true);

        if (typeof e.clientX === 'undefined') {
            settings._mdx = settings.offsetLeft - e.changedTouches[0].clientX;
            settings._mdy = settings.offsetTop - e.changedTouches[0].clientY;
        } else {
            settings._mdx = settings.offsetLeft - e.clientX;
            settings._mdy = settings.offsetTop - e.clientY;
        }
    }
};

L.control.timeslidersettings.addTo = function (map) {
    this.remove();
    this._map = map;
    var container = this._container = this.onAdd(map);
    L.DomUtil.addClass(container, 'leaflet-control');
    map._controlContainer.appendChild(container);
    return this;
};

L.control.timeslidersettings._createEntryText = function (text) {
    console.log(text);
    var entry = document.createElement('td');
    var entryText = document.createElement('div');
    entryText.className = 'settings-entry-text';
    entryText.innerHTML = text;
    entry.appendChild(entryText);
    return entry;
};

L.control.timeslidersettings._createEntryTitle = function (text) {
    this._title = this._div.appendChild(L.DomUtil.create('div', 'settings-title'));
    this._title.innerHTML = text;
    return this._title.innerHTML;
};

L.control.timeslidersettings._createRow = function (elements) {
    var changeDate = [
        { name: "dateInput", node: "input", classes: ["dateInput"], width: 0.75, placeholder: 'now' },
        { name: "dateButton", node: "button", classes: ["dateButton", "submitDate"], text: "Go", width: 0.25 }
    ];

    var controls = [
        { "name": "fastbackwardButton", "classes": ['fastbackwardButton', 'fastbackward', 'symbolButton'], "node": "button", "text": "&#x23EE;" },
        { "name": "backwardButton", "classes": ['backwardButton', 'backward', 'symbolButton'], "node": "button", "text": "&#x23f4;" },
        { "name": "replayButton", "classes": ['replayButton', 'replay', 'symbolButton'], "node": "button", "text": "&#10226;" },
        { "name": "forwardButton", "classes": ['forwardButton', 'forward', 'symbolButton'], "node": "button", "text": "&#x23f5;" },
        { "name": "fastforwardButton", "classes": ['fastforwardButton', 'fastForward', 'symbolButton'], "node": "button", "text": "&#x23ED;" }
    ];

    function buildRow(content, elements, className) {
        var row = document.createElement('div');
        row.className = 'settings-row ' + className;

        for (var i = 0; i < elements.length; i++) {
            var elem = document.createElement(elements[i].node);
            elem.name = elements[i].name;
            if (elements[i].placeholder) {
                if (elements[i].placeholder === 'now') {
                    var date = new Date();
                    elem.placeholder = date.getFullYear() + '-' + (date.getMonth() + 1) + '-' + date.getDate() + ' ' + date.getHours() + ':' + date.getMinutes();
                } else {
                    elem.placeholder = elements[i].placeholder;
                }
            }
            if (elements[i].type) {
                elem.type = elements[i].type;
            }
            if (elements[i].width > 0) {
                elem.style.width = (elements[i].width * 100) + '%';
            }
            if (elements[i].classes) {
                if (elements[i].classes.length < 2) {
                    elem.className = elements[i].classes;
                } else {
                    for (var i2 = 0; i2 < elements[i].classes.length; i2++) {
                        elem.classList.add(elements[i].classes[i2]);
                    }
                }
            }
            switch (elements[i].node) {
                case 'input':
                    row.appendChild(elem);
                    break;
                case 'button':
                    elem.innerHTML = elements[i].text;
                    row.appendChild(elem);
                    break;
                default:
            } // eo switch
        } // eo for loop
        content.appendChild(row);
    }
    var minute = 60000;
    var hour = 60 * minute; // ms
    var day = hour * 24;

    buildRow(this._content, changeDate, 'changeDate');
    buildRow(this._content, controls, 'controls');

    // todo: change ref to timeslider to aTimesliderDiv earlier passed

    document.getElementsByClassName('dateInput')[0].ondblclick = function () {
        this.value = this.placeholder; // todo: does not work correctly (why is it here anyway) seems to be missing last char
    };
    document.getElementsByClassName('replayButton')[0].onclick = function () {
        if (!timeslider.classList.contains('leaflet-control-timeslider-expanded')) {
            // timeslider.resetTimeToToday(new Date());
            timeslider.setDatetimeTimeSlider(false);
        }
    };
    document.getElementsByClassName('dateButton')[0].onclick = function () {
        if (!timeslider.classList.contains('leaflet-control-timeslider-expanded')) {
            timeslider.setDatetimeTimeSlider(document.getElementsByClassName('dateInput')[0].value);
        }
    };
    document.getElementsByClassName('forwardButton')[0].onclick = function () {
        timeslider.setDatetimeTimeSlider(new Date(timeslider.scale.domain()[1] - (timeslider.scale.domain()[1] - timeslider.scale.domain()[0]) / 2), hour);
    };
    document.getElementsByClassName('backwardButton')[0].onclick = function () {
        timeslider.setDatetimeTimeSlider(new Date(timeslider.scale.domain()[1] - (timeslider.scale.domain()[1] - timeslider.scale.domain()[0]) / 2), -hour);
    };
    document.getElementsByClassName('fastforwardButton')[0].onclick = function () {
        timeslider.setDatetimeTimeSlider(new Date(timeslider.scale.domain()[1] - (timeslider.scale.domain()[1] - timeslider.scale.domain()[0]) / 2), day);
    };
    document.getElementsByClassName('fastbackwardButton')[0].onclick = function () {
        timeslider.setDatetimeTimeSlider(new Date(timeslider.scale.domain()[1] - (timeslider.scale.domain()[1] - timeslider.scale.domain()[0]) / 2), -day);
    };
};

L.control.timeslidersettings._createGrid = function (definition, settings) {
    if (definition.title) {
        this._createEntryTitle(definition.title);
        this._createRow();
    }
};

L.control.timeslidersettings.createSettings = function (definition, layerid, aTimesliderControl) {
    if (this._div) {
        this.clearSettings(!this._div.firstChild, layerid);
        // content holding settings elements
        this._content = this._div.appendChild(L.DomUtil.create('div', 'settings-content'));
        this._title = this._div.appendChild(L.DomUtil.create('div', 'settings-title'));
        // close cross right upper
        this._close = this._div.appendChild(L.DomUtil.create('div', 'settings-close'));
        this._close.innerHTML = '&#x2715;';
        this._close.onclick = function (e) {
            this.onclick = null;
            console.log(this.parentNode);
            this.parentNode.removeChild(this);
            L.control.timeslidersettings._div.removeChild(L.control.timeslidersettings._content);
            L.control.timeslidersettings._div.removeChild(L.control.timeslidersettings._title);
            // store last state in timeslider
            aTimesliderControl.options.settingsShowing = false;
            // todo: remove control from map?
        };
        if (definition.grid)
            this._createGrid(definition.grid, definition.settings);
        // todo: else other settings types
    }
};

L.control.timeslidersettings.clearSettings = function (aClearPosition, asettingsLayer) {
    if (typeof asettingsLayer === "undefined")
        this.settingsLayer = null;
    else
        this.settingsLayer = asettingsLayer;

    if (this._div) {
        if (aClearPosition) {
            // was closed before: reset position
            this._div.removeAttribute('style');
        }
        // clear previous contents
        while (this._div.firstChild) {
            this._div.removeChild(this._div.firstChild);
        }
    }
};

L.control.timeslidersettings.update = function (props) {
};

map.addControl(L.control.timeslidersettings);

/*
if (is_touch_device tis_touch_device()) {
    document.addEventListener('touchstart', function (event) {
        this.allowUp = this.scrollTop > 0;
        this.allowDown = this.scrollTop < this.scrollHeight - this.clientHeight;
        this.slideBeginY = event.pageY;
    });

    document.addEventListener('touchmove', function (event) {
        var up = event.pageY > this.slideBeginY;
        var down = event.pageY < this.slideBeginY;
        this.slideBeginY = event.pageY;
        if ((up && this.allowUp) || (down && this.allowDown)) {
            event.stopPropagation();
        }
        else {
            event.preventDefault();
        }
    });
}
function is_touch_device() {
    try {
        document.createEvent("TouchEvent");
        return true;
    } catch (e) {
        return false;
    }
}
*/


