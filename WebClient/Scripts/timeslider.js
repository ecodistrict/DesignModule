// time slider

// http://jsfiddle.net/HF57g/2/


// timeslider control

L.Control.TimeSlider = L.Control.extend({

    options: {
        collapsed: true,
        position: 'bottomright',
        autoZIndex: true,
        showSettings: true,
        settingsShowing: false
    },

    buildTimeSlider: function (aTimesliderDiv) {

        var hour = 1000 * 60 * 60;
        var day = hour * 24;
        var tsCurrent = new Date();
        var tsStart = new Date(tsCurrent.getTime() - day);
        var tsEnd = new Date(tsCurrent.getTime() + day);
        var selectedDateTimeFormat = d3.time.format('%Y-%m-%d %H:%M');
        var shownDateTimeFormat = d3.time.format('%Y-%m-%d %H:%M'); // '%H:%M' // to show only time on now-point
        var rectParent = aTimesliderDiv.getBoundingClientRect();
        var range = function () {
            return [aTimesliderDiv.xMargin, rectParent.width - 1 - aTimesliderDiv.xMargin];
        };
        var scale = d3.time.scale()
            .domain([tsStart, tsEnd])
            .range([aTimesliderDiv.xMargin, rectParent.width - 1 - aTimesliderDiv.xMargin]);
        var brush = d3.svg.brush()
            .x(scale)
            .on("brush", brushed);
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
            .on('mousedown', function (e) {
                // on right mouse button
                if (d3.event.button == 2) {
                    // stop default handling
                    d3.event.preventDefault();
                    d3.event.stopImmediatePropagation();
                    //initiate brush
                    var initialBrushHalfWidthInPixels = 20;
                    brush.extent([
                        xaxis.scale().invert(d3.event.offsetX - initialBrushHalfWidthInPixels),
                        xaxis.scale().invert(d3.event.offsetX + initialBrushHalfWidthInPixels)]);
                    brushContainer.call(brush);
                    // remove background rect for brush: interferes with timeslider control and not needed here
                    brushContainer.selectAll("rect.background").remove();
                    signalBrush();
                }
            })
            .on('contextmenu', function () {
                d3.event.preventDefault();
                //d3.event.stopImmediatePropagation();
                //return false;
            }) // stop default handling (show browser context menu)
            .call(zoom);

        // content group that the x-axis works on
        var context = svg.append('g')
            .attr('class', 'xaxis')
            .attr('transform', 'translate(0,' + aTimesliderDiv.yMargin + ')')
            .call(xaxis)
            .selectAll('text')
            .style('pointer-events', 'none')
            .style('font-size', '10px');

        // group to store events in
        var eventContainer = svg.append('g');
        eventContainer.style('cursor', 'crosshair');
        eventContainer.style('stroke-width', '1');
        eventContainer.style('stroke', 'black');

        // tooltip element for showing event info
        var tip = d3.tip()
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

        svg.call(tip);

        // brush to select range
        var brushContainer = svg.append('g')
            .attr("class", "brush")
            .call(brush);
        
        brushContainer.selectAll("rect")
            .attr("height", rectParent.height - 2 - aTimesliderDiv.yMargin)
            .attr("y", aTimesliderDiv.yMargin + 4)
            .attr("height", rectParent.height - 8 - aTimesliderDiv.yMargin)
            .style("opacity", 0.7)
            //.call(zoom)
            .on('mousedown', function (e) {
                // on right mouse button
                if (d3.event.button == 2) {
                    // stop default handling
                    d3.event.preventDefault();
                    d3.event.stopImmediatePropagation();
                    // set the brush to empty => remove from sight
                    brush.clear();
                    brushContainer.call(brush);
                    signalBrush();
                }
            })
            
            .on('wheel.zoom', function (e) {
            })
            
            .on('contextmenu', function () {
                d3.event.preventDefault();
            }); // stop default handling (show browser context menu)

        // remove background rect for brush: interferes with timeslider control and not needed here
        brushContainer.selectAll("rect.background").remove();

        var line = svg.append('line')
            .classed("timeSliderLine", true)
            .attr('x1', rectParent.width / 2)
            .attr('x2', rectParent.width / 2)
            .attr('y1', aTimesliderDiv.yMargin)
            .attr('y2', rectParent.height)
            .style('pointer-events', 'none')
            .style('stroke', 'rgb(6,120,255)')
            .style('stroke-width', '3');
        var selectedTime = svg.append('text')
            .attr('x', aTimesliderDiv.xSelTextOffset + rectParent.width / 2)
            .attr('y', rectParent.height - aTimesliderDiv.ySelTextOffsetBottom)
            .attr('text-anchor', 'start')
            .text(shownDateTimeFormat(tsCurrent))
            .attr('class', 'selectedTime')
            .style('pointer-events', 'auto')
            .style('font-size', '14px');

        selectedTime.on('click', (function () {
            if (this.options.showSettings) {
                this.options.settingsShowing = this.options.settingsShowing ? !this.options.settingsShowing : true;
                this.options.settingsShowing ? this._expand() : L.control.timeslidersettings.clearSettings();
            }
        }).bind(this));

        aTimesliderDiv.getCurrentTime = function () {
            // determine current selected time from position of hairline on axis
            return xaxis.scale().invert(d3.select(aTimesliderDiv).select('.selectedTime').node().getAttribute('x') - aTimesliderDiv.xSelTextOffset);
        };

        aTimesliderDiv.doSetCurrentTime = function (aNewTime, aDelta) {
             // Set time, no event
            // locally store current time
            var currentTime = aTimesliderDiv.getCurrentTime();
            // decode time if necessary
            if (aNewTime) {
                if (typeof aNewTime === "string") {
                    try {
                        aNewTime = selectedDateTimeFormat.parse(aNewTime);
                    }
                    catch (e) {
                        aNewTime = currentTime;
                    }
                }
            }
            else {
                aNewTime = currentTime;
            }
            // calculate local version aNewTime
            var newTime = new Date(aNewTime.getTime() + (aDelta ? aDelta : 0));
            // calculate lcoal delta
            var delta = newTime.getTime() - currentTime.getTime();
            // translate scale of axis
            // todo: is call to getCurrentTime needed? removed for now..
            // var currentTime = aTimesliderDiv.getCurrentTime();
            // get domain of current scale
            var domain = xaxis.scale().domain();
            // calculate new scale
            var scale = d3.time.scale()
                .domain([new Date(domain[0].getTime() + delta), new Date(domain[1].getTime() + delta)])
                .range([aTimesliderDiv.xMargin, rectParent.width - 1 - aTimesliderDiv.xMargin]);
            // set new scale on xaxis
            xaxis.scale(scale);
            // apply new scale to other elements
            zoom.x(scale);
            // recalculate xaxis
            svg.select('g.xaxis').call(xaxis).selectAll('text').style('font-size', '10px').style('pointer-events', 'none');
            // show time in text
            d3.select(aTimesliderDiv).select('.selectedTime').text(shownDateTimeFormat(newTime)).style('cursor', 'pointer');
            // adjust events to new scale
            updateEvents();
            updateBrush();
            return selectedDateTimeFormat(newTime);
        };

        aTimesliderDiv.setCurrentTime = function (aNewTime, aDelta) {
            var selectedTimeText = aTimesliderDiv.doSetCurrentTime(aNewTime, aDelta);
            // send new time to publisher
            wsSend({
                type: "timeslider",
                payload: { selectedTime: selectedTimeText }
            });
        };

        aTimesliderDiv.setEvents = function (aEvents) {
            var events = eventContainer.selectAll("rect.event").data(aEvents.map(function (d) {
                d.start = typeof d.start == "string" ? selectedDateTimeFormat.parse(d.start) : d.start;
                d.end = typeof d.end == "string" ? selectedDateTimeFormat.parse(d.end) : d.end;
                return d;
            }));
            var scale = xaxis.scale();
            events.enter()
                .append("rect")
                .attr("class", "event")
                .attr("x", function (d) { return scale(d.start); })
                .attr("y", 2)
                .attr("width", function (d) { return scale(d.end) - scale(d.start); })
                .attr("height", 4)
                .on('mouseover', tip.show)
                .on('mouseout', tip.hide)
                .on('click', function (d, i, j) { wsSend({ type: "timeslider", payload: { selectedEvent: d } }); })
                .style("fill", function (d) { return d.color; });
            events.transition()
                .attr("x", function (d) { return scale(d.start); })
                .attr("width", function (d) { return scale(d.end) - scale(d.start); })
                .style("fill", function (d) { return d.color; });
            events.exit()
                .remove();
        };

        aTimesliderDiv.addEvents = function (aEvents) {
            var _existing = eventContainer.selectAll("rect.event").data();
            var _new = aEvents.map(function (d) {
                d.start = typeof d.start == "string" ? selectedDateTimeFormat.parse(d.start) : d.start;
                d.end = typeof d.end == "string" ? selectedDateTimeFormat.parse(d.end): d.end;
                return d;
            });
            var events = eventContainer.selectAll("rect.event").data(d3.merge([_existing, _new]));
            var scale = xaxis.scale();
            events.enter()
                .append("rect")
                .attr("class", "event")
                .attr("x", function (d) { return scale(d.start); })
                .attr("y", 2)
                .attr("width", function (d) { return scale(d.end) - scale(d.start); })
                .attr("height", 4)
                .on('mouseover', tip.show)
                .on('mouseout', tip.hide)
                .on('click', function (d, i, j) { wsSend({ type: "timeslider", payload: { selectedEvent: d } }); })
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

            if (payload.setCurrentTime) {
                aTimesliderDiv.doSetCurrentTime(payload.setCurrentTime);
                //doSetCurrentTime
            }

        };

        // add close button
        var close = L.DomUtil.create('div', 'timeslider-close');
        close.innerHTML = '&#x2715;';
        close.onclick = (function (e) {
            this._collapse();
            L.control.timeslidersettings.clearSettings();
        }).bind(this);
        aTimesliderDiv.appendChild(close);

        // handle resize of window
        resize = function () {
            // store current time
            var currentTime = aTimesliderDiv.getCurrentTime();
            // reset slider size to match window size
            rectParent = aTimesliderDiv.getBoundingClientRect();
            // resize axis scale
            xaxis.scale().range([aTimesliderDiv.xMargin, rectParent.width - 1 - aTimesliderDiv.xMargin]);
            rect.attr('width', rectParent.width);
            // move line to show current time
            line
                .attr('x1', rectParent.width / 2)
                .attr('x2', rectParent.width / 2);
            // move text showing current selected time
            selectedTime.attr('x', aTimesliderDiv.xSelTextOffset + rectParent.width / 2);
            // reset time to handle all else
            aTimesliderDiv.setCurrentTime(currentTime);
        };
        // resize handler on window and on control
        window.addEventListener('resize', resize);
        map.on('tsrescale', resize);

        function updateEvents() {
            var scale = xaxis.scale();
            return eventContainer.selectAll("rect.event")
                .attr("x", function (d) { return scale(d.start); })
                .attr("width", function (d) { return scale(d.end) - scale(d.start); });
        }

        function updateBrush() {
            brush.extent(brush.extent());
            brush.x(xaxis.scale());
            brushContainer.call(brush);
            // remove background rect for brush: interferes with timeslider control and not needed here
            brushContainer.selectAll("rect.background").remove();
        }

        function signalBrush() {
            if (!brush.empty()) {
                var _extent = brush.extent();
                _extent[0] = selectedDateTimeFormat(_extent[0]);
                _extent[1] = selectedDateTimeFormat(_extent[1]);
                wsSend({
                    type: "timeslider",
                    payload: { brush: { extent: _extent } }
                });
            }
            else {
                wsSend({
                    type: "timeslider",
                    payload: { brush: { extent: {} } }
                });
            }
        }

        function rescale() {
            // show new axis with changed scale
            svg.select('g.xaxis').call(xaxis).selectAll('text').style('font-size', '10px').style('pointer-events', 'none');
            // show new time as text
            var selectedTimeTime = aTimesliderDiv.getCurrentTime();
            // change style of time show as text
            selectedTime.text(shownDateTimeFormat(selectedTimeTime)).style('cursor', 'pointer');
            // update events to new axis and scale
            updateEvents();
            updateBrush();
            // send new selected time to publisher
            wsSend({
                type: "timeslider",
                payload: { selectedTime: selectedDateTimeFormat(selectedTimeTime) }
            });
        }

        function brushed(e) {
            brush.extent(brush.extent());
            brush.x(xaxis.scale());
            signalBrush();
        }
    },

    initialize: function (aTimesliderDiv, options) {
        L.setOptions(this, options);
        this._timesliderDiv = aTimesliderDiv;
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
        map.fire('tsrescale');
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

L.control.timeslidersettings._createEntryTitle = function (text) {
    this._title = this._div.appendChild(L.DomUtil.create('div', 'settings-title'));
    this._title.innerHTML = text;
    return this._title.innerHTML;
};

L.control.timeslidersettings._createRow = function (elements) {
    var changeDate = [
        { name: "dateInput", node: "input", classes: ["dateInput"], width: 0.75, placeholder: 'now', ondblclick: function (e) { this.value = this.placeholder; } },
        { name: "dateButton", node: "button", classes: ["dateButton", "submitDate"], text: "Go", width: 0.25, onclick: function (e) { timeslider.setCurrentTime(document.getElementsByClassName('dateInput')[0].value, 0); } }
    ];

    var controls = [
        { "name": "fastbackwardButton", "classes": ['fastbackwardButton', 'fastbackward', 'symbolButton'], "node": "button", "text": "&#x23EE;", onclick: function (e) { timeslider.setCurrentTime(timeslider.getCurrentTime(), -day); } },
        { "name": "backwardButton", "classes": ['backwardButton', 'backward', 'symbolButton'], "node": "button", "text": "&#x23f4;", onclick: function (e) { timeslider.setCurrentTime(timeslider.getCurrentTime(), -hour); } },
        { "name": "replayButton", "classes": ['replayButton', 'replay', 'symbolButton'], "node": "button", "text": "&#10226;", onclick: function (e) { timeslider.setCurrentTime(new Date(), 0); } },
        { "name": "forwardButton", "classes": ['forwardButton', 'forward', 'symbolButton'], "node": "button", "text": "&#x23f5;", onclick: function (e) { timeslider.setCurrentTime(timeslider.getCurrentTime(), hour); } },
        { "name": "fastforwardButton", "classes": ['fastforwardButton', 'fastForward', 'symbolButton'], "node": "button", "text": "&#x23ED;", onclick: function (e) { timeslider.setCurrentTime(timeslider.getCurrentTime(), day); } }
    ];

    function buildRow(content, elements, className) {
        var row = document.createElement('div');
        row.className = 'settings-row ' + className;

        for (var i = 0; i < elements.length; i++) {
            var elem = document.createElement(elements[i].node);
            elem.name = elements[i].name;
            if (elements[i].placeholder) {
                if (elements[i].placeholder === 'now') {
                    elem.placeholder = d3.time.format('%Y-%m-%d %H:%M')(new Date());
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
            if (elements[i].onclick) {
                elem.onclick = elements[i].onclick;
                // prevent drag events on parent
                elem.addEventListener('mousedown', function (e) { e.stopPropagation(); });
                elem.addEventListener('touchstart', function (e) { e.stopPropagation(); });
            }
            if (elements[i].ondblclick)
                elem.ondblclick = elements[i].ondblclick;
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

L.control.timeslidersettings.clearSettings = function (aClearPosition, aSettingsLayer) {
    if (typeof aSettingsLayer === "undefined")
        this.settingsLayer = null;
    else
        this.settingsLayer = aSettingsLayer;

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
