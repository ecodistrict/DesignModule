// http://jsfiddle.net/HF57g/2/

// D3 timeslider, aParent is div
// time slider

function addTimeSlider(aParent, options) {

  // svg.select('g').remove();
  d3.select(timeslider).select('svg').remove();

  var yMargin = 8;
  var xMargin = 32;
  var xSelTextOffset = 8;
  var ySelTextOffsetBottom = 6;

  var selectedDateTimeFormat = d3.time.format('%Y-%m-%d %H:%M');

  var rectParent = aParent.getBoundingClientRect();

  var svg = d3.select(aParent).append('svg');
  svg.attr("width", "100%").attr("height","48px").style("position","absolute").style("bottom","0px");

  var range = function () {
    return [xMargin, rectParent.width - 1 - xMargin];
  };

  var scale = d3.time.scale()
  .domain([options.startRange, options.endRange])
  .range([xMargin, rectParent.width - 1 - xMargin]);

  var xaxis = d3.svg.axis()
  .scale(scale)
  .orient('bottom');

  aParent.sendSelectedTime = function (aSelectedTimeText) {

    var selectedTimeCommand = {
      selectedTime: aSelectedTimeText
    };
    wsSend(selectedTimeCommand);
  };

  // todo: set selectedTime by moving scale
  // todo: implement

  function rescale() {
    svg.select('g').call(xaxis).selectAll('text').style('font-size', '10px').style('pointer-events', 'none');
    var selectedTimeTime = scale.invert(selectedTime.node().getAttribute('x') - xSelTextOffset);
    var selectedTimeText = selectedDateTimeFormat(selectedTimeTime);
    selectedTime.text(selectedTimeText).style('cursor','pointer');
    aParent.sendSelectedTime(selectedTimeText);
  }

  // zoom handler (scroll wheel)
  var zoom = d3.behavior.zoom().on('zoom', rescale).x(scale); //.scaleExtent([options.minScale, options.maxScale]); todo: does not work after resize!

  // zoom handler rect
  var rect = svg.append('rect')
  .attr('x', 0)
  .attr('y', 0)
  .attr('width', rectParent.width)
  .attr('height', rectParent.height)
  .attr('opacity', 0)
  .style('cursor','col-resize')
  .call(zoom);

  svg.append('g')
  .attr('class', 'xaxis')
  .attr('transform', 'translate(0,'+yMargin+')')
  .call(xaxis)
  .selectAll('text')
  .style('pointer-events', 'none')
  .style('font-size', '10px');

  var line = svg.append('line')
  .classed("timeSliderLine", true)
  .attr('x1', rectParent.width / 2)
  .attr('x2', rectParent.width / 2)
  .attr('y1', yMargin)
  .attr('y2', rectParent.height)
  .style('pointer-events', 'auto')
  .style('stroke', 'rgb(6,120,255)')
  .style('stroke-width', '4');


  var selectedTime = svg.append('text')
  .attr('x', xSelTextOffset + rectParent.width / 2)
  .attr('y', rectParent.height - ySelTextOffsetBottom)
  .attr('text-anchor', 'start')
  .style('pointer-events', 'auto') // todo: remove and add popup to select date/time specifically
  .style('font-size', '14px')
  .on('click', function (e) {
    console.log(this.innerHTML);
    d3.select('.timeslider').style("height","85px");
  });

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
    rect.attr('width', rectParent.width);
    zoom.x(scale); // rebind, zoom makes copy! http://stackoverflow.com/questions/27204907/d3-reset-zoom-when-chart-is-brushed
    line
    .attr('x1', rectParent.width / 2)
    .attr('x2', rectParent.width / 2);
    selectedTime.attr('x', xSelTextOffset + rectParent.width / 2);
    rescale();
  };

  aParent.changeTimeSliderDateTo = function(date) {

    var diff = (scale.domain()[1] - scale.domain()[0]) / 2;

    if (!minScale) {
      var minScale = 0.5;
      // 0.5
    }

    if (!maxScale) {
      var maxScale = 500;
      // 500
    }

    var isDate = function(date) {
      return ( (new Date(date) !== "Invalid Date" && !isNaN(new Date(date)) ));
    }


    // day = date.day;
    // month = date.month;
    // year = date.year;
    // hours = date.hours;
    // minutes = date.minutes;
    if (isDate(date)) {
      var tsStart = new Date(date);
      tsStart.setTime(tsStart.getTime() - diff);

      var tsEnd = new Date(date);
      tsEnd.setTime(tsEnd.getTime() + diff);

      d3.select('.timeslider').style("height","48px");
      addTimeSlider(timeslider, { startRange: tsStart, endRange: tsEnd, minScale: minScale, maxScale: maxScale });
    } else {
      alert('not a valid Date');
    }

  }



  // resize handler
  window.addEventListener('resize', resize);

  aParent.resetTimeSlider = function () {
    resize();
  };

  var editTimeSlider = L.DomUtil.create('div', 'timeslider-editor');
  var dateInput = L.DomUtil.create('input','dateInput');

  var dateButton = L.DomUtil.create('button','dateButton');
  var closeButton = L.DomUtil.create('button','closeButton');
  closeButton.onclick = function () {
    d3.select('.timeslider').style("height","48px");
  }
  dateButton.onclick = (function() {

    aParent.changeTimeSliderDateTo(d3.select('.dateInput').node().value);
    timeslider.resetTimeSlider();
  }).bind(scale);
  closeButton.innerHTML = 'X';
  dateButton.innerHTML = 'Change date';
  dateButton.classList.add('submitDate');
  editTimeSlider.appendChild(dateInput);
  editTimeSlider.appendChild(dateButton);
  editTimeSlider.appendChild(closeButton);

  aParent.appendChild(editTimeSlider);

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

    var tsStart = new Date();
    tsStart.setTime(tsStart.getTime() - 86400000);
    var tsEnd = new Date();
    tsEnd.setTime(tsEnd.getTime() + 86400000);
    addTimeSlider(timeslider, { startRange: tsStart, endRange: tsEnd, minScale: 0.5, maxScale: 500 });

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
