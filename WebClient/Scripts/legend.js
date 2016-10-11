// create legend control
L.control.legend = L.control();

// handle onAdd and addTo
L.control.legend.onAdd = function (map) {
  // main div
  this._div = L.DomUtil.create('div', 'legend');
  this._div.addEventListener('mousedown', this._startmove);
  this._div.addEventListener('touchstart', this._startmove);
  L.DomEvent.disableClickPropagation(this._div);
  //this.update();
  return this._div;
};

var legend = null;


L.control.legend._moveit = function (e) {


  if (e.type === 'touchmove') {
    if (legend) {
      legend.style.left = (legend._mdx + e.changedTouches[0].clientX)+'px';
      legend.style.top = (legend._mdy + e.changedTouches[0].clientY) + 'px';
      legend.style.bottom = 'auto';
    }
  } else {
    if (legend) {
      legend.style.left = (legend._mdx + e.clientX)+'px';
      legend.style.top = (legend._mdy + e.clientY) + 'px';
      legend.style.bottom = 'auto';
    }
  }

};

L.control.legend._endmove = function (e) {

  window.removeEventListener('mouseup', L.control.legend._endmove, true);
  window.removeEventListener('touchend', L.control.legend._endmove, true);

  window.removeEventListener('mousemove', L.control.legend._moveit, true);
  window.removeEventListener('touchmove', L.control.legend._moveit, true);

  legend = null;
};

L.control.legend._startmove = function (e) {


  legend = e.target;

  while (legend && ! legend.classList.contains('legend'))
  legend = legend.parentNode;
  if (legend) {

    window.addEventListener('mouseup', L.control.legend._endmove, true);
    window.addEventListener('touchend', L.control.legend._endmove, true);

    window.addEventListener('mousemove', L.control.legend._moveit, true);
    window.addEventListener('touchmove', L.control.legend._moveit, true);

    if (typeof(e.clientX) === 'undefined') {
      legend._mdx = legend.offsetLeft - e.changedTouches[0].clientX;
      legend._mdy = legend.offsetTop - e.changedTouches[0].clientY;
    } else {
      legend._mdx = legend.offsetLeft - e.clientX;
      legend._mdy = legend.offsetTop - e.clientY;
    }

  }
};

L.control.legend._checkDisabledLayers = function () {
};

L.control.legend.addTo = function (map) {
  this.remove();
  this._map = map;
  var container = this._container = this.onAdd(map);
  L.DomUtil.addClass(container, 'leaflet-control');
  map._controlContainer.appendChild(container);
  return this;
};

L.control.legend._createEntryText = function (text) {
  var entry = document.createElement('td');
  var entryText = document.createElement('div');
  entryText.className = 'legend-entry-text';
  entryText.innerHTML = text;
  entry.appendChild(entryText);
  return entry;
};

L.control.legend._createEntryText2 = function (text) {
  var entry = document.createElement('td');
  var entryText = document.createElement('div');
  entryText.className = 'legend-entry-text2';
  entryText.innerHTML = text;
  entry.appendChild(entryText);
  return entry;
};

L.control.legend._createEntryKey = function (colors) {
  var entry = document.createElement('td');
  entry.className = 'legend-entry';
  var entryKey = document.createElement('div');
  entryKey.className = 'legend-entry-key';
  //entryKey.style['background-color'] = colors;
  if (colors.fillColor)
  entryKey.style['background-color'] = colors.fillColor;
  if (colors.outlineColor)
  entryKey.style['border-color'] = colors.outlineColor;
  entry.appendChild(entryKey);
  return entry;
};

L.control.legend._createEntryTitle = function (text) {
  var entry = document.createElement('th');
  entry.colSpan = 20;
  var entryText = entry.appendChild(document.createTextNode(text));
  entryText.className = 'legend-entry-text';
  return entry;
};

L.control.legend._createRow = function (elements) {
  var row = document.createElement('tr');
  row.className = 'legend-row';
  for (var i = 0; i < elements.length; i++) {
    row.appendChild(elements[i]);
  }
  return row;
};

function createGradientLegend(definition) {
  var legendDiv = document.createElement('div');
  var title = document.createElement('div');
  title.appendChild(document.createTextNode(definition.title));
  title.className = 'legend-gradient-title';
  var upper = document.createElement('div');
  upper.className = 'legend-gradient-upper';
  // build gradient styles
  var max = definition.gradients[definition.gradients.length - 1].position;
  var min = definition.gradients[0].position;
  var factor = definition.logScale ? 99.99 / (Math.log(max) - Math.log(min)) : 99.99 / (max - min); // make last value just inside pixel
  var offset = definition.logScale ? Math.log(min) : min;
  var clrs = '';
  for (var g = 0; g < definition.gradients.length; g++) {
    if (g > 0)
    clrs += ',';
    var p = ((definition.logScale ? Math.log(definition.gradients[g].position) : definition.gradients[g].position) - offset) * factor;
    clrs += definition.gradients[g].color + ' ' + p + '%';
  }
  var moz = '';
  var webkit = '';
  var def = '';
  var filt = '';
  var style =
  'width: ' + definition.width + ';' +
  'background: ' + definition.gradients[0].color + ';' + /* Old browsers */
  'background: -moz-linear-gradient(left, ' + clrs + ');' + /* FF3.6-15 */
  'background: -webkit-linear-gradient(left, ' + clrs + ');' + /* Chrome10-25,Safari5.1-6 */
  'background: linear-gradient(to right,  ' + clrs + ');' + /* W3C, IE10+, FF16+, Chrome26+, Opera12+, Safari7+ */
  'filter: progid:DXImageTransform.Microsoft.gradient(' +
  'startColorstr=\'' + definition.gradients[0].color + '\', ' +
  'endColorstr=\'' + definition.gradients[definition.gradients.length - 1].color + '\',GradientType=1);'; /* IE6-9 */
  upper.style.cssText = style;
  var middle = document.createElement('div');
  middle.className = 'legend-gradient-middle';
  var lower = document.createElement('div');
  lower.className = 'legend-gradient-lower';
  lower.style.fontSize = definition.tickFontSize;
  lower.style.height = definition.tickFontSize;
  for (var l = 0; l < definition.labels.length; l++) {
    var tick = document.createElement('div');
    tick.className = 'legend-gradient-tick';
    var marker = document.createElement('div');
    marker.className = 'legend-gradient-marker';
    tick.style.left = (((definition.logScale ? Math.log(definition.labels[l].position) : definition.labels[l].position) - offset) * factor) + '%';
    marker.style.left = tick.style.left;
    var desc = definition.labels[l].description;
    if (!desc)
    desc = definition.labels[l].position;
    marker.appendChild(document.createTextNode(desc));
    middle.appendChild(tick);
    lower.appendChild(marker);
  }
  lower.style.height = (parseInt(lower.style.height.replace('px', '')) + 4) + 'px';
  legendDiv.appendChild(title);
  legendDiv.appendChild(upper);
  legendDiv.appendChild(middle);
  legendDiv.appendChild(lower);
  return legendDiv;
}

L.control.legend._createGrid = function (definition) {
  if (definition.title) {
    var row = [];
    row.push(this._createEntryTitle(definition.title));
    this._table.appendChild(this._createRow(row));
  }
  for (var r = 0; r < definition.labels.length; r++) {
    var row2 = [];
    for (var c in definition.labels[r]) {
      row2.push(this._createEntryKey(definition.labels[r][c]));
      row2.push(this._createEntryText(c));
    }
    this._table.appendChild(this._createRow(row2));
  }
};

L.control.legend._createGrid2 = function (definition) {
  if (definition.title) {
    var row = [];
    row.push(this._createEntryTitle(definition.title));
    this._table.appendChild(this._createRow(row));
  }
  for (var r = 0; r < definition.labels.length; r++) {
    var row2 = [];
    for (var c2 in definition.labels[r]) {
      row2.push(this._createEntryKey(definition.labels[r][c2]));
    }
    this._table.appendChild(this._createRow(row2));
    var row3 = [];
    for (var c3 in definition.labels[r]) {
      row3.push(this._createEntryText2(c3));
    }
    this._table.appendChild(this._createRow(row3));
  }
};

L.control.legend._createScale = function (definition) {
  var entry = document.createElement('td');
  entry.className = 'legend-entry';
  var legend = createGradientLegend(definition);
  entry.appendChild(legend);
  return entry;
};

L.control.legend.createLegend = function (definition) {
  if (this._div) {
    this.clearLegend(!this._div.firstChild);
    // table holding legend elements
    this._table = this._div.appendChild(L.DomUtil.create('table', 'legend-table'));
    // close cross right upper
    this._close = this._div.appendChild(L.DomUtil.create('div', 'legend-close'));
    this._close.innerHTML = '&#x2715;';
    this._close.onclick = function (e) {
      this.onclick = null;
      this.parentNode.removeChild(this);
      L.control.legend._div.removeChild(L.control.legend._table);
      // todo: remove control from map?
    };
    if (definition.grid)
    this._createGrid(definition.grid);
    else if (definition.grid2)
    this._createGrid2(definition.grid2);
    else if (definition.scale)
    this._table.appendChild(this._createRow([this._createScale(definition.scale)]));
    // todo: else other legend types
  }
};

L.control.legend.clearLegend = function (aClearPosition) {
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

L.control.legend.update = function (props) {
  /*
  var definition1 = {
  scale: {
  width: '300px',
  title: 'a value that was never seen before',
  logScale: 1,
  tickFontSize: '11px',
  gradients: [
  { color: '#2ECCFA', position: 1 },
  { color: '#F3F781', position: 10 },
  { color: '#FA58D0', position: 100 },
  { color: '#82FA58', position: 1000 }
],
labels: [
{ description: '1', position: 1 },
{ description: 'zo 2', position: 20 },
{ position: 40 },
{ description: '75', position: 75 },
{ description: '150', position: 150 },
{ position: 1000 }
]
}
};

var definition2 = {
grid: [
{
'Wilderness Areas': 'LightSeaGreen',
'Bureau of Land Management, National Monument': 'LightSalmon'
},
{
'Indian Reservation': 'LightYellow',
'Fish and Wildlife Service': 'SteelBlue'
},
{
'National Park Service': 'Sienna',
'National Forests & Grasslands': 'LightGreen'
}
]
};
*/

//if (typeof this.definition !== 'undefined') {
//    this.createLegend(this.definition);
//}
};



if (is_touch_device()) {
  document.addEventListener('touchstart', function(event) {
    this.allowUp = (this.scrollTop > 0);
    this.allowDown = (this.scrollTop < this.scrollHeight - this.clientHeight);
    this.slideBeginY = event.pageY;
  });

  document.addEventListener('touchmove', function(event) {
    var up = (event.pageY > this.slideBeginY);
    var down = (event.pageY < this.slideBeginY);
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
