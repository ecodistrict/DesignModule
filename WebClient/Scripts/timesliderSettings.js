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

var settings = null;

L.control.timeslidersettings._moveit = function (e) {
  if (e.type === 'touchmove') {
    if (settings) {
      settings.style.left = (settings._mdx + e.changedTouches[0].clientX)+'px';
      settings.style.top = (settings._mdy + e.changedTouches[0].clientY) + 'px';
      settings.style.bottom = 'auto';
    }
  } else {
    if (settings) {
      settings.style.left = (settings._mdx + e.clientX)+'px';
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

  while (settings && ! settings.classList.contains('settings'))
  settings = settings.parentNode;
  if (settings) {

    window.addEventListener('mouseup', L.control.timeslidersettings._endmove, true);
    window.addEventListener('touchend', L.control.timeslidersettings._endmove, true);

    window.addEventListener('mousemove', L.control.timeslidersettings._moveit, true);
    window.addEventListener('touchmove', L.control.timeslidersettings._moveit, true);

    if (typeof(e.clientX) === 'undefined') {
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

L.control.timeslidersettings._createRow = function (elements, aparent) {

  var changeDate = [
    {name:"dateInput", node: "input", classes: ["dateInput"], width: 0.75, placeholder: 'now'},
    {name:"dateButton", node: "button", classes: ["dateButton","submitDate"], text: "Go", width: 0.25}
  ];

  var controls = [
    {"name":"fastbackwardButton","classes":['fastbackwardButton','fastbackward','symbolButton'], "node":"button", "text":"&#x23EE;"},
    {"name":"backwardButton","classes":['backwardButton','backward','symbolButton'], "node":"button", "text":"&#x23f4;"},
    {"name":"replayButton","classes":['replayButton','replay','symbolButton'], "node":"button", "text":"&#10226;"},
    {"name":"forwardButton","classes":['forwardButton','forward','symbolButton'], "node":"button", "text":"&#x23f5;"},
    {"name":"fastforwardButton","classes":['fastforwardButton','fastForward','symbolButton'], "node":"button", "text":"&#x23ED;"}
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
          elem.placeholder = date.getFullYear()+'-'+(date.getMonth() +1)+'-'+date.getDate()+' '+(date.getHours()) +':'+ date.getMinutes();
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
          for (var i2=0;i2 < elements[i].classes.length; i2++) {
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

  document.getElementsByClassName('dateInput')[0].ondblclick = function () {
    this.value = this.placeholder;
  }

  document.getElementsByClassName('replayButton')[0].onclick = function() {
if (!timeslider.classList.contains('leaflet-control-timeslider-expanded')) {
    // timeslider.resetTimeToToday(new Date());
    timeslider.setDatetimeTimeSlider(false);
    }
  }
  document.getElementsByClassName('dateButton')[0].onclick = function() {
    if (!timeslider.classList.contains('leaflet-control-timeslider-expanded')) {
      // timeslider.changeTimeSliderDateTo(document.getElementsByClassName('dateInput')[0].value);
      timeslider.setDatetimeTimeSlider(document.getElementsByClassName('dateInput')[0].value);
    }
  };

  document.getElementsByClassName('forwardButton')[0].onclick = function() {
    timeslider.setDatetimeTimeSlider(new Date(timeslider.scale.domain()[1] - (timeslider.scale.domain()[1] - timeslider.scale.domain()[0]) / 2),hour);
  }
  document.getElementsByClassName('backwardButton')[0].onclick = function() {
    timeslider.setDatetimeTimeSlider(new Date(timeslider.scale.domain()[1] - (timeslider.scale.domain()[1] - timeslider.scale.domain()[0]) / 2),-hour);
  }
  document.getElementsByClassName('fastforwardButton')[0].onclick = function() {
    timeslider.setDatetimeTimeSlider(new Date(timeslider.scale.domain()[1] - (timeslider.scale.domain()[1] - timeslider.scale.domain()[0]) / 2),day);
  }
  document.getElementsByClassName('fastbackwardButton')[0].onclick = function() {
    timeslider.setDatetimeTimeSlider(new Date(timeslider.scale.domain()[1] - (timeslider.scale.domain()[1] - timeslider.scale.domain()[0]) / 2),-day);
  }

};

L.control.timeslidersettings._createGrid = function (definition, settings) {

  if (definition.title) {
    this._createEntryTitle(definition.title);
    this._createRow();
  }

};


L.control.timeslidersettings.createSettings = function (definition, layerid) {

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
      // todo: remove control from map?
    };
    if (definition.grid)
    this._createGrid(definition.grid, definition.settings);

    // todo: else other settings types
  }
};

L.control.timeslidersettings.clearSettings = function (aClearPosition, asettingsLayer) {
  if (typeof asettingsLayer == "undefined")
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

var definition = {};
definition.grid = {};
definition.title = 'Settings';
definition.grid.title = 'Settings';
definition.settings = [];




map.addControl(L.control.timeslidersettings);
