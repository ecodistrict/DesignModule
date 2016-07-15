function cpdAddRadioButton(aControl, aGroup, aTitle, aChecked, aOnClick) {
    var button = aControl.form.appendChild(document.createElement('div'));
    button.className = 'cpdRadioButton';
    var l = button.appendChild(document.createElement('label'));
    var i = l.appendChild(document.createElement('input'));
    i.className = 'cpdRadioButtonInput';
    i.type = 'radio';
    i.value = aTitle;
    i.name = aGroup;
    i.checked = aChecked;
    i.cpd = aControl;
    if (aOnClick) {
        i.onclick = aOnClick;
        //s.onclick = aOnClick;
    }
    var s = l.appendChild(document.createElement('span'));
    s.className = 'cpdRadioButtonSpan';
    s.textContent = aTitle;
    return i
}

function cpdCreateControl(aParent, aOnClick) {
    var control = {};
    control.div = aParent;
    control.form = control.div.appendChild(document.createElement('form'));
    control.current = cpdAddRadioButton(control, 'layerVersion', 'current', true, aOnClick);
    control.previous = cpdAddRadioButton(control, 'layerVersion', 'previous', false, aOnClick);
    control.difference = cpdAddRadioButton(control, 'layerVersion', 'difference', false, aOnClick);
    return control;
}

function cpdSetMode(aControl, aCurrent, aPrevious, aDifference) {
    if (aCurrent + aPrevious + aDifference > 1) {
        // show
        aControl.div.style.removeProperty('display');
    }
    else {
        // none -> hide
        aControl.div.style.display = 'none';
    }
}

/*
L.Control.CurPrevDiff = L.Control.extend({
    options: {
        collapsed: true,
        position: 'bottomright',
        autoZIndex: true,
        hideSingleBase: false
    },

    initialize: function (control, options) {
        L.setOptions(this, options);

        this._control = control; // link definition of control
        this._control._control = this; // link back cur-prev-diff control on definition on control
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
        var className = 'leaflet-control-curprevdiff',
            container = this._container = L.DomUtil.create('div', className + ' ' + className + '-expanded'); // initially hidden ie expanded

        // makes this work on IE touch devices by stopping it from firing a mouseout event when the touch is released
        container.setAttribute('aria-haspopup', true);

        L.DomEvent.disableClickPropagation(container);
        if (!L.Browser.touch) {
            L.DomEvent.disableScrollPropagation(container);
        }

        var form = this._form = L.DomUtil.create('form', className + '-list');

        // add controls to form


        L.DomEvent
            .on(container, 'click', L.DomEvent.stop)
            .on(container, 'click', this._expand, this);

        var link = this._categoriesLink = L.DomUtil.create('a', className + '-toggle', container);
        link.href = '#';
        link.title = 'current-previous-difference';

        L.DomEvent
            .on(link, 'click', L.DomEvent.stop)
            .on(link, 'click', this._expand, this);

        container.appendChild(form);
    },

    _expand: function () {
        L.DomUtil.addClass(this._container, 'leaflet-control-curprevdiff-expanded');
        L.DomUtil.removeClass(this._control, 'leaflet-control-curprevdiff-expanded');
    },

    _collapse: function () {
        L.DomUtil.removeClass(this._container, 'leaflet-control-curprevdiff-expanded');
        L.DomUtil.addClass(this._control, 'leaflet-control-curprevdiff-expanded');
    }
});

L.control.curprevdiff = function (layer, options) {
    return new L.Control.CurPrevDiff(layer, options);
};
*/