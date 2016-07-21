L.Control.CRD = L.Control.extend({
    options: {
        collapsed: true,
        position: 'bottomright',
        autoZIndex: true,
        hideSingleBase: false,

        current: false,
        reference: false,
        difference: false
    },

    initialize: function (options) {
        // options.parent must be set
        L.setOptions(this, options);
    },

    onAdd: function (map) {
        this._initLayout();
        this._update();
        this._map = map;
        return this._container;
    },

    onRemove: function () {
    },

    _initLayout: function () {
        this._container = this.options.parent;
        this.form = this._container.appendChild(document.createElement('form'));
        this.current = this._addRadioButton('layerVersion', 'current', this.options.current);
        this.reference = this._addRadioButton('layerVersion', 'reference', this.options.current.reference);
        this.difference = this._addRadioButton('layerVersion', 'difference', this.options.difference);
        if (this.options.collapsed)
            this._collapse();
        else
            this._expand();
    },

    _addRadioButton: function (aGroup, aTitle, aChecked) {
        var button = this.form.appendChild(document.createElement('div'));
        button.className = 'crdRadioButton';
        var l = button.appendChild(document.createElement('label'));
        var i = l.appendChild(document.createElement('input'));
        i.className = 'crdRadioButtonInput';
        i.type = 'radio';
        i.value = aTitle;
        i.name = aGroup;
        i.checked = aChecked;
        i.crd = this;
        if (this.options.onclick)
            i.onchange = this.options.onclick;
        var s = l.appendChild(document.createElement('span'));
        s.className = 'crdRadioButtonSpan';
        s.textContent = aTitle;
        return i
    },

    _expand: function () {
        this._setVisibility(this._container, true);
    },

    _collapse: function () {
        this._setVisibility(this._container, false);
    },

    _update: function () {
        this.options.collapsed = this.options.current + this.options.reference + this.options.difference < 2;
        if (this.options.collapsed)
            this._collapse();
        else {
            this._setVisibility(this.current.parentElement.parentElement, this.options.current, this.current);
            this._setVisibility(this.reference.parentElement.parentElement, this.options.reference, this.reference);
            this._setVisibility(this.difference.parentElement.parentElement, this.options.difference, this.difference);
            this._expand();
        }
    },

    _setVisibility: function (aElement, aVisible, aInput) {
        if (aVisible) {
            aElement.style.removeProperty('display'); // restore original display style
            if (aInput)
                aInput.onchange = this.options.onclick;
        }
        else {
            aElement.style.display = 'none';
            if (aInput)
                aInput.onchange = undefined;
        }
    },

    reset: function (aCurrent, aReference, aDifference, aOnClick, aActive) {
        this.options.current = aCurrent;
        this.options.reference = aReference;
        this.options.difference = aDifference;
        this.options.onclick = aOnClick;
        if (aActive)
            aActive.checked = true;
        else
            this.current.checked = true;
        this._update();
    }
    /*
    setEnabled: function (aCurrent, aReference, aDifference) {
        if (aCurrent + aReference + aDifference > 1) {
            setVisibility(this._container, true);
            setVisibility(this.current.parentElement.parentElement, aCurrent);
            setVisibility(this.reference.parentElement.parentElement, aReference);
            setVisibility(this.difference.parentElement.parentElement, aDifference);
        }
        else
            setVisibility(this._container, false);
    },

    getEnabled: function () {
        return {
            current: this.current.parentElement.parentElement.style.display != 'none',
            reference: this.reference.parentElement.parentElement.style.display != 'none',
            difference: this.difference.parentElement.parentElement.style.display != 'none'
        }
    },

    setSelected: function (aCurrent, aReference, aDifference) {
        this.current.checked = aCurrent;
        this.reference.checked = aReference;
        this.difference.checked = aDifference;
    },

    getSelected: function () {
        return {
            current: this.current.checked,
            reference: this.reference.checked,
            difference: this.difference.checked
        }
    }
    */
});

L.control.crd = function (options) {
    return new L.Control.CRD(options);
};