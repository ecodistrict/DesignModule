L.Control.CRD = L.Control.extend({
    options: {
        collapsed: true,
        position: 'bottomright',
        autoZIndex: true,

        current: false,
        reference: false,
        difference: false,
        layer: null
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
        this.current = this._addRadioButton('layerVersion', 'active', this.options.current);
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
        var i = l.appendChild(L.DomUtil.create('input', 'crdRadioButtonInput'));
        i.type = 'radio';
        i.value = aTitle;
        i.name = aGroup;
        i.checked = aChecked;
        i.crd = this;
        i.addEventListener("change", this.clickCrd);

        // makes this work on IE touch devices by stopping it from firing a mouseout event when the touch is released
        button.setAttribute('aria-haspopup', true);
        L.DomEvent.disableClickPropagation(button);
        if (!L.Browser.touch) {
            L.DomEvent.disableScrollPropagation(button);
        }
        
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

    reset: function (aCurrent, aReference, aDifference, aLayer, aActive) {
        this.crdLayer = aLayer ? aLayer.id : null;
        this.options.current = aCurrent;
        this.options.reference = aReference;
        this.options.difference = aDifference;
        this.options.layer = aLayer;
        if (aActive)
            aActive.checked = true;
        else
            this.current.checked = true;
        this._update();
    },

    clickCrd: function (e) {

        e.preventDefault();
        e.stopPropagation();
        // handle switching between current, refference and difference layer
        // todo: only implement for tiles now
        var layer = crd.options.layer;

        if (typeof layer.active !== "undefined" && layer.showing) //check if layer is of the new type!
        {
            var maplayer = layer.maplayer;
            layer.showing.hideLayer();
            switch (e.target) {
                case crd.reference:
                    layer.showing = layer.ref;
                    layer.ref.showLayer(maplayer);
                    break;
                case crd.difference:
                    layer.showing = layer.diff;
                    layer.diff.showLayer(maplayer);
                    break;
                default:
                    layer.showing = layer.active;
                    layer.active.showLayer(maplayer);
                    break;
            }
        }
    }
});

L.control.crd = function (options) {
    return new L.Control.CRD(options);
};