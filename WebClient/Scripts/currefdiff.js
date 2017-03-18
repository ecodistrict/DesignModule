L.Control.CRD = L.Control.extend({
    options: {
        collapsed: true,
        position: 'bottomright',
        autoZIndex: true,
        hideSingleBase: false,

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
        var i = l.appendChild(document.createElement('input'));
        i.className = 'crdRadioButtonInput';
        i.type = 'radio';
        i.value = aTitle;
        i.name = aGroup;
        i.checked = aChecked;
        i.crd = this;
        //i.onchange = crd.clickCrd;
        i.addEventListener("change", this.clickCrd);
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
        // handle switching between current, refference and difference layer
        // todo: only implement for tiles now
        var layer = crd.options.layer;

        //if (e.target == crd.current) {
        //    if (layer.tileLayer) {
        //        layer.tileLayer.setUrl(layer.tiles, false);
        //        layer.tileLayer.idShowing = layer.id;
        //        if (layer.legend && legendControl.legendLayer == layer.id)
        //            legendControl.createLegend(layer.legend, layer.id);
        //        else if (legendControl.legendLayer == layer.id)
        //            legendControl.clearLegend(false, layer.id);
        //    }
        //}
        //else if (e.target == crd.reference) {
        //    if (layer.tileLayer) {
        //        layer.tileLayer.setUrl(layer.ref.tiles, false);
        //        layer.tileLayer.idShowing = layer.ref.id;
        //        if (typeof layer.ref != "undefined" && typeof layer.ref.legend != "undefined" && legendControl.legendLayer == layer.id)
        //        {
        //            legendControl.createLegend(layer.ref.legend, layer.id);
        //        }
        //        else if (typeof layer.legend != "undefined" && legendControl.legendLayer == layer.id) {
        //            legendControl.createLegend(layer.legend, layer.id);
        //        }
        //        else if (legendControl.legendLayer == layer.id) {
        //            legendControl.clearLegend(false, layer.id);
        //        }
        //    }
        //}
        //else {
        //    if (layer.tileLayer) {
        //        layer.tileLayer.setUrl(layer.diff.tiles, false);
        //        layer.tileLayer.idShowing = layer.diff.id;
        //        if (layer.diff.legend && legendControl.legendLayer == layer.id)
        //            legendControl.createLegend(layer.diff.legend, layer.id);
        //        else if (legendControl.legendLayer == layer.id)
        //            legendControl.clearLegend(false, layer.id);
        //    }
        //}

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