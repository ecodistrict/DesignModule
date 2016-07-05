L.Control.Measures = L.Control.extend({
    options: {
        collapsed: true,
        position: 'topleft',
        autoZIndex: true,
        hideSingleBase: false,
        selectCategories: []
    },

    initialize: function (measuredefinitions, measuresHistory, options) {
        L.setOptions(this, options);
        this._measuredefinitions = measuredefinitions;
        this._measuresLookup = {};
        this._measuresHistory = measuresHistory;
    },

    onAdd: function (map) {
        this._initLayout();
        this._update();

        this._map = map;
        map.on('zoomend', this._checkDisabledMeasures, this);

        return this._container;
    },

    onRemove: function () {
        this._map.off('zoomend', this._checkDisabledMeasures, this);
    },

    _initLayout: function () {
        var className = 'leaflet-control-measures',
            container = this._container = L.DomUtil.create('div', className);

        // makes this work on IE touch devices by stopping it from firing a mouseout event when the touch is released
        container.setAttribute('aria-haspopup', true);

        L.DomEvent.disableClickPropagation(container);
        if (!L.Browser.touch) {
            L.DomEvent.disableScrollPropagation(container);
        }

        var form = this._form = L.DomUtil.create('form', className + '-list');

        if (this.options.collapsed) {
            if (!L.Browser.android) {
                L.DomEvent.on(container, {
                    mouseenter: this._expand,
                    mouseleave: this._collapse
                }, this);
            }

            var link = this._layersLink = L.DomUtil.create('a', className + '-toggle', container);
            link.href = '#';
            link.title = 'Apply measures to selected objects';

            if (L.Browser.touch) {
                L.DomEvent
                    .on(link, 'click', L.DomEvent.stop)
                    .on(link, 'click', this._expand, this);
            } else {
                L.DomEvent.on(link, 'focus', this._expand, this);
            }

            // work around for Firefox Android issue https://github.com/Leaflet/Leaflet/issues/2033
            /*
            L.DomEvent.on(form, 'click', function () {
                setTimeout(L.bind(this._onInputClick, this), 0);
            }, this);
            */
            this._map.on('click', this._collapse, this);
            // TODO keyboard accessibility
        } else {
            this._expand();
        }
        this._catList = L.DomUtil.create('div', className + '-base', form);
        container.appendChild(form);
    },

    _update: function () {
        if (!this._container) { return this; }

        // clear measures lookup
        this._measuresLookup = {};

        var container = this._catList;
        L.DomUtil.empty(container);
        var h = document.createElement('h4');
        h.textContent = 'Measures';
        container.appendChild(h);

        var catList = document.createElement('ul');
        for (var c = 0; c < this._measuredefinitions.length; c++)
        {
            var catDefinition = this._measuredefinitions[c];
            var catLi = document.createElement('li');
            if (catDefinition.description)
                catLi.title = catDefinition.description;
            catLi.textContent = catDefinition.category;
            if (catDefinition.measures.length > 0) {
                var measureList = document.createElement('ul');
                var catDisabled = true;
                for (var m = 0; m < catDefinition.measures.length; m++) {
                    var measureDefinition = catDefinition.measures[m];
                    var measureLi = document.createElement('li');
                    var measureRef = document.createElement('a');
                    measureRef.href = '#';
                    if (measureDefinition.description)
                        measureRef.title = measureDefinition.description;
                    measureRef.textContent = measureDefinition.measure;
                    this._measuresLookup[L.stamp(measureRef)] = measureDefinition;
                    if (L.Browser.touch) {
                        L.DomEvent
                            .on(measureRef, 'click', L.DomEvent.stop)
                            .on(measureRef, 'click', this._chooseMeasureOption, this);
                    } else {
                        L.DomEvent.on(measureRef, 'focus', this._chooseMeasureOption, this);
                    }
                    measureLi.appendChild(measureRef);
                    if (this._checkMeasureEnabled(measureDefinition)) {
                        catDisabled = false;
                        measureLi.className = 'measure-enabled';
                    }
                    else {
                        measureLi.className = 'measure-disabled';
                    }
                    measureList.appendChild(measureLi);
                }
                catLi.appendChild(measureList);
            }
            if (catDisabled) {
                catLi.className = 'measure-category-disabled';
            }
            else {
                catLi.className = 'measure-category-enabled';
            }
            catList.appendChild(catLi);
        }
        catList.className = 'measures-categories';
        container.appendChild(catList);
        return this;
    },

    _selectedObjectTypes: function (aObjectTypes) {
        if (typeof aObjectTypes !== 'string') {
            if (aObjectTypes.length > 0) {
                for (var i = 0; i < this.options.selectCategories.length; i++) {
                    for (var ot = 0; ot < aObjectTypes.length; ot++) {
                        if (this.options.selectCategories[i] === aObjectTypes[ot]) {
                            return true;
                        }
                    }
                }
                return false;
            }
            else
                return true;
        }
        else {
            if (aObjectTypes !== "") {
                for (var i2 = 0; i2 < this.options.selectCategories.length; i2++) {
                    if (aObjectTypes === this.options.selectCategories[i2])
                        return true;
                }
                return false;
            }
            else
                return true;
        }
    },

    _checkMeasureEnabled: function (def) {
        if (def.actions) {
            for (var a = 0; a < def.actions.length; a++)
                if (this._selectedObjectTypes(def.actions[a].objecttypes))
                    return true;
        }
        return false;
    },

    _chooseMeasureOption: function (e) {
        var measureDefinition = this._measuresLookup[L.stamp(e.target)];
        if (measureDefinition) {
            // build modal dialog for possible actions
            var div = modalDialogCreate(measureDefinition.measure, 
                this.options.selectCategories.length === 0 ? 
                    'Select measure to apply' : 
                    'Select measure to apply to objects of type: ' + this.options.selectCategories);
            div.style.width = '450px';
            // build dialog form
            var f = div.appendChild(document.createElement('form'));
            f.id = 'selectMeasureForm';
            for (var a in measureDefinition.actions) 
                this._addMeasureLine(f, measureDefinition.actions[a]);
            f.appendChild(document.createElement('br'));
            f.appendChild(document.createElement('hr'));
            var mddb = f.appendChild(document.createElement('div'));
            mddb.className = 'modalDialogDevideButtons';
            var _this = this;
            modelDialogAddButton(mddb, 'Apply', function () {
                // todo: add selected measure to history
                var selectedRadio = document.querySelector('input[name=measureOption]:checked');
                if (selectedRadio) {
                    _this._measuresHistory.addMeasure(
                        {
                            id: selectedRadio.value,
                            name: selectedRadio.action,
                            description: selectedRadio.title,
                            measure: measureDefinition.measure
                        },
                        getSelectedObjects(),
                        _this.options.selectCategories
                    );
                    _this._collapse();
                    modalDialogClose();
                }
                //else
                //    alert('Select an option before applying or press cancel to abort');
            });
            modelDialogAddButton(mddb, 'Cancel', modalDialogClose);
        }
    },

    _addMeasureLine: function(aForm, aAction) {
        if (this._selectedObjectTypes(aAction.objecttypes)) {
            var rb = aForm.appendChild(document.createElement('input'));
            rb.className = 'selectMeasureRadio';
            rb.id = aAction.id;
            rb.type = 'radio';
            rb.name = 'measureOption';
            rb.value = aAction.id;
            rb.action = aAction.action;
            rb.title = aAction.description;
            var label = aForm.appendChild(document.createElement('label'));
            label.className = 'selectMeasureLabel';
            label.appendChild(document.createTextNode(aAction.action));
            label.htmlFor = aAction.id;
            label.title = aAction.description;
        }
    },

    _expand: function () {
        if (this._measuredefinitions.length > 0) {
            L.DomUtil.addClass(this._container, 'leaflet-control-measures-expanded');
            this._form.style.height = null;
            var acceptableHeight = this._map._size.y - (this._container.offsetTop + 50);
            if (acceptableHeight < this._form.clientHeight) {
                L.DomUtil.addClass(this._form, 'leaflet-control-measures-scrollbar');
                this._form.style.height = acceptableHeight + 'px';
            } else {
                L.DomUtil.removeClass(this._form, 'leaflet-control-measures-scrollbar');
            }
            this._checkDisabledMeasures();
        }
    },

    _collapse: function () {
        L.DomUtil.removeClass(this._container, 'leaflet-control-measures-expanded');
    },

    setSelectCategories: function(selectCategories) {
        this.options.selectCategories = selectCategories;
        this._checkDisabledMeasures();
        return selectCategories;
    },

    _checkDisabledMeasures: function () {
        var containers = this._catList.getElementsByClassName('measures-categories');
        for (var co = 0; co < containers.length; co++) {
            for (var ca = 0; ca < containers[co].childNodes.length; ca++) {
                var catDisabled = true;
                var catLi = containers[co].childNodes[ca];
                var measureContainers = catLi.getElementsByTagName('ul');
                for (var mc = 0; mc < measureContainers.length; mc++) {
                    for (m = 0; m < measureContainers[mc].childNodes.length; m++) {
                        var measureLi = measureContainers[mc].childNodes[m];
                        var refs = measureLi.getElementsByTagName('a');
                        for (var r = 0; r < refs.length; r++) {
                            var measureRef = refs[r];
                            var measure = this._measuresLookup[L.stamp(measureRef)];
                            if (measure) {
                                if (this._checkMeasureEnabled(measure)) {
                                    measureLi.className = 'measure-enabled';
                                    catDisabled = false;
                                }
                                else {
                                    measureLi.className = 'measure-disabled';
                                }
                            }
                        }
                    }
                }
                if (catDisabled) {
                    catLi.className = 'measure-category-disabled';
                }
                else {
                    catLi.className = 'measure-category-enabled';
                }
            }
        }
    },

    resetMeasures: function (measuredefinitions) {
        this._measuredefinitions = measuredefinitions;
        this._update();
    }
});

L.control.measures = function (measuredefinitions, measuresHistory, options) {
    return new L.Control.Measures(measuredefinitions, measuresHistory, options);
};
