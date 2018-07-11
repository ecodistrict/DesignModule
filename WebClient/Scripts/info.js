L.Control.Info = L.Control.extend({
    options: {
        collapsed: true,
        position: 'bottomleft',
        autoZIndex: true
    },

    initialize: function (info, options) {
        L.setOptions(this, options);

        this._lastZIndex = 0;
        this._handlingClick = false;
    },

    onAdd: function (map) {
        this._initLayout();
        this._update();

        this._map = map;
        map.on('zoomend', this._checkDisabledLayers, this);

        return this._container;
    },

    onRemove: function () {
        this._map.off('zoomend', this._checkDisabledLayers, this);
    },

    _initLayout: function () {
        var className = 'leaflet-control-info',
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

            var link = this._categoriesLink = L.DomUtil.create('a', className + '-toggle', container);
            link.href = '#';
            link.title = 'Info';

            if (L.Browser.touch) {
                L.DomEvent
                .on(link, 'click', L.DomEvent.stop)
                .on(link, 'click', this._expand, this);
            } else {
                L.DomEvent.on(link, 'focus', this._expand, this);
            }

            this._map.on('click', this._collapse, this);
            // TODO: keyboard accessibility
        } else {
            this._expand();
        }

        this._detailList = L.DomUtil.create('div', className + '-base', form);

        container.appendChild(form);
    },

    _update: function () {
        if (!this._container) { return this; }

        var container = this._detailList;
        L.DomUtil.empty(container);
        var h = document.createElement('h3');
        h.style.padding = '10px';
        h.style.margin = '0px';
        container.appendChild(h);

        // hack for demo

        if (session == '4B95BE74F9A44DA0908A30B27C3E8C99') {
            h.textContent = 'Deze ‘Client’ versie van Urban Strategy brengt milieu- en leefomgevingsaspecten in kaart, deze zijn via de knoppen rechtsboven te selecteren. Via een klik op de scenarionaam linksboven zijn verschillende scenario’s te selecteren.';
        } else {
            h.textContent = 'Info project';
        }

        return this;
    },

    _addItem: function (obj) {
        var holder = document.createElement('div');
        holder.className = 'info-line';
        holder.id = obj.id;
        holder.innerHTML = obj.name;

        var container = this._detailList;
        container.appendChild(holder);

        this._checkDisabledLayers();
        return container;
    },

    _expand: function () {
        L.DomUtil.addClass(this._container, 'leaflet-control-info-expanded');
        var infoContainer, positionDiv;
        for (var key in InfoTextControl) {
            if (InfoTextControl.hasOwnProperty(key)) {
                if (InfoTextControl[key].active) {
                    if (!InfoTextControl[key].created) {

                        infoContainer = document.createElement('div');
                        infoContainer.classList.add(key + '-info');
                        infoContainer.name = key;

                        infoContainer.style.position = 'fixed';

                        infoButton = document.createElement('a');
                        infoButton.innerHTML = '&nbsp;';
                        infoButton.href = '#';

                        infoButton.classList.add('leaflet-control-info-toggle-button');
                        infoButton.onclick = function (e) {
                            var infoText;
                            for (var i = 0; i < document.getElementsByClassName('infoText').length; i++) {
                                infoText = document.getElementsByClassName('infoText')[i];
                                infoText.style.display = 'none';
                            }

                            if (this.parentElement.children[1].style.display !== 'inline-block') {
                                this.parentElement.children[1].style.display = 'inline-block';
                            } else {
                                this.parentElement.children[1].style.display = 'none';
                            }

                            e.preventDefault();
                            if (document.body.id == '') {
                                infoContainer.style.position = 'fixed';
                                setPosition(this.parentElement, InfoTextControl[this.parentElement.name], this.parentElement.name);
                            }
                            console.log(document.getElementsByClassName('leaflet-control-info-expanded')[0].getBoundingClientRect().height);
                            this.parentElement.children[1].style.bottom = document.getElementsByClassName('leaflet-control-info-expanded')[0].getBoundingClientRect().height + 27 + 'px';
                            console.log(document.getElementsByClassName('leaflet-control-info-expanded')[0].getBoundingClientRect().height);

                        };

                        infoText = document.createElement('p');
                        infoText.innerHTML = InfoTextControl[key].description;
                        infoText.classList.add('infoText');

                        infoContainer.appendChild(infoButton);
                        infoContainer.appendChild(infoText);

                        document.getElementsByClassName('leaflet-control-info-base')[0].appendChild(infoContainer);
                        setPosition(infoContainer, InfoTextControl[key], key);
                        InfoTextControl[key].created = true;
                    }
                }

            }

        }
        function setPosition(infoContainer, elem, key) {

            infoContainer.style.width = document.getElementsByClassName(key + '-info')[0].children[1].clientWidth;
            if (elem.iconPosition === 'left') {
                if (document.body.id === 'phone') {
                    infoContainer.children[0].style.left = (document.getElementsByClassName(key)[0].getBoundingClientRect().left - document.getElementsByClassName(key + '-info')[0].children[1].clientWidth) - 35 + 'px';
                    infoContainer.children[0].style.top = document.getElementsByClassName(key)[0].getBoundingClientRect().top + 'px';
                } else {
                    infoContainer.style.left = (document.getElementsByClassName(key)[0].getBoundingClientRect().left - document.getElementsByClassName(key + '-info')[0].children[1].clientWidth) - 10 + 'px';
                    infoContainer.style.top = document.getElementsByClassName(key)[0].getBoundingClientRect().top + 'px';
                }
            } else if (elem.iconPosition === 'right') {
                if (document.body.id === 'phone') {
                    infoContainer.children[0].style.left = document.getElementsByClassName(key)[0].getBoundingClientRect().left + 35 + 'px';
                    infoContainer.children[0].style.top = document.getElementsByClassName(key)[0].getBoundingClientRect().top + 'px';
                } else {
                    infoContainer.style.left = document.getElementsByClassName(key)[0].getBoundingClientRect().left + 35 + 'px';
                    infoContainer.style.top = document.getElementsByClassName(key)[0].getBoundingClientRect().top + 'px';
                }
            } else if (elem.iconPosition === 'bottom') {
                if (document.body.id === 'phone') {
                    infoContainer.children[0].style.top = document.getElementsByClassName(key)[0].getBoundingClientRect().bottom + 'px';
                    infoContainer.children[0].style.left = document.getElementsByClassName(key)[0].getBoundingClientRect().left + 'px';
                } else {
                    infoContainer.style.top = document.getElementsByClassName(key)[0].getBoundingClientRect().bottom - 10 + 'px';
                    infoContainer.style.left = document.getElementsByClassName(key)[0].getBoundingClientRect().left + 'px';
                }
            } else if (elem.iconPosition === 'top') {
                if (document.body.id === 'phone') {
                    infoContainer.children[0].style.top = document.getElementsByClassName(key)[0].getBoundingClientRect().top + 'px';
                    infoContainer.children[0].style.left = document.getElementsByClassName(key)[0].getBoundingClientRect().left + 'px';
                } else {
                    infoContainer.style.top = document.getElementsByClassName(key)[0].getBoundingClientRect().top + 'px';
                    infoContainer.style.left = document.getElementsByClassName(key)[0].getBoundingClientRect().left + 'px';
                }
            }

            infoContainer.children[0].style.position = 'fixed';
        }

        this._checkDisabledLayers();
    },

    _collapse: function () {
        L.DomUtil.removeClass(this._container, 'leaflet-control-info-expanded');
        if (document.body.id === 'phone') {
            for (var i = 0; i < document.getElementsByClassName('infoText').length; i++) {
                infoText = document.getElementsByClassName('infoText')[i];
                infoText.style.display = 'none';
            }
        }
    },

    _checkDisabledLayers: function () {
    }
});

L.control.info = function (categories, options) {
    return new L.Control.Info(categories, options);
};
