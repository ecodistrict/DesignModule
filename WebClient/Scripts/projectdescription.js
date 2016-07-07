// create project description control
L.control.projectDescription = L.control();

L.control.projectDescription.showScenarios = function () {
    // show modal dialog with all available scenarios
    if (this.options.scenarios && this.options.scenarios.length > 0) {
        var div = modalDialogCreate('Scenarios', 'choose the active and the reference scenario');
        div.style.width = '800px';
        div.style.margin = '5% auto';
        // build dialog form
        var f = div.appendChild(document.createElement('form'));
        f.id = 'selectScenariosForm';

        var rb2 = f.appendChild(document.createElement('input'));
        rb2.className = 'selectReferenceScenarioRadio';
        rb2.id = 'referenceScenario-1';
        rb2.type = 'radio';
        rb2.name = 'referenceScenario';
        rb2.value = -1;
        if (typeof (this.options.referenceScenario) === 'undefined' || this.options.referenceScenario === -1)
            rb2.checked = true;
        rb2.title = 'none';

        var span = f.appendChild(document.createElement('span'));
        span.appendChild(document.createTextNode('no reference scenario'));
        span.className = 'selectReferenceScenarioLabel';

        f.appendChild(document.createElement('br'));

        var ul = f.appendChild(document.createElement('ul'));
        ul.className = 'selectScenarioUL';

        for (var i = 0; i < this.options.scenarios.length; i++)
            this._addScenario(ul, this.options.scenarios[i]);

        f.appendChild(document.createElement('br'));
        f.appendChild(document.createElement('hr'));
        var mddb = f.appendChild(document.createElement('div'));
        mddb.className = 'modalDialogDevideButtons';
        var _this = this;
        modelDialogAddButton(mddb, 'Apply', function () {
            var selectedRadio = document.querySelector('input[name=activeScenario]:checked');
            if (selectedRadio && _this.options.activeScenario !== selectedRadio.value) {
                _this.options.activeScenario = selectedRadio.value;
                wsSend({ selectScenario: { currentScenario: selectedRadio.value } });
            }
            selectedRadio = document.querySelector('input[name=referenceScenario]:checked');
            if (selectedRadio && _this.options.referenceScenario !== selectedRadio.value) {
                _this.options.referenceScenario = selectedRadio.value;
                wsSend({ selectScenario: { referenceScenario: selectedRadio.value } });
            }
            modalDialogClose();
        });
        modelDialogAddButton(mddb, 'Cancel', modalDialogClose);
    }
};

function selectScenario(e) {
    var referenceScenario = e.currentTarget.defaultReference;
    var referenceScenarioElement = document.getElementById('referenceScenario' + referenceScenario);
    if (referenceScenarioElement)
        referenceScenarioElement.checked = true;
}

L.control.projectDescription._addScenario = function (aParent, aScenario) {
    // scenario.id
    // scenario.name
    // scenario.description
    // scenario.status
    // scenario.reference
    // scenario.children
    var li = aParent.appendChild(document.createElement('li'));
    li.className = 'selectScenarioLI';

    var rb = li.appendChild(document.createElement('input'));
    rb.className = 'selectActiveScenarioRadio';
    rb.id = aScenario.id;
    rb.type = 'radio';
    rb.name = 'activeScenario';
    rb.value = aScenario.id;
    rb.defaultReference = aScenario.reference;
    if (aScenario.id === this.options.activeScenario)
        rb.checked = true;
    //rb.scenario = aScenario;
    //rb.action = aScenario.description;
    rb.title = aScenario.description;
    rb.addEventListener('click', selectScenario);

    var label = li.appendChild(document.createElement('label'));
    label.className = 'selectActiveScenarioLabel';
    // todo: for US: small name (V12) does not need to be shown (conflict with other sources?)
    if (aScenario.name.length > 3)
        label.appendChild(document.createTextNode(aScenario.name + ', ' + aScenario.description));
    else {
        label.appendChild(document.createTextNode(aScenario.description));
        label.title = aScenario.name;
    }
    label.htmlFor = aScenario.id;

    var rb2 = label.appendChild(document.createElement('input'));
    rb2.className = 'selectReferenceScenarioRadio';
    rb2.id = 'referenceScenario' + aScenario.id;
    rb2.type = 'radio';
    rb2.name = 'referenceScenario';
    rb2.value = aScenario.id;
    if (aScenario.id === this.options.referenceScenario)
        rb2.checked = true;
    rb2.title = 'reference scenario: ' + aScenario.name;


    if (aScenario.children.length > 0) {
        var ul = li.appendChild(document.createElement('ul'));
        ul.className = 'selectScenarioUL';
        for (var i = 0; i < aScenario.children.length; i++) {
            this._addScenario(ul, aScenario.children[i]);
        }
    }
};

// handle onAdd and addTo
L.control.projectDescription.onAdd = function (map) {
    this._div = L.DomUtil.create('div', 'projectDescription');
    var _this = this; // capture this
    this._div.addEventListener('click', function () { _this.showScenarios(); });
    L.DomEvent.disableClickPropagation(this._div);
    this.update();
    return this._div;
};

L.control.projectDescription.addTo = function (map) {
    this.remove();
    this._map = map;
    var container = this._container = this.onAdd(map);
    L.DomUtil.addClass(container, 'leaflet-control leaflet-title');
    map._controlContainer.appendChild(container);
    return this;
};

L.control.projectDescription.update = function (props) {
    if (this.options.description)
        this._div.innerHTML = '<h2>' + this.options.description + '</h2>';
};
