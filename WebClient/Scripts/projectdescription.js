// create project description control
L.control.projectDescription = L.control();

L.control.projectDescription.showInfo = true;

L.control.projectDescription.showOptions = function (e) {
    //stop the leaflet and default context menu's from appearing
    e.preventDefault();
    e.stopPropagation();

    var dialog = modalDialogCreate("Options");
    dialog.style.width = "300px";

    var container = L.DomUtil.create("div", "optionsContainer", dialog);

    modelDialogAddButton(container, 'Refresh', function () { wsSend({ scenarioRefresh: DataManager.sessionInfo.scenario }); });

    modelDialogAddButton(container, 'Scenario Overzicht', function () {
        modalDialogClose();
        ScenarioControlsManager.showScenarios(options);
    });

    modelDialogAddButton(container, 'Scenario Planning', function () {
        modalDialogClose();
        ScenarioControlsManager.showScenario(options);
    });

}

L.control.projectDescription.showScenarios = function () {
    // show modal dialog with all available scenarios
    if (options.scenarios && options.scenarios.length > 0) {
        var div = modalDialogCreate('Scenarios', 'choose the active and the reference scenario');
        div.style.width = '800px';
        div.style.margin = '5% auto';

        Scenarios = options.scenarios;

        // build dialog form

        var f = div.appendChild(document.createElement('form'));
        f.id = 'selectScenariosForm';
        var height = window.innerHeight - 200;
        //set max-height and scrollbars if window is small
        f.style.maxHeight = '' + (height - 40) + 'px';
        f.style.overflow = "auto";
        f.style.marginBottom = '40px';

        window.addEventListener("resize", ResizeSelectScenario);


        var ul = f.appendChild(document.createElement("ul"));
        //var ul = listContainer.appendChild("ul");
        ul.id = "list";

        // add active|reference text
        L.control.projectDescription.AddActiveReference(ul);

        // adds the no reference scenario option
        L.control.projectDescription.AddNoReference(ul);

        // adds the scenarios
        L.control.projectDescription.AddScenarios(Scenarios, ul);

        f.appendChild(document.createElement('br'));
        f.appendChild(document.createElement('hr'));
        var mddb = f.appendChild(document.createElement('div'));
        mddb.className = 'modalDialogDevideButtons';
        var _this = this;

        modelDialogAddButton(mddb, 'Cancel', modalDialogClose);
        modelDialogAddButton(mddb, 'Apply', function () {
            var selectedRadio = document.querySelector('input[name=activeScenario]:checked');
            options.activeScenario = selectedRadio ? selectedRadio.value : "";//-1; todo: check if works in schiedam
            selectedRadio = document.querySelector('input[name=referenceScenario]:checked');
            options.referenceScenario = selectedRadio ? selectedRadio.value : "";//-1; todo check if works in schiedam
            wsSend({
                selectScenario: {
                    currentScenario: options.activeScenario,
                    referenceScenario: options.referenceScenario
                }
            });
            modalDialogClose();
        });

        L.control.projectDescription.CheckCurrentScenarios(options);
    }
    if (f) {
        L.DomEvent.addListener(f, 'touchmove', L.DomEvent.stopPropagation);
    }


};

function ResizeSelectScenario() {
    var buttons = document.getElementsByClassName('modalDialogDevideButtons')[0];
    var div = document.getElementById("selectScenariosForm")

    //check if element still excists otherwise remove the eventlistener ?? Is this needed?
    if (div == null) {
        window.removeEventListener("resize", ResizeSelectScenario);
        return;
    }

    //get the current window height
    var height = window.innerHeight - 200;

    //set max-height and scrollbars if window is small but not too small
    div.style.maxHeight = "" + (height - 40) + 'px';
    div.style.overflow = "auto";
    div.style.marginBottom = '40px';


    // document.getElementsByClassName('scrolling')[0].style.position = 'fixed';
    // document.getElementsByClassName('scrolling')[0].style.bottom = buttons.parentElement.parentElement.getBoundingClientRect().top + 'px';
    //
    // console.log(buttons.parentElement.parentElement.getBoundingClientRect());

}


// handle onAdd and addTo
L.control.projectDescription.onAdd = function (map) {
    this._div = L.DomUtil.create('div', 'projectDescription');
    var _this = this; // capture this
    options = this.options;



    window.addEventListener("resize", ResizeProjectDescription);
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
    if (this.options.description) {
        //using global variable projectDescriptionH2
        projectDescriptionH2 = document.getElementById("projectDescriptionH2")
        projectDescriptionH2 = (projectDescriptionH2 != null) ? projectDescriptionH2 : this._div.appendChild(document.createElement("h2"));
        projectDescriptionH2.id = "projectDescriptionH2";

        var textBox;
        if (projectDescriptionH2.children[0] == null) {
            textBox = projectDescriptionH2.appendChild(document.createElement("span"));
            textBox.addEventListener('click', L.control.projectDescription.showScenarios);
            textBox.addEventListener('contextmenu', L.control.projectDescription.showOptions);
            textBox.id = "projectDescriptionTextBox";
        }
        else
            textBox = projectDescriptionH2.children[0];

        textBox.innerText = this.options.description;

        //projectDescriptionH2.innerHTML = this.options.description;

        ResizeProjectDescription();

    }
};

function ResizeProjectDescription() {
    projectDescriptionH2.style.width = "" + (window.innerWidth - 165) + "px";
}

L.control.projectDescription.AddActiveReference = function (ul) {
    var li = ul.appendChild(document.createElement("li"));
    li.className = "listItem";

    var table = L.control.projectDescription.CreateTable(false);

    var activeTextSpan = table[2].appendChild(document.createElement("span"));
    activeTextSpan.id = "activeTextSpan";
    activeTextSpan.appendChild(document.createTextNode("Active"));
    table[2].className = "";
    table[2].id = "activeTextCell";

    var refTextSpan = table[3].appendChild(document.createElement("span"));
    refTextSpan.id = "refTextSpan";
    refTextSpan.appendChild(document.createTextNode("Ref"));
    table[3].className = "";
    table[3].id = "refTextCell";

    li.appendChild(table[0]);
}

L.control.projectDescription.AddNoReference = function (ul) {
    var li = ul.appendChild(document.createElement("li"));
    li.className = "listItem";

    var table = L.control.projectDescription.CreateTable(false);

    var textSpan = table[2].appendChild(document.createElement("span"));
    textSpan.id = "noRefSpan";
    textSpan.appendChild(document.createTextNode("No Reference Scenario"));
    table[2].className = "";
    table[2].id = "noRefTextCell";

    var rightRadioSpan = table[3].appendChild(document.createElement("span"));
    rightRadioSpan.className = "rightRadioSpan";
    table[3].id = "noRefRightRadio";
    table[3].value = "noreference";
    rightRadioSpan.value = "noreference";
    table[3].addEventListener("click", L.control.projectDescription.RightMouseClick);
    var rightRadio = rightRadioSpan.appendChild(document.createElement("input"));
    rightRadio.type = "radio";
    rightRadio.className = "rightRadio";
    rightRadio.id = "noreference";
    rightRadio.value = "noreference";

    li.appendChild(table[0]);
}

L.control.projectDescription.AddScenarios = function (list, ul) {

    for (var i = 0; i < list.length; i++) {
        var li = ul.appendChild(document.createElement("li"));
        li.className = "listItem";
        L.control.projectDescription.FillTable(list[i], li);

        if (list[i].children.length > 0) {
            var subUL = ul.appendChild(document.createElement("li")).appendChild(document.createElement("ul"));
            subUL.className = "subList";
            L.control.projectDescription.AddScenarios(list[i].children, subUL);
        }
    }
}

L.control.projectDescription.CheckCurrentScenarios = function (aOptions) {
    var leftRadioButtons = document.getElementsByClassName("leftRadio");
    var rightRadioButtons = document.getElementsByClassName("rightRadio");

    var activeId = aOptions.activeScenario;

    var referenceId = aOptions.referenceScenario;

    for (var i = 0; i < leftRadioButtons.length; i++) {
        if (leftRadioButtons[i].value == activeId) {
            leftRadioButtons[i].checked = true;
            break;
        }
    }

    for (var i = 0; i < rightRadioButtons.length; i++) {
        if (rightRadioButtons[i].value == referenceId) {
            rightRadioButtons[i].checked = true;
            return;
        }
    }

    for (var i = 0; i < rightRadioButtons.length; i++) {
        if (rightRadioButtons[i].value == "noreference") {
            rightRadioButtons[i].checked = true;
            return;
        }
    }
}

L.control.projectDescription.FillTable = function (aScenario, li) {


    var table = L.control.projectDescription.CreateTable();

    var textSpan = table[1].appendChild(document.createElement("span"));
    table[1].value = aScenario.id;
    textSpan.className = "textSpan";
    textSpan.id = "scenariotext" + aScenario.id;
    textSpan.value = aScenario.id;
    if (aScenario.name.length > 3) {
        var spanText = textSpan.appendChild(document.createTextNode(aScenario.name + ", " + aScenario.description));
    }
    else {
        var spanText = textSpan.appendChild(document.createTextNode(aScenario.description));
    }


    var leftRadioSpan = table[2].appendChild(document.createElement("span"));
    leftRadioSpan.className = "leftRadioSpan";
    leftRadioSpan.value = aScenario.id;
    table[2].value = aScenario.id;
    var leftRadio = leftRadioSpan.appendChild(document.createElement("input"));
    leftRadio.type = "radio";
    leftRadio.className = "leftRadio";
    leftRadio.id = aScenario.id;
    leftRadio.value = aScenario.id;
    leftRadio.name = "activeScenario";


    var rightRadioSpan = table[3].appendChild(document.createElement("span"));
    rightRadioSpan.className = "rightRadioSpan";
    rightRadioSpan.value = aScenario.id;
    table[3].value = aScenario.id;
    var rightRadio = rightRadioSpan.appendChild(document.createElement("input"));
    rightRadio.type = "radio";
    rightRadio.className = "rightRadio";
    rightRadio.id = "referenceScenario" + aScenario.id;
    rightRadio.value = aScenario.id;
    rightRadio.name = "referenceScenario";

    if (L.control.projectDescription.showInfo)
    {
        var infoText = table[4].appendChild(document.createElement("span"));
        infoText.className = "infoText";
        infoText.scenario = aScenario;
        infoText.innerHTML = "i";
        table[4].addEventListener("click", L.control.projectDescription.clickInfo.bind(aScenario));
        table[4].className += " cell4Filled";
        table[4].scenario = aScenario;
    }

    aScenario.span = textSpan;
    aScenario.leftRadio = leftRadio;
    aScenario.rightRadio = rightRadio;

    li.appendChild(table[0]);
}

L.control.projectDescription.CreateTable = function (triggers) {
    
    if (arguments.length == 0) {
        triggers = true;
    }
    var table = document.createElement("table");
    table.className = "scenarioTable";

    var row = table.appendChild(document.createElement("tr"));
    row.className = "tableRow";

    var cell1 = row.appendChild(document.createElement("td"));
    cell1.className = "cell1";
    if (triggers) {
        cell1.addEventListener("mouseenter", L.control.projectDescription.TextMouseEnter);
        cell1.addEventListener("mouseleave", L.control.projectDescription.TextMouseLeave);
        cell1.addEventListener("click", L.control.projectDescription.TextMouseClick);
    }

    var cell2 = row.appendChild(document.createElement("td"));
    cell2.className = "cell2";
    if (triggers) {
        cell2.addEventListener("mouseenter", L.control.projectDescription.LeftMouseEnter);
        cell2.addEventListener("mouseleave", L.control.projectDescription.LeftMouseLeave);
        cell2.addEventListener("click", L.control.projectDescription.LeftMouseClick);
    }

    var cell3 = row.appendChild(document.createElement("td"));
    cell3.className = "cell3";
    if (triggers) {
        cell3.addEventListener("mouseenter", L.control.projectDescription.RightMouseEnter);
        cell3.addEventListener("mouseleave", L.control.projectDescription.RightMouseLeave);
        cell3.addEventListener("click", L.control.projectDescription.RightMouseClick);
    }


    var returnArray = [table, cell1, cell2, cell3];

    if (L.control.projectDescription.showInfo)
    {
        var cell4 = row.appendChild(document.createElement("td"));
        cell4.className = "cell4";
        returnArray.push(cell4);
    }

    return returnArray;
}

L.control.projectDescription.clickInfo = function (event) {
    L.control.projectDescription.showInfoPopup(event.target.scenario);
}

L.control.projectDescription.TextMouseEnter = function (event) {
    target = event.target;
    L.control.projectDescription.AdjustRadioColor(target, "LightBlue");
}

L.control.projectDescription.TextMouseLeave = function (event) {
    target = event.target;
    L.control.projectDescription.AdjustRadioColor(target, "White");
}

L.control.projectDescription.TextMouseClick = function (event) {
    L.control.projectDescription.ActiveClick(event.target.value);
}

L.control.projectDescription.LeftMouseEnter = function (event) {
    target = event.target;
    L.control.projectDescription.AdjustColor(target, "LightBlue");
}

L.control.projectDescription.LeftMouseLeave = function (event) {
    target = event.target;
    L.control.projectDescription.AdjustColor(target, "white");
}

L.control.projectDescription.LeftMouseClick = function (event) {
    L.control.projectDescription.ActiveClick(event.target.value);
}

L.control.projectDescription.RightMouseEnter = function (event) {
    target = event.target;
    L.control.projectDescription.AdjustColor(target, "#d6adeb");
}

L.control.projectDescription.RightMouseLeave = function (event) {
    target = event.target;
    L.control.projectDescription.AdjustColor(target, "white");
}

L.control.projectDescription.RightMouseClick = function (event) {
    var rightRadioButtons = document.getElementsByClassName("rightRadio");
    for (var i = 0; i < rightRadioButtons.length; i++) {
        if (rightRadioButtons[i].value == event.target.value) {
            rightRadioButtons[i].checked = true;
        }
        else {
            rightRadioButtons[i].checked = false;
        }
    }

}

L.control.projectDescription.FindScenario = function (scenarioID, list) {
    for (var i = 0; i < list.length; i++) {
        if (list[i].id == scenarioID) {
            return [true, list[i]];
        }
        else if (list[i].children.length > 0) {
            var temp = L.control.projectDescription.FindScenario(scenarioID, list[i].children);
            if (temp[0]) {
                return temp;
            }
        }
    }
    return [false, null];
}

L.control.projectDescription.ActiveClick = function (scenarioID) {
    var leftRadioButtons = document.getElementsByClassName("leftRadio");
    var rightRadioButtons = document.getElementsByClassName("rightRadio");
    var scenario;
    for (var i = 0; i < Scenarios.length; i++) {
        if (scenarioID == Scenarios[i].id) {
            scenario = Scenarios[i];
        }
    }
    scenario = L.control.projectDescription.FindScenario(scenarioID, Scenarios)[1];


    for (var i = 0; i < leftRadioButtons.length; i++) {
        if (leftRadioButtons[i].value == scenarioID) {
            leftRadioButtons[i].checked = true;
        }
        else {
            leftRadioButtons[i].checked = false;
        }
    }

    var reference = scenario.reference;

    var found = false;

    for (var i = 0; i < rightRadioButtons.length; i++) {
        if (rightRadioButtons[i].value == reference) {
            rightRadioButtons[i].checked = true;
            found = true;
        }
        else {
            rightRadioButtons[i].checked = false;
        }
    }

    if (!found) {
        for (var i = 0; i < rightRadioButtons.length; i++) {
            if (rightRadioButtons[i].value == "noreference") {
                rightRadioButtons[i].checked = true;
                return;
            }
        }
    }
}

L.control.projectDescription.AdjustColor = function (target, color) {
    target.style.backgroundColor = color;

    var radioButton = target.children[0].children[0];

    var spanList = document.getElementsByClassName("textSpan");

    var i;
    for (i = 0; i < spanList.length; i++) {
        if (spanList[i].value == radioButton.value) {
            spanList[i].parentNode.style.backgroundColor = color;
        }
    }
}

L.control.projectDescription.AdjustRadioColor = function (target, color) {
    target.style.backgroundColor = color;

    var textSpan = target.children[0];

    var leftButtonList = document.getElementsByClassName("leftRadio");

    var i;
    for (i = 0; i < leftButtonList.length; i++) {
        if (leftButtonList[i].value == textSpan.value) {
            leftButtonList[i].parentNode.parentNode.style.backgroundColor = color;
        }
    }
}

L.control.projectDescription.showInfoPopup = function (scenario) {
    var popUpContainer;

    if (typeof L.control.projectDescription.popUpContainer === "undefined") {
        popUpContainer = L.control.projectDescription.popUpContainer = document.body.appendChild(document.createElement("div"));
        popUpContainer.id = "description-popup-container";
    }
    else
        popUpContainer = L.control.projectDescription.popUpContainer;

    popUpContainer.innerHTML = "";
    popUpContainer.style.display = "block";

    var popUpDiv = popUpContainer.appendChild(document.createElement("div"));
    popUpDiv.id = "description-popup-div";

    var textDiv = popUpDiv.appendChild(document.createElement("div"));
    textDiv.className = "description-popup-textspan";
    textDiv.innerHTML = "Hier komt dan iets van text enzo, <BR>scenario: <BR>" + scenario.description;

    var closeButton = popUpDiv.appendChild(document.createElement("button"));
    closeButton.addEventListener("click", function (e) { popUpContainer.style.display = "none"; });
    closeButton.innerText = "Close";
}
