function openSimulationDialog(e) {

    //if (typeof DataManager.simulationSetupData === "undefined" || DataManager.simulationSetupData == null)
    //    return;

    // todo: build dialog based on measuresControl.options.selectCategories

    e.target.blur();

    var div = modalDialogCreate('Setup your simulation');

    if (window.outerWidth < 500) {
        div.style.width = '100%';
        div.style.boxSizing = "border-box";
        //div.style.margin = '5% auto';
    } else {
        div.style.width = '400px';
        //div.style.margin = '5% auto';
    }

    var label, optionsArray;

    // build dialog form
    var f = div.appendChild(document.createElement('form'));
    f.id = 'simulationForm';
    f.name = 'simulationForm';

    var errorLog = document.createElement('div');
    errorLog.id = 'errorLog';
    errorLog.style.display = 'none';
    errorLog.innerHTML = '';

    // formElement [string], type [string], required [y/n], optionsArray [false/array], labelText [string], idName [string], extraOptions [false/array-2_Elems] ['steps [int]','postfix' [string]]);
    var data = DataManager.simulationSetupData;
    //var data = [{"formElement":"input","type":"string","required":"y","optionsArray":false,"labelText":"Scenario name","idName":"scenarioName","extraOptions":false},
    //{"formElement":"slider","type":"float","required":"y","optionsArray":["0", "100"],"labelText":"penetration","idName":"penetration","extraOptions":[1, "%"]},
    //{"formElement":"radio","type":"string","required":"y","optionsArray":["Yes", "No"],"labelText":"Record Simulation:","idName":"datasourcerecord","extraOptions":{"checked":"No"}}]
    //var data = [
    //{
    //  "formElement":"input",
    //  "type":"string",
    //  "required":"y",
    //  "optionsArray":false,
    //  "labelText":"Scenario name",
    //  "idName":"scenarioName",
    //  "extraOptions":false
    //},
    //{
    //  "formElement":"slider",
    //  "type":"int",
    //  "required":"y",
    //  "optionsArray":['0', '100'],
    //  "labelText":"Penetration rate",
    //  "idName":"PenetrationRateSelect",
    //  "extraOptions":[1, '%']
    //},
    //{
    //  "formElement":"slider",
    //  "type":"int",
    //  "required":"y",
    //  "optionsArray":['0', '100'],
    //  "labelText":"Follow-on behavior",
    //  "idName":"followOn",
    //  "extraOptions":[1, '%']
    //},
    //{
    //  "formElement":"select",
    //  "type":"int",
    //  "required":"y",
    //  "optionsArray":[['A', 'OD Basis'], ['B', 'OD variant 1'], ['C', 'OD variant 2']],
    //  "labelText":"Origin Destination matrix",
    //  "idName":"MatrixChoice",
    //  "extraOptions":false
    //   },
    //{
    //    "formElement": "radio",
    //    "type": "string",
    //    "required": "y",
    //    "optionsArray": ['Yes','No'],
    //    "labelText": "Record Simulation",
    //    "idName": "Record",
    //    extraOptions: {checked: 'No'}
    //}
    // ];

    if (typeof data === "undefined")
        return;

    for (var i = 0; i < data.length; i++) {
        fillOptions(data[i]);
    }

    var container, optionWrapper, opt;

    // function fillOptions(formElement, type, required, optionsArray, labelText, idName, extraOptions) {
    function fillOptions(arrayItem) {
        var formElement = arrayItem.formElement;
        var type = arrayItem.type;
        var required = arrayItem.required;
        var optionsArray = arrayItem.optionsArray;
        var labelText = arrayItem.labelText;
        var idName = arrayItem.idName;
        var extraOptions = arrayItem.extraOptions;


        // input, select
        // checkbox, textaea, radio,
        if (formElement === 'input') {
            container = document.createElement('div');
            container.id = idName;
            container.style.display = 'flex';
        } else if (formElement === 'radio') {
            container = document.createElement('div');
            container.id = idName;
            container.style.display = "none";
        } else if (formElement === 'select') {
            container = document.createElement('select');
            container.id = idName;
            container.name = idName;
            container.className = 'form-control';
            container.dataset.required = required;
            container.dataset.type = type;
        } else if (formElement === 'textarea') {
            container = document.createElement('div');
            container.id = idName + '-option-row';
        } else if (formElement === 'checkbox') {
            container = document.createElement('div');
            container.id = idName + '-option-row';
            container.style.display = "none";
        } else if (formElement === 'slider') {
            container = document.createElement('div');
            container.id = idName + '-option-row';
            container.style.display = 'block';
            var range = document.createElement('div');
            range.id = 'range_' + idName;
        }



        label = document.createElement('label');
        label.innerHTML = labelText;
        f.appendChild(label);

        if (formElement === 'input') {
            var input = document.createElement('input');
            input.type = 'text';
            input.className = 'form-control';
            input.placeholder = labelText;
            input.name = idName;
            input.dataset.required = required;
            input.dataset.type = type;
            container.appendChild(input);
        } else if (formElement === 'radio') {
            optionWrapper = document.createElement('div');
            optionWrapper.id = idName + '-option-row';
            optionWrapper.className = 'form-control';

            for (var i = 0; i < optionsArray.length; i++) {
                option = optionsArray[i];
                var opt = document.createElement('input');
                var button = document.createElement('button');
                opt.type = 'radio';
                opt.name = idName;
                opt.value = option;
                opt.dataset.required = required;
                opt.dataset.type = type;
                button.innerText = option;
                button.value = option;
                button.name = idName;
                button.type = "button";
                button.addEventListener("click", function (e) {
                    e.preventDefault();
                    for (var k = 0; k < document.simulationForm[idName].length; k++) {
                        elem = document.simulationForm[idName][k];

                        if (elem.type === 'radio') {
                            if (elem.value === e.target.value) {
                                elem.checked = true;
                            } else {
                                elem.checked = false;
                            }
                        }
                    }
                    for (var i = 0; i < e.target.parentElement.children.length; i++) {
                        e.target.parentElement.children[i].classList.remove('selected');
                        e.target.parentElement.children[i].checked = false;
                    }
                    e.target.className = 'selected';
                    var selectedSelect = document.getElementById(e.target.name);
                    selectedSelect.checked = true;
                    selectedSelect.value = e.target.value
                });
                opt.value = option;
                opt.innerHTML = option;
                optionWrapper.className = idName;
                container.appendChild(opt);
                optionWrapper.appendChild(button);
                if (extraOptions && extraOptions.checked && extraOptions.checked == option) {
                    opt.checked = true;
                    opt.value = option;
                    button.className = 'selected';
                }
            }
            f.appendChild(optionWrapper);
        } else if (formElement === 'select') {

            //console.log(options);

            for (var i = 0; i < optionsArray.length; i++) {
                option = optionsArray[i];
                var opt = document.createElement('option');
                if (option.constructor === Array) {
                    opt.value = option[0];
                    opt.innerHTML = option[1];
                } else {
                    opt.value = option;
                    opt.innerHTML = option;
                }

                container.appendChild(opt);
            }

        } else if (formElement === 'checkbox') {
            optionWrapper = document.createElement('div');
            optionWrapper.id = idName + '-option-row';
            optionWrapper.className = 'form-control';
            for (var i = 0; i < optionsArray.length; i++) {
                option = optionsArray[i];
                var opt = document.createElement('input');
                var button = document.createElement('button');
                opt.type = 'checkbox';
                opt.name = idName;
                opt.value = option;
                opt.dataset.required = required;
                opt.dataset.type = type;
                button.innerText = option;
                button.value = option;
                button.type = "button";
                button.addEventListener("click", function (e) {
                    e.preventDefault();
                    for (var k = 0; k < document.simulationForm[idName].length; k++) {
                        elem = document.simulationForm[idName][k];
                        if (elem.type === 'checkbox' && elem.value === e.target.value) {
                            if (elem.checked) {
                                elem.checked = false;
                            } else {
                                elem.checked = true;
                            }
                        }
                    }
                    if (e.target.className === 'selected') {
                        e.target.className = '';
                    } else {
                        e.target.className = 'selected';
                    }
                });
                opt.value = option;
                opt.innerHTML = option;
                optionWrapper.className = idName;
                container.appendChild(opt);
                optionWrapper.appendChild(button);
            }
            f.appendChild(optionWrapper);


        } else if (formElement === 'textarea') {
            var textarea = document.createElement('textarea');
            textarea.type = 'text';
            textarea.className = 'form-control';
            textarea.placeholder = labelText;
            textarea.dataset.required = required;
            textarea.dataset.type = type;
            container.appendChild(textarea);
        } else if (formElement === 'slider') {

            sliderInput = document.createElement('input');
            sliderInput.type = 'text';
            sliderInput.className = 'form-control';
            sliderInput.placeholder = labelText;
            //sliderInput.value = parseInt(optionsArray[0]);
            sliderInput.name = idName;
            sliderInput.id = 'input_' + idName;
            sliderInput.dataset.required = required;
            sliderInput.dataset.type = type;
            sliderInput.dataset.formElement = formElement;
            sliderInput.dataset.optionsArray = optionsArray;
            sliderInput.style.width = '100%';
            sliderInput.style.boxSizing = 'border-box';

            var rangeSlider = noUiSlider.create(range, {
                connect: true, // Display a colored bar between the handles
                start: parseInt(optionsArray[0]),
                step: parseInt(extraOptions[0]),
                behaviour: 'tap',
                extraOption: extraOptions[1],
                tooltips: true,
                range: {
                    'min': parseInt(optionsArray[0]),
                    'max': parseInt(optionsArray[1])
                },
                format: {
                    to: function (value) {

                        if (range.noUiSlider) {
                            return Math.round(value) + range.noUiSlider.options.extraOption;
                        } else {
                            return Math.round(value);
                        }

                    },
                    from: function (value) {

                        if (range.noUiSlider) {
                            if (range.noUiSlider.options.extraOption.length > 0) {
                                return Math.round(value.repace(range.noUiSlider.options.extraOption, ''));
                            } else {
                                return Math.round(value);
                            }
                        } else {
                            return Math.round(value);
                        }

                    }
                }
            }).on('update', function (values, handle) {
                formattedValue = values[handle];

                if (range.noUiSlider.options.extraOption.length > 0 && range.noUiSlider.options.extraOption !== '') {
                    formattedValue = values[handle].replace(range.noUiSlider.options.extraOption, '');
                } else {
                    formattedValue = values[handle];
                }
                if (this.target.parentElement) {
                    this.target.parentElement.children[1].value = formattedValue;
                }

            });

            range.style.height = '20px';
            range.style.width = '100%';
            range.style.margin = '40px auto 10px';



            sliderInput.addEventListener('change', function () {
                range.noUiSlider.set(this.value);
            });


            container.appendChild(range);
            container.appendChild(sliderInput);
        }

        f.appendChild(container);
        f.appendChild(document.createElement('br'));

    }


    // newQueryLine.className = 'queryDialogLine';
    // newQueryLine.innerHTML =
    //     '<select class="datalist"><option value="Inhabitants">Inhabitants</option><option value="Height">Height</option><option value="Surface_area">Surface area</option></select>'+
    //     '<select class="optionList"><option value="<">&lt;</option><option value="<=" selected>&le;</option><option value="=" selected>=</option><option value="<>">&ne;</option><option value=">">&gt;</option><option value=">=">&ge;</option><option value="in">in</option></select>'+
    //     '<input type="text" placeholder="value" />'+
    //     '<img class="removeQuery" src="Content/images/historyremove.png" class="queryDialogAddRemoveButton" onclick="selectByQueryRemoveLine(this)" title="..remove this line from the query" />' +
    //     '<img class="addQuery" src="Content/images/domainadd.png" class="queryDialogAddRemoveButton" onclick="selectByQueryAddLine(this)" title="..add a new line to the query" />';
    // if (e) {
    //     e.parentNode.parentNode.appendChild(newQueryLine);
    //     // remove add-line-image from current entry
    //     e.parentNode.removeChild(e);
    // }

    // buttons section

    f.appendChild(document.createElement('hr'));
    f.appendChild(errorLog);
    var mddb = f.appendChild(document.createElement('div'));
    mddb.className = 'modalDialogDevideButtons';
    modelDialogAddButton(mddb, 'Cancel', modalDialogClose);
    modelDialogAddButton(mddb, 'Apply', simulationDialogApply);
}

function simulationDialogApply() {
    errors = false;
    formResult = [];
    elemValues = [];
    errorLog.innerHTML = '';
    for (var i = 0; i < document.forms['simulationForm'].elements.length; i++) {
        var elem = document.forms['simulationForm'].elements[i];
        if (elem.type === 'text') {
            if (elem.dataset.required === 'y') {
                if (elem.value === '') {
                    if (elem.dataset.type === 'int') {
                        formResult.push({ name: elem.name, value: 0, type: elem.dataset.type });
                        elem.classList.remove('empty');
                    }
                    else if (elem.dataset.formElement === 'slider') {
                        formResult.push({ name: elem.name, value: elem.dataset.optionsArray[0], type: elem.dataset.type });
                        elem.classList.remove('empty');
                    }
                    else {
                        errors = true;
                        elem.classList.add('empty');
                        errorLog.style.display = 'block';
                        errorLog.innerHTML = errorLog.innerHTML + '<span>' + elem.placeholder + ' is not correct!' + '</span>';
                    }
                } else {
                    var result;
                    switch (elem.dataset.type) {
                        case "int": result = parseInt(elem.value);
                            break;
                        case "float": result = parseFloat(elem.value);
                            break;
                        default: result = elem.value;
                            break;
                    }
                    formResult.push({ name: elem.name, value: result, type: elem.dataset.type });
                    elem.classList.remove('empty');
                }
            }
        } else if (elem.tagName === 'SELECT') {
            if (elem.dataset.required === 'y') {
                if (elem.value === '') {
                    errors = true;
                    elem.classList.add('empty');
                    errorLog.style.display = 'block';
                    errorLog.innerHTML = errorLog.innerHTML + elem.name + ' is not correct!';
                } else {
                    formResult.push({ name: elem.name, value: elem.value, type: elem.dataset.type });
                    elem.classList.remove('empty');
                }
            }
        } else if (elem.type === 'textarea') {
            if (elem.dataset.required === 'y') {
                if (elem.value === '') {
                    errors = true;
                    elem.classList.add('empty');
                    errorLog.style.display = 'block';
                } else {
                    formResult.push({ name: elem.name, value: elem.value, type: elem.dataset.type });
                    elem.classList.remove('empty');
                }
            }
        } else if (elem.type === 'radio') {

            if (elem.dataset.required === 'y') {
                if (elem.checked) {
                    formResult.push({ name: elem.name, value: elem.value, type: elem.dataset.type });
                }
                if (document.getElementById('simulationForm')[elem.name].value === '') {
                    optionsArray = document.getElementById(elem.name + '-option-row');
                    optionsArray.classList.add('empty');
                    errorLog.style.display = 'block';
                } else {
                    optionsArray = document.getElementById(elem.name + '-option-row');
                    optionsArray.classList.remove('empty');
                    errorLog.style.display = 'block';
                }
            }

        } else if (elem.type === 'checkbox') {

            if (elem.dataset.required === 'y') {

                var checkboxes = document.getElementsByName(elem.name);
                var vals = "";
                for (var i2 = 0, n = checkboxes.length; i2 < n; i2++) {
                    if (checkboxes[i2].checked) {
                        vals += "," + checkboxes[i2].value;
                    }
                }
                if (vals) {
                    vals = vals.substring(1);
                }

                if (elem.checked) {

                    found = false;
                    for (var i2 = 0; i2 < formResult.length; i2++) {
                        if (formResult[i2].name === elem.name) {
                            found = true;
                            optionsArray = document.getElementById(elem.name + '-option-row');
                            optionsArray.classList.remove('empty');
                            errors = false;
                        }
                    }
                    if (!found) {
                        formResult.push({ name: elem.name, value: vals, type: elem.dataset.type });
                    }
                }
            } // eo elem.dataset.required
        } // eo checkbox
    } // eo for loop



    // // build query
    // var query = '';
    // var lines = document.getElementById('queryDialogLines');
    // for (var i = 0; i < lines.children.length; i++) {
    //   var line = lines.children[i];
    //   if (line.children[0].value != '' && line.children[2].value != '') {
    //     // 1=attribute, 2=operator, 3=value
    //     if (query != '')
    //     query += ' AND ';
    //     query += line.children[0].value + ' ' + line.children[1].value + ' ' + line.children[2].value;
    //   }
    // }
    if (!errors) {
        console.log({ formResult: formResult });
        var sessionRequest = {
            setupSimulation: {
                parameters: formResult
            }
        }
        wsSend(sessionRequest);
        modalDialogClose();
    }

}
