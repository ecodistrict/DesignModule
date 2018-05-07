function openFormDialog(aTitle, aData, aContext) {
    var div = modalDialogCreate(aTitle);

    if (window.outerWidth < 500) {
        div.style.width = '100%';
        div.style.boxSizing = "border-box";
    } else {
        div.style.width = '400px';
    }

    var label, optionsArray;

    // build dialog form
    var f = div.appendChild(document.createElement('form'));
    f.id = 'simulationForm';
    f.name = 'simulationForm';
    f.context = aContext; // store context

    var errorLog = document.createElement('div');
    errorLog.id = 'errorLog';
    errorLog.style.display = 'none';
    errorLog.innerHTML = '';

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

    if (typeof aData === "undefined")
        return;

    for (var i = 0; i < aData.length; i++) {
        fillOptions(aData[i]);
    }

    var container, optionWrapper, opt;

    function fillOptions(arrayItem) {
        var formElement = arrayItem.formElement;
        var type = arrayItem.type;
        var required = arrayItem.required;
        var optionsArray = arrayItem.optionsArray;
        var labelText = arrayItem.labelText;
        var idName = arrayItem.idName;
        var extraOptions = arrayItem.extraOptions;
        var hidden = typeof arrayItem.hidden !== "undefined" ? arrayItem.hidden : false;

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
        } else if (formElement === 'checkbox') {
            container = document.createElement('div');
            container.id = idName;
            container.style.display = "none";
        } else if (formElement === 'textarea') {
            container = document.createElement('div');
            container.id = idName;
        } else if (formElement === 'slider') {
            container = document.createElement('div');
            container.id = idName;
            container.style.display = 'block';
            var range = document.createElement('div');
            range.id = 'range_' + idName;
        }

        label = document.createElement('label');
        label.innerHTML = labelText;
        label.hidden = hidden;
        f.appendChild(label);

        if (formElement === 'input') {
            var input = document.createElement('input');
            input.type = 'text';
            input.className = 'form-control';
            input.value = (extraOptions && typeof extraOptions.defaultValue != "undefined") ? extraOptions.defaultValue : labelText;
            input.name = idName;
            input.dataset.required = required;
            input.dataset.type = type;
            input.hidden = hidden;
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
                    selectedSelect.value = e.target.value;
                });
                opt.value = option;
                opt.innerHTML = option;
                optionWrapper.className = idName;
                container.appendChild(opt);
                optionWrapper.appendChild(button);
                if (extraOptions && (extraOptions.defaultValue && extraOptions.defaultValue == option) || (extraOptions.checked && extraOptions.checked == option)) {
                    opt.checked = true;
                    opt.value = option;
                    button.className = 'selected';
                }
            }
            optionWrapper.hidden = hidden;
            f.appendChild(optionWrapper);
        } else if (formElement === 'select') {
            var selectedDefault = false;
            //console.log(options);
            if (extraOptions && typeof extraOptions.defaultValue != "undefined") {
                selectedDefault = extraOptions.defaultValue;
            }
            for (var i2 = 0; i2 < optionsArray.length; i2++) {
                option = optionsArray[i2];
                var opt2 = document.createElement('option');
                if (option.constructor === Array) {
                    opt2.value = option[0];
                    opt2.innerHTML = option[1];
                } else {
                    opt2.value = option;
                    opt2.innerHTML = option;
                }
                if (selectedDefault && opt2.value == selectedDefault) {
                    opt2.selected = "selected";
                }
                container.appendChild(opt2);
            }
        } else if (formElement === 'checkbox') {
            optionWrapper = document.createElement('div');
            optionWrapper.id = idName + '-option-row';
            optionWrapper.className = 'form-control';
            for (var i3 = 0; i3 < optionsArray.length; i3++) {
                option = optionsArray[i3];
                var opt3 = document.createElement('input');
                var button3 = document.createElement('button');
                opt3.type = 'checkbox';
                opt3.name = idName;
                opt3.value = option;
                opt3.dataset.required = required;
                opt3.dataset.type = type;
                button3.innerText = option;
                button3.value = option;
                button3.type = "button";
                button3.addEventListener("click", function (e) {
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
                opt3.value = option;
                opt3.innerHTML = option;
                optionWrapper.className = idName;
                container.appendChild(opt3);
                optionWrapper.appendChild(button3);
            }
            f.appendChild(optionWrapper);
        } else if (formElement === 'textarea') {
            var textarea = document.createElement('textarea');
            textarea.type = 'text';
            textarea.className = 'form-control';
            textarea.placeholder = labelText;
            textarea.dataset.required = required;
            textarea.dataset.type = type;
            textarea.hidden = hidden;
            container.appendChild(textarea);
        } else if (formElement === 'slider') {
            sliderInput = document.createElement('input');
            sliderInput.type = 'text';
            sliderInput.className = 'form-control';
            sliderInput.placeholder = labelText;
            sliderInput.name = idName;
            sliderInput.id = 'input_' + idName;
            sliderInput.dataset.required = required;
            sliderInput.dataset.type = type;
            sliderInput.dataset.formElement = formElement;
            sliderInput.dataset.optionsArray = optionsArray;
            sliderInput.style.width = '90%';
            sliderInput.style.marginLeft = '5%';
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
                                return Math.round(value.replace(range.noUiSlider.options.extraOption, ''));
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
            range.style.width = '90%';
            range.style.margin = '40px auto 10px';
            sliderInput.addEventListener('change', function () {
                range.noUiSlider.set(this.value);
            });
            // todo: add? rangeSlider.hidden = hidden; 
            range.hidden = hidden;
            container.appendChild(range);
            sliderInput.hidden = hidden;
            container.appendChild(sliderInput);
        }
        container.hidden = hidden;
        f.appendChild(container);
        var br = document.createElement('br');
        br.hidden = hidden;
        f.appendChild(br);
    }

    // buttons section

    f.appendChild(document.createElement('hr'));
    f.appendChild(errorLog);
    var mddb = f.appendChild(document.createElement('div'));
    mddb.className = 'modalDialogDevideButtons';
    modelDialogAddButton(mddb, 'Cancel', modalDialogClose);
    modelDialogAddButton(mddb, 'Apply', modalDialogApply);
}

function modalDialogApply() {
    errors = false;
    formResult = [];
    elemValues = [];
    errorLog.innerHTML = '';
    var form = document.forms['simulationForm'];
    for (var i = 0; i < form.elements.length; i++) {
        var elem = form.elements[i];
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
                    for (var i3 = 0; i3 < formResult.length; i3++) {
                        if (formResult[i3].name === elem.name) {
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
            // todo: for backwards compatibility we also send the specific setupSimulation command!
            setupSimulation: { 
                parameters: formResult
            },
            // todo: only used in santos and response
            formResult: {
                id: DataManager.formDialogID,
                parameters: formResult
            },
            // new format
            type: "formResult",
            payload: {
                id: DataManager.formDialogID,
                parameters: formResult,
                context: form.context,
                selections: DataManager.selectedObjectIDs
            }
        };
        wsSend(sessionRequest);
        modalDialogClose();
    }
}
