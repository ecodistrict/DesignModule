function openSimulationDialog(e) {

  // todo: build dialog based on measuresControl.options.selectCategories

  var div = modalDialogCreate('Setup your simulation');

  if (window.outerWidth < 500) {
    div.style.width = '100%';
    div.style.boxSizing = "border-box";
    //div.style.margin = '5% auto';
  } else {
    div.style.width = '400px';
    //div.style.margin = '5% auto';
  }

  var label, options;

  // build dialog form
  var f = div.appendChild(document.createElement('form'));
  f.id = 'simulationForm';
  f.name = 'simulationForm';

  var errorLog = document.createElement('div');
  errorLog.id = 'errorLog';
  errorLog.style.display = 'none';
  errorLog.innerHTML = '';

  // fillOptions(element, required, options, labelText, idName, extraOptions['steps','postfix']);

  fillOptions('input', 'y', false, 'Scenario name', 'scenarioName', false);
  // fillOptions('slider', 'y', ['1','100'], "Slider", "Sliderkeuze", ['1','']);
  // fillOptions('select', 'y', ['0%','25%','50%','75%'], "Penetration rate", "PenetrationRateSelect", false);
  // fillOptions('select', 'y', ['0%','25%','50%','75%'], "Follow-on behavior", "followOn", false);

  fillOptions('slider', 'y', ['0','100'], "Penetration rate", "PenetrationRateSelect", ['1','%']);
  fillOptions('slider', 'y', ['0','100'], "Follow-on behavior", "followOn", ['1','%']);
  fillOptions('select', 'y', [['A','OD Basis'],['B','OD variant 1'],['C','OD variant 2']], "Origin Destination matrix", "MatrixChoice", false);

  var container,optionWrapper, opt;

  function fillOptions(formElement, required, options, labelText, idName, extraOptions) {
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
      container.appendChild(input);
    } else if (formElement === 'radio') {
      optionWrapper = document.createElement('div');
      optionWrapper.id = idName + '-option-row';
      optionWrapper.className = 'form-control';
      for (var i = 0; i < options.length; i++) {
        option = options[i];
        var opt = document.createElement('input');
        var button = document.createElement('button');
        opt.type = 'radio';
        opt.name = idName;
        opt.value = option;
        opt.dataset.required = required;
        button.innerText = option;
        button.value = option;
        button.name = idName;
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
      }
      f.appendChild(optionWrapper);
    } else if (formElement === 'select') {

      console.log(options);

      for (var i = 0; i < options.length; i++) {
        option = options[i];
        var opt = document.createElement('option');
        opt.value = option;
        opt.innerHTML = option;
        container.appendChild(opt);
      }

    } else if (formElement === 'checkbox') {
      optionWrapper = document.createElement('div');
      optionWrapper.id = idName + '-option-row';
      optionWrapper.className = 'form-control';
      for (var i = 0; i < options.length; i++) {
        option = options[i];
        var opt = document.createElement('input');
        var button = document.createElement('button');
        opt.type = 'checkbox';
        opt.name = idName;
        opt.value = option;
        opt.dataset.required = required;
        button.innerText = option;
        button.value = option;
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
      container.appendChild(textarea);
    } else if (formElement === 'slider') {

      sliderInput = document.createElement('input');
      sliderInput.type = 'text';
      sliderInput.className = 'form-control';
      sliderInput.placeholder = labelText;
      sliderInput.name = idName;
      sliderInput.id = 'input_' + idName;
      sliderInput.dataset.required = required;
      sliderInput.style.width = '100%';
      sliderInput.style.boxSizing = 'border-box';
      var rangeSlider = noUiSlider.create(range, {
        connect: true, // Display a colored bar between the handles
        start: parseInt(options[0]),
        step: parseInt(extraOptions[0]),
        behaviour: 'tap',
        tooltips: true,
        range: {
          'min': parseInt(options[0]),
          'max': parseInt(options[1])
        },
        format: {
          to: function ( value ) {
            return Math.round(value) + extraOptions[1];
          },
          from: function ( value ) {
            return Math.round(value.replace(extraOptions[1], ''));
          }
        }
      });

      rangeSlider.on('update', function( values, handle ) {

        if (extraOptions[1].length > 0) {
          formattedValue = values[handle].replace(extraOptions[1], '');
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



      sliderInput.addEventListener('change', function(){
        range.noUiSlider.set(this.value);
      });



      // sliderInput.style.display = 'none';

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
  modelDialogAddButton(mddb, 'Apply', queryDialogApply);
}

function queryDialogApply() {
  errors = false;
  formResult = [];
  elemValues = [];
  errorLog.innerHTML = '';
  for (var i = 0; i < document.forms['simulationForm'].elements.length; i++) {
    var elem = document.forms['simulationForm'].elements[i];
    if (elem.type === 'text') {
      if (elem.dataset.required === 'y') {
        if (elem.value === '') {
          errors = true;
          elem.classList.add('empty');
          errorLog.style.display = 'block';
          errorLog.innerHTML = errorLog.innerHTML + '<span>' + elem.placeholder + ' is not correct!' + '</span>';
        } else {
          formResult.push({name:elem.name, value:elem.value});
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
          formResult.push({name:elem.name, value:elem.value});
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
          formResult.push({name:elem.name, value:elem.value});
          elem.classList.remove('empty');
        }
      }
    } else if (elem.type === 'radio') {

      if (elem.dataset.required === 'y') {
        if (elem.checked) {
          errors = false;
          formResult.push({name:elem.name, value:elem.value});
        }
        if (document.getElementById('simulationForm')[elem.name].value === '') {
          options = document.getElementById(elem.name + '-option-row');
          options.classList.add('empty');
          errorLog.style.display = 'block';
        } else {
          options = document.getElementById(elem.name + '-option-row');
          options.classList.remove('empty');
          errorLog.style.display = 'block';
        }
      }

    } else if (elem.type === 'checkbox') {

      if (elem.dataset.required === 'y') {

        var checkboxes = document.getElementsByName(elem.name);
        var vals = "";
        for (var i2=0, n=checkboxes.length;i2<n;i2++){
          if (checkboxes[i2].checked)
          {
            vals += ","+checkboxes[i2].value;
          }
        }
        if (vals){
          vals = vals.substring(1);
        }

        if (elem.checked) {

          found = false;
          for (var i2 = 0; i2 < formResult.length; i2++) {
            if (formResult[i2].name === elem.name) {
              found = true;
              options = document.getElementById(elem.name + '-option-row');
              options.classList.remove('empty');
              errors = false;
            }
          }
          if (!found) {
            formResult.push({name:elem.name, value:vals});
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
    console.log(formResult);
    modalDialogClose();
  }

}
