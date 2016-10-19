function openSimulationDialog(e) {

  // todo: build dialog based on measuresControl.options.selectCategories

  var div = modalDialogCreate('setup your simulation');

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

  fillOptions('input', false, 'Simulation name', 'simulationName');
  fillOptions('select', ['0%','25%','50%','75%'], "Penetration rate", "PenetrationRateSelect");
  fillOptions('select', ['0%','25%','50%','75%'], "Follow-on behavior", "followOn");
  fillOptions('select', ['-10%','0%','10%'], "Br", "Br");
  fillOptions('radio', ['-10%','0%','10%'], "radio", "radio");
  fillOptions('checkbox', ['-10%','0%','10%'], "checkbox", "checkbox");
  fillOptions('textarea', false, "textarea", "textarea");


  var container,optionWrapper, opt;

  function fillOptions(formElement, options, labelText, idName) {
    // input, select
    // checkbox, textaea, radio,
    if (formElement === 'input') {
      container = document.createElement('div');
      container.id = idName;
    } else if (formElement === 'radio') {
      container = document.createElement('div');
      container.id = idName;
      container.style.display = "none";
    } else if (formElement === 'select') {
      container = document.createElement('select');
      container.id = idName;
      container.style.display = "none";
    } else if (formElement === 'textarea') {
      container = document.createElement('div');
      container.id = idName + '-option-row';
      container.style.display = "none";
    } else if (formElement === 'checkbox') {
      container = document.createElement('div');
      container.id = idName + '-option-row';
      container.style.display = "none";
    }


    label = document.createElement('label');
    label.innerHTML = labelText;
    f.appendChild(label);

    if (formElement === 'input') {
      var input = document.createElement('input');
      input.type = 'text';
      input.className = 'form-control';
      input.placeholder = labelText;
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
      optionWrapper = document.createElement('div');
      optionWrapper.id = idName + '-option-row';
      optionWrapper.className = 'form-control';
      for (var i = 0; i < options.length; i++) {
        option = options[i];
        var opt = document.createElement('option');
        var button = document.createElement('button');
        button.innerText = option;
        button.value = option;
        button.name = idName;
        button.addEventListener("click", function (e) {
          e.preventDefault();
          for (var i = 0; i < e.target.parentElement.children.length; i++) {
            e.target.parentElement.children[i].classList.remove('selected');
          }
          e.target.className = 'selected';
          var selectedSelect = document.getElementById(e.target.name);
          selectedSelect.value = e.target.value
        });


        opt.value = option;
        opt.innerHTML = option;
        optionWrapper.className = idName;
        container.appendChild(opt);
        optionWrapper.appendChild(button);
      }
      f.appendChild(optionWrapper);
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
        button.innerText = option;
        button.value = option;
        button.name = idName;
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
      container.appendChild(textarea);
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




  // name
  // select -> 25% 50% 75% (penetratie nogwat)
  // select -> 25% 50% 75% (opvolggedrag)
  // select -> -10% 0% +10% (HB)



  // buttons section
  f.appendChild(document.createElement('hr'));
  var mddb = f.appendChild(document.createElement('div'));
  mddb.className = 'modalDialogDevideButtons';
  modelDialogAddButton(mddb, 'Cancel', modalDialogClose);
  modelDialogAddButton(mddb, 'Generate', queryDialogApply);

}

function queryDialogApply() {
  // build query
  var query = '';
  var lines = document.getElementById('queryDialogLines');
  for (var i = 0; i < lines.children.length; i++) {
    var line = lines.children[i];
    if (line.children[0].value != '' && line.children[2].value != '') {
      // 1=attribute, 2=operator, 3=value
      if (query != '')
      query += ' AND ';
      query += line.children[0].value + ' ' + line.children[1].value + ' ' + line.children[2].value;
    }
  }
  modalDialogClose();
}
