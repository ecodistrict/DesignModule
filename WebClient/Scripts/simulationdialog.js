function openSimulationDialog(e) {

    // todo: build dialog based on measuresControl.options.selectCategories

    var div = modalDialogCreate('setup your simulation');

    if (window.outerWidth < 500) {
      div.style.width = '100%';
      div.style.boxSizing = "border-box";
      div.setAttribute("id", "phone");
      //div.style.margin = '5% auto';
    } else {
      div.style.width = '400px';
      //div.style.margin = '5% auto';
    }


    // build dialog form
    var f = div.appendChild(document.createElement('form'));
    f.id = 'simulationForm';


    var label, options;

    var Penetration_rateSelect = document.createElement('select');
    Penetration_rateSelect.id = 'PenetrationRateSelect';
    Penetration_rateSelect.style.display = "none";

    var Opvolggedrag = document.createElement('select');
    Opvolggedrag.id = 'followOn';
    Opvolggedrag.style.display = "none";

    var Hb = document.createElement('select');
    Hb.id = 'Br';
    Hb.style.display = "none";

    options = ['0%','25%','50%','75%'];
    fillOptions(options, Penetration_rateSelect, "Penetration rateSelect", "PenetrationRateSelect", "select");

    // options = ['0%','25%','50%','75%'];
    options = ['0%','25%','50%','75%'];
    fillOptions(options, Opvolggedrag, "Follow-on behavior", "followOn", "select");

    options = ['-10%','0%','10%'];
    fillOptions(options, Hb, "Br", "Br", "select");

    function fillOptions(options, selectedSelect, labelText, idName, formElement) {


      var optionWrapper = document.createElement('div');

      if (formElement === 'select') {
        optionWrapper.id = idName + '-option-row';
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
              e.target.parentElement.children[i].classList = '';
            }
            e.target.className = 'selected';

            var selectedSelect = document.getElementById(e.target.name);
            selectedSelect.value = e.target.value
          });


          opt.value = option;
          opt.innerHTML = option;
          selectedSelect.className = idName;
          selectedSelect.appendChild(opt);
          optionWrapper.appendChild(button);
        }
      }



      label = document.createElement('label');
      label.innerHTML = labelText;
      f.appendChild(label);
      f.appendChild(optionWrapper);
      f.appendChild(selectedSelect);
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
