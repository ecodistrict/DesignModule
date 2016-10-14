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



    var PenetrationSelect = document.createElement('select');
    PenetrationSelect.className = 'test';

    var Opvolggedrag = document.createElement('select');
    var Hb = document.createElement('select');

    var options = ['0%','25%','50%','75%'];
    fillOptions(options, penetration_rateSelect);

    var options = ['0%','25%','50%','75%'];
    fillOptions(options, Opvolggedrag);

    var options = ['-10%','0%','10%'];
    fillOptions(options, Hb);

    function fillOptions(options, selectedSelect) {
      for (var i = 0; i < options.length; i++) {
        option = options[i];
        var opt = document.createElement('option');
        opt.value = option;
        opt.innerHTML = option;
        selectedSelect.appendChild(opt);
      }
    }

    f.appendChild(penetration_rateSelect);
    f.appendChild(Opvolggedrag);
    f.appendChild(Hb);


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
