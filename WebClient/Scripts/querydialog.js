function handleSelectByQuery(e) {

    // todo: build dialog based on measuresControl.options.selectCategories

    var div = modalDialogCreate('Select objects by query', 'Select objects based on values of their attributes');

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
    f.id = 'selectByQueryForm';
    var mdl = f.appendChild(document.createElement('div'));
    mdl.id = 'queryDialogLines';
    mdl.appendChild(selectByQueryAddLine());
    // buttons section
    f.appendChild(document.createElement('hr'));
    var mddb = f.appendChild(document.createElement('div'));
    mddb.className = 'modalDialogDevideButtons';
    modelDialogAddButton(mddb, 'Apply', queryDialogApply);
    modelDialogAddButton(mddb, 'Cancel', modalDialogClose);
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
    signalSelectByQuery(query);
    modalDialogClose();
}

function selectByQueryAddLine(e) {
    // we are the last entry
    // add new entry

    var newQueryLine = document.createElement('div');
    newQueryLine.className = 'queryDialogLine';
    newQueryLine.innerHTML =
        '<select class="datalist"><option value="Inhabitants">Inhabitants</option><option value="Height">Height</option><option value="Surface_area">Surface area</option></select>'+
        '<select class="optionList"><option value="<">&lt;</option><option value="<=" selected>&le;</option><option value="=" selected>=</option><option value="<>">&ne;</option><option value=">">&gt;</option><option value=">=">&ge;</option><option value="in">in</option></select>'+
        '<input type="text" placeholder="value" />'+
        '<img class="removeQuery" src="Content/images/historyremove.png" class="queryDialogAddRemoveButton" onclick="selectByQueryRemoveLine(this)" title="..remove this line from the query" />' +
        '<img class="addQuery" src="Content/images/domainadd.png" class="queryDialogAddRemoveButton" onclick="selectByQueryAddLine(this)" title="..add a new line to the query" />';
    if (e) {
        e.parentNode.parentNode.appendChild(newQueryLine);
        // remove add-line-image from current entry
        e.parentNode.removeChild(e);
    }
    return newQueryLine;
}

function selectByQueryRemoveLine(e) {
    currentQueryLine = e.parentNode;
    currentQueryLines = currentQueryLine.parentNode;
    // we do not want to delete the last entry
    if (currentQueryLines.childElementCount > 1) {
        var addNewQueryLineImage = currentQueryLine.nextElementSibling == null;
        currentQueryLine.parentNode.removeChild(currentQueryLine);

        if (addNewQueryLineImage) {
            var img = document.createElement('img');
            img.src = 'Content/images/domainadd.png';
            img.onclick = function () { selectByQueryAddLine(img); };
            img.style['vertical-align'] = 'text-bottom';
            img.title = 'Add a new line to the query';
            img.className = 'addQuery';
            currentQueryLines.lastElementChild.appendChild(img);
        }
    }
}
