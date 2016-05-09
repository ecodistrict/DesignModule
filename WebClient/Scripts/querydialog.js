function handleSelectByQuery(e) {

    // todo: build dialog based on measuresControl.options.selectCategories

    var div = modalDialogCreate('Select objects by query', 'Select objects based on values of their attributes');
    // build dialog form
    var f = div.appendChild(document.createElement('form'));
    f.id = 'selectByQueryForm';
    // datalist with  options
    var dl = f.appendChild(document.createElement('datalist'));
    dl.id = 'queryAttributes';
    dl.appendChild(document.createElement('option')).value = 'inhabitants';
    dl.appendChild(document.createElement('option')).value = 'height';
    dl.appendChild(document.createElement('option')).value = 'surface area';
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
        '<input type="text" placeholder="attribute name" list="queryAttributes" />'+
        '<select><option value="<">&lt;</option><option value="<=" selected>&le;</option><option value="=" selected>=</option><option value="<>">&ne;</option><option value=">">&gt;</option><option value=">=">&ge;</option><option value="in">in</option></select>'+
        '<input type="text" placeholder="value" />'+
        '<img src="Content/images/historyremove.png" class="queryDialogAddRemoveButton" onclick="selectByQueryRemoveLine(this)" title="..remove this line from the query" />' +
        '<img src="Content/images/domainadd.png" class="queryDialogAddRemoveButton" onclick="selectByQueryAddLine(this)" title="..add a new line to the query" />';
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
            currentQueryLines.lastElementChild.appendChild(img);
        }
    }
}