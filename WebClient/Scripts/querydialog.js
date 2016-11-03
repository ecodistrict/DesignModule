function handleSelectByQuery(e) {

  // todo: build dialog based on measuresControl.options.selectCategories

  var div = modalDialogCreate('Select objects by query');

  // DataManager.queryDialogData = {};
  // DataManager.queryDialogData['building'] = ['optie1','optie2'];
  // DataManager.queryDialogData['building'] = ['optie3','optie4'];
  // DataManager.queryDialogData['space'] = ['optie5','optie6'];

  if (window.outerWidth < 500) {
    div.style.width = '100%';
    div.style.boxSizing = "border-box";
    //div.style.margin = '5% auto';
  } else {
    div.style.width = '700px';
    //div.style.margin = '5% auto';
  }
  cat_container = document.createElement('div');
  cat_container.id = 'cat_container';
  categories_select = document.createElement('select');
  categories_select.name = 'categories';
  categories_select.classList.add('form-control');
  query = DataManager.queryDialogData;
  for (var key in query) {
    cat_option = document.createElement('option');
    cat_option.value = key;
    cat_option.innerText = key;
    categories_select.appendChild(cat_option);
  }

  var catText = document.createElement('span');
  catText.innerText = 'Select objects, based on values, of type';

  cat_container.appendChild(catText);
  cat_container.appendChild(categories_select);



  // build dialog form
  var f = div.appendChild(document.createElement('form'));
  f.id = 'selectByQueryForm';
  f.appendChild(cat_container);
  var mdl = f.appendChild(document.createElement('div'));
  mdl.id = 'queryDialogLines';
  mdl.appendChild(selectByQueryAddLine());
  // buttons section
  f.appendChild(document.createElement('hr'));
  var mddb = f.appendChild(document.createElement('div'));
  mddb.className = 'modalDialogDevideButtons';


  modelDialogAddButton(mddb, 'Cancel', modalDialogClose);
  modelDialogAddButton(mddb, 'Apply', queryDialogApply);

}
function removeOptions(data) {
  for (var i = 0; i < data.length + 1; i++) {
    data[0].remove();
  }
}
function filldatalistOptions(datalist){

  for (var i = 0; i < DataManager.queryDialogData[categories_select.value].length; i++) {
    datalistOption = document.createElement('option');
    datalistOption.value = DataManager.queryDialogData[categories_select.value][i];
    datalist.appendChild(datalistOption);
  }

}

function queryDialogApply() {

var sessionRequest = {};

  sessionRequest.selectObjects = {};
  sessionRequest.selectObjects.mode = '=';
  sessionRequest.selectObjects.selectCategories = [categories_select.value];
  sessionRequest.selectObjects.query = [];

  // build query
  var fieldName, value, operator;
  var lines = document.getElementById('queryDialogLines');
  for (var i = 0; i < lines.children.length; i++) {
    var line = lines.children[i];
    if (line.children[0].value != '' && line.children[2].value != '') {
      fieldName = line.children[0].value;
      operator = line.children[2].value;
      value = line.children[3].value;
      field = { 'field': fieldName, 'operator': operator, 'value': value };
      // 0=attribute, 2=operator, 3=value
      sessionRequest.selectObjects.query.push(field);
    }
  }

  console.log(sessionRequest);
  wsSend(sessionRequest);
  modalDialogClose();
}
var categories = '';
function selectByQueryAddLine(e) {
  // we are the last entry
  // add new entry

  //
  var newQueryLine = document.createElement('div');
  newQueryLine.className = 'queryDialogLine';
  //
  datalistInput = document.createElement('input');
  datalistInput.name = 'datalist_' + document.getElementsByClassName('queryDialogLine').length;
  datalistInput.setAttribute('list','datalist_' + document.getElementsByClassName('queryDialogLine').length);
  datalistInput.classList.add('form-control');

  datalist = document.createElement('datalist');
  datalist.classList.add('datalist','form-control');

  datalist.id = 'datalist_' + document.getElementsByClassName('queryDialogLine').length;

  if (!categories_select.value && document.getElementById('categories')) {
    categories_select.value = document.getElementById('categories').children[0].value;
  }
  if (categories_select.value) {
      filldatalistOptions(datalist);



  operators = document.createElement('select');
  operators.name = '';
  operators.classList.add('optionList','form-control');

  oparatorsOptions = [{'code':'<','text':'&lt;'},{'code':'<=','text':'&le;'},{'code':'=','text':'='},{'code':'<>','text':'&ne;'},{'code':'>','text':'&gt;'},{'code':'>=','text':'&ge;'},{'code':'in','text':'in'}]
  operatorSelect = document.createElement('select');
  operatorSelect.classList.add('form-control');
  for (var i = 0; i < oparatorsOptions.length; i++) {
    operatorOption = document.createElement('option');
    operatorOption.value = oparatorsOptions[i].code;
    operatorOption.innerHTML = oparatorsOptions[i].text;
    if (oparatorsOptions[i].code === '=') {
      operatorOption.selected = true;
    }
    operatorSelect.appendChild(operatorOption);
  }

  addqueryimg = document.createElement('img');
  addqueryimg.classList.add('addQuery','queryDialogAddRemoveButton');
  addqueryimg.src = 'Content/images/domainadd.png';
  addqueryimg.onclick = function () {
    selectByQueryAddLine(this);
  };
  addqueryimg.title = '..add a new line to the query';

  removequeryimg = document.createElement('img');
  removequeryimg.classList.add('removeQuery','queryDialogAddRemoveButton');
  removequeryimg.src = 'Content/images/historyremove.png';
  removequeryimg.onclick = function () {
    selectByQueryRemoveLine(this);
  };
  removequeryimg.title = '..remove this line from the query';

  inputvalue = document.createElement('input');
  inputvalue.type = 'text';
  inputvalue.placeholder = 'value';
  inputvalue.classList.add('form-control');


  // newQueryLine.appendChild(queries);
  newQueryLine.appendChild(datalistInput);
  newQueryLine.appendChild(datalist);
  newQueryLine.appendChild(operatorSelect);
  newQueryLine.appendChild(inputvalue);
  newQueryLine.appendChild(addqueryimg);
  newQueryLine.appendChild(removequeryimg);

} else {
  warningText = document.createElement('h4');
  warningText.innerText = 'There are no categories available';
  warningText.style.color = 'red';
  newQueryLine.appendChild(warningText);
}



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
      img.title = 'Add a new line to the query';
      img.className = 'addQuery';
      currentQueryLines.lastElementChild.appendChild(img);
    }
  }
}
