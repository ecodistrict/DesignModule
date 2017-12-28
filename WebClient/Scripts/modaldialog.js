function modalDialogCreate(aTitle, aDescription) {
    var dialog = document.getElementById('modalDialog');

    // show dialog
    dialog.style.opacity = 1;
    dialog.style['pointer-events'] = 'auto';
    // build dialog
    var div = document.createElement('div');
    dialog.appendChild(div);
    modelDialogAddCloseCross(div);
    var h2 = document.createElement('h2');
    h2.appendChild(document.createTextNode(aTitle));
    div.appendChild(h2);
    if (typeof aDescription !== 'undefined') {
        var p = document.createElement('p');
        p.appendChild(document.createTextNode(aDescription));
        div.appendChild(p);
    }
    div.focus();
    // dialog.focus(); //removes focus from the element that was clicked on!
    return div;
}

function createRequestDialog(aTitle, aDescription, type, buildFunction)
{
    var id = Date.now().toString(); //todo: make better id?
    var request = { dialogDataRequest: {id: id, type: type} };

    var dialogDiv = modalDialogCreate(aTitle, aDescription);
    dialogDiv.requestId = id;
    dialogDiv.buildResponseDialog = buildFunction;
    dialogDiv.id = "requestDialogDiv";
    
    var loaderDiv = dialogDiv.appendChild(document.createElement('div'));
    loaderDiv.className = 'loader';

    var loadingText = dialogDiv.appendChild(document.createElement('div'));
    loadingText.className = 'loaderText';
    loadingText.innerHTML = 'Loading...';

    var mddb = dialogDiv.appendChild(document.createElement('div'));
    mddb.className = 'modalDialogDevideButtons';
    modelDialogAddButton(mddb, 'Close', modalDialogClose);

    wsSend(request);
}

function handleDataResponse(response)
{
    var div = document.getElementById('requestDialogDiv');

    if (div != null && typeof div.requestId !== 'undefined' && div.requestId == response.id)
    {
        div.innerHTML = '';
        div.buildResponseDialog(div, response.data);
    }
}

function modelDialogAddButton(aParent, aTitle, aOnClick) {

    var mddbutton = aParent.appendChild(document.createElement('div'));
    mddbutton.className = 'modalDialogButton';
    var i = document.createElement('input');
    i.className = 'button';
    i.type = 'button';
    i.value = aTitle;
    if (aOnClick)
        i.onclick = aOnClick;
    mddbutton.appendChild(i);
    return mddbutton;
}

function modelDialogAddCloseCross(aParentDiv) {
    var div = aParentDiv.appendChild(document.createElement('div'));
    div.className = 'modalDialog-close';
    div.innerHTML = '&#x2715;';
    div.onclick = modalDialogClose;
}

function modalDialogClose() {
    var dialog = document.getElementById('modalDialog');
    dialog.style.opacity = 0;
    dialog.style['pointer-events'] = 'none';
    dialog.requestId = '';
    dialog.buildFunction = function (a, b) { };
    // remove dialog contents
    while (dialog.firstChild) {
        dialog.removeChild(dialog.firstChild);
    }
}

// handle escape key for closing dialogs etc.
document.addEventListener('keyup', function (e) {

    var dialog;
    if (e.keyCode == 13) {

        //modalDialogButton = document.getElementsByClassName('modalDialogButton')[1];

        dialog = document.getElementById('modalDialog');
        // test if modalDialog is visible
        if (dialog.style.opacity > 0) {
            var modalButtons = document.querySelectorAll(".modalDialogButton .button");
            var succesButton = false;
            for (var i = 0; i < modalButtons.length; i++) {
                if (modalButtons[i].value == "Apply") {
                    succesButton = modalButtons[i];
                    break;
                }
            }
            if (succesButton) {
                succesButton.click();
                // console.log(succesButton);
            }

            // modalDialogClose();
            // queryDialogApply();
            // modalDialogButton.click();

        }
        e.stopPropagation();
    }
    else if (e.keyCode == 27) {
        dialog = document.getElementById('modalDialog');
        // test if modalDialog is visible
        if (dialog.style.opacity > 0) {
            modalDialogClose();
            e.stopPropagation();
        }
    }
});
