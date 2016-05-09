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
    var p = document.createElement('p');
    p.appendChild(document.createTextNode(aDescription));
    div.appendChild(p);
    return div;
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
    return mddbutton
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
    // remove dialog contents
    while (dialog.firstChild) {
        dialog.removeChild(dialog.firstChild);
    }
}

// handle escape key for closing dialogs etc.
document.addEventListener('keyup', function (e) {
    if (e.keyCode == 27) {
        var dialog = document.getElementById('modalDialog');
        // test if modalDialog is visible
        if (dialog.style.opacity > 0) {
            modalDialogClose();
            e.stopPropagation();
        }
    }
});