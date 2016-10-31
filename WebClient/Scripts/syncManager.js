var SyncManager = {
    id: null,
    presenter: false,
    viewer: false,
    viewerControl: null,
    group: "",
    syncs: {
        move: true,
        time: true
    },
    sessionData: {
        center: null,
        zoom: null,
        time: null
    },
    defaultValues: {
        moveSync: true,
        simSync: true,
        viewerControl: {
            orientX: "right",
            orientY: "bottom",
            x: 60,
            y: 60,
        }
    },
    handleCCVMessage: function(message) {
        if (typeof message.move !== "undefined")
        {
            SyncManager.sessionData.center = message.move.center;
            SyncManager.sessionData.zoom = message.move.zoom;
            if (SyncManager.syncs.move)
            {
                map.flyTo(message.move.center, message.move.zoom);
            }
        }
    },
    handleCCPMessage: function(message) {
        if (typeof message.initRequest !== "undefined")
        {
            if (SyncManager.presenter)
            {
                SyncManager.sendMoveMessage();
            }
        }
    },
    handleControlMessage: function (message) {
        if (typeof message.result != "undefined") {
            if (message.result == "success") {
                if (message.command == "presenteron") {
                    SyncManager.group = message.group;
                    SyncManager.setPresenter();
                }
                else if (message.command == "vieweron") {
                    SyncManager.group = message.group;
                    SyncManager.setViewer();
                }

                if (SyncManager.viewerPresenterDialogInfo)
                {
                    SyncManager.viewerPresenterDialogInfo = null;
                    modalDialogClose();
                }
            }
            else {
                if (SyncManager.viewerPresenterDialogInfo)
                {
                    if (message.result == "groupalreadyexists")
                        SyncManager.viewerPresenterDialogInfo.errorDiv.innerHTML = "Can't create, session name already in use<p>";
                    else if (message.result == "groupdoesnotexist")
                        SyncManager.viewerPresenterDialogInfo.errorDiv.innerHTML = "Can't join, session does not exist<p>";
                    else
                        SyncManager.viewerPresenterDialogInfo.errorDiv.innerHTML = "Received error: " + message.result + "<p>";
                }
            }
        }
        else if (message.command == "groupclose" && message.group == SyncManager.group)
        {
            if (SyncManager.presenter)
                SyncManager.removePresenter();
            else if (SyncManager.viewer)
                SyncManager.removeViewer();
        }
    },
    requestPresenter: function (groupid)
    {
        var obj = {
            type: "groupcontrol",
            payload: {
                command: "presenteron",
                group: groupid
            }
        }
        wsSend(obj);
    },
    requestViewer: function (groupid)
    {
        var obj = {
            type: "groupcontrol",
            payload: {
                command: "vieweron",
                group: groupid
            }
        }
        wsSend(obj);
    },
    setPresenter: function()
    {
        //todo: remove for real implementation
        if (DataManager.sessionInfo.session != "SSM")
            return;

        if (SyncManager.presenter)
            return;

        SyncManager.removeViewer();

        SyncManager.presenter = true;
        SyncManager.updateSessionData();
        map.on("move", SyncManager.handleMove);
    },
    removePresenter: function()
    {
        if (!SyncManager.presenter)
            return;
        map.off("move", SyncManager.handleMove);
        SyncManager.presenter = false;
        var obj = {
            type: "groupcontrol",
            payload: {
                command: "presenteroff",
                group: SyncManager.group
            }
        }
        wsSend(obj);

        SyncManager.group = "";
    },
    setViewer: function()
    {
        //todo: remove for real implementation
        if (DataManager.sessionInfo.session.toLowerCase() != "ssm")
            return;

        if (SyncManager.viewer)
            return;

        SyncManager.removePresenter();

        SyncManager.viewer = true;

        SyncManager.disableMove();
        SyncManager.disableTime();
        SyncManager._addViewerControl();
        SyncManager.sendInitRequest();
    },
    removeViewer: function()
    {
        if (!SyncManager.viewer)
            return;

        SyncManager.viewer = false;

        SyncManager._removeViewerControl();
        SyncManager.enableMove();
        SyncManager.enableTime();
        var obj = {
            type: "groupcontrol",
            payload: {
                command: "vieweroff",
                group: SyncManager.group
            }
        }
        wsSend(obj);

        SyncManager.group = "";
    },
    enableMove: function () {
        SyncManager.syncs.move = false;
        map.dragging.enable();
        map.touchZoom.enable();
        map.doubleClickZoom.enable();
        map.scrollWheelZoom.enable();
        map.boxZoom.enable();
        map.zoomControl.enable();
    },
    disableMove: function () {
        SyncManager.syncs.move = true;
        map.dragging.disable();
        map.touchZoom.disable();
        map.doubleClickZoom.disable();
        map.scrollWheelZoom.disable();
        map.boxZoom.disable();
        map.zoomControl.disable();
    },
    enableTime: function () {
        SyncManager.syncs.time = false;
        startControl.enable();
        //todo: implement
    },
    disableTime: function () {
        SyncManager.syncs.time = true;
        startControl.disable();
        //todo: implement
    },
    updateSessionData: function()
    {
        SyncManager.sessionData = {
            center: map.getCenter(),
            zoom: map.getZoom(),
            time: null //todo get time
        }
    },
    handleMove: function (e) {
        SyncManager.updateSessionData();
        SyncManager.sendMoveMessage();
    },
    _addViewerControl: function()
    {
        if (SyncManager.viewerControl == null)
        {
            //initialization!
            var container = document.body.appendChild(document.createElement("div"));
            SyncManager.viewerControl = container;
            container.className = "viewerControlContainer";
            container.style.width = "120px";
            container.style.height = "62px";
            container.style[SyncManager.defaultValues.viewerControl.orientX] = SyncManager.defaultValues.viewerControl.x + "px";
            container.style[SyncManager.defaultValues.viewerControl.orientY] = SyncManager.defaultValues.viewerControl.y + "px";
            container.style.zIndex = "1200";

            var buttonContainer = container.appendChild(document.createElement("div"));
            buttonContainer.className = "viewerControlButtonContainer";

            var moveButton = buttonContainer.appendChild(document.createElement("div"));
            moveButton.className = "viewerControlMoveButton viewerControlButton";
            moveButton.addEventListener("click", SyncManager.toggleMove);
            if (!SyncManager.syncs.move)
            {
                moveButton.style.opacity = "0.6";
            }

            var timeButton = buttonContainer.appendChild(document.createElement("div"));
            timeButton.className = "viewerControlTimeButton viewerControlButton";
            timeButton.addEventListener("click", SyncManager.toggleTime);
            if (!SyncManager.syncs.time)
            {
                timeButton.style.opacity = "0.6";
            }
        }
        else
        {
            SyncManager.viewerControl.style.visibility = "visible";
        }
    },

    _addViewerPresenterControl: function()
    {

    },

    _removeViewerControl: function () {
        if (SyncManager.viewerControl)
            SyncManager.viewerControl.style.visibility = "hidden";
    },
    toggleMove: function (e) {
        SyncManager.syncs.move = !SyncManager.syncs.move;
        if (SyncManager.syncs.move)
        {
            SyncManager.disableMove();
            if (SyncManager.sessionData.center)
            {
                if (SyncManager.sessionData.zoom)
                    map.flyTo(SyncManager.sessionData.center, SyncManager.sessionData.zoom);
                else
                    map.flyTo(SyncManager.sessionData.center);
            }
            e.currentTarget.style.opacity = "1";
        }
        else
        {
            SyncManager.enableMove();
            e.currentTarget.style.opacity = "0.6";
        }
    },
    toggleTime: function(e) {

        SyncManager.syncs.time = !SyncManager.syncs.time;
        if (SyncManager.syncs.time) {
            SyncManager.disableTime();
            e.currentTarget.style.opacity = "1";
        }
        else {
            SyncManager.enableTime();
            e.currentTarget.style.opacity = "0.6";
        }
    },
    sendMoveMessage: function () {
        var obj = {
            type: "ccv",
            group: SyncManager.group,
            payload: {
                move: {
                    center: SyncManager.sessionData.center,
                    zoom: SyncManager.sessionData.zoom
                }
            }
        };
        wsSend(obj);
    },
    sendInitRequest: function () {
        var obj = {
            type: "ccp",
            group: SyncManager.group,
            payload: {
                initRequest: true
            }
        };
        wsSend(obj);
    }
    //test functions!
    ,
    testPresenter: function () {
        SyncManager.requestPresenter("pietje123");
    },
    testViewer: function () {
        SyncManager.requestViewer("pietje123");
    }

}

L.Control.PresenterViewer = L.Control.extend({
    options: {
        collapsed: true,
        position: 'topright',
        autoZIndex: true,
        hideSingleBase: false
    },

    initialize: function (options) {
        L.setOptions(this, options);
    },

    onAdd: function (map) {
        this._initLayout();
        this._update();
        this._map = map;
        return this._container;
    },

    onRemove: function () {
        //todo, do we want to remove controls?
    },

    _initLayout: function () {
        var className = 'leaflet-control-pv',
            container = this._container = L.DomUtil.create('div', className);

        // makes this work on IE touch devices by stopping it from firing a mouseout event when the touch is released
        container.setAttribute('aria-haspopup', true);

        L.DomEvent.disableClickPropagation(container);
        if (!L.Browser.touch) {
            L.DomEvent.disableScrollPropagation(container);
        }

        container.addEventListener("click", this.showPresenterViewerDialog);

        var link = this._categoriesLink = L.DomUtil.create('a', className + '-toggle', container);
        link.href = '#';
        link.title = 'Setup a Presenter session or join a session as viewer';
    },

    showPresenterViewerDialog: function () {
        var div = modalDialogCreate('Setup Presenter/Viewer session');
        div.id = "presenterViewerDialog";

        var errorDiv = div.appendChild(document.createElement("div"));
        errorDiv.className = "presenterViewerError";

        var form = div.appendChild(document.createElement("form"));
        form.id = "presenterViewerForm";

        var viewerPresenterDiv = form.appendChild(document.createElement("div"));

        var viewerRadio = viewerPresenterDiv.appendChild(document.createElement("input"));
        viewerRadio.type = "radio";
        viewerRadio.value = "viewer";
        viewerRadio.name = "viewerPresenterRadio";
        viewerRadio.className = "presenterViewerRadio";
        viewerRadio.checked = true;

        var viewerText = viewerPresenterDiv.appendChild(document.createElement("span"));
        viewerText.className = "presenterViewerText";
        viewerText.innerHTML = "Viewer  ";

        var presenterRadio = viewerPresenterDiv.appendChild(document.createElement("input"));
        presenterRadio.type = "radio";
        presenterRadio.value = "presenter";
        presenterRadio.name = "viewerPresenterRadio";
        presenterRadio.className = "presenterViewerRadio";

        var presenterText = viewerPresenterDiv.appendChild(document.createElement("span"));
        presenterText.className = "presenterViewerText";
        presenterText.innerHTML = "Presenter<P>";

        var groupDiv = form.appendChild(document.createElement("div"));

        var groupText = groupDiv.appendChild(document.createElement("div"));
        groupText.className = "presenterViewerText";
        groupText.innerHTML = "Session name:";

        var groupInput = groupDiv.appendChild(document.createElement("input"));
        groupInput.type = "text";
        groupInput.className = "presenterViewerTextInput";

        SyncManager.viewerPresenterDialogInfo = {
            errorDiv: errorDiv,
            viewerRadio: viewerRadio,
            presenterRadio: presenterRadio,
            groupInput: groupInput
        }

        var whiteSpace = groupDiv.appendChild(document.createElement("p"));

        var mddb = form.appendChild(document.createElement("div"));
        mddb.className = "modalDialogDevideButtons";

        modelDialogAddButton(mddb, 'Cancel', function () {
            SyncManager.viewerPresenterDialogInfo = null;
            modalDialogClose();
        });
        modelDialogAddButton(mddb, 'Connect', function () {

            if (SyncManager.viewerPresenterDialogInfo)
            {
                if (SyncManager.viewerPresenterDialogInfo.groupInput.value == "") {
                    SyncManager.viewerPresenterDialogInfo.errorDiv.innerHTML = "Please provide a session name<p>";
                }
                else {
                    if (SyncManager.viewerPresenterDialogInfo.viewerRadio.checked)
                        SyncManager.requestViewer(SyncManager.viewerPresenterDialogInfo.groupInput.value);
                    else
                        SyncManager.requestPresenter(SyncManager.viewerPresenterDialogInfo.groupInput.value);
                }
            }
            
        });


    },

    _update: function () {

    },

    _expand: function () {
        //L.DomEvent.addListener(this._container, 'touchmove', L.DomEvent.stopPropagation);
        //if (this.hasElements()) {
        //    L.DomUtil.addClass(this._container, 'leaflet-control-details-expanded');
        //    this._form.style.height = null;
        //    var acceptableHeight = this._map._size.y - (this._container.offsetTop + 50);
        //    if (acceptableHeight < this._form.scrollHeight) {
        //        L.DomUtil.addClass(this._form, 'leaflet-control-details-scrollbar');
        //        this._form.style.height = acceptableHeight + 'px';
        //    }
        //    else {
        //        L.DomUtil.removeClass(this._form, 'leaflet-control-details-scrollbar');
        //    }
        //}
    },

    _collapse: function () {
        L.DomUtil.removeClass(this._container, 'leaflet-control-details-expanded');
    },
});

L.control.PresenterViewer = function (categories, options) {
    return new L.Control.PresenterViewer(categories, options);
};