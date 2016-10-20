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
            }
            else {
                //todo: handle error!
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
        if (DataManager.sessionInfo.session != "SSM")
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
            container.style.visibility = "visible";
        }
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