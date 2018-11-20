// web socket connection
var ws;
var wsLastConnectDateTime;
var wsLookup = {
    // message handlers
    measures: function (payload) {
        measuresControl.resetMeasures(payload);
    },
    domains: function (payload) {
        // first domains after login
        domainsControl.resetDomains(payload);
        // remove all basic overlay layers from layers control (and with that from map)
        // removing so iterate from end to start
        for (var i = layerControl._layers.length - 1; i >= 0; i--) {
            var lcl = layerControl._layers[i];
            if (lcl.overlay) {
                layerControl.removeLayer(lcl.layer);
            }
        }
        // add basic layers from domains
        for (var domainName in payload) {
            var domain = payload[domainName];
            for (var id2 in domain.layers) {
                var layer = domain.layers[id2];
                if (layer.basic)
                    addBasicLayer(layer);
            }
        }
    },
    updatedomains: function (payload) {
        // update of domains (on scenario change)
        removeAllDomainLayers();
        legendControl.clearLegend(true);
        crd.reset(false, false, false);
        domainsControl.updateDomains(payload);
        // replace all basic overlay layers from layers control (and with that from map)
        // removing so iterate from end to start
        for (var i = layerControl._layers.length - 1; i >= 0; i--) {
            var lcl = layerControl._layers[i];
            if (lcl.overlay) {
                layerControl.removeLayer(lcl.layer);
            }
        }
        // add basic layers from domains
        for (var domainName in payload) {
            var domain = payload[domainName];
            for (var id2 in domain.layers) {
                var layer = domain.layers[id2];
                if (layer.basic)
                    addBasicLayer(layer);
            }
        }
    },
    refresh: function (payload) {
        LayerManager.UpdateData(payload);
        var elementID = payload.id;
        if (typeof payload.tiles !== "undefined" && payload.tiles != '') {
            detailsControl.updateTilesURL(payload);
            // todo: if basic layer -> check and update that url!
        }
        else if (typeof payload.ref !== "undefined" && payload.ref.tiles !== "undefined" && payload.ref.tiles != '') {
            detailsControl.updateTilesURL(payload);
        }
        else if (typeof payload.diff !== "undefined" && payload.diff.tiles !== "undefined" && payload.diff.tiles != '') {
            detailsControl.updateTilesURL(payload);
        }
        else if (typeof payload.preview !== "undefined")
            detailsControl.updatePreview(elementID, payload.preview);
        else {
            refreshOtherLayer(elementID);
            // todo: check kpis?
            // todo: check charts?
        }
    },
    updatelayer: function (payload) {
        LayerManager.UpdateData(payload);
    },
    updatekpi: function (payload) {
        detailsControl.resetkpi(payload); //todo implement KPI's?
    },
    updatechart: function (payload) {
        GraphManager.UpdateGraphs(payload);
    },
    showchart: function (payload) {
        GraphManager.ShowGraphs(payload);
    },
    showelement: function (payload) {
        for (var i = 0; i < payload.length; i++)
        {
            switch (payload[i].type)
            {
                case 'chart':
                    GraphManager.MakeAndShowChart(payload[i].element)
                    break;
            }
        }
    },
    selectedObjects: function (payload) {
        // add selected objects to layer, ut first adjust selected categories
        measuresControl.setSelectCategories(payload.selectCategories);
        handleObjectSelection(payload);
    },
    session: function (payload) {
        // handle session message
        if (typeof payload.description !== "undefined") { //todo: test if this doesn't create errors
            DataManager.sessionInfo.description = payload.description;
            projectDescription.options.description = payload.description;
            projectDescription.update();

            if (typeof payload.activeScenario !== "undefined") {
                if (DataManager.sessionInfo.scenario != payload.activeScenario) {
                    SyncManager.newActiveScenario(payload.activeScenario);
                }
                DataManager.sessionInfo.scenario = payload.activeScenario;
                projectDescription.options.activeScenario = payload.activeScenario;
            }
            else {
                DataManager.sessionInfo.scenario = null;
                projectDescription.options.activeScenario = payload.activeScenario;
            }
            if (typeof payload.referenceScenario !== "undefined") {
                DataManager.sessionInfo.referenceScenario = payload.referenceScenario;
                projectDescription.options.referenceScenario = payload.referenceScenario;
            }
            else {
                DataManager.sessionInfo.referenceScenario = null;
                projectDescription.options.referenceScenario = payload.referenceScenario;
            }
        }
        if (typeof payload.scenarios !== "undefined")
            projectDescription.options.scenarios = payload.scenarios;
        if (typeof payload.view !== "undefined") {
            map.setView([payload.view.lat, payload.view.lon], payload.view.zoom);
        }
        if (typeof payload.mapView !== "undefined") {
            //projectDescription.options.lead = typeof payload.mapView.lead !== "undefined" ? payload.mapView.lead : projectDescription.options.lead;
            //projectDescription.options.follow = typeof payload.mapView.follow !== "undefined" ? payload.mapView.follow : projectDescription.options.follow;
            //if (typeof payload.mapView.view !== "undefined" && projectDescription.options.follow) 
            //    map.setView([payload.mapView.view.lat, payload.mapView.view.lon], payload.mapView.view.zoom);
            if (typeof payload.mapView.lead !== "undefined")
                map.mapViewModel.lead = payload.mapView.lead;
            if (typeof payload.mapView.follow !== "undefined")
                map.mapViewModel.follow =  payload.mapView.follow;
            if (typeof payload.mapView.view !== "undefined")
                map.mapViewModel.view = payload.mapView.view;

        }
        if (typeof payload.timeslider !== 'undefined') {
            if (payload.timeslider) {
                if (payload.timeslider == 1) {
                    map.addControl(timesliderControl);
                    timesliderControl._collapse();
                    L.Control.InfoTexts['leaflet-control-timeslider'] = { active: false };

                }
                else if (payload.timeslider == 2) {
                    map.addControl(timesliderControl);
                    timesliderControl._expand();
                    L.Control.InfoTexts['leaflet-control-timeslider'] = { description: 'Change the time', active: true };
                }
            }
            else {
                timesliderControl._collapse();
                L.Control.InfoTexts['leaflet-control-timeslider'] = { active: false };
                map.removeControl(timesliderControl);
            }
        }
        if (typeof payload.selectionEnabled !== 'undefined') {
            if (payload.selectionEnabled) {
                addSelectControl();
                L.Control.InfoTexts['leaflet-draw-toolbar'] = { description: 'Select objects', active: true, iconPosition: 'right' };
            }
            else {
                removeSelectControl();
                L.Control.InfoTexts['leaflet-draw-toolbar'] = { active: false };
            }
        }
        if (typeof payload.simulationSettingsEnabled !== 'undefined') {
            // todo: ?
        }
        if (typeof payload.measuresEnabled !== 'undefined') {
            if (payload.measuresEnabled) {
                map.addControl(measuresControl);
                L.Control.InfoTexts['leaflet-control-measures-toggle'] = { description: 'Select measures to applied on objects', active: true, iconPosition: 'right' };
            } else {
                map.removeControl(measuresControl);
                L.Control.InfoTexts['leaflet-control-measures-toggle'] = { active: false };
            }

        }
        if (typeof payload.measuresHistoryEnabled !== 'undefined') {
            if (payload.measuresHistoryEnabled) {
                L.Control.InfoTexts['leaflet-control-history-toggle'] = { description: 'Show and apply all selected measures', active: true, iconPosition: 'left' };
                map.addControl(historyControl);
            } else {
                L.Control.InfoTexts['leaflet-control-history-toggle'] = { active: false };
                map.removeControl(historyControl);
            }
        }
        if (typeof payload.modelControlEnabled !== "undefined") {
            if (payload.modelControlEnabled) {
                map.addControl(DataManager.modelControl);
                L.Control.InfoTexts['leaflet-control-model'] = { description: 'View model control info', active: true, iconPosition: 'left' };
            }
            else {
                map.removeControl(DataManager.modelControl);
                L.Control.InfoTexts['leaflet-control-model'] = { active: false };
            }
        }
        if (typeof payload.filesControlEnabled !== "undefined") {
            if (payload.filesControlEnabled) {
                map.addControl(DataManager.filesControl);
                L.Control.InfoTexts['leaflet-control-files'] = { description: 'Upload/download files', active: true, iconPosition: 'right' };
            }
            else {
                map.removeControl(DataManager.filesControl);
                L.Control.InfoTexts['leaflet-control-files'] = { active: false };
            }
        }
        if (typeof payload.controlsControlEnabled !== "undefined") {
            if (payload.controlsControlEnabled) {
                map.addControl(DataManager.controlsControl);
                L.Control.InfoTexts['leaflet-control-controls'] = { description: 'Shows the controls of the current scenario', active: true, iconPosition: 'left' };
            }
            else {
                map.removeControl(DataManager.controlsControl);
                L.Control.InfoTexts['leaflet-control-controls'] = { active: false };
            }
        }
        if (typeof payload.overviewControlEnabled !== "undefined") {
            if (payload.overviewControlEnabled) {
                map.addControl(DataManager.overviewControl);
                L.Control.InfoTexts['leaflet-control-overview'] = { description: 'Overview of the controls of all scenarios', active: true, iconPosition: 'left' };
            }
            else {
                map.removeControl(DataManager.overviewControl);
                L.Control.InfoTexts['leaflet-control-overview'] = { active: false };
            }
        }
        // basic controls
        L.Control.InfoTexts['leaflet-control-zoom'] = { description: 'Zoom', active: true, iconPosition: 'right' };
        L.Control.InfoTexts['leaflet-control-layers-toggle'] = { description: 'Selecteer de basis kaart', active: true, iconPosition: 'left' };
        L.Control.InfoTexts['leaflet-control-domains-toggle'] = { description: 'Selecteer de relevante domeinen door deze aan te klikken', active: true, iconPosition: 'left' };
        L.Control.InfoTexts['leaflet-control-details-toggle'] = { description: 'Selecteer de gewenste kaart uit een overzicht van beschikbare kaarten', active: true, iconPosition: 'left' };
        L.Control.InfoTexts['projectDescription'] = { description: 'Klik hier om een scenario te selecteren en eventueel een referentiescenario aan te geven', active: true, iconPosition: 'bottom' };

    },
    login: function (payload) {
        // login request, return login information
        // todo: NEW MESSAGE FORMAT
        wsSend({ login: { scenario: DataManager.sessionInfo.scenario, userid: DataManager.sessionInfo.userid } });
        // update mapView state on login
        map.sendMapView();
    },
    connection: function (payload) {
        // connection specific, from ws2imb
        if (payload.message)
          AddErrorMessage(payload.message, payload.messageType, payload.messageTimeOut);

    },
    winddata: function (payload) {
        DataManager.NewWindData(payload);
    },
    addhistorymeasures: function (payload) {
        historyControl.addHistoryItems(payload);
    },
    removehistorymeasures: function (payload) {
        historyControl.removeHistoryItems(payload);
    },
    resetgraphs: function (payload) {
        GraphManager.ResetGraphs(payload);
    },
    ccv: function (payload) {
        SyncManager.handleCCVMessage(payload);
    },
    ccp: function (payload) {
        SyncManager.handleCCPMessage(payload);
    },
    ccb: function (payload) {
        //todo handle broadcast messages
    },
    groupcontrol: function (payload) {
        SyncManager.handleControlMessage(payload);
    },
    queryDialogData: function (payload) {
        DataManager.queryDialogData = payload; // only storage is needed no further action required
    },
    modelcontrol: function (payload) {
        DataManager.modelControl.HandleMessages(payload);
    },
    context: function (payload) {
        ContextManager.contextMessage(payload);
    },
    resetcontext: function (payload) {
        ContextManager.resetContextMenu();
    },
    openformdialog: function (payload) {
        DataManager.formDialogID = payload.id; // can override, but there can only be 1 dialog
        openFormDialog(payload.title, payload.data, payload.context);
    },
    timerangeslider: function (payload) {
        DataManager.NewRangeTimeSliderData(payload);
    },
    rangeslider: function (payload) {
        DataManager.NewRangeSliderData(payload);
    },
    fileDownload: function (payload) {
        DataManager.filesControl.HandleFileDownloadMessage(payload);
    },
    dialogDataResponse: function (payload) {
        handleDataResponse(payload);
    },
    scenarioControlsMessage: function (payload) {
        ScenarioControlsManager.handleMessage(payload);
    },
    timesliderEvents: function (payload) {
        timeslider.HandleEvents(payload);
    },
    canCopyScenario: function (payload) {
        DataManager.canCopyScenario = payload
    },
    controlProperties: function (payload) {
        //make function that shows properties
        //properties at the moment are:
        /*
            {
                name: ...,
                description: ...,
                id: ...,
                properties: [{field: ..., value: ...},...],
                children: [{zelfde als dit object}]
            }
        */
        //so recursive function that builds the HTML code
        //using ul and li tags
        new ControlPropertiesDialog(payload);
    }
};

function wsConnect() {
    ws = new WebSocket(wsBaseURL + '/sessions?session=' + session + (clientType != '' ? '&clienttype=' + clientType : ''));
    //ws = new WebSocket(wsBaseURL + '/sessions?session=' + session);
    ws.onopen = function (e) {
        connectionStatus.classList.remove('connection-status-disconnected');
        connectionStatus.classList.add('connection-status-connected');
        // todo: remove all basic layers?
        // todo: remove all non-basic layers?
        wsLastConnectDateTime = new Date();
    };
    ws.onmessage = function (evt) {
        try {
            var messages = JSON.parse(evt.data);
        }
        catch (err)
        {
            console.log("Error parsing json! Message: ");
            console.log(evt.data);
            throw err;
        }
        // check if single message -> convert single message to array of 1
        if (!(Object.prototype.toString.call(messages) === '[object Array]')) {
            messages = [messages];
        }

        for (var x = 0; x < messages.length; x++) {
            message = messages[x];

            //check if message is of the new type, if so direct call otherwise
            if (typeof message.type !== "undefined") {
                // { type: "type", payload: xx }
                if (DebugLogging) {
                    console.log('received message, type: ' + message.type);
                    console.log(JSON.stringify(message));
                }
                if (typeof wsLookup[message.type] !== "undefined") //only access functions that are defined!
                    wsLookup[message.type](message.payload);
            }
            // todo: temp fix until new message format is active in WS2IMB
            else if (typeof message.connection !== "undefined") {
                wsLookup["connection"]({ message: message.connection.message });
                console.log('>> received old message, type: ' + JSON.stringify(message));
            }
                // todo: temp fix until new message format is active in all publishers
            else if (typeof message.login !== "undefined") {
                wsLookup["login"](message.login);
                console.log('>> received old message, type: ' + JSON.stringify(message));
            }
            else if (typeof message.measures !== "undefined") {
                wsLookup["measures"](message.measures);
                console.log('>> received old message, type: ' + JSON.stringify(message));
            }
            else if (typeof message.addhistorymeasures !== "undefined") {
                wsLookup["addhistorymeasures"](message.addhistorymeasures);
                console.log('>> received old message, type: ' + JSON.stringify(message));
            }
            else if (typeof message.domains !== "undefined") {
                wsLookup["domains"](message.domains);
                console.log('>> received old message, type: ' + JSON.stringify(message));
            }
            else if (typeof message.selectedObjects !== "undefined") {
                wsLookup["selectedObjects"](message.selectedObjects);
                console.log('>> received old message, type: ' + JSON.stringify(message));
            }
            // todo: all fixes failed
            else {
                console.log('## received unsuported old message, type: ' + JSON.stringify(message));
            }
        }
    };
    ws.onerror = function (evt) {
        if (evt.message)
            AddErrorMessage("disconnect: " + evt.message, 'error');
        else
            AddErrorMessage("disconnected", 'error');

        connectionStatus.classList.remove('connection-status-connected');
        connectionStatus.classList.add('connection-status-disconnected');
    };
    ws.onclose = function (evt) {
        connectionStatus.classList.remove('connection-status-connected');
        connectionStatus.classList.add('connection-status-disconnected');
        if (typeof wsLastConnectDateTime !== "undefined") {
            var timeDiff = Math.abs(wsLastConnectDateTime.getTime() - new Date().getTime());
            if (evt.code && evt.code != 1000)
                AddErrorMessage("disconnected (close " + evt.code + ", " + evt.reason + ") after " + Math.round(timeDiff / 1000) + " seconds", 'error');
        }
        else if (evt.code && evt.code != 1000)
            AddErrorMessage("disconnected (close " + evt.code + ", " + evt.reason + ")", 'error');
    };
}

function wsSend(obj) {
    if (ws) {
        if (DebugLogging) {
            if (typeof obj.type !== "undefined")
                console.log('Send message, type: ' + obj.type);
            else
                console.log("Send untyped message");
            console.log(obj);
        }
        return ws.readyState && ws.readyState == WebSocket.OPEN ? ws.send(JSON.stringify(obj)) : -1;
    }
    else
        return -2;
}

function wsClose() {
    ws.close();
}
