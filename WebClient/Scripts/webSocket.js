// web socket connection
var ws;
var wsLastConnectDateTime;
var InfoTextControl = [];
var wsLookup = {
    //message handlers
    measures: function (payload) {
        measuresControl.resetMeasures(payload);
    },
    domains: function (payload) {
        // first domains after login
        domainsControl.resetDomains(payload);
        // remove all basic overlay layers from layers control (and with that from map)
        for (var id in layerControl._layers) {
            var lcl = layerControl._layers[id];
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
        for (var id in layerControl._layers) {
            var lcl = layerControl._layers[id];
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
        //console.log(payload);
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
    selectedobjects: function (payload) {
        // add selected objects to layer, ut first adjust selected categories
        measuresControl.setSelectCategories(payload.selectCategories);
        handleObjectSelection(payload);
    },
    selectedobjectsproperties: function (payload) {
        showSelectedObjectsProperties(payload);
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
        if (typeof payload.timeslider !== 'undefined') {
            if (payload.timeslider) {
                if (payload.timeslider == 1) {
                    map.addControl(timesliderControl);
                    timesliderControl._collapse();
                    InfoTextControl['leaflet-control-timeslider'] = { active: false };

                }
                else if (payload.timeslider == 2) {
                    map.addControl(timesliderControl);
                    timesliderControl._expand();
                    InfoTextControl['leaflet-control-timeslider'] = { description: 'Change the time', active: true };
                }
            }
            else {
                timesliderControl._collapse();
                InfoTextControl['leaflet-control-timeslider'] = { active: false };
                map.removeControl(timesliderControl);
            }
        }
        if (typeof payload.selectionEnabled !== 'undefined') {
            if (payload.selectionEnabled) {
                addSelectControl();
                InfoTextControl['leaflet-draw-toolbar'] = { description: 'Select objects', active: true, iconPosition: 'right' };
            }
            else {
                removeSelectControl();
                InfoTextControl['leaflet-draw-toolbar'] = { active: false };
            }
        }

        if (typeof payload.simulationSettingsEnabled !== 'undefined') {
            // todo: ?
        }

        if (typeof payload.measuresEnabled !== 'undefined') {
            if (payload.measuresEnabled) {
                map.addControl(measuresControl);
                InfoTextControl['leaflet-control-measures-toggle'] = { description: 'Select measures to applied on objects', active: true, iconPosition: 'right' };
            } else {
                map.removeControl(measuresControl);
                InfoTextControl['leaflet-control-measures-toggle'] = { active: false };
            }

        }
        if (typeof payload.measuresHistoryEnabled !== 'undefined') {
            if (payload.measuresHistoryEnabled) {
                InfoTextControl['leaflet-control-history-toggle'] = { description: 'Show and apply all selected measures', active: true, iconPosition: 'left' };
                map.addControl(historyControl);
            } else {
                InfoTextControl['leaflet-control-history-toggle'] = { active: false };
                map.removeControl(historyControl);
            }
        }
        if (typeof payload.simulationControlEnabled !== 'undefined') {
            if (payload.simulationControlEnabled) {
                map.addControl(startControl);
                map.addControl(presenterViewerControl);
                map.addControl(DataManager.modelControl);
                InfoTextControl['leaflet-control-model'] = { description: 'View model control info', active: true, iconPosition: 'left' };
                InfoTextControl['leaflet-control-pv'] = { description: 'Set-up a presenter session or join a session as viewer', active: true, iconPosition: 'left' };
                InfoTextControl['leaflet-control-startstop-stopped'] = { description: 'Play/pause simulation', active: true, iconPosition: 'left' };
            } else {
                map.removeControl(startControl);
                map.removeControl(presenterViewerControl);
                InfoTextControl['leaflet-control-model'] = { active: false };
                InfoTextControl['leaflet-control-pv'] = { active: false };
                InfoTextControl['leaflet-control-startstop-stopped'] = { active: false };
            }
        }

        //else {
        //    map.removeControl(presenterViewerControl)
        //    map.removeControl(startControl);
        //    map.removeControl(DataManager.modelControl);
        //    InfoTextControl['leaflet-control-startstop-stopped'] = { active: false };
        //}
        if (typeof payload.simulationSetup !== "undefined") {
            if (payload.simulationSetup) {
                if (typeof payload.simulationSetup.data !== 'undefined')
                    DataManager.simulationSetupData = payload.simulationSetup.data;
                map.addControl(simulationControl);
                InfoTextControl['leaflet-control-simulation'] = { description: 'Click here to config or edit a simulation', active: true, iconPosition: 'left' };
            }
            else {
                //DataManager.simulationSetupData = null;
                map.removeControl(simulationControl);
                InfoTextControl['leaflet-control-simulation'] = { active: false };
            }
        }
        if (typeof payload.dateForm !== "undefined") {
            if (payload.dateForm) {
                DataManager.formData = payload.dateForm.data;
                map.addControl(DataManager.DateFormControl);
                InfoTextControl['leaflet-control-dateForm'] = { description: 'Click here to config or edit a simulation', active: true, iconPosition: 'left' };
            }
            else {
                DataManager.formData = null;
                map.removeControl(DataManager.DateFormControl);
                InfoTextControl['leaflet-control-dateForm'] = { active: false };
            }
        }
        if (typeof payload.simulationClose !== "undefined")
        {
            if (payload.simulationClose) {
                map.addControl(DataManager.simCloseControl);
                InfoTextControl['leaflet-control-simclose'] = { description: 'Click here to close the current simulation', active: true, iconPosition: 'left' };
            }
            else {
                map.removeControl(DataManager.simCloseControl);
                InfoTextControl['leaflet-control-simclose'] = { active: false };
            }
        }
        
        if (typeof payload.startstopControlEnabled !== "undefined") {
            if (payload.startstopControlEnabled) {
                map.addControl(startControl);
                InfoTextControl['leaflet-control-startstop-stopped'] = { description: 'Play/pause simulation', active: true, iconPosition: 'left' };
            }
            else {
                map.removeControl(startControl);
                InfoTextControl['leaflet-control-startstop-stopped'] = { active: false };
            }
        }
        if (typeof payload.presenterViewerControl !== "undefined") {
            if (payload.presenterViewerControl) {
                map.addControl(presenterViewerControl);
                InfoTextControl['leaflet-control-pv'] = { description: 'Set-up a presenter session or join a session as viewer', active: true, iconPosition: 'left' };
            }
            else {
                map.removeControl(presenterViewerControl);
                InfoTextControl['leaflet-control-pv'] = { active: false };
            }
        }

        // basic controls
        InfoTextControl['leaflet-control-zoom'] = { description: 'Zoom', active: true, iconPosition: 'right' };
        InfoTextControl['leaflet-control-layers-toggle'] = { description: 'Selecteer de basis kaart', active: true, iconPosition: 'left' };
        InfoTextControl['leaflet-control-domains-toggle'] = { description: 'Selecteer de relevante domeinen door deze aan te klikken', active: true, iconPosition: 'left' };
        InfoTextControl['leaflet-control-details-toggle'] = { description: 'Selecteer de gewenste kaart uit een overzicht van beschikbare kaarten', active: true, iconPosition: 'left' };
        InfoTextControl['projectDescription'] = { description: 'Klik hier om een scenario te selecteren en eventueel een referentiescenario aan te geven', active: true, iconPosition: 'bottom' };

    },
    login: function (payload) {
        // login request, return login information
        wsSend({ login: { scenario: DataManager.sessionInfo.scenario, userid: DataManager.sessionInfo.userid } });
    },
    connection: function (payload) {
        // connection specific, from ws2imb
        if (payload.message)
          AddErrorMessage(payload.message, payload.messageType, payload.messageTimeOut);

    },
    sensor: function (payload) {
        DataManager.NewSensor(payload);
    },
    complaint: function (payload) {
        DataManager.NewComplaint(payload);
    },
    sensordata: function (payload) {
        DataManager.NewSensorData(payload);
    },
    winddata: function (payload) {
        DataManager.NewWindData(payload);
    },
    removecomplaint: function (payload) {
        DataManager.RemoveComplaint(payload);
    },
    removesensor: function (payload) {
        DataManager.RemoveSensor(payload);
    },
    addhistorymeasures: function (payload) {
        historyControl.addHistoryItems(payload);
    },
    removehistorymeasures: function (payload) {
        historyControl.removeHistoryItems(payload);
    },
    addcars: function (payload) {
        DataManager.AddCars(payload);
    },
    updatecars: function (payload) {
        DataManager.UpdateCars(payload);
    },
    removecars: function (payload) {
        DataManager.RemoveCars(payload);
    },
    newgtusensor: function (payload) {
        //not implemented

    },
    updategtusensor: function (payload) {
        //not implemented

    },
    newgtustatistics: function (payload) {
        for (var i = 0; i < payload.length; i++)
            GraphManager.MakeGraph(payload[i]);
    },
    updategtustatistics: function (payload) {
        GraphManager.UpdateGraphs(payload);
    },
    simulationcontrol: function (payload) {
        if (typeof payload.start !== "undefined") {
            startControl.SimulationStarted();
        }
        else if (typeof payload.stop !== "undefined") {
            startControl.SimulationStopped();
        }

        if (typeof payload.speed !== "undefined") {
            DataManager.simSpeed = payload.speed;
            AddErrorMessage("Simulation speed changed to: " + payload.speed, "succes", 5000);
        }
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
        DataManager.formDialogID = payload.id; //can override, but there can only be 1 dialog
        openFormDialog(payload.title, payload.data);
    },
    timerangeslider: function (payload) {
        DataManager.NewRangeTimeSliderData(payload);
    }
};

function wsConnect() {
    ws = new WebSocket(wsBaseURL + '/sessions?session=' + session);
    ws.onopen = function (e) {
        connectionStatus.classList.remove('connection-status-disconnected');
        connectionStatus.classList.add('connection-status-connected');
        // todo: remove all basic layers?
        // todo: remove all non-basic layers?
        wsLastConnectDateTime = new Date();
    };
    ws.onmessage = function (evt) {

        var message = JSON.parse(evt.data);

        var messages = message;

        if (!(Object.prototype.toString.call(message) === '[object Array]')) {
            messages = [message];
        }

        for (var x = 0; x < messages.length; x++) {
            message = messages[x];

            //check if message is of the new type, if so direct call otherwise
            if (typeof message.type !== "undefined") {
                if (typeof wsLookup[message.type] !== "undefined") //only access functions that are defined!
                    wsLookup[message.type](message.payload);
            }
            else {
                var messageBuilder = {};
                //build the type/payload message for compatibility with the new standard
                if (message.measures) {
                    messageBuilder.type = "measures";
                    messageBuilder.payload = message.measures;
                }
                else if (message.domains) {
                    messageBuilder.type = "domains";
                    messageBuilder.payload = message.domains;
                }
                else if (message.updatedomains) {
                    messageBuilder.type = "updatedomains";
                    messageBuilder.payload = message.updatedomains;
                }
                else if (message.refresh) {
                    messageBuilder.type = "refresh";
                    messageBuilder.payload = {};
                    messageBuilder.payload.id = message.refresh;
                    if (typeof message.tiles !== "undefined")
                        messageBuilder.payload.tiles = message.tiles;
                    if (typeof message.timestamp !== "undefined")
                        messageBuilder.timestamp = message.timestamp;
                    if (typeof message.preview !== "undefined")
                        messageBuilder.payload.preview = message.preview;
                    if (typeof message.diff !== "undefined")
                        messageBuilder.payload.diff = message.diff;
                    if (typeof message.ref !== "undefined")
                        messageBuilder.payload.ref = message.ref;
                }
                else if (message.updatelayer) {
                    messageBuilder.type = "updatelayer";
                    messageBuilder.payload = message.updatelayer;
                }
                else if (message.updatekpi) {
                    messageBuilder.type = "updatekpi";
                    messageBuilder.payload = message.updatekpi;
                }
                else if (message.selectedObjects) {
                    messageBuilder.type = "selectedobjects";
                    messageBuilder.payload = message.selectedObjects;
                }
                else if (message.selectedObjectsProperties) {
                    messageBuilder.type = "selectedobjectsproperties";
                    messageBuilder.payload = message.selectedObjectsProperties;
                }
                else if (message.session) {
                    messageBuilder.type = "session";
                    messageBuilder.payload = message.session;
                }
                else if (message.login) {
                    messageBuilder.type = "login";
                    messageBuilder.payload = message.login;
                }
                else if (message.connection) {
                    messageBuilder.type = "connection";
                    messageBuilder.payload = message.connection;
                }
                else if (message.sensor) {
                    messageBuilder.type = "sensor";
                    messageBuilder.payload = message.sensor;
                }
                else if (message.complaint) {
                    messageBuilder.type = "complaint";
                    messageBuilder.payload = message.complaint;
                }
                else if (message.sensordata) {
                    messageBuilder.type = "sensordata";
                    messageBuilder.payload = message.sensordata;
                }
                else if (message.winddata) {
                    messageBuilder.type = "winddata";
                    messageBuilder.payload = message.winddata;
                }
                else if (message.removecomplaint) {
                    messageBuilder.type = "removecomplaint";
                    messageBuilder.payload = message.removecomplaint;
                }
                else if (message.removesensor) {
                    messageBuilder.type = "removesensor";
                    messageBuilder.payload = message.removesensor;
                }
                else if (message.addhistorymeasures) {
                    messageBuilder.type = "addhistorymeasures";
                    messageBuilder.payload = message.addhistorymeasures;
                }
                else if (message.removehistorymeasures) {
                    messageBuilder.type = "removehistorymeasures";
                    messageBuilder.payload = message.removehistorymeasures;
                }
                else if (message.addcar) {
                    messageBuilder.type = "addcars";
                    messageBuilder.payload = message.addcar;
                }
                else if (message.updatecar) {
                    messageBuilder.type = "updatecars";
                    messageBuilder.payload = message.updatecar;
                }
                else if (message.removecar) {
                    messageBuilder.type = "removecars";
                    messageBuilder.payload = message.removecar;
                }
                else if (message.newGTUsensor) {
                    messageBuilder.type = "newgtusensor";
                    messageBuilder.payload = message.newGTUsensor;
                }
                else if (message.updateGTUsensor) {
                    messageBuilder.type = "updategtusensor";
                    messageBuilder.payload = message.updateGTUsensor;
                }
                else if (message.newGTUstatistics) {
                    messageBuilder.type = "newgtustatistics";
                    messageBuilder.payload = message.newGTUstatistics;
                }
                else if (message.updateGTUstatistics) {
                    messageBuilder.type = "updategtustatistics";
                    messageBuilder.payload = message.updateGTUstatistics;
                }
                else if (message.simulationControl) {
                    messageBuilder.type = "simulationcontrol";
                    messageBuilder.payload = message.simulationControl;
                }
                else if (message.ccv) {
                    messageBuilder.type = "ccv";
                    messageBuilder.payload = message.ccv;
                }
                else if (message.ccp) {
                    messageBuilder.type = "ccp";
                    messageBuilder.payload = message.ccp;
                }
                else if (message.ccb) {
                    messageBuilder.type = "ccb";
                    messageBuilder.payload = message.ccb;
                }
                else if (message.groupcontrol) {
                    messageBuilder.type = "groupcontrol";
                    messageBuilder.payload = messave.groupcontrol;
                }
                else {
                    console.log("unknown message:");
                    console.log(message);
                    break; //unknown message
                }
                if (typeof wsLookup[messageBuilder.type] !== "undefined")
                    wsLookup[messageBuilder.type](messageBuilder.payload); //only access functions that are defined!
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
        return ws.readyState && ws.readyState == WebSocket.OPEN ? ws.send(JSON.stringify(obj)) : -1;
    }
    else
        return -2;
}

function wsClose() {
    ws.close();
}
