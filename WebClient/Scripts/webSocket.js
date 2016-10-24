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
      for (var id in domain.layers) {
        var layer = domain.layers[id];
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
      for (var id in domain.layers) {
        var layer = domain.layers[id];
        if (layer.basic)
        addBasicLayer(layer);
      }
    }
  },
  refresh: function (payload) {
    var elementID = payload.id;
    if (typeof payload.tiles !== "undefined" && payload.tiles != '') {
      detailsControl.updateTilesURL(elementID, payload.tiles);
      // todo: if basic layer -> check and update that url!
    }
    else if (typeof payload.ref !== "undefined" && payload.ref.tiles !== "undefined" && payload.ref.tiles != '') {
      detailsControl.updateTilesURL(payload.ref.id, payload.ref.tiles);
    }
    else if (typeof payload.diff !== "undefined" && payload.diff.tiles !== "undefined" && payload.diff.tiles != '') {
      detailsControl.updateTilesURL(payload.diff.id, payload.diff.tiles);
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
    var elementID = payload.id;
    // online layer
    for (var mlid in map._layers) {
      var layer = map._layers[mlid];
      if (layer.domainLayer && layer.domainLayer.id && layer.domainLayer.id == elementID) {
        if (typeof payload.newobjects !== "undefined") {
          // dictionary of id: feature
          for (var id in payload.newobjects) {
            layer.addData(payload.newobjects[id]);
          }
        }
        else if (payload.changedcolors) {
          // dictionary of id: color
          for (var lid in layer._layers) {
            var fid = layer._layers[lid].feature.properties.id;
            var newColor = payload.changedcolors[fid];
            if (newColor)
            layer._layers[lid].feature.properties.color = newColor;
          }
          layer.setStyle(
            function (feature) {
              // todo: test code for live trafic.. generalize..
              if (feature.properties.color == '#000000')
              return { color: '#000000', weight: 1 };
              else
              return { color: feature.properties.color }
            });
          }
          else if (payload.removedobjects) {
            // dictionary id: X
            for (var lid in layer._layers) {
              var fid = layer._layers[lid].feature.properties.id;
              if (payload.removedobjects[fid])
              layer.removeLayer(layer._layers[lid]);
            }
          }
          if (payload.timestamp) {
            showLayerTimeStamp(payload.timestamp)

          }
        }
      }
    },
    updatekpi: function (payload) {
      detailsControl.resetkpi(payload);
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
      if (typeof payload.description !== "undefined") {
        DataManager.sessionInfo.description = payload.description;
        projectDescription.options.description = payload.description;
        projectDescription.update();
      }
      if (typeof payload.activeScenario !== "undefined") {
        DataManager.sessionInfo.scenario = payload.activeScenario;
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
            InfoTextControl['leaflet-control-timeslider'] = {active: false};

          }
          else if (payload.timeslider == 2) {
            map.addControl(timesliderControl);
            timesliderControl._expand();
            InfoTextControl['leaflet-control-timeslider'] = {description: 'Change the time', active: true};
          }
        }
        else {
          timesliderControl._collapse();
          InfoTextControl['leaflet-control-timeslider'] = {active: false};
          map.removeControl(timesliderControl);
        }
      }
      if (typeof payload.selectionEnabled !== 'undefined') {
        if (payload.selectionEnabled) {
          addSelectControl();
          InfoTextControl['leaflet-draw-toolbar'] = {description: 'Select objects', active: true, iconPosition:'right'};
        }
        else {
          removeSelectControl();
          InfoTextControl['leaflet-draw-toolbar'] = {active: false};
        }
      }

      if (typeof payload.simulationSettingsEnabled !== 'undefined') {

      }

      if (typeof payload.measuresEnabled !== 'undefined') {
        if (payload.measuresEnabled) {
          map.addControl(measuresControl);
          InfoTextControl['leaflet-control-measures-toggle'] = {description: 'Select measures to applied on objects', active: true, iconPosition:'right'};
        } else {
          map.removeControl(measuresControl);
          InfoTextControl['leaflet-control-measures-toggle'] = {active: false};
        }

      }
      if (typeof payload.measuresHistoryEnabled !== 'undefined') {
        if (payload.measuresHistoryEnabled) {
          InfoTextControl['leaflet-control-history-toggle'] = {description: 'Show and apply all selected measures', active: true, iconPosition:'left'};
          map.addControl(historyControl);
        } else {
          InfoTextControl['leaflet-control-history-toggle'] = {active: false};
          map.removeControl(historyControl);
        }
      }
      if (typeof payload.simulationSettingsEnabled !== 'undefined') {
        if (payload.simulationSettingsEnabled) {
          map.addControl(simulationControl);
          InfoTextControl['leaflet-control-simulation'] = {description: 'Click here to config or edit a simulation', active: true, iconPosition:'left'};
        } else {
          map.removeControl(simulationControl);
          InfoTextControl['leaflet-control-simulation'] = {active: false};
        }
      }
      if (typeof payload.simulationControlEnabled !== 'undefined') {
        if (payload.simulationControlEnabled) {
          map.addControl(startControl);
          InfoTextControl['leaflet-control-simulation-toggle'] = {description: 'Config simulation', active: true, iconPosition:'left'};
        } else {
          map.removeControl(startControl);
          InfoTextControl['leaflet-control-simulation-toggle'] = {active: false};
        }
      } else {
        map.removeControl(startControl);
        InfoTextControl['leaflet-control-simulation-toggle'] = {active: false};
      }

      // basic controls
      InfoTextControl['leaflet-control-zoom'] = {description: 'Zoom', active: true, iconPosition: 'right'};
      InfoTextControl['leaflet-control-layers-toggle'] = {description: 'Select base layer and switch on/off basic layers for all available object types', active: true, iconPosition:'left'};
      InfoTextControl['leaflet-control-domains-toggle'] = {description: 'Switch domain on/off', active: true, iconPosition:'left'};
      InfoTextControl['leaflet-control-details-toggle'] = {description: 'Switch on/off detail information layers for the selected domains', active: true, iconPosition:'left'};
      InfoTextControl['projectDescription'] = {description: 'Click here to select a scenario or select an referention scenario', active: true, iconPosition:'bottom'};

    },
    login: function (payload) {
      // login request, return login information
      wsSend({ login: { scenario: DataManager.sessionInfo.scenario, userid: DataManager.sessionInfo.userid } });
    },
    connection: function (payload) {
      // connection specific, from ws2imb
      if (payload.message)
      showUserMessage(payload.message, payload.messageType);
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
      historyControl.removeHistoryItems(payload)
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
        showUserMessage("Simulation speed changed to: " + payload.speed, 1);
      }
    },
    ccv: function (payload) {
      SyncManager.handleCCVMessage(payload);
    },
    ccm: function (payload) {
      SyncManager.handleCCMMessage(payload);
    },
    ccb: function (payload) {
      //todo handle broadcast messages
    },
    groupcontrol: function (payload) {
      SyncManager.handleControlMessage(payload);
    }
  }

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

          wsLookup[messageBuilder.type](messageBuilder.payload);
        }
      }

    };
    ws.onerror = function (evt) {
      if (evt.message)
      showUserMessage("disconnect: " + evt.message);
      else
      showUserMessage("disconnected");

      connectionStatus.classList.remove('connection-status-connected');
      connectionStatus.classList.add('connection-status-disconnected');
    };
    ws.onclose = function (evt) {
      connectionStatus.classList.remove('connection-status-connected');
      connectionStatus.classList.add('connection-status-disconnected');
      if (typeof wsLastConnectDateTime !== "undefined") {
        var timeDiff = Math.abs(wsLastConnectDateTime.getTime() - new Date().getTime());
        if (evt.code && evt.code != 1000)
        showUserMessage("disconnected (close " + evt.code + ", " + evt.reason + ") after " + Math.round(timeDiff / 1000) + " seconds");
      }
      else if (evt.code && evt.code != 1000)
      showUserMessage("disconnected (close " + evt.code + ", " + evt.reason + ")");
    };
  };

  function wsSend(obj) {
    if (ws) {
      return ws.readyState && ws.readyState == WebSocket.OPEN ? ws.send(JSON.stringify(obj)) : -1;
    }
    else
    return -2;
  };

  function wsClose() {
    ws.close();
  }
