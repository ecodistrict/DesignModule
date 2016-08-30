# Design module, user Interface, functional

Elements and actions in the user interface

- [general](#general)
	- [user interface](#geninterface)
	- [functions](#genfunctions)
	- [development languages and frameworks](#genlangframeworks)
- [connection to server](#connection)
- [project](#project)
- [map](#map)
	- [select objects](#selectobjects)
		- [edit object properties](#editobjects) 
		- [select measures](#measures)
			- [apply measures and history](#history)
	- [select base map](#basemap)
	- [select basic layers](#basiclayers), just selectable uncolored objects per category
	- [select domains of interest](#domains)
		- [select kpis, charts and layers to view on/over map](#details)
			- [legend](#legend)
		- [quick select of current, reference or difference kpi, chart or layer](#currefdiff)
		- [select time](#timeslider)
- [messages](#messages)
- [info, help with controls and operation of the user interface](#info)

## General <a name="general"></a>

<a name="geninterface"></a>The user interface is a web client, part of the design module of the European Ecodistrict projects and the TNO instrument Urban Strategy. This is one component of the design module that comprises of this user interface, a web-socket server, a tiler server that created WMS image tiles form GIS data and a publishing server that connects the web client to different data sources and handles or the logic and persistence.

<a name="genfunctions"></a>This user interface is used to visualize and change GIS data within projects. The projects are defined by a specific case with an "as is" data set and variants on this case. The goal is to build a variant by applying measures to objects within the GIS data set. These measures are translated to changes in properties of the selected objects which trigger recalculation within the data set by "models". The output of these models are again properties of objects in the GIS data set. The objects are divided into domains that contain layers for collections of similar objects. With help of a legend the objects are colored and drawn on the map. Objects with properties without a location can be visualized in charts. A specif version of chart, the bullet chart, is used to visualize KPIs. If a reference variant or "as is" data set is specified, differences can be calculated and visualized. A control is provided to quickly switch to "current", "reference" or "difference" view to make analyzing changes between variants easier. A time slider is provided to browse and select data that has a time component.

<a name="genlangframeworks"></a>The web interface is build with pure JavaScript, plain HTML and CSS. A minimal set of frameworks is used, leaflet and D3, to make dependencies as simple as possible. Specific implementations dependent on these frameworks is put in specific JavaScript and CSS files and separated from the core logic as much as possible. All visual aspects are harmonized with CSS as far as possible.

## Connection to server <a name="connection"></a>

The web interface uses a web socket connection to execute commands, exchange data and state information with the servers. All communication over the web socket is in (GEO-)JSON. A large part of the web interface works as a visualization engine controller by the server via (GEO-)JSON commands. 

The state of the connection to the web socket is visualized with an icon left of the project name. The colour of the icon changes to reflect a connected (green) and disconnected (red) session. A click on the icon toggle the the state from connected to disconnected or tries to reconnect to the server. When a disruption of the connection is noticed the icon is changed accordingly and temporarily a message is show in a red rounded square in the right upper corner of the interface.

The connection to the server is matched by a unique client object on the server side indirectly connected to the specific web socket of this session. A project and a current and reference variant are linked to this client object on the server side. Projects, and variants within projects, are managed in a pool and are shared over the different connected clients.

## Project <a name="project"></a>

The current project is selected through the URL calling the web interface main page. The parameter session=<session-id>$<variant-id>$<user-id> is parsed and passed via the connected web socket to connect to the active project on the server side.

The name of the connected project is always visible in the upper part of the interface. If selecting of variants is enabled from the server side a right click on the project name shows all available variants and options to select reference variants. Change the selections to select a different variant and reference for this connection/session.

## Map <a name="map"></a>

The geo map is implemented with [leaflet](http://leafletjs.com). All controls that are shown over the map are based on leaflet controls to make the style and interaction consistent.

There are always controls for zooming and layer control visible on the map. The map is dragged to an other location by dragging after a left click and hold on the base map.

A right click anywhere shows a context menu with options to show the selected location in geo-coordinates, when an object is selected its properties can be changed etc.. Context menus are dynamic based on the selected location, object or control.

Geo-coordinates shown in the interface are always in WGS84, latitude and longitude in degrees, height and distance are in meters.

## Selecting objects <a name="selectobjects"></a>

There are 5 ways to select objects on the map.

- click on the map to select the nearest object of any type.
- Choose the control with the square icon from the selection control on the left to select objects in a rubber-band-selection square.
- Choose the control with the circle icon from the selection control on the left to select objects in a rubber-band-selection circle.
- Choose the control with the polygon icon from the selection control on the left to select objects in a rubber-band-selection polygon.
- Choose the control with the database icon from the selection control on the left to select objects by values of their properties.

Hold the CTRL-key to add/remove an object within the same category as already selected objects to/from the set of currently selected objects.

## Edit object properties<a name="editobjects"></a>

Properties of objects can be changed by applying [measures](#measures) to them or editing them directly.
Select objects and right-click one of the selected objects to show the context menu. Select Object properties from the pop-up menu. A dialog appears that shows all properties and their values available. On selection of multiple objects only properties that are the same over all are shown. Specific properties can be edited and will the changed value with be applied to the selected objects in the current variants data set on selecting "apply" in the dialog.

## Select measures <a name="measures"></a>

Measures can be selected to be applied to selected objects or on the current variant as a whole. Only measures and measure categories applicable to the selected object type are shown. In the measures control categories or measures are shown. On selecting a category all applicable measures are shown in a separate dialog. When extra information is need a dialog is also shown. After selecting a measure it is put along with the selected objects in the [history list](#history) on the right side of the interface. There all measured can be applied to the selected objects ie properties of objects in the variants data set are changed.

## Applying measures <a name="history"></a>

Selected measures are kept in a history list and can be applied as one action. 

## Base map <a name="basemap"></a>

There can be only one base map selected at a time. It is the layer shown on the map under all other layers. This layer can not be edited only shown and is used for orientation and reference. There are three layers selectable: satellite, colored basic coverage and shade of gray basic coverage.
 
## Basic layers <a name="basiclayers"></a>

All object categories with a geo-location can be viewed as a basic layer that only shows location without any colouring via a legend. These layers can be switched on and off to make selection of objects easier. 

## Domains <a name="domains"></a>

All layers, charts and KPIs are sorted into domains. These can be switched on and off to reduce the number of selectable layers, charts and KPIs to the ones being of interest.

## KPIs, charts and layers

All KPIs, charts and layers belonging to the selected domains are visible in the details control. A preview of KPIs, charts and layers is shown here to select them and make them "live" visible.

## Legend <a name="legend"></a>

For layers the corresponding legend can be shown. This legend is always shown when selecting a layer if available. The legend can be re-positioned by dragging and closed when no longer needed. The position is always reset to the default on when an other layer is selected.

## Quick select of current, reference or difference KPI, chart or layer <a name="currefdiff"></a>

When a reference variant is selected and a corresponding KPI, chart or layer is available a control is shown to quickly show that. When available a difference option is also shown in that control to switch to that "live" calculated version.

## Select time <a name="timeslider"></a>

When data with a time component is shown a control is enabled on the map to select a specific moment in time an corresponding data is loaded in the interface.

## Messages <a name="messages"></a>

The interface can temporarily show messages in the upper right corner. This happens when an asynchronous action is finished executing or an error occurred.

## Info <a name="info"></a>

Information on the interface itself is shown as an overlay over the interface to show extra help with the shown controls.

