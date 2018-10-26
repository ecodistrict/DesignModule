# Graph

Graph component represents all graphs available in the application.

| Graph type | Component | Description |
|---|---|---|
| category | [CategoryGraph](categoryGraph/categoryGraph.md) | A graph with categories on `x` axis. These graphs can contain bars and lines. |

* [Architecture](#architecture)
    * [GraphComponent](#graphComponent)
    * [GraphModel](#graphModel)
    * [GraphView](#graphView)
    * [GraphViewController](#graphViewController)
    * [GraphViewModel](#graphViewModel)
    * [GraphLegendView](#graphLegendView)
    * [GraphLegendViewModel](#graphLegendViewModel)
    * [GraphViewControllerFactory](#graphViewControllerFactory)
    * [GraphViewManager](#graphViewManager)
    * [GraphPreviewFactory](#graphPreviewFactory)
    * [GraphService](#graphService)
* [API reference](#apiReference)
    * [GraphComponent](#graphComponentApi)
    * [GraphModel](#graphModelApi)
    * [GraphView](#graphViewApi)
    * [GraphViewController](#graphViewControllerApi)
    * [GraphViewModel](#graphViewModelApi)
    * [GraphLegendView](#graphLegendViewApi)
    * [GraphLegendViewModel](#graphLegendViewModelApi)
    * [GraphViewControllerFactory](#graphViewControllerFactoryApi)
    * [GraphViewManager](#graphViewManagerApi)
    * [GraphPreviewFactory](#graphPreviewFactoryApi)
    * [GraphService](#graphServiceApi)

## Architecture <a name="architecture"></a>

Graph component consists of several domains: 
- *GraphViewController-GraphView-GraphViewModel* provides a graph window implementation as *[Model View Controller (MVC)](https://en.wikipedia.org/wiki/Model%E2%80%93view%E2%80%93controller)* pattern;
- *Graph preview* allows anyone in the application to get a preview image of a given graph;
- *GraphViewManager* controls the lifecycle of graph windows and places them on the screen;
- *GraphService* provides graph models to the application.

All pieces related to graphs are encapsulated within *GraphComponent* class.

![General structure](./images/GraphArchitecture-General-Structure.svg)

![View](./images/GraphArchitecture-View.svg)

![Preview](./images/GraphArchitecture-Preview.svg)

### GraphComponent <a name="graphComponent"></a>
*[GraphComponent](#graphComponentApi)* creates, configures and stores all graph-related entities like *GraphService* and *GraphViewManager*.

### GraphModel <a name="graphModel"></a>
*[GraphModel](#graphModelApi)* is an application level model that stores data describing a graph and provides simple ways to monitor data changes via subscriptions. This model is not dedicated for view but is a logical representation of a graph.

### GraphView <a name="graphView"></a>
*[GraphView](#graphViewApi)* is a *View* in *MVC* pattern and is responsible for generic graph representation as a separate window. Also provides a legend view. *View* is responsible only for showing the data received from the *Model* and not for modifying it. *View* produces events when somthing need to be done and it is a responsibility of *Controller* how to react on them.
*GraphView* creates and stores *GraphLegendView* view.
*GraphView* provides a set of methods that should be overriden by any child graph view in order to implement graph-related rendering.

### GraphViewController <a name="graphViewController"></a>
*[GraphViewController](#graphViewControllerApi)* is a *Controller* in *MVC* pattern and is responsible for handling *View* events and actions and adapting the *Model* accordingly.
*GraphViewController* receives the *[GraphModel](#graphModel)* as an input and converts it to the *GraphViewModel* and *GraphLegendViewModel*. *GraphViewController* creates *GraphView* and passes *GraphViewModel* and *GraphLegendViewModel* models into the *View*.
*GraphViewController* provides a set of methods that should be overriden by any child graph view controller in order to implement graph-related control flow.

### GraphViewModel <a name="graphViewModel"></a>
*[GraphViewModel](#graphViewModelApi)* is a *Model* in *MVC* pattern and it provides data that is needed by *View* for rendering.

### GraphLegendView <a name="graphLegendView"></a>
*[GraphLegendView](#graphLegendViewApi)* is a helper view class that is used to draw the graph legend.

### GraphLegendViewModel <a name="graphLegendViewModel"></a>
*[GraphLegendViewModel](#graphLegendViewModelApi)* is a helper model that is passed to the *GraphLegendView* by *GraphViewController* and is used *GraphLegendView* for rendering.

### GraphViewControllerFactory <a name="graphViewControllerFactory"></a>
*[GraphViewControllerFactory](#graphViewControllerFactoryApi)* is a factory class that is used to create *GraphViewController* child class instance based on *[GraphModel](#graphModel)* object. This class hides the choice of the concrete child controller from the user.

### GraphViewManager <a name="graphViewManager"></a>
*[GraphViewManager](#graphViewManagerApi)* is a class that manages graph windows and plces them on the screen. *GraphViewManager* is a convinient abstarction that hides all the graph window specific implementation and gives a simple interface to a user. This class allows showing and hiding graph windows using *[GraphModel](#graphModel)* as an input.

### GraphPreviewFactory <a name="graphPreviewFactory"></a>
*[GraphPreviewFactory](#graphPreviewFactoryApi)* is a factory class that produces views (previews) with simplified visual representation of a graph using *[GraphModel](#graphModel)* as an input.
Previews can be used within buttons that trigger showing graph windows on the screen.

### GraphService <a name="graphService"></a>
*[GraphService](#graphServiceApi)* is a provider of *[GraphModel-s](#graphModel)* to the application.

## API reference <a name="apiReference"></a>

### GraphComponent <a name="graphComponentApi"></a>

Extends the [L.Evented](https://leafletjs.com/reference-1.0.0.html#evented) class therefore it provides convenient subscription methods like *on(...)* and *off(...)*.

#### Options <a name="graphComponentOpts"></a>

When constructing GraphComponent an options object should be passed to constructor.

| Property | Type | Description |
|---|---|---|
| windowManager | [WindowManager](../../core/window/windowManager/windowManager.md) | Reference to  a container for WindowView objects (windows). Graphs will be shown in windows that are managed by *windowManager*. |

#### Methods <a name="graphComponentMethods"></a>

| Method | Returns | Description |
|---|---|---|
| service() | [GraphService](#graphServiceApi) | Returns *GraphService* object created by the component. The service can be used to get graph models provided by the backend. |
| graphViewManager() | [GraphViewManager](#graphViewManagerApi) | Returns *GraphViewManager* object created by the component. The *GraphViewManager* can be used to visualize a *GraphModel* object. |
| graphViewControllerFactory() | [GraphViewControllerFactory](#graphViewControllerFactoryApi) | Returns *GraphViewControllerFactory* object created by the component. This factory can be used to instantiate GraphView-GraphViewController pair for a given *GraphModel* object. |
| graphPreviewFactory() | [GraphPreviewFactory](#graphPreviewFactoryApi) | Returns *GraphPreviewFactory* object created by the component. This factory can be used to instantiate *GraphPreview* for a given *GraphModel* object. |
| remove() | void | Removes and deinitialize the entire *GraphComponent*. |

### GraphModel <a name="graphModelApi"></a>

Extends the [L.Evented](https://leafletjs.com/reference-1.0.0.html#evented) class therefore it provides convenient subscription methods like *on(...)* and *off(...)*.

*Graph JSON format* is defined [here](https://github.com/ecodistrict/publisher-api-documentation/blob/master/doc/api/graph/graphJsonFormat.md).

*GraphModel* has exactly the same properties and construction options as fields from the *Graph JSON format*. All properties except *id* and *type* are mutable.

*GraphModel* has change events for each mutable property and event name is the same as the property name. In addition to that, *GraphModel* also sends an cumulative `change` event when something is changed. If more than one field is changed in one shot then only a single `change` event will be emitted. If fileds are changed separatly then `change` event will be emitted for each of them.

#### Methods <a name="graphModelMethods"></a>

| Method | Returns | Description |
|---|---|---|
| set(*options*) | void | Sets new values fro the fields listed in the *options* object. A single `change` event will be emitted if something changes wthin this call. Also, a corresponding change event will be emitted for each field that was modified. *options* object is the same as used during *GraphModel* construction. |

### GraphView <a name="graphViewApi"></a>

Extends the [WindowView](../../core/window/windowView/windowView.md) class.

#### Options <a name="graphViewOpts"></a>

When constructing GraphView an options object should be passed to constructor.

| Property | Type | Description |
|---|---|---|
| graphViewModel | [GraphModel](#graphViewModelApi) | Graph view model to visualize. |
| graphLegendModel | [GraphLegendViewModel](#graphLegendViewModelApi) | Legend model to visualize. |

See also [WindowView](../../core/window/windowView/windowView.md#options) options.

#### Extension methods <a name="graphViewExtensionMethods"></a>
GraphViews that extend from *GraphView* may re-implement (override) the following methods.

| Method | Returns | Description |
|---|---|---|
| onInitializeGraph(object opts) | void | This method is called when view is being constructed. Override this method when a child view requires initialiation. *opts* is the object passed to the constructor. Override this method when you want to add some additional initialization. |
| onRenderGraph(Node viewport) | void | Method is called when view is being rendered during `render()` call. Implement your graph-specific visualization here. *viewport* is a DOM Node where child *GraphView* should render the graph itself. |
| onRemoveGraph() | void | Method is called when view is being removed during `remove()` call. Override this method to free resources and perform deinitialization logic. |
| onAddGraph([WindowManager](../../core/window/windowManager/windowManager.md) windowManager) | void | Method is called when view is added to the WindowManager. When *GraphView* is added to a window manager the geometry of *GraphView* may be changed therefore it might be needed to redraw the *GraphView* content thus override this method and redraw the graph. |

### GraphViewController <a name="graphViewControllerApi"></a>

Extends the [L.Evented](https://leafletjs.com/reference-1.0.0.html#evented) class therefore it provides convenient subscription methods like *on(...)* and *off(...)*.

#### Options <a name="graphViewControllerOpts"></a>

When constructing GraphViewController an options object should be passed to constructor.

| Property | Type | Description |
|---|---|---|
| graphModel | [GraphModel](#graphModelApi) | Graph model to visualize. |

#### Protected properties <a name="graphViewControllerProtectedProperties"></a>
Properties that can be used in child classes derived from the controller.

| Property | Type | Description |
|---|---|---|
| graphModel | [GraphModel](#graphModelApi) | *GraphModel* passed to the controller's constructor. |
| graphLegendViewModel | [GraphLegendViewModel](#graphLegendViewModelApi) | Legend model constructed based on *GraphModel*. |
| graphViewModel | [GraphViewModel](#graphViewModelApi) | Graph view model constructed based on *GraphModel*. |
| graphView | [GraphView](#graphViewApi) | *GraphView* created by `createGraphView()` [method](#graphViewControllerExtensionMethods). |

#### Methods <a name="graphViewControllerMethods"></a>

| Method | Returns | Description |
|---|---|---|
| view() | [GraphView](#graphViewApi) | Returns *GraphView* object created by the controller. |
| closeView() | void | Closes the *GraphView* window. |
| isViewOpen() | boolean | Reurns `true` if *GraphView* window is shown, otherwise returns `false`. |
| remove() | void | Removes the *GraphViewController* object and closes all opened *GraphView* windows. |

#### Extension methods <a name="graphViewControllerExtensionMethods"></a>
Controllers that extend from *GraphViewController* may re-implement (override) the following methods.

| Method | Returns | Description |
|---|---|---|
| onInitialize() | void | This method is called during the initialization of the *GraphViewController* object. Override this method when you want to add some additional initialization.  |
| onRemove() | void | This method is called during *GraphViewController* removal. Override this method to free resources and perform deinitialization logic. |
| createGraphView() | [GraphView](#graphViewApi) | This method is used to create the *GraphView* for the controller. Override this method to create your custom *GraphView* otherwise the base *GraphView* class will be instantiated. |
| processAxes(object axes) | object | This method is called when original *GraphModel* is updated. `graphModel.axes` property is passed to this method and it can return modified version of the *axes* object. Override this method when you need additional processing of the axes. |
| processCategories(Array categories) | Array | This method is called when original *GraphModel* is updated. `graphModel.categories` property is passed to this method and it can return modified version of the *categories* array. Override this method when you need additional processing of the categories. |
| processSeries(Array series) | Array | This method is called when original *GraphModel* is updated. `graphModel.series` property is passed to this method and it can return modified version of the *series* array. Override this method when you need additional processing of the series. |

#### Events <a name="graphViewControllerEvents"></a>

| Property | Type | Description |
|---|---|---|
| viewClosed | { graphViewController: this, view: [GraphView](#graphViewApi) } | Emitted when *GraphView* window is closed. |

### GraphViewModel <a name="graphViewModelApi"></a>
Extends the [L.Evented](https://leafletjs.com/reference-1.0.0.html#evented) class therefore it provides convenient subscription methods like *on(...)* and *off(...)*.

*GraphViewModel* has exactly the same properties and events as [GraphModel](#graphModelApi) model except *id* and *type* that are missing in *GraphViewModel*.

### GraphLegendView <a name="graphLegendViewApi"></a>

Extends the [View](../../core/view/view.md) class.

### GraphLegendViewModel <a name="graphLegendViewModelApi"></a>

Extends the [L.Evented](https://leafletjs.com/reference-1.0.0.html#evented) class therefore it provides convenient subscription methods like *on(...)* and *off(...)*.

#### Options <a name="graphLegendViewModelOpts"></a>

When constructing GraphLegendViewModel an options object should be passed to constructor.

| Property | Type | Description |
|---|---|---|
| entries | Array &lt;[LegendEntry](#graphLegendViewModelLegendEntry)&gt; | Legend entries list. |

#### Properties <a name="graphLegendViewModelProperties"></a>

| Property | Type | Description |
|---|---|---|
| entries | Array &lt;[LegendEntry](#graphLegendViewModelLegendEntry)&gt; | Legend entries list. When a new value is assigned to this property the `entries` event is emitted. |

#### Methods <a name="graphLegendViewModelMethods"></a>

| Method | Returns | Description |
|---|---|---|
| enableEntry([LegendEntry](#graphLegendViewModelLegendEntry) entry) | void | Enables the given entry if it is present in the model. `entries` event is emitted. |
| disableEntry([LegendEntry](#graphLegendViewModelLegendEntry) entry) | void | Disables the given entry if it is present in the model. `entries` event is emitted. |
| toggleEntry([LegendEntry](#graphLegendViewModelLegendEntry) entry) | void | Toggles the given entry between enabled and disabled states. `entries` event is emitted. |
| merge(Array &lt;[LegendEntry](#graphLegendViewModelLegendEntry)&gt; entries) | void | Merges the given entries into the model: adds new entries to the model; removes old entries from the model; preserves *enable* state of exisiting entries in the model. `entries` event is emitted. |
| enabledEntriesCount() | number | Returns number of enabled entries. |

#### Events <a name="graphLegendViewModelEvents"></a>

| Property | Data | Description |
|---|---|---|
| entries | { entries:Array &lt;[LegendEntry](#graphLegendViewModelLegendEntry)&gt; } | Emitted whenever *entries* property of the model changes. |

#### LegendEntry <a name="graphLegendViewModelLegendEntry"></a>

| Property | Type | Description |
|---|---|---|
| id | string | Legend entry id. Should corespond to a certain series id. |
| title | string | Legend entry title to be displayed. |
| color | string | Legend entry color. |
| enabled | boolean | *Optional*. Indicates wether entry is enable (`true`) or not (`false`). Default is `true`. |

### GraphViewControllerFactory <a name="graphViewControllerFactoryApi"></a>

Extends the [L.Evented](https://leafletjs.com/reference-1.0.0.html#evented) class therefore it provides convenient subscription methods like *on(...)* and *off(...)*.

#### Methods <a name="graphViewControllerFactorMethods"></a>

| Method | Returns | Description |
|---|---|---|
| create([GraphModel](#graphModelApi) graphModel, [GraphViewOptions](#GraphViewOptions) graphViewOptions) | [GraphViewController](#graphViewControllerApi) | Creates a view controller for a given *graphModel*. *graphViewOptions* will be passed to the *GraphView* created by the controller. |
| remove() | void | Removes and deinitialize the entire *GraphViewControllerFactory*. |

### GraphViewManager <a name="graphViewManagerApi"></a>

Extends the [L.Evented](https://leafletjs.com/reference-1.0.0.html#evented) class therefore it provides convenient subscription methods like *on(...)* and *off(...)*.

#### Options <a name="graphViewManagerOpts"></a>

When constructing GraphViewManager an options object should be passed to constructor.

| Property | Type | Description |
|---|---|---|
| windowManager | [WindowManager](../../core/window/windowManager/windowManager.md) | Window manager that will manage *GraphViews* positioning on the screen. |
| graphViewOptions | [GraphViewOptions](#GraphViewOptions) | Options to be passed into *GraphView* constructor. |
| graphViewControllerFactory | [GraphViewControllerFactory](#graphViewControllerFactoryApi) | Factory to be used to create *GraphViewControllers* with its' *GrapViews*. |

#### Methods <a name="graphViewManagerMethods"></a>

| Method | Returns | Description |
|---|---|---|
| showGraph([GraphModel](#graphModelApi) graphModel) | void | Shows a graph window defined with the *graphModel* on the screen. `graphShown` event is emitted when graph is shown on the screen. |
| hideGraph([GraphModel](#graphModelApi) graphModel) | void | Hides a graph window defined with the *graphModel* from the screen. `graphHidden` event is emitted when graph is hidden from the screen. |
| isGraphShown([GraphModel](#graphModelApi) graphModel) | boolean | Returns `true` if graph is shown on the screen by this *GraphViewManager*. Otherwise returns `false`. |
| remove() | void | Removes and deinitialize the entire *GraphViewManager*. All open graphs will be closed. |

#### Events <a name="graphViewManagerEvents"></a>

| Property | Data | Description |
|---|---|---|
| graphShown | { graphModel: [GraphModel](#graphModelApi) } | Emitted whenever graph is shown on the screen. |
| graphHidden | { graphModel: [GraphModel](#graphModelApi) } | Emitted whenever graph is hidden from the screen. |
| graphCategoryClicked | { graphModel: [GraphModel](#graphModelApi), categoryId: string } | Emitted whenever user clicked a graph category label. |

### GraphPreviewFactory <a name="graphPreviewFactoryApi"></a>

Extends the [L.Evented](https://leafletjs.com/reference-1.0.0.html#evented) class therefore it provides convenient subscription methods like *on(...)* and *off(...)*.

#### Methods <a name="graphPreviewFactoryMethods"></a>

| Method | Returns | Description |
|---|---|---|
| create([GraphModel](#graphModelApi) graphModel, Node previewViewportElement?) | [View](../../core/view/view.md) | Creates a preview for a given *graphModel*. *previewViewportElement* is a DOM element to be used as a parent for the preview and is optional. If *previewViewportElement* is not provided then preview will be created not attached to the DOM document and user will have to manually attach it. |
| remove() | void | Removes and deinitialize the entire *GraphPreviewFactory*. |

### GraphService <a name="graphServiceApi"></a>

Extends the [L.Evented](https://leafletjs.com/reference-1.0.0.html#evented) class therefore it provides convenient subscription methods like *on(...)* and *off(...)*.

#### Properties <a name="graphServiceProperties"></a>

| Property | Type | Description |
|---|---|---|
| graphsModel | [ModelCollection](../../core/modelCollection/modelCollection.md) | Graph models collection. |

#### Methods <a name="graphServiceMethods"></a>

| Method | Returns | Description |
|---|---|---|
| reportGraphCategorySelected([GraphModel](#graphModelApi) graphModel, string categoryId) | void | Report to the backend that graph category was selected so that beckend can perform corresponding actions. |
