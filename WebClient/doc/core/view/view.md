# View

*View* is a base class for views in the application.

## API reference <a name="apiReference"></a>

Extends the [L.Evented](https://leafletjs.com/reference-1.0.0.html#evented) class therefore it provides convenient subscription methods like *on(...)* and *off(...)*.

#### Options <a name="options"></a>
Options object that should be passed to *View* constructor.

| Property | Type | Description |
|---|---|---|
| parent | Node | *Optional*. DOM node object that will be used as parent for the view. View will be placed within the parent. This field is optional because the view can be put into a DOM node later on, see *[element()](#methods)* method. |

#### Methods <a name="methods"></a>

| Method | Description |
|---|---|
| render() | Renders the button. This method is called automatically when the object is created. This method can be called in order to recreate the view after `remove()` was called. After calling this method the `element()` method will return a corresponding DOM Node object. |
| remove() | Removes the view. After calling this method the `element()` method will return `null`. When this method is called [`remove`](#events) event is emitted. |
| element() | Returns the DOM node object representing the view. Having this node user can place the view wherever he wants. |
| show() | Shows the view. When this method is called [`show`](#events) event is emitted. |
| hide() | Hides the view. Behavior is the same as applying `display: none` style, i.e. view doesn't keep the occupied space when hidden. When this method is called [`hide`](#events) event is emitted. |

#### Extension methods <a name="extensionMethods"></a>
Every child view may re-implement (override) the following methods.

| Method | Returns | Description |
|---|---|---|
| onInitialize(object opts) | void | This method is called when view is being constructed. Override this method when a child view requires initialiation. `opts` is the object passed to the constructor. |
| onRender() | Node | Method is called when view is being rendered during `render()` call. This method should return a DOM Node representing the view. The returned Node is accessible within child with `element()` call or with a dirrect reference `this._rootElement`. |
| onRemove() | void | Method is called when view is being removed during `remove()` call. |

#### Protected methods <a name="protectedMethods"></a>
Every child view use the following methods.

| Method | Returns | Description |
|---|---|---|
| _notifyFocus() | This method sends the `focus` event to *View* subscribers. |

#### Events <a name="events"></a>

| Event | Data | Description
|---|---|---|
| remove | { view: *View* } | Emitted when the view is removed via `remove()` method. |
| focus | { view: *View* } | Can be emitted from child classes when view is focused. |
| show | { view: *View* } | Emitted when the view is shown. |
| hide | { view: *View* } | Emitted when the view is hidden. |
