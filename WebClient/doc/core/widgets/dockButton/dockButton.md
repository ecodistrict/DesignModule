# DockButton

*DockButton* widget represents a button for dock and undock actions. This is only a button and doesn't implement the docking behavior.

## Architecture

The widget provides has a simple Model-View architecture and consists of

*DockButtonView* class implements the dock button widget itself. *View* displays state provided by the *Model* and provides click events to subscribe on. The button has two states: docked and undocked.

*DockButtonViewModel* class implements a dock button model to be displayed.

## API reference

### Usage example

```javascript
    var dockModel = new DockButtonViewModel({
        docked: false
    });

    var buttonsContainer = ... // DOM element used as a container

    dockButton = new DockButtonView({ dockButtonViewModel: dockModel });
    dockButton.on('dockClicked', function () { 
        console.log('Dock button clicked, executing the docking logic...');
        dockModel.docked = true; // update button state
    });
    dockButton.on('undockClicked',  function () { 
        console.log('Undock button clicked, executing the undocking logic...');
        dockModel.docked = false; // update button state
    });
    dockButton.on('focus', function () { console.log('Dock button focused.'); } );

    buttonsContainer.appendChild(dockButton.element());
```

### DockButtonViewModel <a name="dockButtonViewModel"></a>

Extends the [L.Evented](https://leafletjs.com/reference-1.0.0.html#evented) class therefore it provides convenient subscription methods like *on(...)* and *off(...)*.

#### Options
Options object that should be passed to *DockButtonViewModel* constructor.

| Property | Type | Description |
|---|---|---|
| docked | boolean | *Optional*. Initial dock button state. *true* corresponds to docked state while *false* - to undocked state. Default value is *false*. |

#### Properties <a name="categoryGraphViewModelProperties"></a>

| Property | Type | Description |
|---|---|---|
| docked | boolean | Dock button state. *true* corresponds to docked state while *false* - to undocked state. Assign a value to this property in order to change the button state. |

#### Events

| Event | Description
|---|---|
| docked | Triggered when `docked` property is changed. *View* is subscribed on this event in order to reflect the actual state. |

### DockButtonView

Extends the [L.Evented](https://leafletjs.com/reference-1.0.0.html#evented) class therefore it provides convenient subscription methods like *on(...)* and *off(...)*.

#### Options
Options object that should be passed to *DockButtonView* constructor.

| Property | Type | Description |
|---|---|---|
| parent | Node | *Optional*. DOM node object that will be used as parent for the widget. Widget will be placed within the parent. This field is optional because the widget can be put into a DOM node later on, see *[element()](#methods)* method. |
| dockButtonViewModel | [DockButtonViewModel](#dockButtonViewModel) | *Model* object. *View* subscribes on the *Model* and reflects it's state on every change. |

#### Methods <a name="methods"></a>

| Method | Description |
|---|---|
| render() | Renders the button. This method is called automatically when the object is created. This method can be called in order to recreate the widget after `remove()` was called. After calling this method the `element()` method will return a corresponding DOM Node object. |
| remove() | Removes the widget. After calling this method the `element()` method will return `null`. |
| element() | Returns the DOM node object representing the widget. Having this node user can place the widget wherever he wants. |
| show() | Shows the widget. The button is shown by default.  |
| hide() | Hides the widget button. Behavior is the same as applying `display: none` style, i.e. widget doesn't keep the occupied space when hidden. |

#### Events

| Event | Description
|---|---|
| dockClicked | Emitted when dock button is clicked via mouse or via touch. `Dock button` means button in `undocked` state indicating a desire to dock.  |
| undockClicked | Emitted when undock button is clicked via mouse or via touch. `Undock button` means button in `docked` state indicating a desire to undock. |
| focus | Emitted when the dock button gets a focus. |
