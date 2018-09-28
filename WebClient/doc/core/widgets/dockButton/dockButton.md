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

Extends the [View](../../view/view.md) class.

#### Options
Options object that should be passed to *DockButtonView* constructor.

| Property | Type | Description |
|---|---|---|
| dockButtonViewModel | [DockButtonViewModel](#dockButtonViewModel) | *Model* object. *View* subscribes on the *Model* and reflects it's state on every change. |

See also [View](../../view/view.md#options) options.

#### Methods <a name="methods"></a>
See [View](../../view/view.md#methods) methods.

#### Events

| Event | Description
|---|---|
| dockClicked | Emitted when dock button is clicked via mouse or via touch. `Dock button` means button in `undocked` state indicating a desire to dock.  |
| undockClicked | Emitted when undock button is clicked via mouse or via touch. `Undock button` means button in `docked` state indicating a desire to undock. |

See also [View](../../view/view.md#events) events.
