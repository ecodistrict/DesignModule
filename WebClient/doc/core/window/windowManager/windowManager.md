# WindowManager

*WindowManager* is a container for [WindowView](../windowView/windowView.md) objects (windows). *WindowManager* is responsible for positioning windows and controlling windows' z-index depending on window focus. Uses [layout](../layout/layout.md) to dock windows and arrange their location. *Layout* object is injected into the *WindowManager* thus it is not stuck to a certain layout.

When window is added to a *WindowManager*, the *WindowManager* will try to dock it by inserting it into its layout. If *WindowManager* failed to dock the window then window remains undocked (floating).

## API reference

Extends the [L.Evented](https://leafletjs.com/reference-1.0.0.html#evented) class therefore it provides convenient subscription methods like *on(...)* and *off(...)*.

#### Usage example

```javascript
    var layout = new PerimeterLayout(...);

    var windowManager = new WindowManager({
        element: document.querySelector('#windows-area'),
        layout: layout
    });

    var graphViewWindow = new CategoryGraphView(...);

    // graphViewWindow is added to the manager and is shown docked inside #windows-area element.
    windowManager.addWindow(graphViewWindow);
    // undock graphViewWindow and make it floating. graphViewWindow will restore its default geometry.
    windowManager.undockWindow(graphViewWindow);
    // dock graphViewWindow.
    windowManager.dockWindow(graphViewWindow);
    // remove graphViewWindow from the manager. This will cause window to destroy and disappear.
    windowManager.removeWindow(graphViewWindow);
```

#### Options
Options object that should be passed to *WindowManager* constructor.

| Property | Type | Description |
|---|---|---|
| element | Node | DOM node object that will be used as a container for all windows added to the *WindowManager*. *element* can be considered as a surface for windows and layout. |
| layout | [Layout](../layout/layout.md) | Layout used by the *WindowManager* to place windows when docking them. When window is docked it is inserted into the layout. |

#### Properties

| Property | Type | Description |
|---|---|---|
| dockingAvailabilityStatus | boolean | Property indicate whether docking is possible or not. This value is deducted from layout capacity. Docking is allowed If layout can allocate space for a window. When this property is changed internally then `dockingAvailabilityStatus` [event](#events) is emitted. |

#### Methods

| Method | Returns | Description |
|---|---|---|
| addWindow([WindowView](../windowView/windowView.md) window) | void | Adds the window to the *WindowManager*. When added the window is put on top of windows stack showing it over other windows within the *WindowManager*. If possible, window will be docked, if not - window will be left undocked (floating). |
| removeWindow([WindowView](../windowView/windowView.md) window) | void | Removes the window from the *WindowManager* and destroys it. |
| dockWindow([WindowView](../windowView/windowView.md) window) | boolean | Docks the window to a space provided by the *layout*. Returns `true` if docking was sucessfull otherwise returns `false`. |
| undockWindow([WindowView](../windowView/windowView.md) window) | void | Undocks the window leaving it floating. |

#### Events <a name="events"></a>

| Event | Data | Description |
|---|---|---|
| dockingAvailabilityStatus | { dockingAvailabilityStatus: boolean } | Emitted when dock possibility changes. `data.dockingAvailabilityStatus` is `true` when docking is possible and `false` when not possible. |
