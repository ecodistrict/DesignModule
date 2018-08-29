# CloseButton

*CloseButton* widget implements a close button that can be used as a part of window or any other UI component. Please use this widget instead of creating a new close button element wherever needed. This will keep the application consistent both architecturally and visual style wise.

## Architecture

The widget provides a single class *CloseButtonView* which represents a close button widget.

## API reference

### CloseButtonView

Extends the [L.Evented](https://leafletjs.com/reference-1.0.0.html#evented) class therefore it provides convenient subscription methods like *on(...)* and *off(...)*.

#### Usage example

```javascript
    var buttonsContainer = ... // DOM element used as a container

    var closeButton = new CloseButtonView();
    var closeButton.on('closeClicked', function () { console.log('Close button clicked by mouse or by touch'); } );
    var closeButton.on('focus', function () { console.log('Close button focused.'); } );
    buttonsContainer.appendChild(closeButton.element());
```

#### Options
Options object that should be passed to *CloseButtonView* constructor.

| Property | Type | Description |
|---|---|---|
| parent | Node | *Optional*. DOM node object that will be used as parent for the widget. Widget will be placed within the parent. This field is optional because the widget can be put into a DOM node later on, see *[element()](#methods)* method. |

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
| closeClicked | Emitted when the close button is clicked via mouse or via touch. |
| focus | Emitted when the close button gets a focus. |
