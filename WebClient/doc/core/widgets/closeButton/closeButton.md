# CloseButton

*CloseButton* widget implements a close button that can be used as a part of window or any other UI component. Please use this widget instead of creating a new close button element wherever needed. This will keep the application consistent both architecturally and visual style wise.

## Architecture

The widget provides a single class *CloseButtonView* which represents a close button widget.

## API reference

### CloseButtonView

Extends the [View](../../view/view.md) class.

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

See [View](../../view/view.md#options) options.

#### Methods <a name="methods"></a>
See [View](../../view/view.md#methods) methods.

#### Events    

| Event | Description
|---|---|
| closeClicked | Emitted when the close button is clicked via mouse or via touch. |

See also [View](../../view/view.md#events) events.
