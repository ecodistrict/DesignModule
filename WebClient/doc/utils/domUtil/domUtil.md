# DomUtil

Utility functions to work with the DOM tree, used by the application internally.

## API reference

### DomUtil
DomUtil object contains utility methods to work with the DOM tree.

#### Methods

| Method | Returns | Description |
|---|---|---|
| calculateTextSize(Node node, string text) | [TextSize](#textSize) | Calculates size (both width and height) that given text will have inside the given DOM node. This method takes *node's* style into account. Returns text size. |
| truncateTextByWidth(Node node, string text, number width) | string | Truncates the given text to match the given width. This method uses DOM node to get styles that will be applied to the text. If the given text doesn't match the width then it will be truncated and suffixed with `...` ellipsis. Both truncated text and ellipsis should match the width. Returns truncated text. |
| singleShotEventListener(Node node, string eventType, function callback) | void | Register an event handler for the given event and node. This handler will be triggered when event will be emmitted but it will be triggered only once and event handler will be removed from the node.  |
| onResize(Node node, function callback) | [MutationObserver](https://developer.mozilla.org/en-US/docs/Web/API/MutationObserver) | Registers a callback for resize event on the node. Returns *MutationObserver* object that can be used to cancel the subscription. |

#### TextSize <a name="textSize"></a>

| Property | Type | Description |
|---|---|---|
| width | number | Width in pixels. |
| height | number | Height in pixels. |

#### Usage example

```javascript
var node = ...;

var text = 'Amsterdam';
var textSize = DomUtil.calculateTextSize(node, text);
console.log(textSize.width, textSize.height);

var resizeHandle = DomUtil.onResize(node, function() {
    var nodeRect = node.getBoundingClientRect();
    var truncatedText = DomUtil.truncateTextByWidth(node, text, nodeRect.width);
    node.textContent = truncatedText;
});

...

resizeHandle.disconnect(); // stop tracking element's resize events.
```
