# ModelCollection

*ModelCollection* is a storage of models. This storage emits events whenever items are added or removed.

## API reference

Extends the [L.Evented](https://leafletjs.com/reference-1.0.0.html#evented) class therefore it provides convenient subscription methods like *on(...)* and *off(...)*.

#### Options
Options object that should be passed to *ModelCollection* constructor.

| Property | Type | Description |
|---|---|---|
| models | Array&lt;object&gt; | *Optional*. Initial array containing model objects. **Note** that models should have an `id` property. |

#### Properties

| Property | Type | Description |
|---|---|---|
| models | Array&lt;object&gt; | Array containing model objects. Changing this array will not have an effect on the actual property but changing the model objects will actually change them.  |
| length | number | *Read only*. Number of models in the collection. |

#### Methods

| Method | Returns | Description |
|---|---|---|
| set(Array&lt;object&gt; models) | void | Sets the models to the collection. Previously stored models will be removed from this collection. `change` event is emitted. If models are removed from the collection then `remove` event is emitted. |
| add(Array&lt;object&gt; models \| object model) | void | Adds a single model or an array of models to the collection. Previously stored models will be removed from this array. `change` event is emitted. |
| remove(Array&lt;object&gt; models \| object model) | void | Removes a single model or an array of models from the collection. `change` event is emitted. If models are removed from the collection then `remove` event is emitted. |
| contains(object model) | void | returns `true` if the model is stored in the collection. Otherwise returns `false`. |
| getById(string modelId) | object | Returns model object with the given id or returns `null` if such model is missing in the collection. |
| empty() | boolean | returns `true` if the collection is empty and doesn't contain any models. Otherwise returns `false`. |

**Note** if multiple models are removed within a single `set()` or `remove()` call then one `change` event and one `remove` event will be emitted. `change` event will provide the new state of the collection while `remove` event will provide a list of the removed models.

#### Events

| Property | Data | Description |
|---|---|---|
| change | { models:Array &lt;object&gt; } | Emitted whenever models collection change due to models added or removed. |
| remove | { models:Array &lt;object&gt; } | Emitted whenever models are removed from the collection. `data.models` contains an array of removed models. This event may could be triggered by a `remove()` or `set()` calls. |
