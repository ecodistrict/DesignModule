# Core

Is a layer that provides a set of building blocks to build up the application. These modules are generic and don't have any application specific logic thus *core* layer may be reused in different applications.

*Core* layer consists of the following parts

| Layer part | Description |
|---|---|
| [View](./view/view.md) | *View* is a base class for views in the application. |
| [ModelCollection](./modelCollection/modelCollection.md) | *ModelCollection* is a storage of models. This storage emits events whenever items are added or removed. |
| [Widgets](./widgets/widgets.md) | A set of UI components that implement atomic building blocks of UI. These UI components don't implement any specific logic but are used to create a more complex UI elements. *[Close button](./widgets/closeButton/closeButton.md)* is a good example of such widgets. |
| [Window](./window/window.md) | *Window* component implements application window system providing a base view class for all windows in the application and a number of classes to manage windows. |
