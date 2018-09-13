# Overview

The application consists of the following parts

| Part | Description |
|---|---|
| [Components](./components/components.md) | Layer with application-specific components that implement domain logic and provide domain specific representation on UI, e.g. [graphs](./components/graph/graph.md). UI part of the components is usually made of building blocks from *core* layer. |
| [Core](./core/core.md) | Layer that provides a set of building blocks to build up the application. These modules are generic and don't have any application specific logic thus *core* layer may be reused in different applications. |
| [Utils](./utils/utils.md) | A set helper modules used within the application. These modules provide functions to work with DOM, polyfills, generic algorithms, etc. |

*Note*. UML is used for class, component and sequence diagrams within this documentation.
