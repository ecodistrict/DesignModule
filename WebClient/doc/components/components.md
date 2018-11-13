# Components
Layer with application-specific components that implement domain logic and provide domain specific representation on UI.
Application and it's UI are build from such components that represent certain domain-specific controls.

| Component | Description |
|---|---|
| [ScaleSlider](scaleSlider/scaleSlider.md) | Generic scale-based slider that can work with arbitrary data types. This slider supports selecting a current value, setting a brush and display events on the scale |
| [TimeSlider](timeSlider/timeSlider.md) | Project-specific slider that represents a timeline and allows operations with selected time, time brushes and events. This slider uses the [ScaleSlider](scaleSlider/scaleSlider.md) internally. |
| [Graph](graph/graph.md) | Presents data as a graph. Supports different kind of graphs, e.g. category graph, continues graph, etc. |
| [Details](details/details.md) | *Details* component allows selecting KPIs, graphs and map layers. |
| [ModuleControl](moduleControl/moduleControl.md) | *ModuleControl* component represents all modules/models (e.g. Air module, Traffic module) available for the current session. |
