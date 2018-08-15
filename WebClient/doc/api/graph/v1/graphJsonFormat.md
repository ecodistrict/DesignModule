# Graph JSON format

This is a specification of the JSON format generic for all graphs.

## Versioning

Version is defined by the overall graph JSON protocol version. Within a major version changes are expanding and backwards compatible.

| Version | Date | Description |
|---|---|---|
| [1.0](#v1_0) | 2018-08-15 | Basic version of the format. The main concept is the series entity. | 

## Version 1.0 <a name="v1_0"></a>

#### Properties

| Property | Type | Description |
|---|---|---|
| id | string | Unique identifier of the graph. |
| title | string | Graph title. |
| type | string | Graph type. See the possible options [here](#v1_0_graphTypes). |
| axes | [Axes](#v1_0_axes) | Describes what axes are used and what are their properties and settings. Each axis can have a flexible configuration, e.g. axis scale, ticks and units. Every data point is bind to one of the `x` and one of the `y` axes. |
| series | Array&lt;[Series](#v1_0_series)&gt; | A list of objects to be drawn on the graph. Series type and data structure depends on a graph type. |

#### Graph types <a name="v1_0_graphTypes"></a>

| Type | Component | Description |
|---|---|---|
| [category](./categoryGraphJsonFormat.md) | [categoryGraph](../../../components/graph/categoryGraph/categoryGraph.md) | A graph with categories on `x` axis. These graphs can contain bars and lines. |

#### Axes <a name="v1_0_axes"></a>

| Property | Type | Description |
|---|---|---|
| yLeft | [Axis](#v1_0_axis) | `y` axis shown on the left side of the graph. Note that at least one of the `y` axes should be specified. |
| yRight | [Axis](#v1_0_axis) | `y` axis shown on the right side of the graph. Note that at least one of the `y` axes should be specified. |
| xBottom | [Axis](#v1_0_axis) | `x` axis shown on the bottom of the graph. Note that at least one of the `x` axes should be specified. |
| xTop | [Axis](#v1_0_axis) | `x` axis shown on the top of the graph. Note that at least one of the `x` axes should be specified. |

**Note** that each axis property name is axis id that is referenced from within data points, i.e. `"yLeft"` is the id of left `y` axis.

#### Axis <a name="v1_0_axis"></a>

| Property | Type | Description |
|---|---|---|
| title | string | **Optional**. Axis title. |
| scale | Array&lt;number&gt; | **Optional**. Axis scale as an array of two elements where the first element is the axis start value and the second one is it's end. E.g. `[0, 100]` scale will result in a axis with values in the range between 0 and 100. If this property is not set then scale will be selected automatically depending on graph type and series data points. |
| units | string | **Optional**. Axis units. If set then units will be displayed on the axis and together with data point value. |
| ticks | number or Array&lt;number&gt; | **Optional**. If a number then determines how many ticks will be displayed on the axis. If an array then specifies the exact ticks that should be shown, e.g. `[0, 56, 89, 100]` will display 4 ticks on the axis: 0, 56, 89 and 100.  |

#### Series <a name="v1_0_series"></a>

| Property | Type | Description |
|---|---|---|
| id | string | Series id. This id will be used to identify the series so that it can be changed individually instead of changing the whole graph. Because each series is shown as a separate item in the legend the id is used to identify the series that allows enabling or disabling a certain series. |
| type | string | Series type. Possible types are defined individually for every graph type. |
| color | string | **Optional**. If defined series will have the specified color. If not defined then color will be assigned automatically. |
| title | string | Series title. Will be displayed in the legend as well as in tooltip when series is hovered. |
| data | any | Data points of the series. This property is defined individually for every graph type. |
