# Layout

Layout is an entity responsible for managing layout items' placement on a given surface. Layout arranges it's layout items. The main purpose of a layout is to provide a space where layout item can be placed. Layout may ask it's layout items to fill a certain geometry. This can happen on layout resize or when layout items are added or removed.
Layout item could be any visual object that implements [LayoutItem](#layoutItem) interface.

Layout is an abstraction that has custom implementations. This document describes the interface specification for all layouts that can be used together with [WindowManager](../windowManager/windowManager.md). When it is required to have a certain placement of elements on the screen a new layout implementation can be added. See the lists of layout implementations below.

| Layout | Description |
|---|---|
| [PerimeterLayout](../perimeterLayout/perimeterLayout.md) | *Perimeter layout* places layout items within virtual cells along surface borders keeping center and bottom of the surface free. Virtual cells number for each surface border can be specified when constructing the layout. |

**Note**. Important agreement for creating Layouts is that layout shouldn't remove layout items if not asked explicitly.

## API reference

### Layout <a name="layout"></a>

Should extend the [L.Evented](https://leafletjs.com/reference-1.0.0.html#evented) class or its derivatives therefore it provides convenient subscription methods like *on(...)* and *off(...)*.

#### Methods <a name="methods"></a>
The following methods should be implemented to satisfy Layout interface.

| Method | Returns | Description |
|---|---|---|
| resize([Size](#size) size) | void | Specifies surface and layout size. Layout may reposition and resize its layout items. |
| findNearestSpace([LayoutItem](#layoutItem) layoutItem) | [Space](#space) | Returns nearest possible space for the given layout item. It is up to a specific layout to decide on what does it means *nearest* space and if space is available for the item. In case there is no space `null` is returned. |
| findFreeSpace([LayoutItem](#layoutItem) layoutItem) | [Space](#space) | Returns a space not occupied by any other layout item and that can allocate the given layout item. In case there is no such space `null` is returned. |
| getLayoutItem([Space](#space) space) | [LayoutItem](#layoutItem) | Return layout item that is located in the given space. If space is free the `null` is returned. |
| getLayoutItemSpace([LayoutItem](#layoutItem) layoutItem) | [Space](#space) | Returns space that contains the given layout item. If layout item is not present in the layout then `null` is returned. |
| insert([LayoutItem](#layoutItem) layoutItem, [Space](#space) space?)  | boolean | Inserts the given layout item into the layout. *space* is an optional argument. If *space* is provided then layout should try to place the layout item into the space otherwise layout will try to find a free space for the layout item. Returns `true` if layout item was inserted otherwise returns `false`. If item was inserted and no free space left anymore then `freeSpaceStatus` event is emitted with data indicating that no space is left. See [events](#events). |
| remove([LayoutItem](#layoutItem) layoutItem) | void | Removes the given layout item from the layout. If item was removed and some free space appeared then `freeSpaceStatus` event is emitted with data indicating that there is some free space left. See [events](#events). |
| hasFreeSpace() | boolean | Returns `true` if layout still has some free space to place a new layout item. |

#### Events <a name="events"></a>

| Event | Data | Description |
|---|---|---|
| freeSpaceStatus | { freeSpaceAvailable: boolean } | Event is emitted when free space availability changes, e.g. when after adding a new layout item the layout runs out of free spaces. |

### LayoutItem <a name="layoutItem"></a>
Layout item could be any UI element, e.g. WindowView. The main requirement for an UI element to be a LayoutItem is to satisfy the following interface.

| Method | Returns | Description |
|---|---|---|
| fillSpace([SpaceGeometry](#spaceGeometry) spaceGeometry) | void | Fill given space geometry. Layout should move to the given position and resize to the given size. |
| getBoundingRect() | [SpaceGeometry](#spaceGeometry) | Returns layout item geometry including item's position on the surface and its size. |

### Space <a name="space"></a>
Any specific layout can extend the object for its needs but should contain at least the following fields.

| Property | Type | Description |
|---|---|---|
| geometry | [SpaceGeometry](#spaceGeometry) | Space geometry including space position on the surface and its size. |

#### SpaceGeometry <a name="spaceGeometry"></a>

| Property | Type | Description |
|---|---|---|
| x | number | Left position of a space in pixels relative to layout surface. |
| y | number | Top position of a space in pixels relative to layout surface. |
| width | number | Width in pixels. |
| height | number | Height in pixels. |

#### Size <a name="size"></a>

| Property | Type | Description |
|---|---|---|
| width | number | Width in pixels. |
| height | number | Height in pixels. |
