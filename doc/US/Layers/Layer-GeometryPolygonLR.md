# Tiler Layer: Geometry Polygon Left-Right

This tiler layer has been assigned the LAYER_TYPE number 5 in the database.


The layer can be used to display data for both sides of the road.

The data in this layer is visualized with the help of polygons.  
The color and thickness (width) of these polygons can be used to display certain information about a road.

This layer has 2 class implementations:
1. For visualizing values of any scenario (reference or active).
2. For visualizing the difference between any two scenarios (reference & active).

## Scenario Layer Logic

![ActiveLayer](./images/Layer5.png)

A road has two sides - left & right.  
This layer helps visualize data for both sides of the road with the help of colored polygons.

Both the color and the width of the polygon can be used to convey certain type of information regarding a road.

## Difference Layer Logic

Following are the 3 possible cases for the difference layer:

<table class="temperatureViewStates">
<tr><td><img src="images/Layer5_RefActiveDiff1.png" alt="Case1"/></td></tr>
<tr align="center"><td>Case 1</td></tr>
</table>

<br/>

<table class="temperatureViewStates">
<tr><td><img src="images/Layer5_RefActiveDiff2.png" alt="Case1"/></td></tr>
<tr align="center"><td>Case 2</td></tr>
</table>

<br/>

<table class="temperatureViewStates">
<tr><td><img src="images/Layer5_RefActiveDiff3.png" alt="Case1"/></td></tr>
<tr align="center"><td>Case 3</td></tr>
</table>

In the above cases, the width of the polygon in the reference and the active layers represent a certain value parameter for a road.

The width of the difference layer is computed by calculating the average of the widths of the reference and the active layers ((WidthRef + WidthActive) / 2).

The color of the difference layer represents the following:

|Case|Condition|Diff. Layer Color|
|---|---|---|
|1|WidthRef < WidthActive|Red|
|2|WidthRef > WidthActive|Green|
|3|WidthRef == WidthActive|White|

## Implementation

As mentioned above, the Geometry Polygon Left-Right layer has implementations for two different types of layer.  
1. The Active & the Reference layers.  
2. The Difference layer.

Examples of Active & Reference type layers using Geometry Polygon Left-Right layer:

---

**Example 1**
![ActiveLayer](./images/Layer51_ActiveLayer.PNG)

**Example 2**
![ActiveLayer](./images/Layer52_ActiveLayer.PNG)


Difference layer using Geometry Polygon Left-Right layer:

---

**Difference Layer**
![DifferenceLayer](./images/Layer5_DiffLayer.PNG)
