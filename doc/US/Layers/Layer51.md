# Tiler Layer 51

Tiler layer 51 is a child of layer type 5. Hence, it inherits the property of visualizing the data on both sides of the road with the help of color filled polygons.

The base idea behind the new difference layer was to highlight only the increase or the decrease in a certain parameter when comparing the reference and the active layers.

## Client Requirement

Following is the client requirement provided for this layer:
![Layer51ClientRequirement](./images/Layer51_clientRequirement.PNG) 

The first and the second layers represent the data for the reference and the active layers, while the third layer represents the expected difference layer. 

## Implementation

Layer type 51 reuses the layer computation from its parent layer type 5 for computing the active and the reference layers.

Only a new implementation for the difference layer has been added for this layer.

Active Layer:
![ActiveLayer](./images/Layer51_ActiveLayer.PNG)

Reference Layer:
![ReferenceLayer](./images/Layer51_RefLayer.PNG)


Difference Layer:
![ReferenceLayer](./images/Layer51_DiffLayer.PNG)

Difference Layer - Zoomed in:
![ReferenceLayer](./images/Layer51_DiffLayer_ZoomedIn.PNG)