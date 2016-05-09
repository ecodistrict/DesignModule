tiler will handle diff's on layers
layers can be added dynamically: id and url is returned
layers can be removed or auto removed after a time out or restart
layers can be persistent
tiler is always lat/lon

kinds of layers: 
-geometry, line color, fill color
	updates per geometry, per attribute
-poi
	update location and type
	group pois on zoom level?
	
-receptors+legend
	update per receptor, value, legend
-diff: 2 layer id's{+legend}
	update any of the layers referenced
-png+extent (stretch)
	update of (part of) png (section)

-return affected extent with layer id



TModel: exists once per instance,

handles imb connection and internal administration
handles layers

layers are addressed by id and time



TLayer:
	basic class for tile generation
	layer id
	contents is in TObjectList<TSlice>, sorted by time; 
	if no specific time is used use time=0 (or first)
	http://stackoverflow.com/questions/13252169/how-do-i-sort-a-generic-list-using-a-custom-comparer


TSlice, base class of tiling layer contents

TSliceReceptor, type = 1
	palette to look up value to color
	{id; point x,y in latlon; value}
	delaunay net {triangles}
TSliceGeometry, type = 2
	outline palette
	fill palette
	{id; geometryExtent; geometry, polygon; value}
TSlicePOI, type = 3
	{imageID; image: png}
	{id; point x,y in latlon; imageID}
	zoom handler
TSlicePNG, type = 4
	extent in latlon
	image: png
	discreteColorsOnStretch: boolean
TSliceDiffReceptor, type = -1
	palette
	currentSlice, type = 1
	refSlice, type = 1
TSliceDiffGeometry, type = -2
	palette
	currentSlice, type = 2
	refSlice, type = 2
TSliceDiffPOI, type = -3
	currentSlice, type = 3
	refSlice, type = 3
	color removed poi
	color same poi
	color new poi
TSliceDiffPNG, type = -4
	currentSlice, type = 4
	refSlice, type = 4
	how to buld diff png



TTilerWebModule:

exists per IIS thread
handles IIS request
- default handler, informative, debug, test info
- request tile from layer
- request value on point in layer
