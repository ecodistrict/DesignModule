unit TilerLib;

interface

const
  icehTilerBase = 1400;
  icehTilerSliceID = icehTilerBase+0; // wt64bit, double
  icehTilerSliceUpdate = icehTilerBase+1; // wtLengthDelimited
  icehTilerSliceAction = icehTilerBase+2; // wtVarInt

  icehTilerRequestNewLayer = icehTilerBase+3; // wtVarInt, slice type
  icehTilerEventName = icehTilerBase+4; // wtLengthDelimited, event name
  icehTilerEdgeLength = icehTilerBase+5; // wt64bit, max edge length in meters
  icehTilerPersistent = icehTilerBase+6; // wtVarInt, boolean, store layer information on shutdown to auto load on next start

  icehTilerID = icehTilerBase+7;
  icehTilerURL = icehTilerBase+8;
  icehTilerRefresh = icehTilerBase+9; // wt64bit, TDateTime (ie double) time stamp of data set
  icehTilerRequestPreviewImage = icehTilerBase+10; // wtVarInt, width in pixels of image(=height, square) // todo: make it time stamp?
  icehTilerPreviewImage = icehTilerBase+11; // wtLengthDelimited, png image
  icehTilerLayerDescription = icehTilerBase+12; // wtLengthDelimited, string, description of layer

  //icehRequestData = icehTilerBase+11; // wtVarInt..

  icehTilerStartup = icehTilerBase+20; // vt64bit, TDateTime, tiler startup time

  icehTilerOutlinePalette = icehTilerBase+21; //wtVarInt, dummy
  //icehFillPalette = icehTilerBase+27; //wtVarInt, dummy
  icehTilerPOIImage = icehTilerBase+23; // wtLengthDelimited
  icehTilerPNGExtent = icehTilerBase+24; // wtLengthDelimited
  icehTilerPNGImage = icehTilerBase+30; // wtLengthDelimited
  icehTilerDiscreteColorsOnExtent = icehTilerBase+31; // wtVarInt, boolean
  icehTilerColorRemovedPOI = icehTilerBase+32; //wtVarInt, cardinal=uint32
  icehTilerColorSamePOI = icehTilerBase+33; //wtVarInt, cardinal=uint32
  icehTilerColorNewPOI = icehTilerBase+34; //wtVarInt, cardinal=uint32

  icehTilerLayer = icehTilerBase+41; // wtVarInt, layer id for slice reference
  icehTilerCurrentSlice = icehTilerBase+42; // wt64Bit, date/time, slice id
  icehTilerRefSlice = icehTilerBase+43; // wt64Bit, date/time, slice id

// attributes
  icehAttributeBase = 14; // imb4
  icehTilerGeometry = icehAttributeBase+0;
  icehTilerGeometryPoint = icehAttributeBase+1;
  icehTilerValue = icehAttributeBase+2;
  // us layer IC
  icehTilerValue2 = icehAttributeBase+3;
  icehTilerTexture = icehAttributeBase+4;
  icehTilerTexture2 = icehAttributeBase+5;

  icehTilerPOI = icehAttributeBase+6;

  icehTilerLocationRadius = icehAttributeBase+7;

// slice types
  // normal slice types
  stReceptor = 1;
  stGeometry = 2;
  stGeometryI = 3;
  stGeometryIC = 4;
  stGeometryICLR = 5;
  stPOI  = 6;
  stPNG = 7;
  stLocation = 8;


  // diff slice types
  stDiffReceptor = -stReceptor;
  stDiffGeometry = -stGeometry;
  stDiffGeometryI = -stGeometryI;
  stDiffGeometryIC = -stGeometryIC;
  stDiffGeometryICLR = -stGeometryICLR;
  stDiffPOI = -stPOI;
  stDiffPNG = -stPNG;
  stDiffLocation = -stLocation;



implementation

end.