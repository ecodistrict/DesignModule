unit GisDebug;

interface

uses
  //USRepLib,
  SysUtils, Graphics,
  GisDefs, GisControlLegend, GisViewer, GisViewerWnd, GisLayerVector;

var
  gdCurrentViewer: TGIS_ViewerWnd;
  gdLegend: TGIS_ControlLegend;
  gdLayerCount: Integer;
  gdCurrentVectorLayer: TGIS_LayerVector;
  gdCurrentShape: TGIS_Shape;

procedure gdNewLayer;
procedure gdAddShape(aShapeType: TGIS_ShapeType=gisShapeTypePolygon);
procedure gdAddPoint(x, y: Double);
procedure gdAddPointShape(x, y: Double; aID: Integer);
procedure gdUpdate;

type
  TGIS_ViewerWnd_helper = class helper for TGIS_ViewerWnd
    function  GISLayerExists           ( const aLayerName: String): Boolean;
    procedure GISLayerRemoveExisting   ( const aLayerName: String);
    function  GISLayerAddVector        ( const aLayerName, aLayerDescription: string;
                                         aLineWidth: Integer; aLineColor, aFillColor: TColor; aTransparency: Integer;
                                         const aLabelField: string=''): TGIS_LayerVector;
  end;

implementation

function TGIS_ViewerWnd_helper.GISLayerAddVector(const aLayerName,
  aLayerDescription: string; aLineWidth: Integer; aLineColor, aFillColor: TColor;
  aTransparency: Integer; const aLabelField: string): TGIS_LayerVector;
begin
  // remove existing layer with same name
  GISLayerRemoveExisting(aLayerName);
  // build new vector layer
  Result := TGIS_LayerVector.Create;
  try
    Result.Name := aLayerName;
    Result.Comments := aLayerDescription;
    Result.UseRTree := false;
    Result.Params.Line.Width := aLineWidth;
    Result.Params.Line.Color := aLineColor;
    Result.Params.Line.OutlineColor := aLineColor;
    Result.Params.Area.OutlineColor := aLineColor;
    Result.Params.Area.Color := aFillColor;
    Result.Transparency := aTransparency;
    if aLabelField<>'' then
    begin
      Result.Params.Labels.Position := [gisLabelPositionFlow];
      Result.Params.Labels.Field := aLabelField; // 'nHApm';
      Result.Params.Labels.Allocator := True;
      Result.Params.Labels.Duplicates := False;
      Result.Params.Labels.Alignment := gisLabelAlignmentFollow;
      Result.Params.Labels.Color := clNone;
    end;
  finally
    Add(Result);
  end;
end;



procedure gdNewLayer;
begin
  gdCurrentVectorLayer := gdCurrentViewer.GISLayerAddVector('Debug'+IntToStr(gdLayerCount), 'Debug'+IntToStr(gdLayerCount), 1, clBlack, clNone, 60);
  gdCurrentVectorLayer.AddField('ID', gisFieldTypeNumber, 10, 0);
  gdLayerCount := gdLayerCount+1;
end;

procedure gdAddShape(aShapeType: TGIS_ShapeType);
begin
  gdCurrentShape := gdCurrentVectorLayer.CreateShape(aShapeType);
  gdCurrentShape.AddPart;
end;

procedure gdAddPoint(x, y: Double);
begin
  gdCurrentShape.AddPoint(GISPoint(x, y));
end;

procedure gdAddPointShape(x, y: Double; aID: Integer);
var
  LocShape: TGIS_Shape;
begin
  LocShape := gdCurrentVectorLayer.CreateShape(gisShapeTypePoint);
  LocShape.AddPart;
  LocShape.AddPoint(GISPoint(x, y));
  LocShape.SetField('ID', aID);
end;

procedure gdUpdate;
begin
  gdCurrentViewer.Update;
end;

function TGIS_ViewerWnd_helper.GISLayerExists(const aLayerName: String): Boolean;
begin
  Result := Assigned(Get(aLayerName));
end;

procedure TGIS_ViewerWnd_helper.GISLayerRemoveExisting(const aLayerName: String);
begin
  if GISLayerExists(aLayerName)
  then Delete(aLayerName);
end;

initialization
  gdCurrentViewer := nil;
  gdCurrentVectorLayer := nil;
  gdCurrentShape := nil;
  gdLayerCount := 0;
end.
