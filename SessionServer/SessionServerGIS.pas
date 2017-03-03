unit SessionServerGIS;

interface

uses
  WorldDataCode,
  GisDefs, GisCsSystems, GisLayerSHP, GisLayerVector;

procedure projectGeometryPoint(aGeometryPoint: TWDGeometryPoint; aSourceProjection: TGIS_CSProjectedCoordinateSystem);
procedure projectGeometry(aGeometry: TWDGeometry; aSourceProjection: TGIS_CSProjectedCoordinateSystem);

implementation

procedure projectGeometryPoint(aGeometryPoint: TWDGeometryPoint; aSourceProjection: TGIS_CSProjectedCoordinateSystem);
var
  p: TGIS_Point;
begin
  p.X := aGeometryPoint.x;
  p.Y := aGeometryPoint.y;
  p := aSourceProjection.ToGeocs(p);
  aGeometryPoint.x := p.X;
  aGeometryPoint.y := p.Y;
end;

procedure projectGeometry(aGeometry: TWDGeometry; aSourceProjection: TGIS_CSProjectedCoordinateSystem);
var
  part: TWDGeometryPart;
  point: TWDGeometryPoint;
begin
  for part in aGeometry.parts do
  begin
    for point in part.points
    do projectGeometryPoint(point, aSourceProjection);
  end;
end;

end.
