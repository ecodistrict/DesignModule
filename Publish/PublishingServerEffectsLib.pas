unit PublishingServerEffectsLib;

interface

uses
  StdIni,
  imb4,
  Data.DB,
  SessionServerLib,
  Logger,

  // geometries
  WorldDataCode, // world
  Points2D, Polygons, // effects
  GisDefs, GisCsSystems, GisLayerSHP, GisLayerVector, // tatuk
  ProjectionLib,

  // weather
  wunderground,

  ModelFamilies,

  ProductDefinition,
  CopyProtection,
  OSLib,
  XMLlib,

  CalculationsLib,
  EffectsProjects,
  GridParameters,
  AppAndUserOptions,
  GUIAppAndUserOptions,
  ChemicalDatabases,
  ChemicalDatabasesFD,
  DatabaseForms,
  //EffectsServerDataFormatLib,
  //EffectsServerDataFormat,
  EffectsServerLib,
  ModelParameters,

  AllModelCallers,

  Xml.xmldom, Xml.XMLIntf, Xml.Win.msxmldom, Xml.XMLDoc,

  System.JSON,
  WinApi.Windows,
  Generics.Collections, System.Classes, System.Variants, System.SysUtils, System.StrUtils;

type
  TParameterOverride = record
  class function Create(const aName: string; aValue: Double): TParameterOverride; static;
  public
    name: string;
    value: Double;
  end;

  TIncident = class(TLayerMarker)
  constructor Create(
    aLayer: TLayer; aLat, aLon: Double; const
    aModelName: string;
    const aParameterOverrides: TArray<TParameterOverride>;
    const aContoursToShow: TArray<string>);
  destructor Destroy; override;
  protected
    fModel: TModel;
    fContoursToShow: TArray<string>;
    fContours: TObjectList<TLayerObject>; //  refs
  end;

  TEffectsSessionProject = class(TProject) // publisher project
  constructor Create(aSessionModel: TSessionModel; aConnection: TConnection;
    const aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL: string; aMapView: TMapView;
    aTimeSlider: Integer; aSelectionEnabled, aMeasuresEnabled, aMeasuresHistoryEnabled, aSimulationControlEnabled, aAddBasicLayers: Boolean;
    const aSimulationSetup: string; aMaxNearestObjectDistanceInMeters: Integer);
  destructor Destroy; override;
  private
    fAppOptions: TXMLDocument;
    fUserOptions: TXMLDocument;
    fContours: TLayer;
    fIncidents: TLayer;
  private
    fOwnerXMLComponents: TComponent;
    fOptions: TGUIOptionsClass;
    fDisabledSources: TArray<TCDSourceAndID>;
    fEffectsProject: TEffectsProject; // effects project in publisher project
    fMercatorProjection: TGIS_CSProjectedCoordinateSystem;
    fMeteo: TMeteoInfo;

  public
    property options: TGUIOptionsClass read fOptions;
    procedure Calculate(const aModelName: string; aSetDefaultValues: Boolean);
    procedure ReadBasicData(); override;

    procedure handleTypedClientMessage(const aMessageType: string; var aJSONObject: TJSONObject); override;
    procedure handleClientMessage(aClient: TClient; aScenario: TScenario; aJSONObject: TJSONObject); override;
  end;

function ContourToGeometry(
  aMercatorProjection: TGIS_CSProjectedCoordinateSystem;
  aReleasePointLat, aReleasePointLon: Double; aContourParameter: TContourParameterBase;
  aGeometry: TWDGeometry): Boolean;

implementation

function ContourToGeometry(
  aMercatorProjection: TGIS_CSProjectedCoordinateSystem;
  aReleasePointLat, aReleasePointLon: Double;  aContourParameter: TContourParameterBase;
  aGeometry: TWDGeometry): Boolean;
var
  MeterToMapUnitFactor: Double;
  ParamValue: TPolygon;
  I: Integer;
  Pt: TPoint2D;
  releaseMercator: TGIS_Point;
  p: TGIS_Point;
begin
  releaseMercator := aMercatorProjection.FromGeocs(GisPoint(aReleasePointLon, aReleasePointLat));
  // based on Effects\AbstractionLayer8\GUIGenGIS\ContourToLayer
  MeterToMapUnitFactor := 1/Cos(aReleasePointLat);
  ParamValue := aContourParameter.Value;
  aGeometry.parts.Clear;
  for I := 1 to ParamValue.Count do
  begin
    Pt := ParamValue[I];
    p := aMercatorProjection.ToGeocs(GisPoint(releaseMercator.x + Pt.x*MeterToMapUnitFactor, releaseMercator.y + Pt.y*MeterToMapUnitFactor));
    aGeometry.AddPoint(p.X, p.Y, Double.NaN);
  end;
  Result := True;
end;

function getDisabledSourcesFromOptions(aOptions: TOptionsClass; aChemicalDatabaseSet: TChemicalDatabaseSet): TArray<TCDSourceAndID>;
var
  node: IXMLNode;
  i: Integer;
  sp: TPair<TGUID, TCDSource>;
begin
  node := aOptions.PathToNode(opDisabledChemicalSources, False);
  if Assigned(node) and node.HasChildNodes then
  begin
    SetLength(Result, node.ChildNodes.Count);
    for i := 0 to node.ChildNodes.Count-1 do
    begin
      for sp in ChemicalDatabaseSet.Sources do
      begin
        if sp.Value.SourceWithYear=node.ChildNodes[i].Text then
        begin
          Result[i].SourceID := sp.Key;
          Result[i].Source := sp.Value.Source;
          Result[i].Date := sp.Value.Date;
          Break;
        end;
      end;
    end;
  end
  else SetLength(Result, 0);
end;

var
  incidentNumber: Integer = 0;

function newIncidentName: string;
begin
  Result := 'Incident '+InterlockedIncrement(incidentNumber).ToString;
end;

{ TParameterOverride }

class function TParameterOverride.Create(const aName: string; aValue: Double): TParameterOverride;
begin
  Result.name := aName;
  Result.value := aValue;
end;

{ TIncident }

constructor TIncident.Create(aLayer: TLayer; aLat, aLon: Double; const aModelName: string; const aParameterOverrides: TArray<TParameterOverride>;
  const aContoursToShow: TArray<string>);
var
  incidentName: string;
  p: Integer;
  parameter: TParameter;
begin
  incidentName := newIncidentName;
  inherited Create(aLayer, UTF8String(incidentName.Replace(' ', '')), aLat, aLon);
  fContoursToShow := aContoursToShow;
  fContours := TObjectList<TLayerObject>.Create(False);
  fModel := ModelClasses.CreateModel(aModelName, (aLayer.scenario.project as TEffectsSessionProject).fOptions);
  // set parameter values
  for p := 0 to length(aParameterOverrides)-1 do
  begin
    if fModel.ParameterExists(aParameterOverrides[p].name) then
    begin
      parameter := fModel.Parameters[aParameterOverrides[p].name];
      if parameter is TDoubleParameter
      then (parameter as TDoubleParameter).Value := aParameterOverrides[p].Value
      else Log.WriteLn('Cannot set value of parameter '+aParameterOverrides[p].name+' of type '+parameter.ClassName+' in model '+fModel.Name, llWarning, 1);
    end
    else Log.WriteLn('Parameter '+aParameterOverrides[p].name+' not found in model '+fModel.Name, llWarning, 1);
  end;
  fModel.IsChangedAfterCalc := True;
  fModel.SetDefaultInputValues;
  fModel.ParametersAllScreenRules;
end;

destructor TIncident.Destroy;
begin
  FreeAndNil(fContours);
  FreeAndNil(fModel);
  inherited;
end;

{ TEffectsSessionProject }

procedure TEffectsSessionProject.Calculate(const aModelName: string; aSetDefaultValues: Boolean);
var
  model: TModel;
  p: Integer;
  parameter: TParameter;
  geometry: TWDGeometry;
  _json: string;
begin
  model := ModelClasses.CreateModel(aModelName, fOptions);
  try
    if Assigned(model) then
    begin
      Log.WriteLn('JSONDoCalculation: created model');
      // fill model input from JSON
      //(Model.Parameters[pTotalMassInvolvedInBLEVE] as TDoubleParameter).Value := 1000;
      //FillParametersFromESDF(Model, RequestedCalculation.Parameters, True, False);
      Log.WriteLn('JSONDoCalculation: filled model input');
      model.IsChangedAfterCalc := True;
      if aSetDefaultValues then
      begin
        model.SetDefaultInputValues;

        Log.WriteLn('JSONDoCalculation: set default values');
      end;
      (model.Parameters[pTotalMassReleased{ Total mass released'}] as TDoubleParameter).value := 2437002;
      model.ParametersAllScreenRules;
      Log.WriteLn('JSONDoCalculation: processed screen rules');
      if model.Calculate(vExpert) then
      begin
        Log.WriteLn('OK', llOK);
        // todo: dump parameters to log
        for p := 0 to model.ParameterCount-1 do
        begin
          parameter := model.ParameterByIndex[p];
          Log.WriteLn(p.ToString+', '+parameter.Name+': '+parameter.AsString, llDump, 2);
        end;
        // release point
        _json := fIncidents.AddObject(TLayerMarker.Create(fIncidents, 'the incident', 52.08606, 5.17689, 'the incident')).JSON2D[fIncidents.geometryType, ''];
        _json := '{"type":"updatelayer","payload":{"id":"'+fIncidents.ElementID+'","data":[{"newobject":'+_json+'}]}}';
        fIncidents.forEachClient(procedure(aClient: TClient)
          begin
            aClient.signalString(_json);
          end);
        // add contours to layer
        geometry := TWDGeometry.Create;
        ContourToGeometry(fMercatorProjection, 52.08606, 5.17689, model.ParameterByIndex[71]{['ToxicContourPlotAtZd'{pFirstHeatRadiationContour]} as TContourParameter, geometry);
        _json := fContours.AddObject(TGeometryLayerObject.Create(fContours, 'contour', geometry, 1)).JSON2D[fContours.geometryType, ''];
        _json := '{"type":"updatelayer","payload":{"id":"'+fContours.ElementID+'","data":[{"newobject":'+_json+'}]}}';
        fContours.forEachClient(procedure(aClient: TClient)
          begin
            aClient.signalString(_json);
          end);
      end
      else Log.WriteLn('Failed', llWarning);
      //then CalcResult.Status := TesdfCalculationStatus.ecsCompleted
      //else CalcResult.Status := TesdfCalculationStatus.ecsFailed;
      //Log.WriteLn('JSONDoCalculation: calculated, status '+Ord(CalcResult.Status).toString());
      //CalcResult.ID := RequestedCalculation.ID;
      //CalcResult.Parameters := ParametersToESDF(Model, True, True);
      //CalcResult.Log := LogToESDF(Model.Log);
      //Log.WriteLn('JSONDoCalculation: build results');
      //Result := ctx.AsJson<TesdfCalculationResult>(CalcResult);
      //Log.WriteLn('JSONDoCalculation: encoded results as JSON');
    end
    else Log.WriteLn('Could not create model', llError);
  finally
    model.Free;
  end;
end;

constructor TEffectsSessionProject.Create(aSessionModel: TSessionModel; aConnection: TConnection; const aProjectID, aProjectName, aTilerFQDN,
  aTilerStatusURL: string; aMapView: TMapView; aTimeSlider: Integer; aSelectionEnabled, aMeasuresEnabled, aMeasuresHistoryEnabled,
  aSimulationControlEnabled, aAddBasicLayers: Boolean; const aSimulationSetup: string; aMaxNearestObjectDistanceInMeters: Integer);
var
  baseFileName: string;
  ds: TCDSourceAndID;
  scenario: TScenario;
begin
  fMercatorProjection := CSProjectedCoordinateSystemList.ByEPSG(epsgWebMercator);
  fMeteo := TMeteoInfo.Create(aMapView.lat, aMapView.lon); // default meteo at map center
  // effects environment
  fOwnerXMLComponents := TComponent.Create(nil);
  fAppOptions := TXMLDocument.Create(fOwnerXMLComponents);
  fUserOptions := TXMLDocument.Create(fOwnerXMLComponents);
  baseFileName := ExtractFileName(GetThisModuleFileName);
  baseFileName := LeftStr(baseFileName, Length(baseFileName) - 4);
  fOptions := CreateOptions(ProductBaseName, baseFileName, fAppOptions, fUserOptions);
  LoadSystemDatabase;
  LoadChemicalDatabases(fOptions);
  fDisabledSources := getDisabledSourcesFromOptions(fOptions, ChemicalDatabaseSet);
  if Length(fDisabledSources)>0 then
  begin
    Log.WriteLn('Disabled chemical database sources');
    for ds in fDisabledSources do
    begin
      Log.WriteLn(ds.SourceWithYear, llNormal, 2);
    end;
  end;
  ApplyDefaultOptions(fOptions, nil);
  fEffectsProject := TEffectsProject.Create('','');

  // create 1 scenario
  scenario := TScenario.Create(self, 'main', '1', 'main Effects scenario', False, aMapView, False);
  //scenarios.Add(scenario.ID, scenario);

  // todo: problem: scenario create before project is fully created: ReadBasicData could try to access project which is not fully initialized!

  inherited Create(aSessionModel, aConnection, aProjectID, aProjectName, aTilerFQDN, aTilerStatusURL, nil,
    aTimeSlider, aSelectionEnabled, aMeasuresEnabled, aMeasuresHistoryEnabled, aSimulationControlEnabled, aAddBasicLayers, aSimulationSetup,
    aMaxNearestObjectDistanceInMeters, aMapView,
    scenario, nil);

  // add layers
  fContours := TLayer.Create(scenario, 'Effects', 'contours', 'contours', 'contours', True, '"contour"', 'MultiPolygon', 'geo', True, 1, False);
  //fContours.tilerLayer.palette := TWD
  scenario.Layers.Add(fContours.ID, fContours);
  fIncidents := TLayer.Create(scenario, 'Effects', 'incidents', 'incidents', 'incidents', True, '"point"', 'Marker', 'marker', True, 1, False);
  scenario.Layers.Add(fIncidents.ID, fIncidents);

  // todo: resend domains?
end;

destructor TEffectsSessionProject.Destroy;
begin
  inherited;
  // todo: check
  FreeAndNil(fEffectsProject);
  FreeAndNil(fOptions);
  FreeAndNil(fOwnerXMLComponents); // owns options
end;

procedure TEffectsSessionProject.handleClientMessage(aClient: TClient; aScenario: TScenario; aJSONObject: TJSONObject);
begin
  // handle message
end;

procedure TEffectsSessionProject.handleTypedClientMessage(const aMessageType: string; var aJSONObject: TJSONObject);
var
  layerID: string;
  objectID: string;
  moveto: TJSONObject;
  lat: double;
  lon: double;
  payload: TJSONObject;
  remove: TJSONObject;
begin
  if aMessageType='updatelayerobject' then
  begin
    payload := aJSONObject.GetValue<TJSONObject>('payload');
    layerID := payload.GetValue<string>('layerid');
    objectID := payload.GetValue<string>('objectid');
    if payload.TryGetValue<TJSONObject>('moveto', moveto) then
    begin
      lat := moveto.GetValue<double>('lat');
      lon := moveto.GetValue<double>('lon');
      Log.WriteLn('move '+objectID+' to '+lat.toString+','+lon.toString);
    end;
    if payload.TryGetValue<TJSONObject>('remove', remove) then
    begin
      // todo: implement
    end;
    // ..
  end;
end;

procedure TEffectsSessionProject.ReadBasicData;
begin
  // nothing to do.. for now..

end;

end.
