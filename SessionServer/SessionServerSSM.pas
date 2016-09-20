unit SessionServerSSM;

interface

uses
  WorldDataCode,
  SessionServerLib;

type
  TSSMCar = class(TLayerObject)
  constructor Create(aLayer: TLayer; const aID: TWDID; aX, aY, aZ, aZRotation: Double);
  private
    fX: Double;
    fY: Double;
    fZ: Double;
    fZRotation: Double;
  public
    property x: Double read fX;
    property y: Double read fY;
    property z: Double read fZ;
    property zRotation: Double read fZRotation;
  end;

  TSSMLayer  = class(TLayer)

  end;

  TSSMProject  = class(TProject)

  end;



implementation

{ TSSMCar }

constructor TSSMCar.Create(aLayer: TLayer; const aID: TWDID; aX, aY, aZ, aZRotation: Double);
begin
  inherited Create(aLayer, aID);
  fX := aX;
  fY := aY;
  fZ := aZ;
  fZRotation := aZRotation;
end;

end.
