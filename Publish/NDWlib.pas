unit NDWLib;

interface

uses
  WorldDataCode,
  ByteBuffers,
  IMB3NativeClient,
  SysUtils;

const
  tagNDWTime = 1;
  tagNDWLinkID = 2;
  tagNDWSpeed = 3;
  tagNDWFlow = 4;

type
  TNDWConnection = class
  constructor Create(const aRemoteHost: string; aRemotePort: Integer);
  destructor Destroy; override;
  private
    fConnection: TIMBConnection;
    fLiveEvent: TIMBEventEntry;
    procedure HandleNormalEvent(aEvent: TIMBEventEntry; var aPayload: TByteBuffer); stdcall;
  end;

implementation

{ TNDWConnection }

constructor TNDWConnection.Create(const aRemoteHost: string; aRemotePort: Integer);
begin
  inherited Create;
  fConnection := TIMBConnection.Create(aRemoteHost, aRemotePort, 'NDWlistener', 0, 'NDW');
  fLiveEvent := fConnection.Subscribe('Live');
  fLiveEvent.OnNormalEvent := HandleNormalEvent;
end;

destructor TNDWConnection.Destroy;
begin
  fLiveEvent := nil;
  FreeAndNil(fConnection);
  inherited;
end;

procedure TNDWConnection.HandleNormalEvent(aEvent: TIMBEventEntry; var aPayload: TByteBuffer);
begin
//
end;

end.