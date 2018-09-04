library TilerWebService;

uses
  Winapi.GDIPOBJ,
  Winapi.GDIPAPI,
  FMX.Types,
  Winapi.ActiveX,
  System.Win.ComObj,
  Web.WebBroker,
  Web.Win.ISAPIApp,
  Web.Win.ISAPIThreadPool,
  TilerWebModule in 'TilerWebModule.pas';

{WebModuleTiler: TWebModule}

{$R *.res}

exports
  GetExtensionVersion,
  HttpExtensionProc,
  TerminateExtension;

begin
  // gdi init work-a-round for Winapi.GDIPOBJ NOT initializing GDIplus when it is used in a module/dll
  // Initialize StartupInput structure
  StartupInput.DebugEventCallback := nil;
  StartupInput.SuppressBackgroundThread := False;
  StartupInput.SuppressExternalCodecs   := False;
  StartupInput.GdiplusVersion := 1;
  GdiplusStartup(gdiplusToken, @StartupInput, nil);

  CoInitFlags := COINIT_MULTITHREADED;
  Application.Initialize;
  Application.WebModuleClass := WebModuleClass;
  Application.Run;
end.
