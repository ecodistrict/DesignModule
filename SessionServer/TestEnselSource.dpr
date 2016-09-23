program TestEnselSource;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  StdIni,
  Logger, LogConsole, LogFile,
  imb4,
  System.SysUtils;

const
  RemoteHostSwitch = 'RemoteHost';
  RemotePortSwitch = 'RemotePort';


procedure HandleException(aConnection: TConnection; aException: Exception);
begin
  Log.WriteLn('FATAL IMB4 connection exception: '+aException.Message, llError);
end;

procedure HandleDisconnect(aConnection: TConnection);
begin
  Log.WriteLn('DISCONNECT from IMB4 connection', llWarning);
end;

const sources_latitude = 113;
const sources_longitude = 121;
const sources_height = 129;
const sources_timeutc = 137;
const sources_emission_strength_forecast = 145;
const sources_emission_strength_analysis = 153;
const sources_probability_forecast = 161;
const sources_probability_analysis = 169;
const sources_sourceid = 176;


var
  imbConnection: TConnection;
  c: string;
  i: Integer;

  id: TGUID;
  sourceID: Integer;
  lat, lon: Double;
  valueHigh, valueLow: Double;
begin
  try
    FileLogger.SetLogDef(AllLogLevels, [llsTime]);
    // todo: change to tls connection
    imbConnection := TSocketConnection.Create(
      'PublishingServer', 12,
      'ensel',
      GetSetting(RemoteHostSwitch, imbDefaultRemoteHost), GetSetting(RemotePortSwitch, imbDefaultRemoteSocketPort));
    try
      imbConnection.onException := HandleException;
      imbConnection.onDisconnect := HandleDisconnect;
      i := 0;

      id := TGUID.Create('{c07a7ac6-0d91-45e1-aa66-9fe1b96d44b5}');
      sourceID := 219;
      lat := 454550;
      lon := 140350;
      valueHigh := 200;
      valueLow := 1;

      repeat
        ReadLn(c);
        i := i+1;
        if (i mod 2) = 0 then
        begin

          // "fbe5d519-5f0a-4e24-9310-513cecdb662d";63;455550;139750;;"2016-09-16 15:40:00";9.9376202687792;9.93933089469643;0.0343580963534727;0.0343580938174303

          //"c07a7ac6-0d91-45e1-aa66-9fe1b96d44b5";219;454550;140350;;"2016-09-16 12:20:00";1;1;0.25;0.25
          WriteLn('low');
          imbConnection.publish('sources').signalEvent(
            TByteBuffer.bb_tag_double(sources_emission_strength_analysis shr 3, valueLow)+
            TByteBuffer.bb_tag_double(sources_latitude shr 3, lat)+
            TByteBuffer.bb_tag_double(sources_longitude shr 3, lon)+
            TByteBuffer.bb_tag_int32(sources_sourceid shr 3, sourceID)+
            TByteBuffer.bb_tag_guid(icehObjectID, id)
          );
        end
        else
        begin
          WriteLn('high');
          imbConnection.publish('sources').signalEvent(
            TByteBuffer.bb_tag_double(sources_emission_strength_analysis shr 3, valueHigh)+
            TByteBuffer.bb_tag_double(sources_latitude shr 3, lat)+
            TByteBuffer.bb_tag_double(sources_longitude shr 3, lon)+
            TByteBuffer.bb_tag_int32(sources_sourceid shr 3, sourceID)+
            TByteBuffer.bb_tag_guid(icehObjectID, id)
          );
        end;

      until (c='Q') or (c='q');

    finally
      imbConnection.Free;
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
