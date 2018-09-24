unit Zipper;

interface

uses
  AbZipKit, AbUtils, AbZipTyp, AbArcTyp,
  system.Classes,
  system.SysUtils; // zip functionality


function ZipFiles(const aZipFilePath: string; const aRootPath: string; const aFileNames: TArray<string>): Boolean;

function UnZipFiles(const aZipFilePath: string; const aRootPath: string; out aFileNames: TArray<string>): Boolean;


implementation

function ZipFiles(const aZipFilePath: string; const aRootPath: string; const aFileNames: TArray<string>): Boolean;
var
  AbZipKit: TAbZipKit;
  fn: string;
  stream: TFileStream;
begin
  AbZipKit := TAbZipKit.Create(nil);
  try
    AbZipKit.BaseDirectory := aRootPath;
    AbZipKit.ArchiveType := atZip;
    AbZipKit.ForceType := True;
    AbZipKit.FileName := aZipFilePath;
    // store data streams
    for fn in aFileNames do
    begin
      stream := TFileStream.Create(aRootPath+'\'+fn, fmOpenRead);
      try
        AbZipKit.AddFromStream(fn, stream);
      finally
        stream.Free;
      end;
    end;
    AbZipKit.CloseArchive;
    Result := True;
  finally
    FreeAndNil(AbZipKit);
  end;
end;

function UnZipFiles(const aZipFilePath: string; const aRootPath: string; out aFileNames: TArray<string>): Boolean;
var
  AbZipKit: TAbZipKit;
  i: Integer;
begin
  AbZipKit := TAbZipKit.Create(nil);
  try
    AbZipKit.OpenArchive(aZipFilePath);
    AbZipKit.BaseDirectory := aRootPath;
    AbZipKit.ExtractFiles('*.*');
    setLength(aFileNames, AbZipKit.Count);
    for i := 0 to AbZipKit.Count-1
    do aFileNames[i] := AbZipKit.Items[i].FileName;
    AbZipKit.CloseArchive;
    Result := True;
  finally
    FreeAndNil(AbZipKit);
  end;
end;

end.
