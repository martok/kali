{
Kali Engine (C) Martok
https://github.com/martok/kali/
-----------------------------------------------------------------------
For copyright information see file license.txt.
}

unit uResources;

interface

uses Windows, SysUtils, Graphics, GraphicEx, uSprite;

type
  TResources = class
  public
    function LoadGraphic(Filename: string): TBitmap;

    function Sprite(ShortName: string): TSprite;
    function CutSprite(ShortName: string; Width, Height: integer): TMultiSprite;
    function TileSprite(ShortName: string; NumX, NumY: integer): TMultiSprite;

    function Font(ShortName: string): TFontName;
  end;

var
  Resources: TResources;

implementation

uses Classes, uTTFUtils, uLogger, uVFS;

{ TResources }

function TResources.LoadGraphic(Filename: string): TBitmap;
var
  gxg: TGraphicExGraphic;
  gxc: TGraphicExGraphicClass;
  stream: IStreamHandle;
begin
  Result:= nil;
  if VFSManager.ReadFile(Filename, stream) then begin
    gxc:= FileFormatList.GraphicFromContent(stream.GetStream);
    if Assigned(gxc) then begin
      gxg:= gxc.Create;
      try
        gxg.LoadFromStream(stream.GetStream);
        GetLogger.Log('Resource','LoadGraphic(%s) as %s SUCCESS',[FileName, gxc.ClassName]);
        Result:= gxg;
      except
        GetLogger.Log('Resource','LoadGraphic(%s) as %s FAILED',[FileName, gxc.ClassName]);
        FreeAndNil(gxc);
        raise;
      end;
    end else
      raise EFOpenError.CreateFmt('Could not load file "%s"', [Filename]);
  end;  
end;

function TResources.Sprite(ShortName: string): TSprite;
var
  s: TBitmap;
begin
  s:= LoadGraphic(ShortName);
  try
    Result:= TSprite.CreateTemplate(s);
  finally
    s.Free;
  end;
end;

function TResources.CutSprite(ShortName: string; Width, Height: integer): TMultiSprite;
var
  s: TBitmap;
begin
  s:= LoadGraphic(ShortName);
  try
    Result:= TMultiSprite.Create(s, Width, Height);
  finally
    s.Free;
  end;
end;

function TResources.TileSprite(ShortName: string; NumX, NumY: integer): TMultiSprite;
var
  s: TBitmap;
begin
  s:= LoadGraphic(ShortName);
  try
    Result:= TMultiSprite.Create(s, s.Width div NumX, s.Height div NumY);
  finally
    s.Free;
  end;
end;

function TResources.Font(ShortName: string): TFontName;
var
  ms: TMemoryStream;
  cn: dword;
  stream: IStreamHandle;
begin
  Result:= '';
  ms:= TMemoryStream.Create;
  try
    if VFSManager.ReadFile(ShortName, stream) then begin
      ms.CopyFrom(stream.GetStream, 0);
      ms.Position:= 0;
      Result:= GetTTFontFullNameFromStream(ms, GetCurrentLocale);
      AddFontMemResourceEx(ms.Memory, ms.Size, nil, @cn);
    end;
  finally
    ms.Free;
  end;
end;

initialization
  Resources:= TResources.Create;
finalization
  FreeAndNil(Resources);
end.

