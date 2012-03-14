unit uResources;

interface

uses Windows, SysUtils, Graphics, GraphicEx, uSprite;

type
  TResources = class
  private
    FPath: string;
    procedure SetPath(const Value: string);
  public
    function LoadGraphic(Filename: string): TBitmap;
    function Qualify(NamePart: string): string;

    function Sprite(ShortName: string): TSprite;
    function CutSprite(ShortName: string; Width, Height: integer): TMultiSprite;
    function TileSprite(ShortName: string; NumX, NumY: integer): TMultiSprite;
  published
    property Path: string read FPath write SetPath;
  end;

var
  Resources: TResources;

implementation

uses Classes;

{ TResources }

function TResources.LoadGraphic(Filename: string): TBitmap;
var
  gxg: TGraphicExGraphic;
  gxc: TGraphicExGraphicClass;
begin
  gxc:= FileFormatList.GraphicFromContent(Filename);
  if Assigned(gxc) then begin
    gxg:= gxc.Create;
    try
      gxg.LoadFromFile(Filename);
      Result:= gxg;
    except
      FreeAndNil(gxc);
      raise;
    end;
  end else
    raise EFOpenError.CreateFmt('Could not load file "%s"', [Filename]);
end;

function TResources.Qualify(NamePart: string): string;
begin
  Result:= FPath + NamePart;
end;

procedure TResources.SetPath(const Value: string);
begin
  FPath:= IncludeTrailingPathDelimiter(Value);
end;

function TResources.Sprite(ShortName: string): TSprite;
var
  s: TBitmap;
begin
  s:= LoadGraphic(Qualify(ShortName));
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
  s:= LoadGraphic(Qualify(ShortName));
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
  s:= LoadGraphic(Qualify(ShortName));
  try
    Result:= TMultiSprite.Create(s, s.Width div NumX, s.Height div NumY);
  finally
    s.Free;
  end;
end;

initialization
  Resources:= TResources.Create;
  Resources.Path:= ExtractFilePath(ParamStr(0));
finalization
  FreeAndNil(Resources);
end.

