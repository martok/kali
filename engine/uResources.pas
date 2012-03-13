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
begin
  Result:= TSprite.CreateTemplate(LoadGraphic(Qualify(ShortName)));
end;

function TResources.CutSprite(ShortName: string; Width, Height: integer): TMultiSprite;
begin
  Result:= TMultiSprite.Create(LoadGraphic(Qualify(ShortName)), Width, Height);
end;

initialization
  Resources:= TResources.Create;
  Resources.Path:= ExtractFilePath(ParamStr(0));
finalization
  FreeAndNil(Resources);
end.

