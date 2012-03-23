unit uSurface;

interface

uses Windows, Types, SysUtils, Graphics, uSprite;

type
  TSurface = class(TBitmap)
  private
    function GetData(X, Y: Cardinal): PCardinal;
    function GetPixel(X, Y: integer): TColor;
    procedure SetPixel(X, Y: integer; const Value: TColor);
  public
    constructor Create; override;
    procedure Clear(Color: TColor);
    procedure Blit(X, Y: integer; Graphic: TSprite);
    property Pixel[X, Y: integer]: TColor read GetPixel write SetPixel;
  end;

implementation

{ TSurface }

procedure TSurface.Clear(Color: TColor);
begin
  Canvas.Brush.Color:= Color;
  Canvas.Brush.Style:= bsSolid;
  Canvas.FillRect(Rect(0, 0, Width, Height));
end;

procedure TSurface.Blit(X, Y: integer; Graphic: TSprite);
begin
  Graphic.TransparentDraw(Canvas, X, Y);
end;

constructor TSurface.Create;
begin
  inherited;
  PixelFormat:= pf32bit;
  Canvas.Font.Name:= 'Fixedsys';
  Canvas.Font.Size:= 16;
end;

function TSurface.GetPixel(X, Y: integer): TColor;
begin
  Result:= GetData(X, Y)^;
end;

procedure TSurface.SetPixel(X, Y: integer; const Value: TColor);
begin
  GetData(X, Y)^:= Value;
end;

function TSurface.GetData(X, Y: Cardinal): PCardinal;
begin
  Result:= PCardinal(Cardinal(Scanline[Y]) + X * sizeof(Cardinal));
end;

end.

