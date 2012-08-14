{
Kali Engine (C) Martok
https://github.com/martok/kali/
-----------------------------------------------------------------------
For copyright information see file license.txt.
}

unit uSurface;

interface

uses Windows, Types, SysUtils, Graphics, uSprite;

type
  TTextVAlign = (vaTop, vaBottom, vaMiddle);
  TTextHAlign = (haLeft, haRight, haCenter);

  TSurface = class(TBitmap)
  private
    FOrigin: TPoint;
    function GetData(X, Y: Cardinal): PCardinal;
    function GetPixel(X, Y: integer): TColor;
    procedure SetPixel(X, Y: integer; const Value: TColor);
  public
    constructor Create; override;
    procedure Clear(Color: TColor);
    procedure Blit(X, Y: integer; Graphic: TSprite);
    procedure BlitColored(X, Y: integer; Graphic: TSprite; Color: TColor; Alpha: byte=255);
    procedure BlitBlended(X, Y: integer; Graphic: TSprite; BlendFunc: TBlendFunc; Color: TColor; Alpha: byte=255);
    procedure TextAlign(X, Y: integer; Text: string; Vertical: TTextVAlign; Horizontal: TTextHAlign);
    procedure TransformViewport(Origin: TPoint);
    property Pixel[X, Y: integer]: TColor read GetPixel write SetPixel;
  end;

const
  POINT_ORIGIN : TPoint = (X: 0; Y: 0);

implementation

{ TSurface }

constructor TSurface.Create;
begin
  inherited;
  PixelFormat:= pf32bit;
  Canvas.Font.Name:= 'Fixedsys';
  Canvas.Font.Size:= 16;
  TransformViewport(POINT_ORIGIN);
end;

procedure TSurface.Clear(Color: TColor);
begin
  Canvas.Brush.Color:= Color;
  Canvas.Brush.Style:= bsSolid;
  Canvas.FillRect(Rect(0, 0, Width, Height));
end;

procedure TSurface.Blit(X, Y: integer; Graphic: TSprite);
begin
  Graphic.DrawTo(Self, X - FOrigin.X, Y - FOrigin.Y);
end;

procedure TSurface.BlitBlended(X, Y: integer; Graphic: TSprite; BlendFunc: TBlendFunc; Color: TColor; Alpha: byte);
begin
  Graphic.DrawTo(Self, X - FOrigin.X, Y - FOrigin.Y, Color or (Alpha shl 24),BlendFunc);
end;

procedure TSurface.BlitColored(X, Y: integer; Graphic: TSprite; Color: TColor; Alpha: byte);
begin
  Graphic.DrawTo(Self, X - FOrigin.X, Y - FOrigin.Y, Color or (Alpha shl 24));
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

procedure TSurface.TextAlign(X, Y: integer; Text: string; Vertical: TTextVAlign; Horizontal: TTextHAlign);
var cc: TSize;
begin
  cc:= Canvas.TextExtent(Text);
  case Vertical of
    vaTop: ;
    vaBottom: dec(Y, cc.cy);
    vaMiddle: dec(Y, cc.cy div 2);
  end;
  case Horizontal of
    haLeft: ;
    haRight: dec(X, cc.cx);
    haCenter: dec(X, cc.cx div 2);
  end;
  Canvas.TextOut(X, Y, Text);
end;

procedure TSurface.TransformViewport(Origin: TPoint);
begin
  SetWindowOrgEx(Canvas.Handle, Origin.X, Origin.Y, nil);
  FOrigin:= Origin;
end;

end.

