unit uSurface;

interface

uses Windows, Types, SysUtils, Graphics, uSprite;

type
  TSurface = class(TBitmap)
  private
  public
    constructor Create; override;
    procedure Clear(Color: TColor);
    procedure Blit(X, Y: integer; Graphic: TSprite);
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
  PixelFormat:= pf24bit;
  Canvas.Font.Name:= 'Fixedsys';
  Canvas.Font.Size:= 16;
end;

end.

