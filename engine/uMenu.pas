{
Kali Engine (C) Martok
https://github.com/martok/kali/
-----------------------------------------------------------------------
For copyright information see file license.txt.
}

unit uMenu;

interface

uses Types, Windows, Classes, uSurface;

type
  TMenu = class
  private
    FSelectEvent: TNotifyEvent;
    FItems: array of string;
    FItem: integer;
  protected
    function GetDim(Surface: TSurface): TSize;
  public
    constructor Create(SelectEvent: TNotifyEvent);
    function Add(Text: string; Select: boolean = false): TMenu;
    procedure Render(Surface: TSurface);
    procedure KeyDown(Key: Word);

    property Item: integer read FItem;
  end;

implementation

uses Graphics;

{ TMenu }

constructor TMenu.Create(SelectEvent: TNotifyEvent);
begin
  inherited Create;
  FSelectEvent:= SelectEvent;
  SetLength(FItems, 0);
  FItem:= -1;
end;

function TMenu.Add(Text: string; Select: boolean): TMenu;
var
  v: integer;
begin
  v:= length(FItems);
  SetLength(FItems, v + 1);
  FItems[v]:= Text;
  if Select then
    FItem:= v;
  Result:= Self;
end;

procedure TMenu.Render(Surface: TSurface);
var
  hw: TSize;
  xy: TPoint;
  i: integer;
begin
  hw:= GetDim(Surface);
  xy.X:= (Surface.Width - hw.cx) div 2;
  xy.y:= (Surface.Height - hw.cy) div 2;
  for i:= 0 to high(FItems) do begin
    if FItem = i then
      Surface.Canvas.TextOut(xy.X - Surface.Canvas.TextWidth('>'), xy.Y, '>');
    Surface.Canvas.TextOut(xy.X, xy.Y, FItems[i]);
    inc(xy.Y, Surface.Canvas.TextHeight(FItems[i]));
  end;
end;

function TMenu.GetDim(Surface: TSurface): TSize;
var
  i: integer;
begin
  Result.cx:= 0;
  Result.cy:= 0;
  for i:= 0 to high(FItems) do begin
    inc(Result.cy, Surface.Canvas.TextHeight(FItems[i]));
    if Result.cx < Surface.Canvas.TextWidth(FItems[i]) then
      Result.cx:= Surface.Canvas.TextWidth(FItems[i]);
  end;
end;

procedure TMenu.KeyDown(Key: Word);
begin
  case Key of
    VK_DOWN: FItem:= (FItem + 1) mod length(FItems);
    VK_UP: FItem:= (FItem + length(FItems) - 1) mod length(FItems);
    VK_RETURN: FSelectEvent(Self);
    VK_ESCAPE:  begin
      FItem:= -1;
      FSelectEvent(Self);
    end;
  end;
end;

end.

