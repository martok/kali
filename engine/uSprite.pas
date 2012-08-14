{
Kali Engine (C) Martok
https://github.com/martok/kali/
-----------------------------------------------------------------------
For copyright information see file license.txt.
}

unit uSprite;

interface

uses SysUtils, Windows, Types, Graphics, Contnrs;

type
  TRGBAColor = type Cardinal;
  PRGBA = ^TRGBA;
  TRGBA = packed record
    //Bitmap aligned: BGR(A)
    B, G, R, A: byte;
  end;

  TBlendFunc = function(Dest, Source, Color: PRGBA): TRGBA;

  TSprite = class(TBitmap)
  private
    FAlphaReady: boolean;
  protected
    procedure Changed(Sender: TObject); override;
  public
    constructor Create; override;
    constructor CreateTemplate(SourceBitmap: TBitmap);
    procedure DrawTo(Bitmap: TBitmap; X, Y: integer; Color: TRGBAColor = $FFFFFFFF; BlendFunc: TBlendFunc = nil);
  end;

  TMultiSpriteArray = array of array of TSprite;
  TMultiSprite = class
  private
    FFrames: TMultiSpriteArray;
    FCountX, FCountY: integer;
    function GetSprite(X, Y: Integer): TSprite;
  public
    constructor Create(SourceBitmap: TBitmap; TileWidth, TileHeight: integer);
    destructor Destroy; override;
    property Frames[X, Y: Integer]: TSprite read GetSprite; default;
    property CountX: integer read FCountX;
    property CountY: integer read FCountY;
  end;

function BlendFunc_GDI_BitBlt(Dest, Source, Color: PRGBA): TRGBA;
function BlendFunc_GDI_TransparentBlt(Dest, Source, Color: PRGBA): TRGBA;
function BlendFunc_GDI_AlphaBlt(Dest, Source, Color: PRGBA): TRGBA;
function BlendFunc_AlphaBlt(Dest, Source, Color: PRGBA): TRGBA;
function BlendFunc_Alpha_Mask(Dest, Source, Color: PRGBA): TRGBA;

implementation

uses Math, uLogger, Classes;

procedure AlphaBlitBitmap(Source: TBitmap; Dest: TBitmap; SrcRect: TRect; Dst: TPoint; GlobalColor: TColor; BlendFunc: TBlendFunc);
var
  w, h, x, y: integer;
  sln, dln: PRGBA;
  gc: TRGBA;
begin
  Assert(Source.PixelFormat = pf32bit, 'Source PixelFormat must be 32bit');
  Assert(Dest.PixelFormat = pf32bit, 'Dest PixelFormat must be 32bit');
  IntersectRect(SrcRect, SrcRect, Bounds(0, 0, Source.Width, Source.Height));
  if Dst.X < 0 then begin
    inc(SrcRect.Left, -Dst.X);
    Dst.X:= 0;
  end;
  if Dst.Y < 0 then begin
    inc(SrcRect.Top, -Dst.Y);
    Dst.Y:= 0;
  end;
  if Dst.X + (SrcRect.Right - SrcRect.Left) > Dest.Width then
    SrcRect.Right:= Dest.Width - Dst.X + SrcRect.Left;
  if Dst.Y + (SrcRect.Bottom - SrcRect.Top) > Dest.Height then
    SrcRect.Bottom:= Dest.Height - Dst.Y + SrcRect.Top;

  gc.R:= (GlobalColor and $FF);
  gc.G:= (GlobalColor and $FF00) shr 8;
  gc.B:= (GlobalColor and $FF0000) shr 16;
  gc.A:= (GlobalColor and $FF000000) shr 24;
  w:= SrcRect.Right - SrcRect.Left;
  h:= SrcRect.Bottom - SrcRect.Top;
  for y:= 0 to h - 1 do begin
    sln:= Source.ScanLine[SrcRect.Top + y];
    dln:= Dest.ScanLine[Dst.Y + y];
    inc(dln, Dst.X);
    inc(sln, SrcRect.Left);
    for x:= 0 to w - 1 do begin
      dln^:= BlendFunc(dln, sln, @gc);
      inc(dln);
      inc(sln);
    end;
  end;
end;

function BlendFunc_GDI_BitBlt(Dest, Source, Color: PRGBA): TRGBA;
{$IFDEF PUREPASCAL}
begin
  Move(Source^, Result, sizeof(TRGBA));
{$ELSE}
asm
  mov eax, [Source]
{$ENDIF}
end;

function BlendFunc_GDI_TransparentBlt(Dest, Source, Color: PRGBA): TRGBA;
{$IFDEF PUREPASCAL}
begin
  if Source.A > 0 then
    Move(Source^, Result, sizeof(TRGBA))
  else
    Move(Dest^, Result, sizeof(TRGBA))
{$ELSE}
asm
  mov ecx, Source
  cmp byte ptr [ecx].TRGBA.A, 0
  jbe @@retdest
  mov eax, [Source]
  jmp @@end
  @@retdest:
  mov eax, [Dest]
  @@end:
{$ENDIF}
end;

function BlendFunc_GDI_AlphaBlt(Dest, Source, Color: PRGBA): TRGBA;
var
  A, DA: Cardinal;
begin
  A:= Source.A;
  DA:= $FF - A;
  Result.R:= ((DA * Dest.R) + (A * Source.R) + 127) div $100;
  Result.G:= ((DA * Dest.G) + (A * Source.G) + 127) div $100;
  Result.B:= ((DA * Dest.B) + (A * Source.B) + 127) div $100;
end;

function BlendFunc_AlphaBlt(Dest, Source, Color: PRGBA): TRGBA;
var
  A, DA: Cardinal;
begin
  A:= (Source.A * Color.A) div $100;
  DA:= $FF - A;
  Result.R:= ((DA * Dest.R) + (A * (Source.R * Color.R) div $100) + 127) div $100;
  Result.G:= ((DA * Dest.G) + (A * (Source.G * Color.G) div $100) + 127) div $100;
  Result.B:= ((DA * Dest.B) + (A * (Source.B * Color.B) div $100) + 127) div $100;
end;

function BlendFunc_Alpha_Mask(Dest, Source, Color: PRGBA): TRGBA;
var
  A, DA: Cardinal;
begin
  A:= (Source.A * Color.A) div $100;
  DA:= $FF - A;
  Result.R:= ((DA * Dest.R) + (A * Color.R) + 127) div $100;
  Result.G:= ((DA * Dest.G) + (A * Color.G) + 127) div $100;
  Result.B:= ((DA * Dest.B) + (A * Color.B) + 127) div $100;
end;

{ TSprite }

constructor TSprite.Create;
begin
  inherited;
  PixelFormat:= pf32bit;
end;

constructor TSprite.CreateTemplate(SourceBitmap: TBitmap);
begin
  Create;
  Width:= SourceBitmap.Width;
  Height:= SourceBitmap.Height;
  Canvas.Lock;
  try
    SourceBitmap.Canvas.Lock;
    try
      GetLogger.Log('TSprite', 'Handle: %x, WxH: %dx%d', [Canvas.Handle, Width, Height]);
      BitBlt(Canvas.Handle, 0, 0, Width, Height, SourceBitmap.Canvas.Handle, 0, 0, SRCCOPY);
      Changed(Self);
    finally
      SourceBitmap.Canvas.Unlock;
    end;
  finally
    Canvas.Unlock;
  end;
end;

procedure TSprite.Changed(Sender: TObject);
begin
  FAlphaReady:= false;
  inherited;
end;

procedure TSprite.DrawTo(Bitmap: TBitmap; X, Y: integer; Color: TRGBAColor; BlendFunc: TBlendFunc);
begin
  if not Assigned(BlendFunc) then
    BlendFunc:= BlendFunc_GDI_TransparentBlt;
  AlphaBlitBitmap(Self, Bitmap, Bounds(0, 0, Width, Height), Point(X, Y), Color, BlendFunc);
end;

{ TMultiSprite }

constructor TMultiSprite.Create(SourceBitmap: TBitmap; TileWidth, TileHeight: integer);
var
  cx, cy, x, y: integer;
begin
  inherited Create;
  cx:= SourceBitmap.Width div TileWidth;
  cy:= SourceBitmap.Height div TileHeight;
  SetLength(FFrames, cx, cy);
  for x:= 0 to cx - 1 do begin
    for y:= 0 to cy - 1 do begin
      FFrames[x, y]:= TSprite.Create;
      with FFrames[x, y] do begin
        Width:= TileWidth;
        Height:= TileHeight;
        BitBlt(Canvas.Handle, 0, 0, Width, Height, SourceBitmap.Canvas.Handle, X * Width, Y * Height, SRCCOPY);
      end;
    end;
  end;
  FCountX:= cx;
  FCountY:= cy;
end;

destructor TMultiSprite.Destroy;
var
  x, y: integer;
begin
  for x:= 0 to high(FFrames) do
    for y:= 0 to high(FFrames[x]) do begin
      FFrames[X, Y].Free;
    end;
  inherited;
end;

function TMultiSprite.GetSprite(X, Y: Integer): TSprite;
begin
  if (X>=0) and (Y>=0) and (X<length(FFrames)) and (Y<length(FFrames[X])) then
    Result:= FFrames[X, Y]
  else
    raise EListError.CreateFmt('MultiSprite index out of bounds: %d,%d',[X,Y]);
end;

end.

