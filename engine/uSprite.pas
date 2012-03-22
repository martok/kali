unit uSprite;

interface

uses SysUtils, Windows, Graphics, Contnrs;

type
  TSprite = class(TBitmap)
  private
    FAlphaReady: boolean;
  protected
    procedure Changed(Sender: TObject); override;
  public
    constructor Create; override;
    constructor CreateTemplate(SourceBitmap: TBitmap);
    procedure FakeAlpha;
    procedure TransparentDraw(ACanvas: TCanvas; X, Y: integer);
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

const
  BITMAP_TRANSPARENT_COLOR = clPurple;
  BITMAP_SPECIAL_COLOR = (not BITMAP_TRANSPARENT_COLOR) and $FFFFFF;

implementation

uses uLogger;

{ TSprite }

constructor TSprite.Create;
begin
  inherited;
  PixelFormat:= pf32bit;
  Transparent:= true;
  TransparentMode:= tmFixed;
  TransparentColor:= BITMAP_TRANSPARENT_COLOR;
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

procedure TSprite.FakeAlpha;
var
  x, y: integer;
  sl: PDWORD;
begin
  for y:= 0 to Height - 1 do begin
    sl:= ScanLine[y];
    for x:= 0 to Width - 1 do begin
      if (sl^ and $FF000000) = 0 then begin
        sl^:= BITMAP_TRANSPARENT_COLOR;
      end;
      inc(sl);
    end;
  end;
  FAlphaReady:= true;
end;

procedure TSprite.TransparentDraw(ACanvas: TCanvas; X, Y: integer);
begin
  (*
  // Sadly, the headers doen't include parameter names, so heres what MSDN says:
  BOOL TransparentBlt(
    __in  HDC hdcDest,
    __in  int xoriginDest,
    __in  int yoriginDest,
    __in  int wDest,
    __in  int hDest,
    __in  HDC hdcSrc,
    __in  int xoriginSrc,
    __in  int yoriginSrc,
    __in  int wSrc,
    __in  int hSrc,
    __in  UINT crTransparent
  );
  *)
  if not FAlphaReady then
    FakeAlpha;
  TransparentBlt(ACanvas.Handle, X, Y, Width, Height,
    Canvas.Handle, 0, 0, Width, Height, BITMAP_TRANSPARENT_COLOR);
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
  Result:= FFrames[X, Y];
end;

end.

