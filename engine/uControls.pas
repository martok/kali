unit uControls;

interface

uses Types, SysUtils, Windows, Graphics, Classes, Contnrs, Controls, uScreen, uSurface, uSprite, uResources, uUtil;

type
  TColorTableEntry = (ceText, ceDisabled, ceHover, ceActive);

  TGUI = class
  private
    FScreen: TScreen;
    FControls: TObjectList;
    FArt: TMultiSprite;
    FMouse: TMouse;
    FMousePt: TPoint;
    FColors: array[TColorTableEntry] of TColor;
    function GetColor(Entry: TColorTableEntry): TColor;
    procedure SetColor(Entry: TColorTableEntry; const Value: TColor);
  protected
    property Mouse: TMouse read FMouse;
    property MousePt: TPoint read FMousePt;
  public
    constructor Create(Screen: TScreen);
    destructor Destroy; override;

    procedure LoadArt(ImageFile: string);
    property Color[Entry: TColorTableEntry]: TColor read GetColor write SetColor;

    procedure Clear;

    procedure UpdateAndRender(Surface: TSurface);
    procedure KeyDown(Key: Word);
  end;

  TWidget = class
  private
    FGUI: TGUI;
    FPosition: TRect;
    FID: integer;
    FEnabled: boolean;
    procedure SetPosition(const Value: TRect);
  public
    constructor Create(GUI: TGUI);
    procedure Render(Surface: TSurface); virtual;
    procedure KeyDown(Key: Word); virtual;

    property ID: integer read FID write FID;
    property Position: TRect read FPosition write SetPosition;
    property Enabled: boolean read FEnabled write FEnabled;
  end;

  TClickable = class(TWidget)
  private
    FCaption: string;
    FClicked: boolean;
    FOnClick: TNotifyEvent;
  public
    procedure Render(Surface: TSurface); override;
    property Caption: string read FCaption write FCaption;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

implementation

{ TGUI }

constructor TGUI.Create(Screen: TScreen);
begin
  inherited Create;
  FScreen:= Screen;
  FControls:= TObjectList.Create(true);
  FColors[ceText]:= $050505;
  FColors[ceDisabled]:= $505050;
  FColors[ceHover]:= $AAFFFF;
  FColors[ceActive]:= $FFFFFF;
end;

destructor TGUI.Destroy;
begin
  FreeAndNil(FControls);
  inherited;
end;

procedure TGUI.Clear;
begin
  FControls.Clear;
end;

procedure TGUI.KeyDown(Key: Word);
var
  i: integer;
begin
  for i:= 0 to FControls.Count - 1 do
    TWidget(FControls[i]).KeyDown(Key);
end;

procedure TGUI.UpdateAndRender(Surface: TSurface);
var
  i: integer;
begin
  FMouse:= FScreen.Mouse;
  FMousePt:= Point(FMouse.X, FMouse.Y);
  for i:= 0 to FControls.Count - 1 do
    TWidget(FControls[i]).Render(Surface);
end;

procedure TGUI.LoadArt(ImageFile: string);
begin
  FArt:= Resources.TileSprite(ImageFile, 3, 2);
end;

function TGUI.GetColor(Entry: TColorTableEntry): TColor;
begin
  Result:= FColors[Entry];
end;

procedure TGUI.SetColor(Entry: TColorTableEntry; const Value: TColor);
begin
  FColors[Entry]:= Value;
end;

{ TWidget }

constructor TWidget.Create(GUI: TGUI);
begin
  inherited Create;
  FGUI:= GUI;
  FGUI.FControls.Add(Self);
  FID:= -1;
  FEnabled:= true;
end;

procedure TWidget.KeyDown(Key: Word);
begin
end;

procedure TWidget.Render(Surface: TSurface);
begin
end;

procedure TWidget.SetPosition(const Value: TRect);
begin
  FPosition:= Value;
  if FPosition.Right < FPosition.Left + FGUI.FArt[0, 1].Width then
    FPosition.Right:= FPosition.Left + FGUI.FArt[0, 1].Width;
  if FPosition.Bottom < FPosition.Top + FGUI.FArt[0, 1].Height then
    FPosition.Bottom:= FPosition.Top + FGUI.FArt[0, 1].Height;
end;

{ TClickable }

procedure TClickable.Render(Surface: TSurface);
var
  k: integer;
  c: TSize;
  r: TRect;
begin
  k:= FPosition.Left + FGUI.FArt[0, 1].Width;
  while k < FPosition.Right - FGUI.FArt[2, 1].Width do begin
    Surface.Blit(k, FPosition.Top, FGUI.FArt[1, 1]);
    inc(k, FGUI.FArt[1, 1].Width);
  end;

  Surface.Blit(FPosition.Left, FPosition.Top, FGUI.FArt[0, 1]);
  Surface.Blit(FPosition.Right - FGUI.FArt[2, 1].Width, FPosition.Top, FGUI.FArt[2, 1]);

  c:= Surface.Canvas.TextExtent(FCaption);
  r:= rect(0, 0, c.cx, c.cy);
  CenterRectInRect(FPosition, r);

  Surface.Canvas.Brush.Style:= bsClear;
  if FEnabled then begin
    if PtInRect(FPosition, FGUI.MousePt) then begin
      if FGUI.Mouse.Buttons = [mbLeft] then begin
        if not FClicked then begin
          FClicked:= true;
          if Assigned(FOnClick) then
            FOnClick(Self);
        end;
      end else
        FClicked:= false;
      if FClicked then
        Surface.Canvas.Font.Color:= FGUI.Color[ceActive]
      else
        Surface.Canvas.Font.Color:= FGUI.Color[ceHover];
    end else begin
      Surface.Canvas.Font.Color:= FGUI.Color[ceText];
      FClicked:= false;
    end;
  end else begin
    Surface.Canvas.Font.Color:= FGUI.Color[ceDisabled];
    FClicked:= false;
  end;
  Surface.Canvas.Font.Size:= 16;
  Surface.Canvas.TextRect(r, r.Left, r.Top, FCaption);
end;

end.

