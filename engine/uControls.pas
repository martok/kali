{
Kali Engine (C) Martok
https://github.com/martok/kali/
-----------------------------------------------------------------------
For copyright information see file license.txt.
}

unit uControls;

interface

uses Types, SysUtils, Windows, Graphics, Classes, Contnrs, Controls, uScreen, uSurface, uSprite, uResources, uUtil;

type
  TColorTableEntry = (ceText, ceDisabled, ceHover, ceActive);
  TWidget = class;
  TInput = class;

  TGUI = class
  private
    FScreen: TScreen;
    FControls: TObjectList;
    FArt: TMultiSprite;
    FMouse: TMouse;
    FMousePt: TPoint;
    FClicked: boolean;
    FColors: array[TColorTableEntry] of TColor;
    function GetColor(Entry: TColorTableEntry): TColor;
    procedure SetColor(Entry: TColorTableEntry; const Value: TColor);
  protected
    property Mouse: TMouse read FMouse;
    property MousePt: TPoint read FMousePt;
    function HandleMouse: boolean;
  public
    constructor Create(Screen: TScreen);
    destructor Destroy; override;

    procedure LoadArt(ImageFile: string);
    property Color[Entry: TColorTableEntry]: TColor read GetColor write SetColor;

    procedure Clear;
    procedure SetFocus(Widget: TInput);

    procedure Update(out MouseEventHandled: boolean);
    procedure Render(Surface: TSurface);
    procedure KeyDown(var Key: Word);
    procedure KeyPress(Key: Char);
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
    procedure KeyDown(var Key: Word); virtual;
    procedure KeyPress(var Key: Char); virtual;

    property ID: integer read FID write FID;
    property Position: TRect read FPosition write SetPosition;
    property Enabled: boolean read FEnabled write FEnabled;
  end;

  TClickable = class(TWidget)
  private
    FClicked: boolean;
    FCaption: string;
    FOnClick: TNotifyEvent;
  protected
    procedure Click; virtual;
  public
    procedure Render(Surface: TSurface); override;
    property Caption: string read FCaption write FCaption;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

  TKeyEvent = procedure (Sender: TObject; var Key: char) of object;
  TInput = class(TClickable)
  private
    FFocused: boolean;
    FOnChange: TNotifyEvent;
    FOnKeyPress: TKeyEvent;
  protected
    procedure Changed; virtual;
  public
    procedure Render(Surface: TSurface); override;
    procedure KeyPress(var Key: Char); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnKeyPress: TKeyEvent read FOnKeyPress write FOnKeyPress;
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

procedure TGUI.KeyDown(var Key: Word);
var
  i: integer;
begin
  for i:= 0 to FControls.Count - 1 do
    TWidget(FControls[i]).KeyDown(Key);
end;

procedure TGUI.KeyPress(Key: Char);
var
  i: integer;
begin
  for i:= 0 to FControls.Count - 1 do
    TWidget(FControls[i]).KeyPress(Key);
end;

procedure TGUI.Update(out MouseEventHandled: boolean);
begin
  FMouse:= FScreen.Mouse;
  FMousePt:= Point(FMouse.X, FMouse.Y);
  MouseEventHandled:= HandleMouse;
end;

procedure TGUI.Render(Surface: TSurface);
var
  i: integer;
begin
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

procedure TGUI.SetFocus(Widget: TInput);
var
  i: integer;
begin
  for i:= 0 to FControls.Count - 1 do
    if (FControls[i] is TInput) then
      TInput(FControls[i]).FFocused:= FControls[i] = Widget;
end;

function TGUI.HandleMouse: boolean;
var
  i: integer;
  c: TClickable;
begin
  Result:= false;
  if (Mouse.Buttons = [mbLeft]) then begin
    if not FClicked then begin
      FClicked:= true;
      SetFocus(nil);
      for i:= 0 to FControls.Count - 1 do
        if (FControls[i] is TClickable) then begin
          c:= FControls[i] as TClickable;
          if c.Enabled and
            PtInRect(c.Position, MousePt) then begin
            Result:= true;
            c.FClicked:= true;
            c.Click;
            if c is TInput then
              SetFocus(TInput(c));
            break;
          end else
            c.FClicked:= false;
        end;
    end;
  end else begin
    FClicked:= false;
    for i:= 0 to FControls.Count - 1 do
      if (FControls[i] is TClickable) then
        TClickable(FControls[i]).FClicked:= false;
  end;
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

procedure TWidget.KeyDown(var Key: Word);
begin
end;

procedure TWidget.KeyPress(var Key: Char);
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

procedure TClickable.Click;
begin
  if Assigned(FOnClick) then
    FOnClick(Self);
end;

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
      if FClicked then
        Surface.Canvas.Font.Color:= FGUI.Color[ceActive]
      else
        Surface.Canvas.Font.Color:= FGUI.Color[ceHover];
    end else
      Surface.Canvas.Font.Color:= FGUI.Color[ceText];
  end else
    Surface.Canvas.Font.Color:= FGUI.Color[ceDisabled];
  Surface.Canvas.Font.Size:= 16;
  Surface.Canvas.TextRect(r, r.Left, r.Top, FCaption);
end;

{ TInput }

procedure TInput.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TInput.KeyPress(var Key: Char);
var
  t: string;
begin
  if FFocused then begin
    t:= Caption;
    if Assigned(FOnKeyPress) then FOnKeyPress(Self, Key);
    case Key of
      #0..#7: ;
      #8: Caption:= Copy(Caption, 1, Length(Caption) - 1);
      #9..pred(' '): ;
    else
      Caption:= Caption + Key;
    end;
    if t <> Caption then
      Changed;
  end else
    inherited;
end;

procedure TInput.Render(Surface: TSurface);
var
  k: integer;
  c: TSize;
  p: TRect;
  t: string;
begin
  k:= FPosition.Left + FGUI.FArt[0, 1].Width;
  while k < FPosition.Right - FGUI.FArt[2, 0].Width do begin
    Surface.Blit(k, FPosition.Top, FGUI.FArt[1, 0]);
    inc(k, FGUI.FArt[1, 0].Width);
  end;

  Surface.Blit(FPosition.Left, FPosition.Top, FGUI.FArt[0, 0]);
  Surface.Blit(FPosition.Right - FGUI.FArt[2, 0].Width, FPosition.Top, FGUI.FArt[2, 0]);

  t:= Caption;
  if FFocused and ((trunc(FGUI.FScreen.WorldTime / 0.6) mod 2) = 0) then
    t:= t + '_';

  c:= Surface.Canvas.TextExtent(t);
  p:= FPosition;

  p:= FPosition;
  InflateRect(p, -2, -2);

  Surface.Canvas.Brush.Style:= bsClear;
  if FEnabled then
    Surface.Canvas.Font.Color:= FGUI.Color[ceText]
  else
    Surface.Canvas.Font.Color:= FGUI.Color[ceDisabled];
  Surface.Canvas.Font.Size:= 16;
  Surface.Canvas.TextOut(p.Left, p.Top - 3, t);
end;

end.

