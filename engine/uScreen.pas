unit uScreen;

interface

uses Types, SysUtils, Windows, Forms, Controls, Classes, Graphics, uSurface, uUtil;

type
  TSurfaceEvent = procedure(Sender: TObject; Surface: TSurface) of object;
  TTickEvent = procedure(Sender: TObject; DT: single) of object;

  TMouseButtons = set of TMouseButton;
  TMouse = record
    Hover: boolean;
    X, Y: integer;
    Buttons: TMouseButtons;
    Focused: boolean;
  end;

  TScreen = class(TForm)
  private
    FRunning: Boolean;
    FFrameLimiter: Boolean;
    FFrameTarget: integer;
    FFPS: single;
    FTimeStart,
      FTimeLastCount,
      FTimeLastFrame: PrecisionTime;
    FFrameCount: Integer;
    FActualFrame: TRect;
    FWorldTime: double;
    FOnRender: TSurfaceEvent;
    FOnInitSurface: TSurfaceEvent;
    FOnTick: TTickEvent;
    FSurface: TSurface;
    FFullscreen: boolean;
    FMouse: TMouse;
  protected
    procedure AppIdle(Sender: TObject; var Done: Boolean);
    procedure DoClose(var Action: TCloseAction); override;
    procedure Resize; override;
    procedure InitSurface;
    procedure Tick(const DT: single);
    procedure Render;
    procedure Blit;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Activate; override;
    procedure Deactivate; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer; Y: Integer); override;
  public
    procedure AfterConstruction; override;
    procedure Run;
    procedure GoFullscreen;
    procedure ExitFullscreen;
  published
    property FrameLimiter: Boolean read FFrameLimiter write FFrameLimiter;
    property FrameTarget: integer read FFrameTarget write FFrameTarget;
    property CurrentFPS: single read FFPS;
    property Fullscreen: boolean read FFullscreen;

    property WorldTime: Double read FWorldTime;

    property Mouse: TMouse read FMouse;

    property OnInitSurface: TSurfaceEvent read FOnInitSurface write FOnInitSurface;
    property OnTick: TTickEvent read FOnTick write FOnTick;
    property OnRender: TSurfaceEvent read FOnRender write FOnRender;
  end;

implementation

uses Math;

{ TScreen }

procedure TScreen.AfterConstruction;
begin
  inherited;
  Color:= clBlack;
  Position:= poScreenCenter;
  Application.OnIdle:= AppIdle;
  FFrameTarget:= 60;
  FFrameLimiter:= true;
end;

procedure TScreen.DoClose(var Action: TCloseAction);
begin
  inherited;
  Application.Terminate;
end;

procedure TScreen.Run;
begin
  FRunning:= True;
  try
    InitSurface;
    QueryPerformanceCounter(FTimeStart);
    FTimeLastCount:= FTimeStart;
    FTimeLastFrame:= FTimeStart;
    FFrameCount:= 0;
    FWorldTime:= 0;
    Show;
    repeat
      try
        Application.HandleMessage;
      except
        Application.HandleException(Self);
      end;
    until Application.Terminated;
  finally
    FRunning:= False;
  end;
end;

procedure TScreen.AppIdle(Sender: TObject; var Done: Boolean);
var
  ts, te: PrecisionTime;
  dt, ft: single;
begin
  Done:= false;
  QueryPerformanceCounter(ts);
  dt:= (ts - FTimeLastFrame) / PerformanceFrequency;

  FTimeLastFrame:= ts;
  FWorldTime:= FWorldTime + dt;
  Tick(dt);
  Render;
  Blit;
  QueryPerformanceCounter(te);
  inc(FFrameCount);

  if FFrameLimiter then begin
    ft:= 1 / FFrameTarget - (te - ts) / PerformanceFrequency;
    if ft > 0 then
      Sleep(trunc(ft * 1000));
  end;

  dt:= (te - FTimeLastCount) / PerformanceFrequency;
  if dt > 0.1 then begin
    FFPS:= 0.5 * FFPS + 0.5 * (FFrameCount / dt);
    QueryPerformanceCounter(FTimeLastCount);
    FFrameCount:= 0;
  end;
end;

procedure TScreen.InitSurface;
begin
  FSurface:= TSurface.Create;
  if Assigned(FOnInitSurface) then
    FOnInitSurface(Self, FSurface);
  ClientWidth:= FSurface.Width;
  ClientHeight:= FSurface.Height;
end;

procedure TScreen.Render;
begin
  if Assigned(FOnRender) then
    FOnRender(Self, FSurface);
end;

procedure TScreen.Tick(const DT: single);
begin
  if Assigned(FOnTick) then
    FOnTick(Self, DT);
end;

procedure TScreen.Blit;
var
  s: single;
  fr, su: TRect;
begin
  s:= min(ClientWidth / FSurface.Width, ClientHeight / FSurface.Height);
  if s < 1 then
    s:= 1 / ceil(1 / s)
  else
    s:= floor(s);
  fr:= ClientRect;
  su:= Bounds(0, 0, Trunc(FSurface.Width * s), Trunc(FSurface.Height * s));
  CenterRectInRect(fr, su);
  Canvas.Brush.Color:= Color;
  Canvas.Brush.Style:= bsSolid;
  Canvas.FillRect(Rect(0, 0, su.Left, ClientHeight));
  Canvas.FillRect(Rect(0, 0, ClientWidth, su.Top));
  Canvas.FillRect(Rect(su.Right, 0, ClientWidth, ClientHeight));
  Canvas.FillRect(Rect(0, su.Bottom, ClientWidth, ClientHeight));

  SetStretchBltMode(Canvas.Handle, BLACKONWHITE);
  StretchBlt(Canvas.Handle, su.Left, su.Top, su.Right - su.Left, su.Bottom - su.Top,
    FSurface.Canvas.Handle, 0, 0, FSurface.Width, FSurface.Height, SRCCOPY);
  FActualFrame:= su;
end;

procedure TScreen.Resize;
begin
  inherited;
  if (ClientWidth > 0) and (ClientHeight > 0) and Assigned(FSurface) then
    Blit;
end;

procedure TScreen.GoFullscreen;
begin
  BorderStyle:= bsNone;
  WindowState:= wsMaximized;
  FFullscreen:= true;
end;

procedure TScreen.ExitFullscreen;
begin
  WindowState:= wsNormal;
  BorderStyle:= bsSizeable;
  FFullscreen:= false;
end;

procedure TScreen.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (Key = VK_RETURN) and (shift = [ssAlt]) then begin
    if FFullscreen then
      ExitFullscreen
    else
      GoFullscreen;
  end else
    inherited;
end;

procedure TScreen.Activate;
begin
  inherited;
  FMouse.Focused:= true;
end;

procedure TScreen.Deactivate;
begin
  inherited;
  FMouse.Focused:= false;
end;

procedure TScreen.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  Include(FMouse.Buttons, Button);
end;

procedure TScreen.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  FMouse.Hover:= PtInRect(FActualFrame, Point(X,Y));
  if FMouse.Hover then begin
    FMouse.X:= (X - FActualFrame.Left) * FSurface.Width div (FActualFrame.Right-FActualFrame.Left);
    FMouse.Y:= (Y - FActualFrame.Top) * FSurface.Height div (FActualFrame.Bottom-FActualFrame.Top);
  end;
end;

procedure TScreen.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  Exclude(FMouse.Buttons, Button);
end;

end.

