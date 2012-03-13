unit uGame;

interface

uses Windows, Types, Graphics, SysUtils, Classes, uScreen, uSurface, uResources, uSprite,
  uUtil;

type
  TGame = class
  private
    FScreen: TScreen;
    ballx, bally: single;
    Artwork: record
      Ball: TMultiSprite;
      Char: TMultiSprite;
      Floor: TSprite;
    end;
    KeyMap: array[0..255] of boolean;
    Jumpt: TTimer;
    CanJump: boolean;
    charfrm: integer;
    chrt: TTimer;
  public
    constructor Create(Screen: TScreen);
    procedure InitSurface(Sender: TObject; Surface: TSurface);
    procedure Tick(Sender: TObject; DT: Single);
    procedure Render(Sender: TObject; Surface: TSurface);
    procedure KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
  end;

implementation

{ TGame }

constructor TGame.Create(Screen: TScreen);
begin
  inherited Create;
  FScreen:= Screen;
  FScreen.OnInitSurface:= InitSurface;
  FScreen.OnTick:= Tick;
  FScreen.OnRender:= Render;
  FScreen.OnKeyDown:= KeyDown;
  FScreen.OnKeyUp:= KeyUp;

  Artwork.Ball:= Resources.CutSprite('gfx/ball.png', 32, 32);
  Artwork.Floor:= Resources.Sprite('gfx/floor.png');
  Artwork.Char:= Resources.CutSprite('gfx/StarChar.png', 16, 24);
  ballx:= 120;
  bally:= 120;
  charfrm:= 0;
  chrt:= Timer(0.15, true);
end;

procedure TGame.InitSurface(Sender: TObject; Surface: TSurface);
begin
  Surface.Width:= 320;
  Surface.Height:= 240;
end;

procedure TGame.Tick(Sender: TObject; DT: Single);
var
  k: integer;
begin
  if KeyMap[VK_LEFT] then
    ballx:= ballx - dt * 32 * 4;
  if KeyMap[VK_RIGHT] then
    ballx:= ballx + dt * 32 * 4;
  if KeyMap[VK_SPACE] and CanJump then begin
    Jumpt:= Timer(0.2);
    CanJump:= false;
  end;
  if Jumpt.Step(dt) = 0 then begin
    bally:= bally - dt * 32 * 6;
  end else begin
    bally:= bally + dt * 32 * 4;
    if bally >= 120 then begin
      CanJump:= true;
      Bally:= 120;
    end;
  end;
  k:= chrt.Step(dt);
  if k > 0 then begin
    charfrm:= (charfrm + k) mod 6;
  end;
end;

procedure TGame.Render(Sender: TObject; Surface: TSurface);
var
  i: integer;
begin
  FScreen.Caption:= Format('Target: %d  FPS: %f', [FScreen.FrameTarget, FScreen.CurrentFPS]);
  Surface.Clear(clSilver);
  for i:= 0 to 9 do
    Surface.Blit(i * 32, 120, Artwork.Floor);
  Surface.Blit(trunc(ballx), 120, Artwork.Ball[1, 0]);
  Surface.Blit(trunc(ballx), trunc(bally - 3 + 2 * sin(2 * pi * frac(TScreen(Sender).WorldTime / 2))), Artwork.Ball[0,
    0]);
  Surface.Blit(32, 120, Artwork.Char[charfrm, 0]);
end;

procedure TGame.KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  KeyMap[Key and $FF]:= true;
end;

procedure TGame.KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  KeyMap[Key and $FF]:= false;
end;

end.

