unit uSceneGame;

interface

uses
  SysUtils, Classes, Types, Graphics, uGame, uSurface, uControls, uSprite, uResources, uUtil;

type
  TSceneGame = class(TScene)
  private
    FGUI: TGUI;
    Art: record
      Ball: TMultiSprite;
    end;
    FBall, FVec: TFPoint;
  protected
    procedure Init; override;
    procedure Done; override;
    procedure ClickBack(Sender: TObject);
  public
    procedure Tick(DT: Single); override;
    procedure Render(Surface: TSurface); override;
  end;

implementation

uses
  uSceneMainMenu;

{ TSceneMainMenu }

procedure TSceneGame.Init;
begin
  inherited;
  FGUI:= TGUI.Create(Game.Screen);
  FGUI.LoadArt('gfx/gui.png');
  with TClickable.Create(FGUI) do begin
    Position:= Bounds(190,5,0100,12);
    Caption:= 'Back';
    OnClick:= ClickBack;
  end;

  Art.Ball:= Resources.TileSprite('gfx/ball.png',2,1);
  FBall.X:= 100;
  FBall.Y:= 100;
  FVec.X:= 400*(random-0.5);
  FVec.Y:= 400*(random-0.5);
end;

procedure TSceneGame.Done;
begin
  FreeAndNil(FGUI);
  inherited;
end;

procedure TSceneGame.Render(Surface: TSurface);
begin
  Surface.Blit(round(FBall.X)-16, round(FBall.Y)-16,Art.Ball[0,0]);

  FGUI.Render(Surface);
end;

procedure TSceneGame.Tick(DT: Single);
var
  mouse: Boolean;
  bp: TFPoint;
  p: TPoint;
begin
  FGUI.Update(mouse);
  bp.X:= FBall.X + FVec.X * DT;
  bp.Y:= FBall.Y + FVec.Y * DT;
  p:= Point(Round(bp.X), Round(bp.Y));
  if PolyCircleIntersect(RectToPolygon(Rect(0,0,10,320)), p, 8) or
     PolyCircleIntersect(RectToPolygon(Rect(470,0,480,320)), p, 8) then
    FVec.X:= -FVec.X;

  if PolyCircleIntersect(RectToPolygon(Rect(0,0,480,10)), p, 8) or
     PolyCircleIntersect(RectToPolygon(Rect(0,310,480,320)), p, 8) then
    FVec.Y:= -FVec.Y;

  FBall.X:= FBall.X + FVec.X * DT;
  FBall.Y:= FBall.Y + FVec.Y * DT;
end;

procedure TSceneGame.ClickBack(Sender: TObject);
begin
  Game.Scene:= TSceneMainMenu.Create(Game);
end;

end.
