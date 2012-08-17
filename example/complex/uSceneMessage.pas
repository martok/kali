unit uSceneMessage;

interface

uses
  SysUtils, Classes, Graphics, uGame, uSurface, uControls;

type
  TSceneMessage = class(TScene)
  private
    FGUI: TGUI;
    FMsg: string;
  protected
    procedure Init; override;
    procedure Done; override;
    procedure ClickBack(Sender: TObject);
  public
    constructor Create(AGame: TGame; Message: string);
    procedure Tick(DT: Single); override;
    procedure Render(Surface: TSurface); override;
  end;

implementation

uses
  uSceneMainMenu;

{ TSceneMainMenu }

procedure TSceneMessage.Init;
begin
  inherited;
  FGUI:= TGUI.Create(Game.Screen);
  FGUI.LoadArt('gfx/gui.png');

  with TClickable.Create(FGUI) do begin
    Position:= Bounds(190, 300, 100, 12);
    Caption:= 'Back to Main';
    OnClick:= ClickBack;
  end;
end;

procedure TSceneMessage.Done;
begin
  FreeAndNil(FGUI);
  inherited;
end;

procedure TSceneMessage.Render(Surface: TSurface);
begin
  Surface.Canvas.Font.Color:= clWhite;
  Surface.TextAlign(Surface.Width div 2, Surface.Height div 2, FMsg, vaTop, haCenter);
  FGUI.Render(Surface);
end;

procedure TSceneMessage.Tick(DT: Single);
var
  mouse: Boolean;
begin
  FGUI.Update(mouse);
end;

constructor TSceneMessage.Create(AGame: TGame; Message: string);
begin
  inherited Create(AGame);
  FMsg:= Message;
end;

procedure TSceneMessage.ClickBack(Sender: TObject);
begin
  Game.Scene:= TSceneMainMenu.Create(Game);
end;

end.
