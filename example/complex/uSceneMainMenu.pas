unit uSceneMainMenu;

interface

uses
  SysUtils, Classes, uGame, uSurface, uControls;

type
  TSceneMainMenu = class(TScene)
  private
    FGUI: TGUI;
    FInput: TInput;
  protected
    procedure Init; override;
    procedure Done; override;
    procedure ClickFocusInput(Sender:TObject);
    procedure ClickTestMessage(Sender:TObject);
    procedure ClickGame(Sender:TObject);
    procedure KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);  override;
    procedure KeyPress(Sender: TObject; var Key: Char); override;
  public
    procedure Tick(DT: Single); override;
    procedure Render(Surface: TSurface); override;
  end;

implementation

uses uSceneMessage, uSceneGame;

{ TSceneMainMenu }

procedure TSceneMainMenu.Init;
begin
  inherited;
  FGUI:= TGUI.Create(Game.Screen);
  FGUI.LoadArt('gfx/gui.png');
  with TClickable.Create(FGUI) do begin
    Position:= Bounds(20, 50, 150, 12);
    Caption:= 'Input: ->';
    OnClick:= ClickFocusInput;
  end;
  FInput:= TInput.Create(FGUI);
  with FInput do begin
    Position:= Bounds(200, 50, 100, 12);
  end;
  with TClickable.Create(FGUI) do begin
    Position:= Bounds(240, 70, 60, 12);
    Caption:= '[Test]';
    OnClick:= ClickTestMessage;
  end;

  with TClickable.Create(FGUI) do begin
    Position:= Bounds(20, 100, 150, 12);
    Caption:= 'Play a Game';
    OnClick:= ClickGame;
  end;
end;

procedure TSceneMainMenu.Done;
begin
  FreeAndNil(FGUI);
  inherited;
end;

procedure TSceneMainMenu.Render(Surface: TSurface);
begin
  FGUI.Render(Surface);
end;

procedure TSceneMainMenu.Tick(DT: Single);
var
  mouse: Boolean;
begin
  FGUI.Update(mouse);
end;

procedure TSceneMainMenu.ClickFocusInput(Sender: TObject);
begin
  FGUI.SetFocus(FInput);
end;

procedure TSceneMainMenu.KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  inherited;
  FGUI.KeyDown(Key);
end;

procedure TSceneMainMenu.KeyPress(Sender: TObject; var Key: Char);
begin
  inherited;
  FGUI.KeyPress(Key);

end;

procedure TSceneMainMenu.ClickTestMessage(Sender: TObject);
begin
  Game.Scene:= TSceneMessage.Create(Game, 'Hello, '+FInput.Caption);
end;

procedure TSceneMainMenu.ClickGame(Sender: TObject);
begin
  Game.Scene:= TSceneGame.Create(Game);
end;

end.
