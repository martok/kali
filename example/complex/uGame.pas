unit uGame;

interface

uses Classes, Graphics, uScreen, uSurface;

type
  TScene = class;
  TGame = class
  private
    FScreen: TScreen;
    FScene: TScene;
    KeyMap: array[0..255] of boolean;
    procedure SetScene(const Value: TScene);
  public
    constructor Create(Screen: TScreen);
    procedure InitSurface(Sender: TObject; Surface: TSurface);
    procedure Tick(Sender: TObject; DT: Single);
    procedure Render(Sender: TObject; Surface: TSurface);
    procedure KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure KeyPress(Sender: TObject; var Key: Char);
    procedure KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    property Screen: TScreen read FScreen;
    property Scene: TScene read FScene write SetScene;
  end;

  TScene = class
  protected
    Game: TGame;
  protected
    procedure Init; virtual;
    procedure Done; virtual;
    procedure KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState); virtual;
    procedure KeyPress(Sender: TObject; var Key: Char); virtual;
  public
    constructor Create(AGame: TGame);
    destructor Destroy; override;
    procedure Tick(DT: Single); virtual; abstract;
    procedure Render(Surface: TSurface); virtual; abstract;
  end;

implementation

uses
  uSceneMainMenu, SysUtils, uVFS, uVFSFolder;

{ TGame }

constructor TGame.Create(Screen: TScreen);
begin
  inherited Create;
  FScreen:= Screen;
  FScreen.OnInitSurface:= InitSurface;
  FScreen.OnTick:= Tick;
  FScreen.OnRender:= Render;
  FScreen.OnKeyDown:= KeyDown;
  FScreen.OnKeyPress:= KeyPress;
  FScreen.OnKeyUp:= KeyUp;

  VFSManager.AddOverlay(0, '', TvfsFileSystemFolder.Create(ExtractFilePath(ParamStr(0))));

  Scene:= TSceneMainMenu.Create(Self);
end;

procedure TGame.InitSurface(Sender: TObject; Surface: TSurface);
begin
  Surface.Width:= 480;
  Surface.Height:= 320;
end;

procedure TGame.Render(Sender: TObject; Surface: TSurface);
begin
  Surface.Clear(clBlack);
  if Assigned(FScene) then
    FScene.Render(Surface);
end;

procedure TGame.SetScene(const Value: TScene);
begin
  FreeAndNil(FScene);
  FScene:= Value;
end;

procedure TGame.Tick(Sender: TObject; DT: Single);
begin
  if Assigned(FScene) then
    FScene.Tick(DT);
end;

procedure TGame.KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Assigned(FScene) then
    FScene.KeyDown(Sender, Key, Shift);
  if Key > 0 then
    KeyMap[Key and $FF]:= true;
end;

procedure TGame.KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  KeyMap[Key and $FF]:= false;
end;

procedure TGame.KeyPress(Sender: TObject; var Key: Char);
begin
  if Assigned(FScene) then
    FScene.KeyPress(Sender, Key);
end;


{ TScene }

constructor TScene.Create(AGame: TGame);
begin
  inherited Create;
  Game:= AGame;
  Init;
end;

destructor TScene.Destroy;
begin
  Done;
  inherited;
end;

procedure TScene.Done;
begin
end;

procedure TScene.Init;
begin
end;

procedure TScene.KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
end;

procedure TScene.KeyPress(Sender: TObject; var Key: Char);
begin
end;

end.
