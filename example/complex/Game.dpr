program Game;

uses
  Forms,
  uScreen,
  uGame in 'uGame.pas',
  uSceneGame in 'uSceneGame.pas';

var
  Screen:TScreen;
begin
  Randomize;
  Application.Initialize;
  Screen:= TScreen.CreateNew(Application);
  TGame.Create(Screen);
  Screen.Run;
end.
