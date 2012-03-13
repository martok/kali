program GameTest;

uses
  Forms,
  uScreen,
  uGame in 'uGame.pas',
  uMenu in '..\..\engine\uMenu.pas',
  uGameHost in 'uGameHost.pas';

var
  Screen: TScreen;

begin
  Application.Initialize;
  Screen:= TScreen.CreateNew(Application);
  TGame.Create(Screen);
  Screen.Run;
end.

