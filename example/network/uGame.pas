unit uGame;

interface

uses Windows, Types, Graphics, SysUtils, Classes, uScreen, uSurface, uResources, uSprite,
  uUtil, uMenu, NarsesBFPA, uNetworking, uHostThread, Dialogs;

type
  TGame = class
  private
    FScreen: TScreen;
    FMenu: TMenu;
    KeyMap: array[0..255] of boolean;
    FHost: THostThread;
    FClient: TClient;
    procedure DummyMenuSelect(Sender: TObject);
    procedure LobbyMenuSelect(Sender: TObject);
    procedure ChatMenuSelect(Sender: TObject);
  protected
    procedure StartServer;
    procedure StartClient(Host: string);
    procedure NetHandshake(Sender: TObject; Conn: TConnectionState; PA: TProtocolAdapter);
    procedure NetConnectFail(Sender: TObject; PA: TProtocolAdapter; Reason: TConnectError; ErrorCode: integer);
    procedure NetDisconnected(Sender: TObject; PA: TProtocolAdapter);
    procedure NetEntityMessage(Sender: TObject; Connection: TConnectionState; Entity: TEntityID; Method: Word; Params: array of Variant);
    procedure NetServerStateChange(Sender: TObject);
  public
    constructor Create(Screen: TScreen);
    procedure InitSurface(Sender: TObject; Surface: TSurface);
    procedure Tick(Sender: TObject; DT: Single);
    procedure Render(Sender: TObject; Surface: TSurface);
    procedure KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure MainMenuSelect(Sender: TObject);
  end;

  TChatterClient = class(TConnectionState)
  public
    Name: string;
    Ready: boolean;
  end;

implementation

uses uGameHost;

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
  FMenu:= TMenu.Create(MainMenuSelect).
    Add('Client', true).
    Add('Server').
    Add('Exit');
end;

procedure TGame.InitSurface(Sender: TObject; Surface: TSurface);
begin
  Surface.Width:= 320;
  Surface.Height:= 240;
end;

procedure TGame.Tick(Sender: TObject; DT: Single);
begin
end;

procedure TGame.Render(Sender: TObject; Surface: TSurface);
begin
  FScreen.Caption:= Format('Target: %d  FPS: %f', [FScreen.FrameTarget, FScreen.CurrentFPS]);
  Surface.Clear(clSilver);
  if Assigned(FMenu) then
    FMenu.Render(Surface);
end;

procedure TGame.KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  KeyMap[Key and $FF]:= true;
  if Assigned(FMenu) then
    FMenu.KeyDown(Key);
end;

procedure TGame.KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  KeyMap[Key and $FF]:= false;
end;

procedure TGame.MainMenuSelect(Sender: TObject);
begin
  case TMenu(Sender).Item of
    0: StartClient(InputBox('Connect','Hostname','localhost'));
    1: StartServer;
    2: FScreen.Close;
  else
    exit;
  end;
  FreeAndNil(FMenu);
end;

procedure TGame.DummyMenuSelect(Sender: TObject);
begin
end;

procedure TGame.StartServer;
begin
  FHost:= THostThread.Create;
  TGameHost.Create(FHost);
  StartClient('localhost');
end;

procedure TGame.StartClient(Host: string);
begin
  FClient:= TClient.Create(nil);
  FClient.UserDataClass:= TChatterClient;
  FClient.OnHandshake:= NetHandshake;  
  FClient.OnConnectFailed:= NetConnectFail;
  FClient.OnDisconnected:= NetDisconnected;
  FClient.OnEntityMessage:= NetEntityMessage;
  FClient.OnServerStateChange:= NetServerStateChange;
  FClient.Connect(Host, NET_PORT);
end;

procedure TGame.NetDisconnected(Sender: TObject; PA: TProtocolAdapter);
begin
  FreeAndNil(FMenu);
  FMenu:= TMenu.Create(MainMenuSelect).
    Add('Client', true).
    Add('Server').
    Add('Exit');
end;

procedure TGame.NetEntityMessage(Sender: TObject; Connection: TConnectionState; Entity: TEntityID; Method: Word; Params: array of Variant);
  procedure ReMenu;
  begin
    FreeAndNil(FMenu);
    FMenu:= TMenu.Create(LobbyMenuSelect).
      Add('Name: ' + TChatterClient(Connection).Name).
      Add('Fertig auf ' + BoolToStr(not TChatterClient(Connection).Ready, true));
  end;

begin
  case FClient.ServerState of
    ssLobby: case Entity of
        ENTITY_MASTER: case Method of
            CMD_SetName: begin
                TChatterClient(Connection).Name:= Params[0];
                ReMenu;
              end;
            CMD_SetReady: begin
                TChatterClient(Connection).Ready:= Params[0];
                ReMenu;
              end;
            CMD_ClearUsers: begin
                ReMenu;
              end;
            CMD_AddUser: begin
                FMenu.
                  Add(' ' + Params[0] + ' ' + BoolToStr(Params[1], true));
              end;
          end;
      end;
    ssRunning: case Entity of
        ENTITY_MASTER: case Method of
            CMD_Write: begin
                FMenu.
                  Add(Params[0] + ': ' + Params[1]);
              end;
          end;
      end;
  end;
end;

procedure TGame.NetHandshake(Sender: TObject; Conn: TConnectionState; PA: TProtocolAdapter);
begin
end;

procedure TGame.LobbyMenuSelect(Sender: TObject);
begin
  case TMenu(Sender).Item of
    0: FClient.CallEntity(ENTITY_MASTER, CMD_SetName, [InputBox('Name', 'Name', TChatterClient(FClient.ConnectionState).Name)]);
    1: FClient.CallEntity(ENTITY_MASTER, CMD_SetReady, [not TChatterClient(FClient.ConnectionState).Ready]);
  else
    exit;
  end;
  FreeAndNil(FMenu);
end;

procedure TGame.ChatMenuSelect(Sender: TObject);
begin
  case TMenu(Sender).Item of
    0: FClient.CallEntity(ENTITY_MASTER, CMD_Write, [InputBox('Text', TChatterClient(FClient.ConnectionState).Name, '')]);
  else
    exit;
  end;
end;

procedure TGame.NetServerStateChange(Sender: TObject);
begin
  case FClient.ServerState of
    ssLobby: begin
        FMenu:= TMenu.Create(LobbyMenuSelect).Add('Name: ').Add('Fertig');
      end;
    ssRunning: begin
        FMenu:= TMenu.Create(ChatMenuSelect).Add('Schreiben als ' + TChatterClient(FClient.ConnectionState).Name);
      end;
  end;
end;

procedure TGame.NetConnectFail(Sender: TObject; PA: TProtocolAdapter; Reason: TConnectError; ErrorCode: integer);
begin
  NetDisconnected(Sender, PA);
end;

end.

