unit uGameHost;

interface

uses Types, uGame, NarsesBFPA, uNetworking, uHostThread;

const
  NET_PORT = 31337;

  CMD_SetName = 1;
  CMD_SetReady = 2;
  CMD_AddUser = 3;
  CMD_Write = 4;
  CMD_ClearUsers = 5;

  SUM_ChatMessage = 1;

type
  TChatUser = class(TConnectionState)
    Name: string;
    Ready: boolean;
  end;

  TGameHost = class
  private
    FHost: THostThread;
    procedure CheckReady;
  public
    constructor Create(Thread: THostThread);
    procedure ThreadBeforeStart(Sender: TObject);
    procedure ThreadAfterEnd(Sender: TObject);
    procedure HostCallEntity(Sender: TObject; Connection: TConnectionState; Entity: TEntityID; Method: Word; Params: array of Variant);
    procedure HostClientDisconnected(Sender: TObject; Connection: TConnectionState; PA: TProtocolAdapter);
  end;

implementation

{ TGameHost }

constructor TGameHost.Create(Thread: THostThread);
begin
  inherited Create;
  FHost:= Thread;
  FHost.OnBeforeStart:= ThreadBeforeStart;
  FHost.OnAfterEnd:= ThreadAfterEnd;
  FHost.Start;
end;

procedure TGameHost.ThreadBeforeStart(Sender: TObject);
begin
  FHost.Host.UserDataClass:= TChatUser;
  FHost.Host.OnCallEntity:= HostCallEntity;
  FHost.Host.OnClientDisconnect:= HostClientDisconnected;
  FHost.Host.Listen(NET_PORT);
end;

procedure TGameHost.ThreadAfterEnd(Sender: TObject);
begin
  //
end;

procedure TGameHost.HostCallEntity(Sender: TObject; Connection: TConnectionState; Entity: TEntityID; Method: Word;
  Params: array of Variant);
var
  l: TConnectionStateList;
  i: integer;
begin
  case Entity of
    ENTITY_MASTER: begin
        case Method of
          CMD_SetName: begin
              TChatUser(Connection).Name:= Params[0];
              Connection.EntityMessage(ENTITY_MASTER, CMD_SetName, [TChatUser(Connection).Name]);
              FHost.Host.EntityMessage(ENTITY_MASTER, CMD_ClearUsers, []);
              l:= FHost.Host.GetAllStates;
              for i:= 0 to high(l) do
                FHost.Host.EntityMessage(ENTITY_MASTER, CMD_AddUser, [TChatUser(l[i]).Name, TChatUser(l[i]).Ready]);
            end;
          CMD_SetReady: begin
              TChatUser(Connection).Ready:= Params[0];
              Connection.EntityMessage(ENTITY_MASTER, CMD_SetReady, [TChatUser(Connection).Ready]);
              FHost.Host.EntityMessage(ENTITY_MASTER, CMD_ClearUsers, []);
              l:= FHost.Host.GetAllStates;
              for i:= 0 to high(l) do
                FHost.Host.EntityMessage(ENTITY_MASTER, CMD_AddUser, [TChatUser(l[i]).Name, TChatUser(l[i]).Ready]);
              CheckReady;
            end;
          CMD_Write: begin
              FHost.Host.EntityMessage(ENTITY_MASTER, CMD_Write, [TChatUser(Connection).Name, Params[0]]);
            end;
        end;
      end;
  end
end;

procedure TGameHost.CheckReady;
var
  l: TConnectionStateList;
  i, r: integer;
begin
  l:= FHost.Host.GetAllStates;
  r:= 0;
  for i:= 0 to high(l) do begin
    if TChatUser(l[i]).Ready then
      inc(r);
  end;
  if length(l) = r then begin
    FHost.Host.StateChange(ssRunning);
  end;
end;

procedure TGameHost.HostClientDisconnected(Sender: TObject; Connection: TConnectionState; PA: TProtocolAdapter);
begin
  FHost.Host.EntityMessage(ENTITY_MASTER, CMD_Write, ['::', TChatUser(Connection).Name+' disconnected']);
end;

end.
 
