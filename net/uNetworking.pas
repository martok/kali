{
Kali Engine (C) Martok
https://github.com/martok/kali/
-----------------------------------------------------------------------
For copyright information see file license.txt.
}

unit uNetworking;

interface

uses Windows, Types, SysUtils, Classes, NarsesBFPA, uNetworkConst;

type
  TCallParams = array of Variant;
  TEntityID = type Cardinal;
  TServerState = (ssLobby, ssRunning);

const
  ENTITY_MASTER = TEntityID(0);
  ENTITY_ALL = TEntityID(-1);

type
  THost = class;
  TConnectionState = class
  private
    FIsHandshaked: boolean;
    FPA: TProtocolAdapter;
    FHost: THost;
    function GetID: integer;
  public
    constructor Create(PA: TProtocolAdapter; Host: THost); virtual;
  published
    property IsHandshaked: boolean read FIsHandshaked;
    property PA: TProtocolAdapter read FPA;
    property Host: THost read FHost;
    property ID: integer read GetID;
    procedure EntityMessage(Entity: TEntityID; Method: Word; Params: array of Variant);
    procedure EntityMessageFast(Entity: TEntityID; Method: Word; Params: array of Variant);
  end;
  TConnectionStateClass = class of TConnectionState;
  TConnectionStateList = array of TConnectionState;

  TEntityAddEvent = procedure(Sender: TObject; Connection: TConnectionState; Entity: TEntityID; EntityClass: Cardinal) of object;
  TEntityRemoveEvent = procedure(Sender: TObject; Connection: TConnectionState; Entity: TEntityID) of object;
  TEntityCallEvent = procedure(Sender: TObject; Connection: TConnectionState; Entity: TEntityID; Method: Word; Params:
    array of Variant) of object;
  TPAStatusEvent = procedure(Sender: TObject; Connection: TConnectionState; PA: TProtocolAdapter) of object;
  TConnectError = (ceWSA, ceNBFP, ceProtocol, ceOutOfMemory, ceException);
  TConnectErrorEvent = procedure(Sender: TObject; PA: TProtocolAdapter; Reason: TConnectError; ErrorCode: integer) of object;

  THost = class
  private
    FServer: TNBFPAServer;
    FProtoName: string;
    FServerState: TServerState;
    FUserDataClass: TConnectionStateClass;
    FOnCallEntity: TEntityCallEvent;
    FOnClientConnected: TPAStatusEvent;
    FOnClientHandshake: TPAStatusEvent;
    FOnClientDisconnect: TPAStatusEvent;
    procedure ServerClientConnect(Sender: TObject; PA: TProtocolAdapter);
    procedure ServerClientDisconnect(Sender: TObject; const InvalidSessionID: integer);
    procedure ServerClientDestroy(Sender: TObject; PA: TProtocolAdapter);
    procedure ServerClientExecute(Sender: TObject; PA: TProtocolAdapter);
  protected
    FDestroyedState: TConnectionState;
    FDestroyedSession: integer;
    procedure DoClientConnected(PA: TProtocolAdapter); virtual;
    procedure DoHandshaked(Conn: TConnectionState; PA: TProtocolAdapter); virtual;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    property ProtocolName: string read FProtoName write FProtoName;
    property UserDataClass: TConnectionStateClass read FUserDataClass write FUserDataClass;

    property Server: TNBFPAServer read FServer;

    procedure Listen(Port: Integer);

    property State: TServerState read FServerState;
    procedure StateChange(newState: TServerState);
    function GetAllStates: TConnectionStateList;
    procedure EntityAdd(Entity: TEntityID; EntityType: Cardinal);
    procedure EntityRemove(Entity: TEntityID);
    procedure EntityMessage(Entity: TEntityID; Method: Word; Params: array of Variant);

    property OnClientConnected: TPAStatusEvent read FOnClientConnected write FOnClientConnected;
    property OnClientHandshake: TPAStatusEvent read FOnClientHandshake write FOnClientHandshake;
    property OnClientDisconnect: TPAStatusEvent read FOnClientDisconnect write FOnClientDisconnect;
    property OnCallEntity: TEntityCallEvent read FOnCallEntity write FOnCallEntity;
  end;

  TClient = class
  private
    FClient: TNBFPAClient;
    FProtoName: string;
    FUserDataClass: TConnectionStateClass;
    FOnDisconnected: TPANotifyEvent;
    FOnConnected: TPANotifyEvent;
    FOnHandshake: TPAStatusEvent;
    FOnEntityMessage: TEntityCallEvent;
    FServerState: TServerState;
    FOnServerStateChange: TNotifyEvent;
    FOnConnectFailed: TConnectErrorEvent;
    FOnEntityAdd: TEntityAddEvent;
    FOnEntityRemove: TEntityRemoveEvent;
    procedure ClientConnect(Sender: TObject; PA: TProtocolAdapter);
    procedure ClientDisconnect(Sender: TObject; PA: TProtocolAdapter);
    procedure ClientExecute(Sender: TObject; PA: TProtocolAdapter);
    procedure ClientError(Sender: TObject; PA: TProtocolAdapter; Error: TPAError; const ErrorCode: Integer);
  protected
    procedure DoConnected(PA: TProtocolAdapter); virtual;
    procedure DoHandshaked(Conn: TConnectionState; PA: TProtocolAdapter); virtual;
    procedure DoDisonnected(PA: TProtocolAdapter); virtual;
    procedure DoConnectError(PA: TProtocolAdapter; Reason: TConnectError; ErrorCode: integer);
    function GetConn: TConnectionState;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    property ProtocolName: string read FProtoName write FProtoName;
    property UserDataClass: TConnectionStateClass read FUserDataClass write FUserDataClass;

    property Client: TNBFPAClient read FClient;

    procedure Connect(Hostname: string; Port: Integer);

    property ServerState: TServerState read FServerState;
    property ConnectionState: TConnectionState read GetConn;

    procedure CallEntity(Entity: TEntityID; Method: Word; Params: array of Variant);

    property OnConnectFailed: TConnectErrorEvent read FOnConnectFailed write FOnConnectFailed;
    property OnConnected: TPANotifyEvent read FOnConnected write FOnConnected;
    property OnHandshake: TPAStatusEvent read FOnHandshake write FOnHandshake;
    property OnDisconnected: TPANotifyEvent read FOnDisconnected write FOnDisconnected;
    property OnEntityAdd: TEntityAddEvent read FOnEntityAdd write FOnEntityAdd;
    property OnEntityRemove: TEntityRemoveEvent read FOnEntityRemove write FOnEntityRemove;
    property OnEntityMessage: TEntityCallEvent read FOnEntityMessage write FOnEntityMessage;
    property OnServerStateChange: TNotifyEvent read FOnServerStateChange write FOnServerStateChange;
  end;

  TPacket = class
  private
  protected
    class procedure ParseInbound(Frames: TInboundFrameList; Types: string; Data: string; out Params: TCallParams);
    class procedure SerializeParams(Params: array of Variant; Packet: TCmdSeq); overload;
    class procedure SerializeParams(Params: array of Variant; out Types, Data: string); overload;
  public
    class function EntityAdd(Entity: TEntityID; EntityClass: Cardinal): TCmdSeq;
    class function EntityRemove(Entity: TEntityID): TCmdSeq;
    class function EntityMessage(Entity: TEntityID; Method: Word): TCmdSeq;
    class function EntityMessageFast(Entity: TEntityID; Method: Word; Types, Data: string): TCmdSeq;
    class function EntityCall(Entity: TEntityID; Method: Word): TCmdSeq;
    class function StateTransition(NewState: TServerState): TCmdSeq;

    class procedure Params(var Params: TCallParams; SetNew: array of Variant);
    class procedure ParamsPush(var Params: TCallParams; New: array of Variant);
  end;

implementation

uses TypInfo, Variants, WinSock, uUtil;

{ TConnectionState }

constructor TConnectionState.Create(PA: TProtocolAdapter; Host: THost);
begin
  inherited Create;
  FIsHandshaked:= false;
  FPA:= PA;
  FHost:= Host;
end;

procedure TConnectionState.EntityMessage(Entity: TEntityID; Method: Word; Params: array of Variant);
var
  cmd: TCmdSeq;
begin
  cmd:= TPacket.EntityMessage(Entity, Method);
  TPacket.SerializeParams(Params, cmd);
  FPA.Outbound.AddCmdAndFree(cmd);
  FPA.Send();
end;

procedure TConnectionState.EntityMessageFast(Entity: TEntityID; Method: Word; Params: array of Variant);
var
  cmd: TCmdSeq;
  tt, dd: string;
  pp: TCallParams;
  i: integer;
begin
  SetLength(pp, length(Params));
  for i:= 0 to high(pp) do
    pp[i]:= VarCompress(Params[i], true);
  TPacket.SerializeParams(pp, tt, dd);
  cmd:= TPacket.EntityMessageFast(Entity, Method, tt, dd);
  FPA.Outbound.AddCmdAndFree(cmd);
  FPA.Send();
end;

function TConnectionState.GetID: integer;
begin
  if Assigned(FPA) then
    Result:= FPA.SessionID
  else
    Result:= -1;
end;

{ THost }

constructor THost.Create(AOwner: TComponent);
begin
  inherited Create;
  FServer:= TNBFPAServer.Create(AOwner);

  FServer.OnClientConnect:= ServerClientConnect;
  FServer.OnClientDisconnect:= ServerClientDisconnect;
  FServer.OnDestroyPA:= ServerClientDestroy;
  FServer.OnClientExecute:= ServerClientExecute;

  FProtoName:= 'NBFPABASEPROT';
  FUserDataClass:= TConnectionState;
end;

destructor THost.Destroy;
begin
  FreeAndNil(FServer);
  inherited;
end;

procedure THost.Listen(Port: Integer);
var
  flag: integer;
begin
  FServerState:= ssLobby;
  FServer.Port:= Port;
  FServer.Open;
  flag:= 1;
  setsockopt(FServer.Socket.SocketHandle, IPPROTO_TCP, TCP_NODELAY, @flag, sizeof(flag));
end;

procedure THost.ServerClientConnect(Sender: TObject; PA: TProtocolAdapter);
var
  cmd: TCmdSeq;
begin
  if FServerState = ssRunning then begin
    cmd:= TCmdSeq.Create(NETWORK_VERB_STATEERROR);
    PA.Outbound.AddCmdAndFree(cmd);
    PA.Send;
    PA.Disconnect;
  end else begin
    PA.RefObject:= FUserDataClass.Create(PA, Self);
    PA.OwnsRefObject:= true;
    DoClientConnected(PA);
    cmd:= TCmdSeq.Create(NETWORK_VERB_HANDSHAKE);
    cmd.Add(FProtoName);
    PA.Outbound.AddCmdAndFree(cmd);
    PA.Send;
  end;
end;

procedure THost.ServerClientExecute(Sender: TObject; PA: TProtocolAdapter);
var
  verb: Word;
  conn: TConnectionState;
  cmd: TCmdSeq;
  e: TEntityID;
  m: Word;
  p: TCallParams;
begin
  verb:= PA.CurrentToken;
  conn:= TConnectionState(PA.RefObject);
  if not conn.IsHandshaked then begin
    if (verb = NETWORK_VERB_HANDSHAKE) and (PA.Inbound.Strings[1] = FProtoName) then begin
      conn.FIsHandshaked:= true;
      DoHandshaked(conn, PA);
      cmd:= TPacket.StateTransition(FServerState);
      PA.Outbound.AddCmdAndFree(cmd);
      PA.Send;
    end else begin
      cmd:= TCmdSeq.Create(NETWORK_VERB_HSERROR);
      cmd.Add(FProtoName);
      PA.Outbound.AddCmdAndFree(cmd);
      PA.Send;
      PA.Disconnect;
    end;
  end else begin
    case verb of
      NETWORK_VERB_ENTITY_CALL: begin
          if Assigned(FOnCallEntity) then begin
            e:= PA.Inbound.AsCardinal[1];
            m:= PA.Inbound.AsWord[2];
            TPacket.ParseInbound(PA.Inbound, PA.Inbound.Strings[3], PA.Inbound.Strings[4], p);
            FOnCallEntity(Self, Conn, e, m, p);
          end;
        end;
    end;
  end;
end;

procedure THost.DoClientConnected(PA: TProtocolAdapter);
begin
  if Assigned(FOnClientConnected) then
    FOnClientConnected(Self, TConnectionState(PA.RefObject), PA);
end;

procedure THost.EntityAdd(Entity: TEntityID; EntityType: Cardinal);
var
  cmd: TCmdSeq;
begin
  cmd:= TPacket.EntityAdd(Entity, EntityType);
  FServer.Broadcast(cmd, [boImmediate, boFreeCmd]);
end;

procedure THost.EntityRemove(Entity: TEntityID);
var
  cmd: TCmdSeq;
begin
  cmd:= TPacket.EntityRemove(Entity);
  FServer.Broadcast(cmd, [boImmediate, boFreeCmd]);
end;

procedure THost.EntityMessage(Entity: TEntityID; Method: Word; Params: array of Variant);
var
  cmd: TCmdSeq;
begin
  cmd:= TPacket.EntityMessage(Entity, Method);
  TPacket.SerializeParams(Params, cmd);
  FServer.Broadcast(cmd, [boImmediate, boFreeCmd]);
end;

procedure THost.DoHandshaked(Conn: TConnectionState; PA: TProtocolAdapter);
begin
  if Assigned(FOnClientHandshake) then
    FOnClientHandshake(Self, Conn, PA);
end;

procedure THost.StateChange(newState: TServerState);
var
  cmd: TCmdSeq;
begin
  if FServerState <> newState then begin
    FServerState:= newState;
    cmd:= TPacket.StateTransition(newState);
    FServer.Broadcast(cmd, [boImmediate, boFreeCmd]);
  end;
end;

function THost.GetAllStates: TConnectionStateList;
var
  i, k: integer;
begin
  SetLength(Result, FServer.Connections.Count);
  k:= 0;
  for i:= 0 to FServer.Connections.Count - 1 do begin
    if Assigned(TProtocolAdapter(FServer.Connections.Objects[i]).RefObject) and
      (TProtocolAdapter(FServer.Connections.Objects[i]).RefObject is TConnectionState) and
      TConnectionState(TProtocolAdapter(FServer.Connections.Objects[i]).RefObject).IsHandshaked then begin
      Result[k]:= TConnectionState(TProtocolAdapter(FServer.Connections.Objects[i]).RefObject);
      inc(k);
    end;
  end;
  SetLength(Result, k);
end;

procedure THost.ServerClientDisconnect(Sender: TObject; const InvalidSessionID: integer);
begin
  if Assigned(FOnClientDisconnect) then
    FOnClientDisconnect(Self, FDestroyedState, nil);
end;

procedure THost.ServerClientDestroy(Sender: TObject; PA: TProtocolAdapter);
begin
  FDestroyedSession:= PA.SessionID;
  FDestroyedState:= TConnectionState(PA.RefObject);
  FDestroyedState.FPA:= nil;
  PA.RefObject:= nil;
end;

{ TClient }

constructor TClient.Create(AOwner: TComponent);
begin
  inherited Create;
  FClient:= TNBFPAClient.Create(AOwner);

  FClient.OnConnect:= ClientConnect;
  FClient.OnDisconnect:= ClientDisconnect;
  FClient.OnExecute:= ClientExecute;
  FClient.OnError:= ClientError;

  FProtoName:= 'NBFPABASEPROT';
  FUserDataClass:= TConnectionState;
end;

destructor TClient.Destroy;
begin
  FreeAndNil(FClient);
  inherited;
end;

procedure TClient.Connect(Hostname: string; Port: Integer);
begin
  FClient.Host:= Hostname;
  FClient.Port:= Port;
  FClient.Connect;
end;

procedure TClient.ClientConnect(Sender: TObject; PA: TProtocolAdapter);
var
  cmd: TCmdSeq;
  flag: integer;
begin
  flag:= 1;
  setsockopt(PA.Socket.SocketHandle, IPPROTO_TCP, TCP_NODELAY, @flag, sizeof(flag));

  PA.RefObject:= FUserDataClass.Create(PA, nil);
  PA.OwnsRefObject:= true;
  DoConnected(PA);
  cmd:= TCmdSeq.Create(NETWORK_VERB_HANDSHAKE);
  cmd.Add(FProtoName);
  FClient.SendAndFree(cmd);
end;

procedure TClient.ClientDisconnect(Sender: TObject; PA: TProtocolAdapter);
begin
  DoDisonnected(PA);
end;

procedure TClient.ClientExecute(Sender: TObject; PA: TProtocolAdapter);
var
  verb: Word;
  conn: TConnectionState;
  cmd: TCmdSeq;
  e: TEntityID;
  t: Cardinal;
  m: Word;
  p: TCallParams;

  procedure HandleFast;
  var
    d: TStringStream;
    c: byte;
    t, dd: string;
  begin
    d:= TStringStream.Create(PA.Inbound.Strings[1]);
    try
      d.Read(e, sizeof(e));
      d.Read(m, sizeof(m));
      d.Read(c, sizeof(c));
      SetLength(t, c);
      d.Read(t[1], c);
      SetLength(dd, d.Size-d.Position);
      d.Read(dd[1], Length(dd));
      TPacket.ParseInbound(PA.Inbound, t, dd, p);
      FOnEntityMessage(Self, Conn, e, m, p);
    finally
      d.Free;
    end;
  end;
begin
  verb:= PA.CurrentToken;
  conn:= TConnectionState(PA.RefObject);
  if verb = NETWORK_VERB_HSERROR then begin
    DoConnectError(PA, ceProtocol, Integer(PA.Inbound.Strings[1]));
  end else begin
    if not conn.IsHandshaked then begin
      if (verb = NETWORK_VERB_HANDSHAKE) and (PA.Inbound.Strings[1] = FProtoName) then begin
        conn.FIsHandshaked:= true;
        DoHandshaked(conn, PA);
      end else begin
        cmd:= TCmdSeq.Create(NETWORK_VERB_HSERROR);
        cmd.Add(FProtoName);
        PA.Outbound.AddCmdAndFree(cmd);
        PA.Send;
        PA.Disconnect;
      end;
    end else begin
      case verb of
        NETWORK_VERB_STATE_TRANSITION: begin
            FServerState:= TServerState(PA.Inbound.AsByte[1]);
            if Assigned(FOnServerStateChange) then
              FOnServerStateChange(Self);
          end;
        NETWORK_VERB_ENTITY_ADD: begin
            if Assigned(FOnEntityAdd) then begin
              e:= PA.Inbound.AsCardinal[1];
              t:= PA.Inbound.AsCardinal[2];
              FOnEntityAdd(Self, Conn, e, t);
            end;
          end;
        NETWORK_VERB_ENTITY_REMOVE: begin
            if Assigned(FOnEntityRemove) then begin
              e:= PA.Inbound.AsCardinal[1];
              FOnEntityRemove(Self, Conn, e);
            end;
          end;
        NETWORK_VERB_ENTITY_MESSAGE: begin
            if Assigned(FOnEntityMessage) then begin
              e:= PA.Inbound.AsCardinal[1];
              m:= PA.Inbound.AsWord[2];
              TPacket.ParseInbound(PA.Inbound, PA.Inbound.Strings[3], PA.Inbound.Strings[4], p);
              FOnEntityMessage(Self, Conn, e, m, p);
            end;
          end;
        NETWORK_VERB_ENTITY_MESSAGE_FAST: begin
            if Assigned(FOnEntityMessage) then begin
              HandleFast;
            end;
          end;
      end;
    end;
  end;
end;

procedure TClient.ClientError(Sender: TObject; PA: TProtocolAdapter; Error: TPAError; const ErrorCode: Integer);
begin
  if PA.Socket.Connected then begin
  end else
    case Error of
      paeWSA: DoConnectError(PA, ceWSA, ErrorCode);
      paeNoNBFP,
        paeVersion: DoConnectError(PA, ceNBFP, ErrorCode);
      paeOutOfMemory: DoConnectError(PA, ceOutOfMemory, ErrorCode);
      paeException: DoConnectError(PA, ceException, ErrorCode);
    end;
end;

procedure TClient.DoConnected(PA: TProtocolAdapter);
begin
  if Assigned(FOnConnected) then
    FOnConnected(Self, PA);
end;

procedure TClient.DoDisonnected(PA: TProtocolAdapter);
begin
  if Assigned(FOnDisconnected) then
    FOnDisconnected(Self, PA);
end;

procedure TClient.DoHandshaked(Conn: TConnectionState;
  PA: TProtocolAdapter);
begin
  if Assigned(FOnHandshake) then
    FOnHandshake(Self, Conn, PA);
end;

procedure TClient.CallEntity(Entity: TEntityID; Method: Word; Params: array of Variant);
var
  cmd: TCmdSeq;
begin
  cmd:= TPacket.EntityCall(Entity, Method);
  TPacket.SerializeParams(Params, cmd);
  FClient.SendAndFree(cmd);
end;

function TClient.GetConn: TConnectionState;
begin
  Result:= TConnectionState(FClient.PA.RefObject);
end;

procedure TClient.DoConnectError(PA: TProtocolAdapter; Reason: TConnectError; ErrorCode: integer);
begin
  if Assigned(FOnConnectFailed) then
    FOnConnectFailed(Self, PA, Reason, ErrorCode);
end;

{ TPacket }

class procedure TPacket.ParseInbound(Frames: TInboundFrameList; Types: string; Data: string; out Params: TCallParams);
var
  i: integer;
  dat: TStringStream;
  c: Cardinal;
  str: string;
  sin: Single;
  dou: Double;
  cur: Currency;
  byt: Byte;
  wor: Word;
  int: Integer;
  car: Cardinal;
  in6: int64;
  boo: boolean;
begin
  SetLength(Params, length(types));
  dat:= TStringStream.Create(Data);
  try
    for i:= 0 to length(types) - 1 do begin
      case types[i + 1] of
        'N': Params[i]:= Null;
        's': begin
            dat.Read(c, sizeof(c));
            SetLength(str, c);
            dat.Read(str[1], c);
            Params[i]:= str;
          end;
        'S': begin
            dat.Read(c, sizeof(c));
            SetLength(str, c);
            dat.Read(str[1], c);
            Params[i]:= UTF8Decode(str);
          end;
        'f': begin
            dat.Read(sin, sizeof(sin));
            Params[i]:= sin;
          end;
        'd': begin
            dat.Read(dou, sizeof(dou));
            Params[i]:= dou;
          end;
        'D': begin
            dat.Read(dou, sizeof(dou));
            Params[i]:= TDateTime(dou);
          end;
        'c': begin
            dat.Read(cur, sizeof(cur));
            Params[i]:= cur;
          end;
        'B': begin
            dat.Read(byt, sizeof(byt));
            Params[i]:= ShortInt(byt);
          end;
        'b': begin
            dat.Read(byt, sizeof(byt));
            Params[i]:= byt;
          end;
        'W': begin
            dat.Read(wor, sizeof(wor));
            Params[i]:= SmallInt(wor);
          end;
        'w': begin
            dat.Read(wor, sizeof(wor));
            Params[i]:= wor;
          end;
        'I': begin
            dat.Read(int, sizeof(int));
            Params[i]:= int;
          end;
        'i': begin
            dat.Read(car, sizeof(car));
            Params[i]:= car;
          end;
        '6': begin
            dat.Read(in6, sizeof(in6));
            Params[i]:= in6;
          end;
        'L': begin
            dat.Read(boo, sizeof(boo));
            Params[i]:= boo;
          end;
      else
        raise EVariantInvalidArgError.CreateFmt('Unknown transferred type: %s', [types[i + 1]]);
      end;
    end;
  finally
    dat.Free;
  end;
end;

class procedure TPacket.SerializeParams(Params: array of Variant; out Types, Data: string);
var
  dat: TStringStream;
  i: integer;
  b: boolean;
  v: TVarData;

  procedure WriteStr(const S: string);
  var
    c: cardinal;
  begin
    c:= Length(s);
    dat.Write(c, sizeof(c));
    dat.Write(s[1], Length(s) * Sizeof(Char));
  end;

begin
  types:= '';
  dat:= TStringStream.Create('');
  try
    for i:= 0 to high(Params) do begin
      v:= TVarData(Params[i]);
      case V.VType of
        varEmpty,
          varNull,
          varUnknown: begin
            types:= types + 'N';
            //Packet.Add(Byte(0)); // don't even transfer
          end;
        varString: begin
            types:= types + 's';
            WriteStr(string(V.VString));
          end;
        varOleStr: begin
            types:= types + 'S';
            WriteStr(UTF8Encode(WideString(V.VOleStr)));
          end;
        varSingle: begin
            types:= types + 'f';
            dat.Write(V.VSingle, sizeof(V.VSingle));
          end;
        varDouble: begin
            types:= types + 'd';
            dat.Write(v.VDouble, Sizeof(v.VDouble));
          end;
        varDate: begin
            types:= types + 'D';
            dat.Write(v.VDate, sizeof(v.VDate));
          end;
        varCurrency: begin
            types:= types + 'c';
            dat.Write(v.VCurrency, sizeof(v.VCurrency));
          end;
        varShortInt: begin
            types:= types + 'B';
            dat.Write(v.VShortInt, Sizeof(v.VShortInt));
          end;
        varByte: begin
            types:= types + 'b';
            dat.Write(v.VByte, sizeof(v.VByte));
          end;
        varSmallInt: begin
            types:= types + 'W';
            dat.Write(v.VSmallInt, Sizeof(v.VSmallInt));
          end;
        varWord: begin
            types:= types + 'w';
            dat.Write(v.VWord, sizeof(v.VWord));
          end;
        varInteger: begin
            types:= types + 'I';
            dat.Write(V.VInteger, sizeof(v.VInteger));
          end;
        varLongWord: begin
            types:= types + 'i';
            dat.Write(v.VLongWord, sizeof(v.VLongWord));
          end;
        varInt64: begin
            types:= types + '6';
            dat.Write(v.VInt64, sizeof(v.VInt64));
          end;
        varBoolean: begin
            types:= types + 'L';
            b:= v.VBoolean;
            dat.Write(b, sizeof(b));
          end;
      else
        raise EVariantInvalidArgError.CreateFmt('Type not transferrable: %s', [VarTypeAsText(V.VType)]);
      end;
    end;
    Data:= dat.DataString;
  finally
    dat.Free;
  end;
end;

class procedure TPacket.SerializeParams(Params: array of Variant; Packet: TCmdSeq);
var
  tt, dd: string;
begin
  SerializeParams(Params, tt, dd);
  Packet.Add(tt);
  Packet.Add(dd);
end;

class function TPacket.EntityCall(Entity: TEntityID; Method: Word): TCmdSeq;
begin
  Result:= TCmdSeq.Create(NETWORK_VERB_ENTITY_CALL);
  Result.Add(Entity);
  Result.Add(Method);
end;

class function TPacket.EntityAdd(Entity: TEntityID; EntityClass: Cardinal): TCmdSeq;
begin
  Result:= TCmdSeq.Create(NETWORK_VERB_ENTITY_ADD);
  Result.Add(Entity);
  Result.Add(EntityClass);
end;

class function TPacket.EntityRemove(Entity: TEntityID): TCmdSeq;
begin
  Result:= TCmdSeq.Create(NETWORK_VERB_ENTITY_REMOVE);
  Result.Add(Entity);
end;

class function TPacket.EntityMessage(Entity: TEntityID; Method: Word): TCmdSeq;
begin
  Result:= TCmdSeq.Create(NETWORK_VERB_ENTITY_MESSAGE);
  Result.Add(Entity);
  Result.Add(Method);
end;

class function TPacket.EntityMessageFast(Entity: TEntityID; Method: Word; Types, Data: string): TCmdSeq;
var
  d: TStringStream;
  tt: ShortString;
begin
  d:= TStringStream.Create('');
  try
    Result:= TCmdSeq.Create(NETWORK_VERB_ENTITY_MESSAGE_FAST);
    d.Write(Entity, Sizeof(Entity));
    d.Write(Method, Sizeof(Method));
    tt:= Types;
    d.Write(Byte(Length(tt)), Sizeof(Byte));
    d.Write(tt[1], length(tt));
    d.Write(Data[1], length(Data));
    Result.Add(d.DataString);
  finally
    d.Free;
  end;
end;

class function TPacket.StateTransition(NewState: TServerState): TCmdSeq;
begin
  Result:= TCmdSeq.Create(NETWORK_VERB_STATE_TRANSITION);
  Result.Add(byte(NewState));
end;

class procedure TPacket.Params(var Params: TCallParams; SetNew: array of Variant);
var
  i: integer;
begin
  SetLength(Params, Length(SetNew));
  for i:= 0 to high(Params) do
    Params[i]:= SetNew[i];
end;

class procedure TPacket.ParamsPush(var Params: TCallParams; New: array of Variant);
var
  i: integer;
begin
  SetLength(Params, Length(Params) + Length(New));
  for i:= high(New) downto 0 do begin
    Params[high(Params) - i]:= New[high(New) - i]
  end;
end;

end.

