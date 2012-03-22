unit uLogger;

interface

uses Classes, SysUtils, Windows, SyncObjs;

type
  TLogger = class
  private
    FStream: TFileStream;
    FLock: TCriticalSection;
  protected
    procedure Write(S: string);
  public
    constructor Create(Filename: string);
    destructor Destroy; override;

    procedure Log(Context, Message: string); overload;
    procedure Log(Context, MessageFmt: string; Arguments: array of const); overload;

    procedure Log(Message: string); overload;
    procedure Log(MessageFmt: string; Arguments: array of const); overload;
  end;

function GetLogger: TLogger;

implementation

var
  Logger: TLogger;

function GetLogger: TLogger;
begin
  if not Assigned(Logger) then
    Logger:= TLogger.Create(ParamStr(0) + '.' + IntToStr(GetCurrentProcessId) + '.log');
  Result:= Logger;
end;

{ TLogger }

constructor TLogger.Create(Filename: string);
begin
  inherited Create;
  FStream:= TFileStream.Create(Filename, fmCreate or fmShareDenyWrite);
  FLock:= TCriticalSection.Create;
end;

destructor TLogger.Destroy;
begin
  FreeAndNil(FLock);
  FreeAndNil(FStream);
  inherited;
end;

procedure TLogger.Log(MessageFmt: string; Arguments: array of const);
begin
  Log(Format(MessageFmt, []));
end;

procedure TLogger.Log(Message: string);
begin
  Log('', Message);
end;

procedure TLogger.Log(Context, MessageFmt: string; Arguments: array of const);
begin
  Log(Context, Format(MessageFmt, Arguments));
end;

procedure TLogger.Log(Context, Message: string);
var
  dt, l: string;
begin
  dt:= FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now);
  l:= Format('%-23s %20s %s' + sLineBreak, [dt, Context, Message]);
  Write(l);
end;

procedure TLogger.Write(S: string);
begin
  FLock.Acquire;
  try
    FStream.Write(S[1], Length(S) * Sizeof(Char));
    FlushFileBuffers(FStream.Handle);
  finally
    FLock.Release;
  end;
end;

end.

