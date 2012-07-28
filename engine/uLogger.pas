{
Kali Engine (C) Martok
https://github.com/martok/kali/
-----------------------------------------------------------------------
For copyright information see file license.txt.
}

unit uLogger;

interface

uses Classes, SysUtils, Windows, SyncObjs;

type
  TLogger = class
  private
    FName: TFileName;
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
    Logger:= TLogger.Create(ParamStr(0) + '.log');
  Result:= Logger;
end;

{ TLogger }

constructor TLogger.Create(Filename: string);
begin
  inherited Create;
  FName:= Filename;
  FLock:= TCriticalSection.Create;
  Log('Logger', 'Initialized');
end;

destructor TLogger.Destroy;
begin
  FreeAndNil(FLock);
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
  l:= Format('[%d] %-23s %20s %s' + sLineBreak, [GetCurrentProcessId, dt, Context, Message]);
  Write(l);
end;

procedure TLogger.Write(S: string);
var
  f: TStream;
  h: THandle;
begin
  FLock.Acquire;
  try
    repeat
      h:= CreateFile(PChar(FName), GENERIC_WRITE, FILE_SHARE_READ, nil, OPEN_ALWAYS, 0, 0);
    until (h <> INVALID_HANDLE_VALUE) or (GetLastError <> ERROR_SHARING_VIOLATION);
    if h <> INVALID_HANDLE_VALUE then
      try
        f:= THandleStream.Create(h);
        try
          f.Seek(0, soFromEnd);
          f.Write(S[1], Length(S) * Sizeof(Char));
        finally
          FreeAndNil(f);
        end;
      finally
        CloseHandle(h);
      end;
  finally
    FLock.Release;
  end;
end;

end.

