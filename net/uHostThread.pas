unit uHostThread;

interface

uses Windows, Messages, SysUtils, Classes, MessageThread, uScreen, uNetworking, uUtil;

const
  HTM_IDLE = WM_USER + 1234;                                // posted to self & re-posted ad inf
  // since post-messages get queued in at the end this message always means
  // "after anything else is done", aka where we would idle
  // just be sure to always sleep off some time in the message handler or this is
  // effectively busywaiting!
  HTM_BEFORESTART = WM_USER + 1235;                         // fire OnBeforeStart-Event

type
  THostThread = class(TMessageThread)
  private
    FFrameTarget: integer;
    FFPS: single;
    FTimeStart,
      FTimeLastCount,
      FTimeLastFrame: PrecisionTime;
    FFrameCount: Integer;
    FWorldTime: double;
    FOnAfterEnd: TNotifyEvent;
    FOnBeforeStart: TNotifyEvent;
    FOnTick: TTickEvent;
  protected
    FHost: THost;
    procedure BeforeExecute; override;
    procedure AfterExecute; override;
    procedure Tick(DT: single);
    procedure Idle(SelfSourced: boolean); virtual;
    procedure HTMIdle(var Msg: TMessage); message HTM_IDLE;
    procedure HTMBeforeStart(var Msg: TMessage); message HTM_BEFORESTART;
  public
    constructor Create;
    property FrameTarget: integer read FFrameTarget write FFrameTarget;
    property CurrentFPS: single read FFPS;

    property Host: THost read FHost;

    property OnBeforeStart: TNotifyEvent read FOnBeforeStart write FOnBeforeStart;
    property OnAfterEnd: TNotifyEvent read FOnAfterEnd write FOnAfterEnd;
    property OnTick: TTickEvent read FOnTick write FOnTick;
  end;

implementation

{ THostThread }

constructor THostThread.Create;
begin
  FHost:= THost.Create(nil);
  FFrameTarget:= 60;
  inherited;
end;

procedure THostThread.BeforeExecute;
begin
  inherited;
  PostMessage(Handle, HTM_BEFORESTART, 1, 0);
  PostMessage(Handle, HTM_IDLE, 1, 0);

  QueryPerformanceCounter(FTimeStart);
  FTimeLastCount:= FTimeStart;
  FTimeLastFrame:= FTimeStart;
  FFrameCount:= 0;
  FWorldTime:= 0;
end;

procedure THostThread.AfterExecute;
begin
  if Assigned(FOnAfterEnd) then
    FOnAfterEnd(Self);
  FreeAndNil(FHost);
  inherited;
end;

procedure THostThread.HTMIdle(var Msg: TMessage);
begin
  Idle(Msg.WParam = 1);
  if Msg.wParam = 1 then begin
    PostMessage(Handle, HTM_IDLE, 1, 0);
  end;
  Msg.Result:= 1;
end;

procedure THostThread.Idle(SelfSourced: boolean);
var
  ts, te: PrecisionTime;
  dt, ft: single;
begin
  QueryPerformanceCounter(ts);
  dt:= (ts - FTimeLastFrame) / PerformanceFrequency;

  FTimeLastFrame:= ts;
  FWorldTime:= FWorldTime + dt;
  Tick(dt);
  QueryPerformanceCounter(te);
  inc(FFrameCount);

  ft:= 1 / FFrameTarget - (te - ts) / PerformanceFrequency;
  if ft > 0 then
    Sleep(trunc(ft * 1000));

  dt:= (te - FTimeLastCount) / PerformanceFrequency;
  if dt > 0.1 then begin
    FFPS:= 0.5 * FFPS + 0.5 * (FFrameCount / dt);
    QueryPerformanceCounter(FTimeLastCount);
    FFrameCount:= 0;
  end;
end;

procedure THostThread.Tick(DT: single);
begin
  if Assigned(FOnTick) then
    FOnTick(Self, DT);
end;

procedure THostThread.HTMBeforeStart(var Msg: TMessage);
begin
  if Assigned(FOnBeforeStart) then
    FOnBeforeStart(Self);
end;

end.

