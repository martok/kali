{
Kali Engine (C) Martok
https://github.com/martok/kali/
-----------------------------------------------------------------------
For copyright information see file license.txt.
}

unit uSoundAL;

interface

uses SysUtils, Classes, OpenAL, SyncObjs;

const
  DECODER_BUFFER_COUNT = 3;
type
  TALContext = class;
  TALBuffer = class;
  TALSource = class;
  TALListener = class;

  PALDevice = Pointer;

  TVector3f = array[0..2] of TALfloat;

  TALInfo = class
  private
    FALVersion: string;
    FALRenderer: string;
    FALExtensions: string;
    FALVendor: string;
  public
    constructor Create;
    property Version: string read FALVersion;
    property Vendor: string read FALVendor;
    property Renderer: string read FALRenderer;
    property Extensions: string read FALExtensions;
  end;

  TOpenAL = class
  private
    FOpenDevs: TStringList;
    FOpenALAvailable: boolean;
    FInfo: TALInfo;
    FActiveSources: TList;
    function GetDevice(DeviceName: string): PALDevice;
    function GetInfo: TALInfo;
  protected
    FUpdaterThread: TThread;
    procedure Tick;
  public
    constructor Create;
    destructor Destroy; override;
    property Available: boolean read FOpenALAvailable;
    property Device[DeviceName: string]: PALDevice read GetDevice;
    property VersionInfo: TALInfo read GetInfo;
  end;

  TALContext = class
  private
    FHandle: OpenAL.TALCcontext;
    FListener: TALListener;
    FExtensions: string;
    FVersion: string;
    FDevice: PALDevice;
    function GetSOS: TALfloat;
    procedure SetSOS(const Value: TALfloat);
    function GetDoppler: TALfloat;
    procedure SetDoppler(const Value: TALfloat);
  public
    constructor Create(Device: PALDevice; SampleFreq: integer = -1; MonoSources: integer = -1; RefreshRate: integer = -1; StereoSources: integer = -1;
      SyncContext: Pointer = nil);
    destructor Destroy; override;
    property Device: PALDevice read FDevice;
    property Handle: OpenAL.TALCcontext read FHandle;
    procedure MakeCurrent;
    property Listener: TALListener read FListener;
    property Version: string read FVersion;
    property Extensions: string read FExtensions;
    property SpeedOfSound: TALfloat read GetSOS write SetSOS;
    property DopplerFactor: TALfloat read GetDoppler write SetDoppler;
  end;

  TALBuffer = class
  private
    FHandle: TALuint;
    FLength: TALfloat;
  protected
    procedure ComputeLength(format: TALenum; size, freq: TALsizei);
  public
    constructor Create();
    destructor Destroy; override;
    property Handle: TALuint read FHandle;
    procedure LoadFromFile(FileName: string);
    procedure LoadFromStream(Stream: TStream); virtual; abstract;
    property Length: TALfloat read FLength;
  end;

  TALBufferWAV = class(TALBuffer)
  private
  public
    procedure LoadFromStream(Stream: TStream); override;
  end;

  TSourceStateTickEvent = procedure(Sender: TObject; const LastState, NewState: TALint) of object;
  TALSource = class
  private
    FHandle: TALuint;
    FState: TALInt;
    FBuffer: TALBuffer;
    FOnTick: TSourceStateTickEvent;
    procedure SetBuffer(ABuffer: TALBuffer);
    function GetBool(const Index: Integer): boolean;
    procedure SetBool(const Index: Integer; const Value: boolean);
    function GetFloat(const Index: Integer): TALfloat;
    procedure SetFloat(const Index: Integer; const Value: TALfloat);
    function GetInt(const Index: Integer): TALint;
    function GetFloat3f(const Index: Integer): TVector3f;
    procedure SetFloat3f(const Index: Integer; const Value: TVector3f);
  protected
    procedure Tick;
  public
    constructor Create;
    destructor Destroy; override;
    property Handle: TALuint read FHandle;
    property Buffer: TALBuffer read FBuffer write SetBuffer;
    procedure Play;
    procedure Pause;
    procedure Stop;
    procedure Rewind;
    property State: TALint index AL_SOURCE_STATE read GetInt;
    property Loop: boolean index AL_LOOPING read GetBool write SetBool;
    property Position: TVector3f index AL_POSITION read GetFloat3f write SetFloat3f;
    property PositionRelative: boolean index AL_SOURCE_RELATIVE read GetBool write SetBool;
    property Velocity: TVector3f index AL_VELOCITY read GetFloat3f write SetFloat3f;
    procedure SetOrientation(Front, Up: TVector3f);
    property Pitch: TALfloat index AL_PITCH read GetFloat write SetFloat;
    property Gain: TALfloat index AL_GAIN read GetFloat write SetFloat;
    property Offset: TALfloat index AL_SEC_OFFSET read GetFloat write SetFloat;
    procedure QueueBuffer(ABuffer: TALBuffer);
    procedure UnqueueBuffer(ABuffer: TALBuffer);

    property OnTick: TSourceStateTickEvent read FOnTick write FOnTick;
  end;

  TALListener = class
  private
    FContext: TALContext;
    function GetFloat(const Index: Integer): TALfloat;
    procedure SetFloat(const Index: Integer; const Value: TALfloat);
    function GetFloat3f(const Index: Integer): TVector3f;
    procedure SetFloat3f(const Index: Integer; const Value: TVector3f);
  public
    constructor Create(Context: TALContext);
    destructor Destroy; override;
    property Position: TVector3f index AL_POSITION read GetFloat3f write SetFloat3f;
    property Velocity: TVector3f index AL_VELOCITY read GetFloat3f write SetFloat3f;
    procedure SetOrientation(Front, Up: TVector3f);
    property Gain: TALfloat index AL_GAIN read GetFloat write SetFloat;
  end;

  TALDecoderStream = class;
  TALDecoderThread = class(TThread)
  private
    FOwner: TALDecoderStream;
    FWaiting: boolean;
    FSleeper: TEvent;
  protected
    procedure SyncCallThread;
    procedure Execute; override;
  public
    constructor Create(AOWner: TALDecoderStream);
    procedure Run;
    procedure Pause;
  end;

  TALDecoderStream = class
  private
    FSource: TALSource;
    FStream: TStream;
    FState: TALint;
    FOwnsStream: Boolean;
    FBuffers: array[0..DECODER_BUFFER_COUNT - 1] of TALUInt;
    procedure SetSource(const Value: TALSource);
  protected
    DecoderThread: TALDecoderThread;
    procedure ThreadCycle;
    procedure BindSource(Source: TALSource);
    function DecodeStream(Into: Cardinal): boolean; virtual; abstract;
    procedure InitStream; virtual; abstract;
    procedure FinalizeStream; virtual; abstract;
    function GetOffset: TALfloat; virtual; abstract;
    procedure SetOffset(const Value: TALfloat); virtual; abstract;
    function GetLength: TALFloat; virtual; abstract;
  public
    constructor Create(Filename: string); overload;
    constructor Create(Stream: TStream; OwnsStream: boolean); overload;
    destructor Destroy; override;
    procedure Play;
    procedure Pause;
    procedure Stop;
    procedure Rewind;
    property ALSource: TALSource read FSource write SetSource;
    property State: Integer read FState;
    property Stream: TStream read FStream;
    property Offset: TALfloat read GetOffset write SetOffset;
    property Length: TALFloat read GetLength;
  end;

function SoundAL: TOpenAL;

function alGetStringError(Code: integer): string;

implementation

var
  SingletonOpenAL: TOpenAL = nil;

function SoundAL: TOpenAL;
begin
  if not Assigned(SingletonOpenAL) then
    SingletonOpenAL:= TOpenAL.Create;
  Result:= SingletonOpenAL;
end;

function alGetStringError(Code: integer): string;
begin
  Result:= alGetString(Code);
end;

type
  TUpdaterThread = class(TThread)
  private
    FEnabled: boolean;
  public
    constructor Create;
    procedure Execute; override;
  end;

  { TUpdaterThread }

constructor TUpdaterThread.Create;
begin
  inherited Create(false);
  FreeOnTerminate:= true;
  FEnabled:= true;
end;

procedure TUpdaterThread.Execute;
begin
  while not Terminated do begin
    Sleep(10);
    if FEnabled then
      Synchronize({$IFDEF FPC}@{$ENDIF}SingletonOpenAL.Tick);
  end;
end;

{ TOpenAL }

constructor TOpenAL.Create;
begin
  inherited;
  FOpenALAvailable:= InitOpenAL;
  FOpenDevs:= TStringList.Create;
  FOpenDevs.Sorted:= true;
  FActiveSources:= TList.Create;
  FUpdaterThread:= TUpdaterThread.Create;
end;

destructor TOpenAL.Destroy;
var
  i: integer;
begin
  for i:= 0 to FOpenDevs.Count - 1 do begin
    alcCloseDevice(FOpenDevs.Objects[i]);
  end;
  TUpdaterThread(FUpdaterThread).FEnabled:= false;
  FUpdaterThread.Terminate;
  FActiveSources.Free;
  FOpenDevs.Free;
  inherited;
end;

function TOpenAL.GetDevice(DeviceName: string): PALDevice;
var
  i: integer;
begin
  i:= FOpenDevs.IndexOf(DeviceName);
  if i < 0 then begin
    Result:= alcOpenDevice(PAnsiChar(DeviceName));
    if Result <> nil then
      FOpenDevs.AddObject(DeviceName, TObject(Result));
  end else
    Result:= FOpenDevs.Objects[i];
end;

function TOpenAL.GetInfo: TALInfo;
var
  ctx: Pointer;
begin
  if not Assigned(FInfo) then begin
    ctx:= alcGetCurrentContext;
    if ctx <> nil then
      FInfo:= TALInfo.Create
    else
      FInfo:= nil;
  end;
  Result:= FInfo;
end;

procedure TOpenAL.Tick;
var
  i: integer;
begin
  for i:= FActiveSources.Count - 1 downto 0 do begin
    TALSource(FActiveSources[i]).Tick;
  end;
end;

{ TALContext }

constructor TALContext.Create(Device: PALDevice; SampleFreq, MonoSources, RefreshRate, StereoSources: integer; SyncContext: Pointer);
var
  Data: array[0..4] of array[0..1] of TALCint;
  i: integer;
begin
  inherited Create;
  FillChar(Data[0, 0], 5 * 2 * Sizeof(TALCint), 0);
  i:= 0;
  if SampleFreq >= 0 then begin
    Data[i][0]:= ALC_FREQUENCY;
    Data[i][1]:= SampleFreq;
    inc(i);
  end;
  if MonoSources >= 0 then begin
    Data[i][0]:= ALC_MONO_SOURCES;
    Data[i][1]:= MonoSources;
    inc(i);
  end;
  if RefreshRate >= 0 then begin
    Data[i][0]:= ALC_REFRESH;
    Data[i][1]:= RefreshRate;
    inc(i);
  end;
  if StereoSources >= 0 then begin
    Data[i][0]:= ALC_STEREO_SOURCES;
    Data[i][1]:= StereoSources;
    inc(i);
  end;
  if SyncContext <> nil then begin
    Data[i][0]:= ALC_FREQUENCY;
    Data[i][1]:= Integer(SyncContext);
    inc(i);
  end;
  if i = 0 then
    FHandle:= alcCreateContext(Device, nil)
  else
    FHandle:= alcCreateContext(Device, @Data[0, 0]);
  FDevice:= Device;
  FListener:= TALListener.Create(Self);
end;

destructor TALContext.Destroy;
begin
  alcDestroyContext(FHandle);
  inherited;
end;

function TALContext.GetDoppler: TALfloat;
begin
  Result:= alGetFloat(AL_DOPPLER_FACTOR);
end;

function TALContext.GetSOS: TALfloat;
begin
  Result:= alGetFloat(AL_SPEED_OF_SOUND);
end;

procedure TALContext.MakeCurrent;
var
  i: integer;
begin
  alcMakeContextCurrent(FHandle);
  FExtensions:= alcGetString(FDevice, AL_EXTENSIONS);
  alcGetIntegerv(FDevice, ALC_MAJOR_VERSION, 1, @i);
  FVersion:= IntToStr(i) + '.';
  alcGetIntegerv(FDevice, ALC_MINOR_VERSION, 1, @i);
  FVersion:= FVersion + IntToStr(i);
end;

procedure TALContext.SetDoppler(const Value: TALfloat);
begin
  alDopplerFactor(Value)
end;

procedure TALContext.SetSOS(const Value: TALfloat);
begin
  alSpeedOfSound(Value);
end;

{ TALBuffer }

constructor TALBuffer.Create;
begin
  inherited;
  alGenBuffers(1, @FHandle);
  FLength:= 0;
end;

destructor TALBuffer.Destroy;
begin
  alDeleteBuffers(1, @FHandle);
  inherited;
end;

procedure TALBuffer.ComputeLength(format: TALenum; size, freq: TALsizei);
var
  bps, samples: TALsizei;
begin
  case format of
    AL_FORMAT_MONO8    :  bps:= 1;
    AL_FORMAT_MONO16   :  bps:= 2;
    AL_FORMAT_STEREO8  :  bps:= 2;
    AL_FORMAT_STEREO16 :  bps:= 4;
  else
    bps:= 1;
  end;
  samples:= size div bps;
  FLength:= samples / freq;
end;

procedure TALBuffer.LoadFromFile(FileName: string);
var
  fs: TFileStream;
begin
  fs:= TFileStream.Create(FileName, fmOpenRead);
  try
    LoadFromStream(fs);
  finally
    fs.Free;
  end;
end;

{ TALBufferWAV }

procedure TALBufferWAV.LoadFromStream(Stream: TStream);
var
  format: TALenum;
  data: TALvoid;
  size: TALsizei;
  freq: TALsizei;
  loop: TALint;
begin
  data:= nil;
  LoadWavStream(Stream, format, data, size, freq, loop);
  if Assigned(data) then begin
    alBufferData(Handle, format, data, size, freq);
    ComputeLength(format, size, freq);
    alutUnloadWAV(format, data, size, freq);
  end; //TODO: error
end;
{ TALSource }

constructor TALSource.Create;
begin
  inherited Create;
  FBuffer:= nil;
  alGenSources(1, @FHandle);
  FState:= AL_INITIAL;
  SoundAL.FActiveSources.Add(Self);
end;

destructor TALSource.Destroy;
begin
  SoundAL.FActiveSources.Remove(Self);
  alDeleteSources(1, @FHandle);
  inherited;
end;

procedure TALSource.SetBuffer(ABuffer: TALBuffer);
begin
  if Assigned(ABuffer) then
    alSourcei(FHandle, AL_BUFFER, ABuffer.Handle)
  else
    alSourcei(FHandle, AL_BUFFER, AL_NONE);
  FBuffer:= ABuffer;
end;

procedure TALSource.Play;
begin
  alSourcePlay(FHandle);
end;

procedure TALSource.Pause;
begin
  alSourcePause(FHandle);
end;

procedure TALSource.Stop;
begin
  alSourceStop(FHandle);
end;

procedure TALSource.Rewind;
begin
  alSourceRewind(FHandle);
end;

function TALSource.GetBool(const Index: Integer): boolean;
var
  i: TALInt;
begin
  alGetSourcei(FHandle, Index, @i);
  Result:= i <> AL_FALSE;
end;

procedure TALSource.SetBool(const Index: Integer; const Value: boolean);
var
  i: TALInt;
begin
  if Value then
    i:= AL_TRUE
  else
    i:= AL_FALSE;
  alSourcei(FHandle, Index, i);
end;

function TALSource.GetFloat(const Index: Integer): TALfloat;
begin
  alGetSourcef(FHandle, Index, @Result);
end;

procedure TALSource.SetFloat(const Index: Integer; const Value: TALfloat);
begin
  alSourcef(FHandle, Index, Value);
end;

function TALSource.GetInt(const Index: Integer): TALint;
begin
  alGetSourcei(FHandle, Index, @Result);
end;

function TALSource.GetFloat3f(const Index: Integer): TVector3f;
begin
  alGetSourcefv(FHandle, Index, @Result);
end;

procedure TALSource.SetFloat3f(const Index: Integer; const Value: TVector3f);
begin
  alSourcefv(FHandle, Index, @Value);
end;

procedure TALSource.SetOrientation(Front, Up: TVector3f);
var
  a: array[0..1] of TVector3f;
begin
  a[0]:= Front;
  a[1]:= Up;
  alSourcefv(FHandle, AL_ORIENTATION, @a);
end;

procedure TALSource.QueueBuffer(ABuffer: TALBuffer);
begin
  alSourceQueueBuffers(FHandle, 1, @(ABuffer.Handle));
end;

procedure TALSource.UnqueueBuffer(ABuffer: TALBuffer);
begin
  alSourceUnqueueBuffers(FHandle, 1, @(ABuffer.Handle));
end;

procedure TALSource.Tick;
var
  st: TALint;
begin
  st:= FState;
  FState:= State;
  if Assigned(FOnTick) then
    FOnTick(Self, st, FState);
  // no access after this event! - Self could be freed by now! 
end;

{ TALListener }

constructor TALListener.Create(Context: TALContext);
begin
  inherited Create;
  FContext:= Context;
end;

destructor TALListener.Destroy;
begin
  inherited;
end;

function TALListener.GetFloat(const Index: Integer): TALfloat;
begin
  alGetListenerfv(Index, @Result);
end;

function TALListener.GetFloat3f(const Index: Integer): TVector3f;
begin
  alGetListenerfv(Index, @Result);
end;

procedure TALListener.SetFloat(const Index: Integer; const Value: TALfloat);
begin
  alListenerf(Index, Value);
end;

procedure TALListener.SetFloat3f(const Index: Integer; const Value: TVector3f);
begin
  alListenerfv(Index, @Value);
end;

procedure TALListener.SetOrientation(Front, Up: TVector3f);
var
  a: array[0..1] of TVector3f;
begin
  a[0]:= Front;
  a[1]:= Up;
  alListenerfv(AL_ORIENTATION, @a);
end;

{ TALInfo }

constructor TALInfo.Create;
begin
  inherited;
  ReadOpenALExtensions;
  FALVendor:= alGetString(AL_VENDOR);
  FALRenderer:= alGetString(AL_RENDERER);
  FALVersion:= alGetString(AL_VERSION);
  FALExtensions:= alGetString(AL_EXTENSIONS);
end;

{ TALDecoderStream }

constructor TALDecoderStream.Create(Filename: string);
begin
  Create(TFileStream.Create(Filename, fmOpenRead), True);
end;

constructor TALDecoderStream.Create(Stream: TStream; OwnsStream: boolean);
begin
  inherited Create;
  SetSource(nil);
  FStream:= Stream;
  FOwnsStream:= OwnsStream;
  alGenBuffers(DECODER_BUFFER_COUNT, @FBuffers[0]);
  FState:= AL_INITIAL;
  DecoderThread:= TALDecoderThread.Create(Self);
end;

destructor TALDecoderStream.Destroy;
begin
  SetSource(nil);
  DecoderThread.FOwner:= nil;
  DecoderThread.Run;
  DecoderThread.FreeOnTerminate:= true;
  DecoderThread:= nil;
  if FOwnsStream and Assigned(FStream) then
    FreeAndNil(FStream);
  inherited;
end;

procedure TALDecoderStream.SetSource(const Value: TALSource);
begin
  if FSource <> Value then begin
    if Assigned(FSource) then
      BindSource(nil);
    FSource:= Value;
    if Assigned(FSource) then
      BindSource(FSource);
  end;
end;

procedure TALDecoderStream.BindSource(Source: TALSource);
begin
  if Assigned(Source) then begin
    InitStream;
    Source.Stop;
    Source.SetBuffer(nil);
    FState:= AL_INITIAL;
  end else begin
    FinalizeStream;
    if Assigned(FSource) then begin
      Stop;
      FSource.SetBuffer(nil);
    end;
  end;
end;

procedure TALDecoderStream.ThreadCycle;
var
  Queued, Processed: Integer;
  Buffer: TALuint;
  b: integer;
begin
  if Assigned(FSource) then begin
    alGetSourcei(FSource.Handle, AL_BUFFERS_QUEUED, @Queued);
    if Queued < DECODER_BUFFER_COUNT then begin             // initial filling
      if not DecodeStream(FBuffers[Queued]) then
        Stop;
      alSourceQueueBuffers(FSource.Handle, 1, @FBuffers[Queued]);
    end else begin                                          // refill
      alGetSourcei(FSource.Handle, AL_BUFFERS_PROCESSED, @Processed);
      for b:= 1 to Processed do begin
        alSourceUnqueueBuffers(FSource.Handle, 1, @Buffer);
        if not DecodeStream(Buffer) then
          Stop;
        alSourceQueueBuffers(FSource.Handle, 1, @Buffer);
      end;
    end;
    if (FSource.State <> FState) then
      case FState of
        AL_INITIAL: FSource.Rewind;
        AL_PLAYING: FSource.Play;
        AL_PAUSED: FSource.Rewind;
        AL_STOPPED: FSource.Stop;
      end;
  end;
end;

procedure TALDecoderStream.Play;
begin
  if Assigned(FSource) then begin
    DecoderThread.Run;
    FSource.Play;
    if FState = AL_PLAYING then
      Offset:= 0;
    FState:= AL_PLAYING;
  end;
end;

procedure TALDecoderStream.Pause;
begin
  if Assigned(FSource) then begin
    FSource.Pause;
    DecoderThread.Pause;
    FState:= AL_PAUSED;
  end;
end;

procedure TALDecoderStream.Stop;
begin
  if Assigned(FSource) then begin
    FSource.Stop;
    FState:= AL_STOPPED;
    DecoderThread.Pause;
  end;
end;

procedure TALDecoderStream.Rewind;
begin
  if Assigned(FSource) then begin
    FSource.Rewind;
    FState:= AL_INITIAL;
    Offset:= 0;
  end;
end;

{ TALDecoderThread }

constructor TALDecoderThread.Create(AOwner: TALDecoderStream);
begin
  inherited Create(true);
  FOwner:= AOwner;
  FSleeper:= TEvent.Create(nil, false, false, '');
  FWaiting:= false;
end;

procedure TALDecoderThread.Execute;
begin
  while not Terminated do begin
    Synchronize({$IFDEF FPC}@{$ENDIF}SyncCallThread);
    if FWaiting then
      FSleeper.WaitFor(10000)
    else
      Sleep(10);
  end;
end;

procedure TALDecoderThread.Pause;
begin
  FWaiting:= true;
end;

procedure TALDecoderThread.Run;
begin
  FWaiting:= false;
  if Suspended then                                         // first time
    Resume
  else
    FSleeper.SetEvent;
end;

procedure TALDecoderThread.SyncCallThread;
begin
  if Assigned(FOwner) then
    Synchronize(FOwner.ThreadCycle);
end;

initialization
finalization
  FreeAndNil(SingletonOpenAL);
end.

