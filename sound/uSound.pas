{
Kali Engine (C) Martok
https://github.com/martok/kali/
-----------------------------------------------------------------------
For copyright information see file license.txt.
}

unit uSound;

interface

uses
  SysUtils, Classes, Contnrs, OpenAL, uSoundAL;

type
  TVector3f = uSoundAL.TVector3f;
  TSoundKind = (skBlock, skStream);
  TSoundFiletype = (ftWave, ftOggVorbis);

  TsndSound = class;
  TsndEmitter = class;
  TSoundSystem = class
  private
    FCtx: TALContext;
    FAvailable: boolean;
    function GetWorldScale: double;
    procedure SetWorldScale(const Value: double);
  public
    constructor Create;
    destructor Destroy; override;
    property Context: TALContext read FCtx;
    property Available: boolean read FAvailable;

    function LoadSound(Kind: TSoundKind; Stream: TStream; Filetype: TSoundFiletype): TsndSound;
    function Emitter(X, Y, Z: Double; Relative: boolean): TsndEmitter;

    property WorldScale: double read GetWorldScale write SetWorldScale;
  end;

  TsndSound = class
  private
    FKind: TSoundKind;
    FBlock: TALBuffer;
    FStream: TMemoryStream;
    FStreamFileType: TSoundFiletype;
  protected
    function GetStream: TALDecoderStream;
  public
    constructor Create(AKind: TSoundKind);
    property Kind: TSoundKind read FKind;
    procedure LoadFromStream(Stream: TStream; Filetype: TSoundFiletype);
  end;

  TsndEmitter = class
  private
    FInstances: TObjectList;
    FPosition: TVector3f;
    FPositionRelative: boolean;
    FVelocity: TVector3f;
    function GetPosition: TVector3f;
    function GetRelative: boolean;
    function GetVelo: TVector3f;
    procedure SetPosition(const Value: TVector3f);
    procedure SetRelative(const Value: boolean);
    procedure SetVelo(const Value: TVector3f);
  protected
    procedure UpdateInstances;
    function GetSource: TALSource;
    procedure DeleteSource(Src: TALSource);
  public
    constructor Create;
    destructor Destroy; override;

    property Position: TVector3f read GetPosition write SetPosition;
    property PositionRelative: boolean read GetRelative write SetRelative;
    property Velocity: TVector3f read GetVelo write SetVelo;
  end;

  TsndInstance = class
  private
    FSource: TALSource;
    FEmitter: TsndEmitter;
    FSound: TsndSound;
    FStreamCopy: TALDecoderStream;
    FFreeOnStop: boolean;
    FLoop: Integer;
    FFadeIn, FFadeOut: Single;
    FSetGain: Single;
    function GetGain: single;
    procedure SetGain(const Value: single);
    procedure SourceTick(Sender: TObject; const LastState, NewState: TALint);
  public
    constructor Create(Sound: TsndSound; Emitter: TsndEmitter);
    destructor Destroy; override;

    function Play(FadeIn: single = 0; FadeOut: single = 0): TsndInstance;
    function Loop(Count: Integer = -1; FadeIn: single = 0): TsndInstance;
    procedure Stop;
    property FreeOnStop: boolean read FFreeOnStop write FFreeOnStop;
    property Gain: single read GetGain write SetGain;
  end;

implementation

uses uLogger, oval, Math;

const
  LOG_CTX = 'Sound';

var
  GLOB_WorldScale: double;

  { TSoundSystem }

constructor TSoundSystem.Create;
var
  dev: PALDevice;
begin
  inherited Create;
  FAvailable:= false;
  if not SoundAL.Available then begin
    GetLogger.Log(LOG_CTX, 'No OpenAL found!');
    exit;
  end;
  dev:= SoundAL.Device['OpenAL Soft'];
  if not Assigned(dev) then begin
    GetLogger.Log(LOG_CTX, 'Initializing "OpenAL Soft" failed. Is soft_oal.dll present?');
    GetLogger.Log(LOG_CTX, 'Falling back to generic default. Positional Audio may not work.');
    dev:= SoundAL.Device[''];
  end;
  FCtx:= TALContext.Create(dev);
  FCtx.MakeCurrent;
  GetLogger.Log(LOG_CTX, 'Initialized Audio: ' +
    'OpenAL ' + SoundAL.VersionInfo.Version +
    ' ALC: ' + FCtx.Version);
  GLOB_WorldScale:= 1;
  FAvailable:= true;
end;

destructor TSoundSystem.Destroy;
begin
  FreeAndNil(FCtx);
  inherited;
end;

function TSoundSystem.LoadSound(Kind: TSoundKind; Stream: TStream; Filetype: TSoundFiletype): TsndSound;
begin
  Result:= TsndSound.Create(Kind);
  Result.LoadFromStream(Stream, Filetype);
end;

function TSoundSystem.Emitter(X, Y, Z: Double; Relative: boolean): TsndEmitter;
var
  p: TVector3f;
begin
  Result:= TsndEmitter.Create;
  p[0]:= X;
  p[1]:= Y;
  p[2]:= Z;
  Result.PositionRelative:= Relative;
  Result.Position:= p;
end;

function TSoundSystem.GetWorldScale: double;
begin
  Result:= GLOB_WorldScale;
end;

procedure TSoundSystem.SetWorldScale(const Value: double);
begin
  GLOB_WorldScale:= Value;
end;

{ TsndSound }

constructor TsndSound.Create(AKind: TSoundKind);
begin
  inherited Create;
  FKind:= AKind;
end;

function TsndSound.GetStream: TALDecoderStream;
var
  ms: TMemoryStream;
begin
  Result:= nil;
  ms:= TMemoryStream.Create;
  ms.LoadFromStream(FStream);
  try
    case FStreamFileType of
      ftWave: raise Exception.Create('ENotImpl');
      ftOggVorbis: Result:= TALDecoderStreamOGG.Create(ms, true);
    end;
  except
    ms.Free;
    raise;
  end;
end;

procedure TsndSound.LoadFromStream(Stream: TStream; Filetype: TSoundFiletype);
begin
  case FKind of
    skBlock: begin
        case Filetype of
          ftWave: FBlock:= TALBufferWAV.Create;
          ftOggVorbis: FBlock:= TALBufferOGG.Create;
        end;
        FBlock.LoadFromStream(Stream);
      end;
    skStream: begin
        FStream:= TMemoryStream.Create;
        FStream.CopyFrom(Stream, 0);
        FStreamFileType:= Filetype;
      end;
  end;
end;

{ TsndEmitter }

constructor TsndEmitter.Create;
begin
  inherited Create;
  FInstances:= TObjectList.Create(false);
end;

destructor TsndEmitter.Destroy;
begin
  FInstances.OwnsObjects:= true;
  FInstances.Free;
  inherited;
end;

function TsndEmitter.GetPosition: TVector3f;
begin
  Result:= FPosition;
end;

function TsndEmitter.GetRelative: boolean;
begin
  Result:= FPositionRelative;
end;

procedure TsndEmitter.DeleteSource(Src: TALSource);
begin
  FInstances.Remove(Src);
end;

function TsndEmitter.GetSource: TALSource;
begin
  Result:= TALSource.Create;
  FInstances.Add(Result);
  UpdateInstances;
end;

function TsndEmitter.GetVelo: TVector3f;
begin
  Result:= FVelocity;
end;

procedure TsndEmitter.SetPosition(const Value: TVector3f);
begin
  FPosition:= Value;
  UpdateInstances;
end;

procedure TsndEmitter.SetRelative(const Value: boolean);
begin
  FPositionRelative:= Value;
  UpdateInstances;
end;

procedure TsndEmitter.SetVelo(const Value: TVector3f);
begin
  FVelocity:= Value;
  UpdateInstances;
end;

procedure TsndEmitter.UpdateInstances;
var
  i: integer;
  sr: TALSource;
  f: Single;
begin
  for i:= 0 to FInstances.Count - 1 do begin
    sr:= TALSource(FInstances[i]);
    sr.PositionRelative:= FPositionRelative;
    sr.Position:= FPosition;
    sr.Velocity:= FVelocity;
    f:= GLOB_WorldScale;
    alSourcef(sr.Handle, AL_ROLLOFF_FACTOR, f);
  end;
end;

{ TsndInstance }

constructor TsndInstance.Create(Sound: TsndSound; Emitter: TsndEmitter);
begin
  inherited Create;
  FFreeOnStop:= true;
  FSource:= Emitter.GetSource;
  FSource.OnTick:= SourceTick;
  FEmitter:= Emitter;
  case Sound.Kind of
    skBlock: FSound:= Sound;
    skStream: begin
        FSound:= Sound;
        FStreamCopy:= FSound.GetStream;
      end;
  end;
  FSetGain:= 1.0;
end;

destructor TsndInstance.Destroy;
begin
  case FSound.Kind of
    skBlock: ;
    skStream: FStreamCopy.Free;
  end;
  FEmitter.DeleteSource(FSource);
  FSource.Free;
  inherited;
end;

function TsndInstance.GetGain: single;
begin
  Result:= FSource.Gain;
end;

procedure TsndInstance.SetGain(const Value: single);
begin
  FSetGain:= Value;
end;

function TsndInstance.Loop(Count: Integer; FadeIn: single): TsndInstance;
begin
  Result:= Self;
  if Count >= 0 then
    FLoop:= Count
  else
    FLoop:= MaxInt;
  FFadeIn:= FadeIn;
  FFadeOut:= 0;
  case FSound.Kind of
    skBlock: begin
        FSource.Rewind;
        FSource.Buffer:= FSound.FBlock;
        FSource.Play;
      end;
    skStream: begin
        FStreamCopy.Rewind;
        FStreamCopy.ALSource:= FSource;
        FStreamCopy.Play;
      end;
  end;
end;

function TsndInstance.Play(FadeIn, FadeOut: single): TsndInstance;
begin
  Result:= Self;
  FLoop:= 0;
  FFadeIn:= FadeIn;
  FFadeOut:= FadeOut;
  case FSound.Kind of
    skBlock: begin
        FSource.Buffer:= FSound.FBlock;
        FSource.Play;
      end;
    skStream: begin
        FStreamCopy.ALSource:= FSource;
        FStreamCopy.Play;
      end;
  end;
end;

procedure TsndInstance.Stop;
begin
  FLoop:= 0;
  case FSound.Kind of
    skBlock: FSource.Stop;
    skStream: FStreamCopy.Stop;
  end;
end;

procedure TsndInstance.SourceTick(Sender: TObject; const LastState, NewState: TALint);
var
  gtarg: TALfloat;
  t, e: Double;
begin
  if LastState <> NewState then begin
    if (LastState <> AL_INITIAL) and (NewState = AL_STOPPED) then begin
      if FLoop > 0 then
        Loop(FLoop - 1, 0)
      else if FFreeOnStop then begin
        Free;
        exit;
      end;
    end;
  end;
  case FSound.Kind of
    skBlock: begin
        t:= FSource.Offset;
        if Assigned(FSource.Buffer) then
          e:= FSource.Buffer.Length
        else
          e:= 0;
      end;
    skStream: begin
        t:= FStreamCopy.Offset;
        e:= FStreamCopy.Length;
      end;
  else
    t:= -1;
    e:= 0;
  end;
  gtarg:= FSetGain;
  if (FFadeIn > 0) then begin
    if (t <= FFadeIn) then
      gtarg:= t / FFadeIn * FSetGain
    else begin
      FFadeIn:= 0;
      gtarg:= FSetGain;
    end;
  end;
  if (FFadeOut > 0) and (t > e - FFadeOut) then
    gtarg:= (e - t) / FFadeOut * FSetGain;
  FSource.Gain:= gtarg;
end;

end.

