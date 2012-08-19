unit oval;

interface
uses
{$IFDEF FPC}ctypes, {$ENDIF}
  SysUtils, Classes, openal, Windows, ogg, vorbis, uSoundAL;

const
  OGG_BUFFER_SIZE = 4096 * 8;
type
  TALBufferOGG = class(TALBuffer)
  public
    procedure LoadFromStream(Stream: TStream); override;
  end;

  TALDecoderStreamOGG = class(TALDecoderStream)
  private
    FFile: OggVorbis_File;
    FFormat: TALuint;
    VorbisInfo: vorbis_info;
  protected
    procedure InitStream; override;
    function DecodeStream(Into: Cardinal): boolean; override;
    procedure FinalizeStream; override;
    function GetOffset: Single; override;
    procedure SetOffset(const Value: Single); override;
    function GetLength: Single; override;
  public
  end;

procedure ovalLoadOGGStream(stream: TStream; var format: TALenum; var data: TALvoid; var size: TALsizei; var freq: TALsizei; var loop: TALint);
procedure ovalUnloadOGG(format: TALenum; data: TALvoid; size: TALsizei; freq: TALsizei);

procedure Debug(Fmt: string; Args: array of const);

implementation

const
  { Constants taken from the MSVC++6 ANSI C library. These values may be
    different for other C libraries! }
  SEEK_SET = 0;
  SEEK_CUR = 1;
  SEEK_END = 2;

  EOF = -1;
var
  ov_stream_callbacks: ov_callbacks;

procedure Debug(Fmt: string; Args: array of const);
begin
  //OutputDebugString(PChar(SysUtils.Format(Fmt, Args)));
end;

function ogg_read_func(ptr: pointer; size, nmemb: csize_t; datasource: pointer): csize_t; cdecl;
{ Returns amount of items completely read successfully, returns indeterminate
  value on error. The value of a partially read item cannot be determined. Does
  not lead to valid feof or ferror responses, because they are not possible to
  supply to VorbisFile }
begin
  if (size = 0) or (nmemb = 0) then begin
    result:= 0;
    exit;
  end;

  try
    Debug('Read %d/%d, %dx%d', [TStream(datasource).Position, TStream(datasource).Size, size, nmemb]);
    result:= Int64(TStream(datasource).Read(ptr^, size * nmemb)) div Int64(size);
    Debug('Read result %d', [Result]);
  except
    result:= 0;                                             { Assume nothing was read. No way to be sure of TStreams }
  end;
end;

function ogg_seek_func(datasource: pointer; offset: ogg_int64_t; whence: cint): cint; cdecl;
{ Returns zero on success, returns a non-zero value on error, result is undefined
  when device is unseekable. }
begin
  Debug('seek %d from %d', [offset, whence]);
  try
    case whence of
      SEEK_CUR: TStream(datasource).Seek(offset, soFromCurrent);
      SEEK_END: TStream(datasource).Seek(offset, soFromEnd);
      SEEK_SET: TStream(datasource).Seek(offset, soFromBeginning);
    end;
    result:= 0;
  except
    result:= -1;
  end;
end;

function ogg_close_func(datasource: pointer): cint; cdecl;
{ Returns zero when device was successfully closed, EOF on error. }
begin
  Debug('close', []);
  try
    TStream(datasource).Free;
    result:= 0;
  except
    result:= EOF;
  end;
end;

function ogg_tell_func(datasource: pointer): clong; cdecl;
{ Returns the current position of the file pointer on success, returns -1 on
  error, result is undefined when device is unseekable, does not set 'errno',
  does not perform linebreak conversion. }
begin
  try
    result:= TStream(datasource).Position;
    Debug('tell %d', [Result]);
  except
    result:= -1;
  end;
end;

procedure ovalLoadOGGStream(stream: TStream; var format: TALenum; var data: TALvoid; var size: TALsizei; var freq: TALsizei; var loop: TALint);
var
  Res: Integer;
  ovfile: OggVorbis_File;
  VorbisInfo: vorbis_info;
  samples: Integer;
begin
  // we can't use ov_open in Delphi, as this function works with C-file handles.
  // So we use ov_open_callbacks instead which uses a filestream
  FillChar(ovfile, sizeof(OggVorbis_File), 0);
  Res:= ov_open_callbacks(stream, ovfile, nil, 0, ov_stream_callbacks);
  if Res <> 0 then
    exit;
  // Get some infos out of the OGG-file
  VorbisInfo:= ov_info(ovfile, -1)^;
  if VorbisInfo.channels = 1 then
    format:= AL_FORMAT_MONO16
  else
    format:= AL_FORMAT_STEREO16;
  freq:= VorbisInfo.rate;
  loop:= AL_FALSE;

  samples:= ov_pcm_total(ovfile, -1);
  size:= samples * VorbisInfo.channels * 2;
  data:= nil;
  GetMem(data, size);
  try
    ov_read_ext(ovfile, data, size, False, 2, true);
  except
    FreeMem(data);
    data:= nil;
    size:= 0;
  end;
  ovfile.datasource:= nil;                                  //wtf why do you even touch this vorbis!?
  ov_clear(ovfile);
end;

procedure ovalUnloadOGG(format: TALenum; data: TALvoid; size: TALsizei; freq: TALsizei);
begin
  //Clean up
  if data <> nil then
    freemem(data);
end;

{ TALBufferOGG }

procedure TALBufferOGG.LoadFromStream(Stream: TStream);
var
  format: TALenum;
  data: TALvoid;
  size: TALsizei;
  freq: TALsizei;
  loop: TALint;
begin
  data:= nil;
  ovalLoadOggStream(Stream, format, data, size, freq, loop);
  alBufferData(Handle, format, data, size, freq);
  ComputeLength(format, size, freq);
  ovalUnLoadOgg(format, data, size, freq);
end;

{ TALDecoderStreamOGG }

procedure TALDecoderStreamOGG.InitStream;
var
  Res: Integer;
begin
  FillChar(FFile, Sizeof(OggVorbis_File), 0);
  Res:= ov_open_callbacks(Stream, FFile, nil, 0, ov_stream_callbacks);
  if Res <> 0 then begin
    exit;
  end;
  // Get some infos out of the OGG-file
  VorbisInfo:= ov_info(FFile, -1)^;
  if VorbisInfo.channels = 1 then
    FFormat:= AL_FORMAT_MONO16
  else
    FFormat:= AL_FORMAT_STEREO16;
end;

procedure TALDecoderStreamOGG.FinalizeStream;
begin
  FFile.datasource:= nil;
  ov_clear(FFile);
end;

function TALDecoderStreamOGG.DecodeStream(Into: Cardinal): boolean;
var
  Data: PByte;
  Size: Integer;
begin
  GetMem(Data, OGG_BUFFER_SIZE);
  try
    Size:= ov_read_ext(FFile, data, OGG_BUFFER_SIZE, False, 2, true);
    Result:= Size > 0;
    alBufferData(Into, FFormat, Data, Size, VorbisInfo.Rate);
  finally
    FreeMem(Data);
  end;
end;

function TALDecoderStreamOGG.GetOffset: Single;
begin
  Result:= ov_time_tell(FFile);
end;

procedure TALDecoderStreamOGG.SetOffset(const Value: Single);
begin
  ov_time_seek(FFile, Value);
end;

function TALDecoderStreamOGG.GetLength: Single;
begin
  Result:= ov_time_total(FFile, -1);
end;

initialization
  ov_stream_callbacks.read:= @ogg_read_func;
  ov_stream_callbacks.seek:= @ogg_seek_func;
  ov_stream_callbacks.close:= @ogg_close_func;
  ov_stream_callbacks.tell:= @ogg_tell_func;

end.

