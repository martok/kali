unit uVFSZipArchive;

interface

uses
  SysUtils, Classes, uVFS, KAZip;

type

  { TvfsZipArchive }

  TvfsZipArchive = class(TvfsProvider)
  private
    FZipper: TKAZip;
    procedure Flush(Handle: TvfsStreamHandleWrite; Data: Pointer; DataSize: integer);
  public
    constructor Create(const FileSpec:string; const ExtendedData: string=''); override;
    destructor Destroy; override;
    class function StorageName: string; override;
    function StorageGetFileSpec: string; override;
    function GetFileInfo(const FileName: string; out FileInfo: TvfsFileInfo): Boolean; override;
    function OpenRead(const FileName: String; out Stream: IStreamHandle): Boolean; override;
    function OpenWrite(const FileName: String; const CanCreate: Boolean; out Stream: IStreamHandle): Boolean; override;
    function CreateFile(const FileName: string; out Stream: IStreamHandle): boolean; override;
    function Rename(const OldName, NewName: string): boolean; override;
    procedure DirectoryIndex(AddFunction: TvfsDirectoryAddFunc; List: TvfsDirectoryList; Path: string; Subdirs: boolean); override;

    function ZipFilename: string;
  end;

implementation

{ TvfsZipArchive }

constructor TvfsZipArchive.Create(const FileSpec: string; const ExtendedData: string);
begin
  inherited;
  FZipper:= TKAZip.Create(nil);
  FZipper.Open(CanonPathDelim(FileSpec));
  FZipper.CompressionType:= ctFast;
end;

destructor TvfsZipArchive.Destroy;
begin
  FreeAndNil(FZipper);
  inherited;
end;

class function TvfsZipArchive.StorageName: string;
begin
  Result:= 'zip';
end;

function TvfsZipArchive.StorageGetFileSpec: string;
begin
  Result:= Self.ZipFileName;
end;

procedure TvfsZipArchive.DirectoryIndex(AddFunction: TvfsDirectoryAddFunc; List: TvfsDirectoryList; Path: string; Subdirs: boolean);
var
  i: integer;
  a: TvfsDirectoryEntry;
  e: TKAZipEntriesEntry;
  fp: string;
  dirOk: boolean;
begin
  for i:= 0 to FZipper.Entries.Count-1 do begin
    e:= FZipper.Entries.Items[i];
    fp:= FZipper.GetFilePath(e.FileName);
    if Subdirs then
      dirOk:= 0=AnsiCompareStr(Path, Copy(fp, 1, Length(Path)))
    else
      dirOk:= 0=AnsiCompareFileName(Path, fp);
    if dirOk then begin
      a:= TvfsDirectoryEntry.Create;
      a.FileInfo.Size:= e.SizeUncompressed;
      a.FileInfo.Attributes:= e.Attributes;
      a.FileInfo.ModDate:= e.Date;
      AddFunction(fp+FZipper.GetFileName(e.FileName), a, List);
    end;
  end;
end;

function TvfsZipArchive.GetFileInfo(const FileName: String; out FileInfo: TvfsFileInfo): Boolean;
var
  i:integer;
begin
  i:= FZipper.Entries.IndexOf(CanonPathDelim(FileName));
  Result:= i>=0;
  if Result then begin
    FileInfo.Size:= FZipper.Entries.Items[i].SizeUncompressed;
    FileInfo.Attributes:= FZipper.Entries.Items[i].Attributes;
    FileInfo.ModDate:= FZipper.Entries.Items[i].Date;
  end;
end;

function TvfsZipArchive.OpenRead(const FileName: String; out Stream: IStreamHandle): Boolean;
var
  i: integer;
  ms: TMemoryStream;
begin
  Stream:= nil;
  Result:= false;
  try
    i:= FZipper.Entries.IndexOf(CanonPathDelim(FileName));
    Result:= i>=0;
    if Result then begin
      ms:= TMemoryStream.Create;
      FZipper.Entries.Items[i].ExtractToStream(ms);
      ms.Position:= 0;
      Stream:= TvfsStreamHandleRead.Create(ms);
      Result:= true;
    end;
  except
    FreeAndNil(ms);
    raise;
  end;
end;

function TvfsZipArchive.OpenWrite(const FileName: String; const CanCreate: Boolean; out Stream: IStreamHandle): Boolean;
var
  i: integer;
  fc: String;
  ms: TMemoryStream;
begin
  fc:= CanonPathDelim(FileName);
  Result:= false;
  try
    ms:= TMemoryStream.Create;
    i:= FZipper.Entries.IndexOf(fc);
    if i>=0 then
      FZipper.Entries.Items[i].ExtractToStream(ms)
    else
      if not CanCreate then begin
        FreeAndNil(ms);
        exit;
      end;
    if Assigned(ms) then begin
      ms.Position:= ms.Size;
      Stream:= TvfsStreamHandleWrite.Create(ms, {$IFDEF FPC}@{$ENDIF}Self.Flush, PChar(FileName), 1+strlen(PChar(FileName)));
      Result:= true;
    end;
  except
    FreeAndNil(ms);
    raise;
  end;
end;

function TvfsZipArchive.CreateFile(const FileName: string; out Stream: IStreamHandle): boolean;
var
  i: integer;
  fc: String;
  ms: TMemoryStream;
begin
  fc:= CanonPathDelim(FileName);
  Result:= false;
  try
    ms:= TMemoryStream.Create;
    Stream:= TvfsStreamHandleWrite.Create(ms, {$IFDEF FPC}@{$ENDIF}Self.Flush, PChar(FileName), 1+strlen(PChar(FileName)));
    Result:= true;
  except
    FreeAndNil(ms);
    raise;
  end;
end;

function TvfsZipArchive.Rename(const OldName, NewName: string): boolean;
begin
  if FZipper.Entries.IndexOf(OldName)>=0 then
    FZipper.Rename(OldName, NewName)
  else
    FZipper.RenameFolder(OldName, NewName);
end;

procedure TvfsZipArchive.Flush(Handle: TvfsStreamHandleWrite; Data: Pointer; DataSize: integer);
var s:string;
begin
  s:= StrPas(Data);
  Handle.GetStream.Position:= 0;
  FZipper.AddStream(s, Handle.GetStream);
end;

function TvfsZipArchive.ZipFilename: string;
begin
  Result:= FZipper.FileName;
end;

initialization
  VFSManager.RegisterProvider(TvfsZipArchive);
end.

