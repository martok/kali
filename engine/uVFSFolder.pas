unit uVFSFolder;

interface

uses
  SysUtils, Classes, uVFS;

type

  { TvfsFileSystemFolder }

  TvfsFileSystemFolder = class(TvfsProvider)
  private
    FRoot: string;
    function ExpandPath(P:string): string;
  public
    constructor Create(const FileSpec:string; const ExtendedData: string=''); override;
    class function StorageName: string; override;
    function StorageGetFileSpec: string; override;
    // ACHTUNG: FileInfo.Size wird mit einem Integer gefüllt. Bei Dateien > 2GB aufpassen!
    function GetFileInfo(const FileName: string; out FileInfo: TvfsFileInfo): Boolean; override;
    function OpenRead(const FileName: String; out Stream: IStreamHandle): Boolean; override;
    function OpenWrite(const FileName: String; const CanCreate: Boolean; out Stream: IStreamHandle): Boolean; override;
    function CreateFile(const FileName: string; out Stream: IStreamHandle): boolean; override;
    function Rename(const OldName, NewName: string): boolean; override;
    procedure DirectoryIndex(AddFunction: TvfsDirectoryAddFunc; List: TvfsDirectoryList; Path: string; Subdirs: boolean); override;

    function Root: string;
  end;


implementation

{ TvfsFileSystemFolder }

constructor TvfsFileSystemFolder.Create(const FileSpec: string; const ExtendedData: string);
begin
  inherited;
  FRoot:= CanonPathDelim(FileSpec);
end;

class function TvfsFileSystemFolder.StorageName: string;
begin
  Result:= 'folder';
end;

function TvfsFileSystemFolder.StorageGetFileSpec: string;
begin
  Result:= Self.Root;
end;

procedure TvfsFileSystemFolder.DirectoryIndex(AddFunction: TvfsDirectoryAddFunc; List: TvfsDirectoryList; Path: string; Subdirs: boolean);
  procedure FindFiles(sRoot, sPfad: String);
  var
    Rec:TSearchRec;
    s: string;
    a: TvfsDirectoryEntry;
  begin
    if sPfad>'' then
      sPfad:= IncludeTrailingPathDelimiter(sPfad);
    if FindFirst(sRoot+sPfad+'*', faAnyFile,Rec)=0 then
    repeat
      if (Rec.Name<>'.') and (Rec.Name<>'..') then begin
        s:= sPfad+Rec.Name;
        a:= TvfsDirectoryEntry.Create;
        a.FileInfo.Size:= Rec.Size;
        a.FileInfo.Attributes:= Rec.Attr;
        a.FileInfo.ModDate:= FileDateToDateTime(Rec.Time);
        if (Rec.Attr and faDirectory = faDirectory) then begin
          AddFunction(IncludeTrailingPathDelimiter(s), a, List);
          if Subdirs then
            FindFiles(sRoot, s);
        end else
          AddFunction(s, a, List);
      end;
    until FindNext(Rec)<>0;
    Findclose(Rec);
  end;
begin
  FindFiles(IncludeTrailingPathDelimiter(FRoot),Path);
end;

function TvfsFileSystemFolder.ExpandPath(P: string): string;
begin
  Result:= CanonPathDelim(IncludeTrailingPathDelimiter(FRoot)+P);
end;


function TvfsFileSystemFolder.GetFileInfo(const FileName: String; out FileInfo: TvfsFileInfo): Boolean;
var
  sr: TSearchRec;
begin
  Result:= FindFirst(ExpandPath(FileName),faAnyFile, sr) = 0;
  try
    if Result then begin
      FileInfo.Size:= sr.Size;
      FileInfo.Attributes:= sr.Attr;
      FileInfo.ModDate:= FileDateToDateTime(sr.Time);
    end;
  finally
    FindClose(sr);
  end;
end;

function TvfsFileSystemFolder.OpenRead(const FileName: String; out Stream: IStreamHandle): Boolean;
var
  fs: TFileStream;
begin
  Stream:= nil;
  Result:= false;
  try
    Result:= FileExists(ExpandPath(FileName));
    if Result then begin
      fs:= TFileStream.Create(ExpandPath(FileName), fmOpenRead);
      Result:= Assigned(fs);
      if Result then
        Stream:= TvfsStreamHandleRead.Create(fs);
    end;
  except
    FreeAndNil(fs);
    raise;
  end;
end;

function TvfsFileSystemFolder.OpenWrite(const FileName: String; const CanCreate: Boolean; out Stream: IStreamHandle): Boolean;
var
  opt: word;
  fs: TFileStream;
begin
  Stream:= nil;
  Result:= false;
  try
    if FileExists(FileName) then
      opt:= fmOpenReadWrite
    else
      if CanCreate then begin
        opt:= fmCreate;
        ForceDirectories(ExtractFilePath(ExpandPath(FileName)))
      end else
        exit;
    fs:= TFileStream.Create(ExpandPath(FileName), opt);
    Result:= Assigned(fs);
    if Result then
      // We don't need to do that on a simple file :)
      Stream:= TvfsStreamHandleWrite.Create(fs, nil, nil, 0);
  except
    FreeAndNil(fs);
    raise;
  end;
end;

function TvfsFileSystemFolder.CreateFile(const FileName: string; out Stream: IStreamHandle): boolean;
var
  fs: TFileStream;
begin
  Stream:= nil;
  Result:= false;
  try
    ForceDirectories(ExtractFilePath(ExpandPath(FileName)));
    fs:= TFileStream.Create(ExpandPath(FileName), fmCreate);
    Stream:= TvfsStreamHandleWrite.Create(fs, nil, nil, 0);
    Result:= true;
  except
    FreeAndNil(fs);
    raise;
  end;
end;

function TvfsFileSystemFolder.Rename(const OldName, NewName: string): boolean;
begin
  Result:= RenameFile(ExpandPath(OldName), ExpandPath(NewName));
end;

function TvfsFileSystemFolder.Root: string;
begin
  Result:= FRoot;
end;


initialization
  VFSManager.RegisterProvider(TvfsFileSystemFolder);
end.

