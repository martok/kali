{
Kali Engine (C) Martok
https://github.com/martok/kali/
-----------------------------------------------------------------------
For copyright information see file license.txt.
}

unit uVFS;

interface

uses Classes, SysUtils, Contnrs;

type
  TvfsLayer = type Word;     // größere Werte überdecken kleinere

  TvfsFileInfo = record
    Size: Int64;
    Attributes: Integer;
    ModDate: TDateTime;
  end;

  TvfsOverlay = class;
  TvfsDirectoryList = class;
  TvfsListingOptions = set of (loLayer, loPath, loRecursive, loFilter, loAttrib);

  TvfsDirectoryEntry = class
    Source: TvfsOverlay;
    FileInfo: TvfsFileInfo;
  end;

  EvfsError = class(Exception);

  IStreamHandle = interface
  ['{57F8D713-B231-4268-81CA-EE3CE25664FE}']
    function GetStream: TStream;
  end;

  TvfsDirectoryAddFunc = procedure (FileName: string; Entry: TvfsDirectoryEntry; List: TvfsDirectoryList) of object;


  { TvfsStreamHandleRead }

  TvfsStreamHandleRead = class(TInterfacedObject, IStreamHandle)
  private
    fStream: TStream;
  public
    constructor Create(aStream: TStream);
    destructor Destroy; override;
    function GetStream: TStream;
  end;

  { TvfsStreamHandleWrite }
  TvfsStreamHandleWrite = class;
  TvfsWriteFunc = procedure(Handle: TvfsStreamHandleWrite; Data: Pointer; DataSize: integer) of object;
  TvfsStreamHandleWrite = class(TvfsStreamHandleRead)
  private
    fData: Pointer;
    fSize: Integer;
    fFlushFunction: TvfsWriteFunc;
  public
    constructor Create(aStream: TStream; aFlushFunction: TvfsWriteFunc; Data: Pointer; DataSize: integer);
    destructor Destroy; override;
  end;

  (*  Alles, was mit den wirklichen Daten spricht ist von [TvfsProvider] abgeleitet.
   *
   *  Methoden:
   *    FileExists       - Existiert diese Datei?
   *    OpenRead         - Öffnen mit dem Ziel, nur zu lesen
   *    OpenWrite        - Öffnen mit dem Ziel, zu schreiben und ggf. zu lesen.
   *                       CanCreate: Datei anlegen, falls sie nicht existiert
   *    CreateFile       - Datei erstellen oder Überschreiben & leeren
   *    Rename           - Datei oder Verzeichnis umbenennen
   *    DirectoryIndex   - Alle verfügbaren Dateien listen.
   *    StorageName      - Name, unter dem dieser Provider in VFSTAB geführt wird
   *    StorageGetData   - Daten für das ExtendedData-Feld der VFSTAB erzeugen
   *
   *  Dateinamen sind immer relativ auf den Mountpunkt, also FileSpec im Konstruktor
   *)

  { TvfsProvider }

  TvfsProvider = class
    // Achievement Get: WTF FPC?  -- virtueller Konstruktor, damit "class of" funktioniert, H- damit der leere Body keine Warnungen spammt
    constructor Create(const {%H-}FileSpec: string; const {%H-}ExtendedData: string=''); virtual;
    function GetFileInfo(const FileName: string; out FileInfo: TvfsFileInfo): boolean; virtual; abstract;
    function OpenRead(const FileName: string; out Stream: IStreamHandle): boolean; virtual; abstract;
    function OpenWrite(const FileName: string; const CanCreate: boolean; out Stream: IStreamHandle): boolean; virtual; abstract;
    function CreateFile(const FileName: string; out Stream: IStreamHandle): boolean; virtual; abstract;
    function Rename(const OldName, NewName: string): boolean; virtual; abstract;
    procedure DirectoryIndex(AddFunction: TvfsDirectoryAddFunc; List: TvfsDirectoryList; Path: string; Subdirs: boolean); virtual; abstract;

    class function StorageName: string; virtual; abstract;
    function StorageGetFileSpec: string; virtual; abstract;
    function StorageGetData: string; virtual;
  end;
  TvfsProviderClass = class of TvfsProvider;

  { TvfsOverlay }

  TvfsOverlay = class
  private
    FListingAttrib: integer;
    FListingFilter: string;
    procedure DirectoryAdd(FileName: string; Entry: TvfsDirectoryEntry; List: TvfsDirectoryList);
  public
    Layer: TvfsLayer;
    Provider: TvfsProvider;
    Mountpoint: string;
    function TranslatePath(const FileName: string; out RelativeName: string): boolean;
    constructor Create(aLayer: TvfsLayer; aProvider: TvfsProvider; aMountpoint: string);
    destructor Destroy; override;

    function GetFileInfo(const FileName: string; out FileInfo: TvfsFileInfo): boolean;
    function OpenRead(const FileName: string; out Stream: IStreamHandle): boolean;
    function OpenWrite(const FileName: string; const CanCreate: boolean; out Stream: IStreamHandle): boolean;
    function CreateFile(const FileName: string; out Stream: IStreamHandle): boolean;
    function Rename(const OldName, NewName: string): boolean;
    procedure Listing(List: TvfsDirectoryList; const Options: TvfsListingOptions; const Path: string; const Filter: string; const Attrib: integer);
  end;

  TvfsDirectoryList = class(TStringList)
  private
    function GetEntry(Index: Integer): TvfsDirectoryEntry;
  protected
    procedure ClearObjects;
  public
    destructor Destroy; override;
    procedure Delete(Index: Integer); override;
    procedure Clear; override;
    property Entry[Index: Integer]: TvfsDirectoryEntry read GetEntry;
    // Eintrag einfügen. Im Fall eines Duplikats wird AObject direkt freigegeben
    function AddEntry(const S: String; AObject: TvfsDirectoryEntry): Integer;
  end;

  (*
   *  Here's the magic :)
   *)

  { TvfsManager }

  TvfsManager = class
  private
    FLayers: TObjectList;
    function LocateFile(const Filename: string; const FilterLayer: boolean; const Layer: TvfsLayer): TvfsOverlay;
    function GetCount: integer;
    function GetOverlay(Index: integer): TvfsOverlay;
  protected
    FRegisteredProviders: TClassList;
    procedure InsertOverlay(Overlay: TvfsOverlay);
  public
    constructor Create;
    destructor Destroy; override;

    // Overlay hinzufügen
    function AddOverlay(const Layer: TvfsLayer; const Mountpoint: string; Provider: TvfsProvider): TvfsOverlay; overload;
    // Overlay (vollständig) entfernen
    procedure Remove(const Layer: TvfsLayer); overload;
    procedure Remove(const Overlay: TvfsOverlay); overload;
    procedure RemoveAll;

    // Zugriff auf den obersten Overlay mit dieser LayerID
    function FindOverlay(const Layer: TvfsLayer): TvfsOverlay;

    // Direktzugriff auf Provider
    property OverlayCount: integer read GetCount;
    property Overlay[Index: integer]: TvfsOverlay read GetOverlay;

    // Verzeichnislisting
    // List -> muss vorher erstellt werden
    // Options: loRecursive, sonst gibts nur das gewählte Verzeichnis (oder Root, wenn loPath nicht verwendet wird)
    //          andere Options aktivieren die Parameter
    procedure Listing(List: TvfsDirectoryList; const Options: TvfsListingOptions; const Layer: TvfsLayer = 0;
                  const Path: string = ''; const Filter: string = '*.*';
                  const Attrib: integer = 0);

    // Datei aus der obersten Ebene lesen.
    function FileExists(const FileName: String): Boolean;
    function DirectoryExists(const FileName: String): Boolean;
    function ReadFile(const Filename: string; out Stream: IStreamHandle): boolean; overload;

    // Provider registrieren
    procedure RegisterProvider(const ClassRef: TvfsProviderClass);
    // Provider zum Storage-Name suchen, nil wenn keiner gefunden
    function FindProvider(const StorageName: string): TvfsProviderClass;
    // Provider nach Einfügereihenfolge, nil wenn Index ungültig
    function GetProvider(const Index: integer): TvfsProviderClass;
  end;

function VFSManager: TvfsManager;
function CanonPathDelim(s: String): string;
function ExpandFileName(const Filename, Base: string): string;
function FileNameLike(const AString, APattern: String): Boolean;

implementation

uses
  uLogger;

const
  VFSTAB_COMMENT   = '#';
  VFSTAB_QUOTE     = '"';
  VFSTAB_SEPARATOR = #9;

var
  VFSSingleton: TvfsManager = nil;

function VFSManager: TvfsManager;
begin
  if not Assigned(VFSSingleton) then
    VFSSingleton:= TvfsManager.Create;
  Result:= VFSSingleton;
end;

function CanonPathDelim(s: String): string;
var i:integer;
begin
  for i:= 1 to Length(s) do
    if s[i] in ['\','/'] then
      s[i]:= PathDelim;
  Result:= S;
end;

function ExpandFileName(const Filename, Base: string): string;
begin
{$IF defined(WIN32) or defined(WIN64)}
  if (ExtractFileDrive(Filename)>'') then
{$ELSE}
  if (Copy(Filename,1,1)=PathDelim) then
{$IFEND}
    Result:= Filename
  else
    Result:= IncludeTrailingPathDelimiter(Base)+Filename;
end;

{ Like('Delphi', 'D*p?i') -> true.}
  {Michael Winter}
function Like(const AString, APattern: String): Boolean;
var
  StringPtr, PatternPtr: PChar;
  StringRes, PatternRes: PChar;
begin
  Result:=false;
  StringPtr:=PChar(AString);
  PatternPtr:=PChar(APattern);
  StringRes:=nil;
  PatternRes:=nil;
  if APattern='*' then begin Result:= true; exit end;
  repeat
    repeat // ohne vorangegangenes "*"
      case PatternPtr^ of
        #0: begin
          Result:=StringPtr^=#0;
          if Result or (StringRes=nil) or (PatternRes=nil) then
            Exit;
          StringPtr:=StringRes;
          PatternPtr:=PatternRes;
          Break;
        end;
        '*': begin
          inc(PatternPtr);
          PatternRes:=PatternPtr;
          Break;
        end;
        '?': begin
          if StringPtr^=#0 then
            Exit;
          inc(StringPtr);
          inc(PatternPtr);
        end;
        else begin
          if StringPtr^=#0 then
            Exit;
          if StringPtr^<>PatternPtr^ then begin
            if (StringRes=nil) or (PatternRes=nil) then
              Exit;
            StringPtr:=StringRes;
            PatternPtr:=PatternRes;
            Break;
          end
          else begin
            inc(StringPtr);
            inc(PatternPtr);
          end;
        end;
      end;
    until false;
    repeat // mit vorangegangenem "*"
      case PatternPtr^ of
        #0: begin
          Result:=true;
          Exit;
        end;
        '*': begin
          inc(PatternPtr);
          PatternRes:=PatternPtr;
        end;
        '?': begin
          if StringPtr^=#0 then
            Exit;
          inc(StringPtr);
          inc(PatternPtr);
        end;
        else begin
          repeat
            if StringPtr^=#0 then
              Exit;
            if StringPtr^=PatternPtr^ then
              Break;
            inc(StringPtr);
          until false;
          inc(StringPtr);
          StringRes:=StringPtr;
          inc(PatternPtr);
          Break;
        end;
      end;
    until false;
  until false;
end;

function FileNameLike(const AString, APattern: String): Boolean;
begin
  Result:= Like(AnsiLowerCaseFileName(AString), AnsiLowerCaseFileName(APattern));
end;

{ TvfsStreamHandleRead }

constructor TvfsStreamHandleRead.Create(aStream: TStream);
begin
  inherited Create;
  fStream:= aStream;
end;

destructor TvfsStreamHandleRead.Destroy;
begin
  fStream.Free;
  fStream:= nil;
  inherited Destroy;
end;

function TvfsStreamHandleRead.GetStream: TStream;
begin
  Result:= fStream;
end;

{ TvfsStreamHandleWrite }

constructor TvfsStreamHandleWrite.Create(aStream: TStream; aFlushFunction: TvfsWriteFunc; Data: Pointer; DataSize: integer);
begin
  inherited Create(aStream);
  fFlushFunction:= aFlushFunction;
  if Assigned(Data) and (DataSize>0) then begin
    GetMem(fData, DataSize);
    Move(Data^, fData^, DataSize);
  end else
    fData:= nil;
  fSize:= DataSize;
end;

destructor TvfsStreamHandleWrite.Destroy;
begin
  if Assigned(fFlushFunction) then
    fFlushFunction(Self, fData, fSize);
  if Assigned(fData) then
    Freememory(fData);
  inherited Destroy;
end;

{ TvfsProvider }

constructor TvfsProvider.Create(const FileSpec: string; const ExtendedData: string);
begin
  inherited Create;
end;

function TvfsProvider.StorageGetData: string;
begin
  Result:= '';
end;

{ TvfsOverlay }

constructor TvfsOverlay.Create(aLayer: TvfsLayer; aProvider: TvfsProvider; aMountpoint: string);
var mp: string;
begin
  Layer:= aLayer;
  Provider:= aProvider;
  mp:= CanonPathDelim(aMountpoint);
  mp:= IncludeTrailingPathDelimiter(mp);
  while (mp>'') and (mp[1]=PathDelim) do
    Delete(mp, 1, 1);
  Self.Mountpoint:= mp;
end;

destructor TvfsOverlay.Destroy;
begin
  FreeAndNil(Provider);
  inherited;
end;

function TvfsOverlay.GetFileInfo(const FileName: string; out FileInfo: TvfsFileInfo): boolean;
var fn: string;
begin
  Result:= TranslatePath(Filename, fn) and Provider.GetFileInfo(fn, FileInfo);
end;

function TvfsOverlay.OpenRead(const FileName: string; out Stream: IStreamHandle): boolean;
var fn: string;
begin
  Result:= TranslatePath(Filename, fn) and Provider.OpenRead(fn, Stream);
end;

function TvfsOverlay.OpenWrite(const FileName: string; const CanCreate: boolean; out Stream: IStreamHandle): boolean;
var fn: string;
begin
  Result:= TranslatePath(Filename, fn) and Provider.OpenWrite(fn, CanCreate, Stream);
end;

function TvfsOverlay.CreateFile(const FileName: string; out Stream: IStreamHandle): boolean;
var fn: string;
begin
  Result:= TranslatePath(Filename, fn) and Provider.CreateFile(fn, Stream);
end;

function TvfsOverlay.Rename(const OldName, NewName: string): boolean;
var fon, fnn: string;
begin
  Result:= TranslatePath(OldName, fon) and TranslatePath(NewName, fnn) and Provider.Rename(fon, fnn);
end;

procedure TvfsOverlay.Listing(List: TvfsDirectoryList; const Options: TvfsListingOptions; const Path: string; const Filter: string; const Attrib: integer);
var
  subpath: string;
  e: TvfsDirectoryEntry;
  m: string;
begin
  subpath:= CanonPathDelim(IncludeTrailingPathDelimiter(Path));
  if not (loPath in Options) or TranslatePath(Path, subpath) then begin
    m:= CanonPathDelim(Mountpoint);
    while m>'' do begin
      if (not (loPath in Options)) or (0=AnsiCompareStr(Path, Copy(m, 1, Length(Path)))) then begin
        e:= TvfsDirectoryEntry.Create;
        e.FileInfo.Attributes:= faDirectory or faSymLink;
        e.FileInfo.ModDate:= 0;
        e.FileInfo.Size:= 0;
        e.Source:= Self;
        List.AddEntry(m, e);
      end;
      Delete(m, Length(m),1);
      m:= copy(m, 1, LastDelimiter(PathDelim, m));
    end;
    if loAttrib in Options then
      FListingAttrib:= Attrib
    else
      FListingAttrib:= Integer($FFFFFFFF);
    if loFilter in Options then
      FListingFilter:= Filter
    else
      FListingFilter:= '*';
    if not (loPath in Options) then
      subpath:= '';
    Provider.DirectoryIndex({$IFDEF FPC}@{$ENDIF}DirectoryAdd, List, subpath, loRecursive in Options);
  end;
end;

procedure TvfsOverlay.DirectoryAdd(FileName: string; Entry: TvfsDirectoryEntry; List: TvfsDirectoryList);
var fn: string;
begin
  Entry.Source:= Self;
  fn:= ExtractFileName(FileName);
  if ((FListingAttrib and Entry.FileInfo.Attributes) > 0) and     // Attrib passt
     ((Entry.FileInfo.Attributes and faDirectory >0) or           // Ist Verzeichnis, oder...
     FileNameLike(fn, FListingFilter)) then                       // ...DATEIname passt auf Maske
    List.AddEntry(Mountpoint+Filename, Entry)
  else
    Entry.Free;
end;

function TvfsOverlay.TranslatePath(const FileName: string; out RelativeName: string): boolean;
var ff: string;
begin
  ff:= Copy(CanonPathDelim(FileName),1, Length(Mountpoint));
  Result:= 0 = AnsiCompareText(ff, Mountpoint);
  if Result then
    RelativeName:= Copy(CanonPathDelim(FileName),length(ff)+1, Maxint);
end;

{ TvfsDirectoryList }

destructor TvfsDirectoryList.Destroy;
begin
  ClearObjects;
  inherited;
end;

procedure TvfsDirectoryList.Clear;
begin
  ClearObjects;
  inherited;
end;

procedure TvfsDirectoryList.Delete(Index: Integer);
var
  f: TvfsDirectoryEntry;
begin
  f:= TvfsDirectoryEntry(Objects[Index]);
  Objects[Index]:= nil;
  F.Free;
  inherited;
end;

procedure TvfsDirectoryList.ClearObjects;
var
  i: integer;
  f: TvfsDirectoryEntry;
begin
  for i:= 0 to Count-1 do begin
    f:= TvfsDirectoryEntry(Objects[i]);
    Objects[i]:= nil;
    F.Free;
  end;
end;

function TvfsDirectoryList.GetEntry(Index: Integer): TvfsDirectoryEntry;
begin
  Result:= TvfsDirectoryEntry(Objects[Index]);
end;

function TvfsDirectoryList.AddEntry(const S: String; AObject: TvfsDirectoryEntry): Integer;
begin
  if IndexOf(S)>=0 then begin
    Result:= -1;
    AObject.Free;
  end else
    Result:= AddObject(S, AObject);
end;

{ TvfsManager }

constructor TvfsManager.Create;
begin
  inherited Create;
  FLayers:= TObjectList.Create(true);
  FRegisteredProviders:= TClassList.Create;
end;

destructor TvfsManager.Destroy;
begin
  FreeAndNil(FRegisteredProviders);
  FreeAndNil(FLayers);
  inherited;
end;

function TvfsManager.AddOverlay(const Layer: TvfsLayer; const Mountpoint: string; Provider: TvfsProvider): TvfsOverlay;
var ol: TvfsOverlay;
begin
  Result:= nil;
  ol:= TvfsOverlay.Create(Layer, Provider, Mountpoint);
  try
    GetLogger.Log('VFS', 'Loaded Layer %d as /%s from %s',[Layer,Mountpoint, Provider.StorageGetFileSpec]);
    InsertOverlay(ol);
    Result:= ol;
  except
    FreeAndNil(ol);
    raise;
  end;
end;

procedure TvfsManager.InsertOverlay(Overlay: TvfsOverlay);
var
  i: integer;
begin
  // add on top of the matching layer
  for i:= 0 to FLayers.Count-1 do begin
    if TvfsOverlay(FLayers[i]).Layer > Overlay.Layer then begin
      FLayers.Insert(i, Overlay);
      Exit;
    end;
  end;
  // not inserted anything? then new layer is larger than anything before
  FLayers.Add(Overlay);
end;

function TvfsManager.LocateFile(const Filename: string; const FilterLayer: boolean; const Layer: TvfsLayer): TvfsOverlay;
var
  i: integer;
  ol: TvfsOverlay;
  dummy: TvfsFileInfo;
begin
  Result:= nil;
  for i:= FLayers.Count-1 downto 0 do begin
    ol:= TvfsOverlay(FLayers[i]);
    if not FilterLayer or (ol.Layer=Layer) then begin
      if ol.GetFileInfo(FileName, dummy) then begin
        Result:= ol;
        exit;
      end;
    end;
  end;
end;

function TvfsManager.ReadFile(const Filename: string; out Stream: IStreamHandle): boolean;
var
  ol: TvfsOverlay;
begin
  ol:= LocateFile(Filename,false,0);
  Result:= Assigned(ol) and ol.OpenRead(FileName, Stream);
end;

procedure TvfsManager.Remove(const Layer: TvfsLayer);
var
  i: integer;
begin
  for i:= FLayers.Count-1 downto 0 do
    if TvfsOverlay(FLayers[i]).Layer=Layer then
      FLayers.Delete(i);
end;

procedure TvfsManager.Remove(const Overlay: TvfsOverlay);
begin
  FLayers.Remove(Overlay);
end;

procedure TvfsManager.RemoveAll;
begin
  FLayers.Clear;
end;

function TvfsManager.FindOverlay(const Layer: TvfsLayer): TvfsOverlay;
var
  i: integer;
begin
  Result:= nil;
  for i:= FLayers.Count-1 downto 0 do
    if TvfsOverlay(FLayers[i]).Layer=Layer then begin
      Result:= TvfsOverlay(FLayers[i]);
      exit;
    end;
end;

procedure TvfsManager.Listing(List: TvfsDirectoryList; const Options: TvfsListingOptions;
  const Layer: TvfsLayer; const Path: string; const Filter: string;
  const Attrib: integer);
var
  i: integer;
begin
  List.Sorted:= true;
  List.Duplicates:= dupIgnore;
  for i:= FLayers.Count-1 downto 0 do
    if not (loLayer in Options) or (TvfsOverlay(FLayers[i]).Layer=Layer) then begin
      TvfsOverlay(FLayers[i]).Listing(List, Options - [loLayer], Path, Filter, Attrib);
    end;
end;

function TvfsManager.FileExists(const FileName: String): Boolean;
var
  ol: TvfsOverlay;
  fi: TvfsFileInfo;
begin
  ol:= LocateFile(Filename,false,0);
  Result:= Assigned(ol) and ol.GetFileInfo(FileName, fi) and ((fi.Attributes and faDirectory)=0);
end;

function TvfsManager.DirectoryExists(const FileName: String): Boolean;
var
  ol: TvfsOverlay;
  fi: TvfsFileInfo;
begin
  ol:= LocateFile(Filename,false,0);
  Result:= Assigned(ol) and ol.GetFileInfo(FileName, fi) and ((fi.Attributes and faDirectory)>0);
end;

function TvfsManager.GetCount: integer;
begin
  Result:= FLayers.Count;
end;

function TvfsManager.GetOverlay(Index: integer): TvfsOverlay;
begin
  Result:= TvfsOverlay(FLayers[Index]);
end;


procedure TvfsManager.RegisterProvider(const ClassRef: TvfsProviderClass);
begin
  if Assigned(ClassRef) and (FRegisteredProviders.IndexOf(ClassRef)<0) then
    FRegisteredProviders.Add(ClassRef);
end;

function TvfsManager.FindProvider(const StorageName: string): TvfsProviderClass;
var
  i: integer;
begin
  Result:= nil;
  for i:= FRegisteredProviders.Count-1 downto 0 do
    if AnsiCompareText(StorageName, TvfsProviderClass(FRegisteredProviders[i]).StorageName)=0 then begin
      Result:= TvfsProviderClass(FRegisteredProviders[i]);
      break;
    end;
end;

function TvfsManager.GetProvider(const Index: integer): TvfsProviderClass;
begin
  if (Index>=0) and (Index<FRegisteredProviders.Count) then
    Result:= TvfsProviderClass(FRegisteredProviders[Index])
  else
    Result:= nil;
end;

initialization
finalization
  FreeAndNil(VFSSingleton);
end.
