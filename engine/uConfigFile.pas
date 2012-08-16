{
Kali Engine (C) Martok
https://github.com/martok/kali/
-----------------------------------------------------------------------
For copyright information see file license.txt.
}

unit uConfigFile;

interface

uses
  SysUtils, Classes;

type
  EConfigException = class(Exception)
  end;
  TkcfSection = class;
  TkcfConfigFile = class;

  TkcfSection = class
  private
    FSections,
      FValues: TStringList;
  protected
    procedure ClearSections;
    procedure ClearValues;
    procedure SaveData(Stream: TStream; Indent: string);
    procedure LoadData(Data: TStream);
    procedure AddValueChecked(Name: String; Val: TObject);
  public
    constructor Create;
    destructor Destroy; override;

    function SectionExists(Name: string): boolean;
    function Section(Name: string): TkcfSection;
    procedure DeleteSection(Name: string);

    function ValueExists(Name: string): boolean;
    function GetValue(Name: string; Default: Int64 = 0): Int64; overload;
    function GetValue(Name: string; Default: Double = 0): Double; overload;
    function GetValue(Name: string; Default: WideString = ''): WideString; overload;
    function GetValue(Name: string; Default: Boolean = false): Boolean; overload;
    procedure SetValue(Name: string; Value: Int64); overload;
    procedure SetValue(Name: string; Value: Double); overload;
    procedure SetValue(Name: string; Value: WideString); overload;
    procedure SetValue(Name: string; Value: Boolean); overload;
    procedure UnsetValue(Name: string);
  end;

  TkcfConfigFile = class(TkcfSection)
  private
  public
    constructor Create(Data: TStream);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
  end;

implementation

uses IniFiles, Variants, StrUtils, Windows;

const
  sComment = '#';
  sSectionEnd = 'end';
  sSectionMarker = ':';
  sValueDelim = '=';
  sValueQuote = '"';
  sValueDecimal = '.';

type
  StoredValue = Variant;
  TkcfValue = class
  private
    Format: TFormatSettings;
    FValue: StoredValue;
    procedure SetValue(const Value: StoredValue);
  protected
    procedure LoadData(Data: string);
    function SaveData: string;
    class function Escape(Value: WideString): AnsiString;
    class function Unescape(Value: AnsiString): WideString;
  public
    constructor Create(Val: StoredValue);
    property Value: StoredValue read FValue write SetValue;
  end;

  { TkcfValue }

constructor TkcfValue.Create(Val: StoredValue);
begin
  inherited Create;
  SetValue(Val);
  GetLocaleFormatSettings($0409, Format);
end;

procedure TkcfValue.SetValue(const Value: Variant);
begin
  FValue:= Value;
end;

procedure TkcfValue.LoadData(Data: string);
var
  b: boolean;
  i: int64;
  d: double;
  p: PChar;
begin
  if TryStrToInt64(Data, i) then
    Value:= i
  else if TryStrToFloat(Data, d, Format) then
    Value:= d
  else if TryStrToBool(Data, b) then
    Value:= b
  else begin
    p:= PChar(Data);
    if p^=sValueQuote then
      Value:= Unescape(AnsiExtractQuotedStr(p, sValueQuote))
    else
      Value:= trim(Data);
  end;
end;

function TkcfValue.SaveData: string;
begin
  if VarIsType(FValue, varBoolean) then
    Result:= BoolToStr(FValue, false)
  else if VarIsType(FValue, varInt64) then
    Result:= IntToStr(FValue)
  else if VarIsType(FValue, varDouble) then
    Result:= FloatToStr(FValue, Format)
  else
    Result:= AnsiQuotedStr(Escape(FValue), sValueQuote);
end;

class function TkcfValue.Escape(Value: WideString): AnsiString;
var
  i: integer;
  wc: WideChar;
begin
  Result:= '';
  for i:= 1 to length(Value) do begin
    wc:= Value[i];
    case Ord(wc) of
      $20,
      //'"'
      $22..$3c,
      //'='
      $3E..$5B,
      //'\'
      $5D..$7E: Result:= Result + wc;
    else
      Result:= Result + '\'+IntToHex(ord(wc),4);
    end;
  end;
end;

class function TkcfValue.Unescape(Value: AnsiString): WideString;
var
  i: integer;
  c: Char;
begin
  Result:= '';
  i:= 1;
  while i <= length(value) do begin
    c:= Value[i];
    if c='\' then begin
      Result:= Result + WideChar(StrToInt('$'+Copy(Value,i+1,4)));
      inc(i, 4);
    end else
      Result:= Result + c;
    inc(i);
  end;
end;

{ TkcfCompound }

constructor TkcfSection.Create;
begin
  inherited;
  FSections:= THashedStringList.Create;
  FSections.Sorted:= true;
  FSections.Duplicates:= dupError;
  FValues:= THashedStringList.Create;
  FValues.Sorted:= true;
  FValues.Duplicates:= dupError;
end;

destructor TkcfSection.Destroy;
begin
  ClearSections;
  ClearValues;
  FreeAndNil(FSections);
  FreeAndNil(FValues);
  inherited;
end;

procedure TkcfSection.ClearSections;
var
  i: integer;
begin
  for i:= FSections.Count - 1 downto 0 do
    FSections.Objects[i].Free;
  FSections.Clear;
end;

procedure TkcfSection.ClearValues;
var
  i: integer;
begin
  for i:= FValues.Count - 1 downto 0 do
    FValues.Objects[i].Free;
  FValues.Clear;
end;

function TkcfSection.SectionExists(Name: string): boolean;
begin
  Result:= FSections.IndexOf(Name) >= 0;
end;

function TkcfSection.Section(Name: string): TkcfSection;
var
  i: integer;
begin
  i:= FSections.IndexOf(Name);
  if i >= 0 then
    Result:= TkcfSection(FSections.Objects[i])
  else begin
    Result:= TkcfSection.Create;
    FSections.AddObject(Name, Result);
  end;
end;

procedure TkcfSection.DeleteSection(Name: string);
var
  i: integer;
begin
  i:= FSections.IndexOf(Name);
  if i >= 0 then begin
    FSections.Objects[i].Free;
    FSections.Delete(i);
  end;
end;

function TkcfSection.ValueExists(Name: string): boolean;
begin
  Result:= FValues.IndexOf(Name) >= 0;
end;

function TkcfSection.GetValue(Name: string; Default: Int64): Int64;
var
  i: integer;
begin
  i:= FValues.IndexOf(Name);
  if i < 0 then
    Result:= Default
  else
    Result:= TkcfValue(FValues.Objects[i]).Value;
end;

function TkcfSection.GetValue(Name: string; Default: Double): Double;
var
  i: integer;
begin
  i:= FValues.IndexOf(Name);
  if i < 0 then
    Result:= Default
  else
    Result:= TkcfValue(FValues.Objects[i]).Value;
end;

function TkcfSection.GetValue(Name: string; Default: WideString): WideString;
var
  i: integer;
begin
  i:= FValues.IndexOf(Name);
  if i < 0 then
    Result:= Default
  else
    Result:= TkcfValue(FValues.Objects[i]).Value;
end;

function TkcfSection.GetValue(Name: string; Default: Boolean): Boolean;
var
  i: integer;
begin
  i:= FValues.IndexOf(Name);
  if i < 0 then
    Result:= Default
  else
    Result:= TkcfValue(FValues.Objects[i]).Value;
end;


procedure TkcfSection.AddValueChecked(Name: String; Val: TObject);
var
  i: integer;
begin
  for i:= 1 to Length(Name) do
    if not (Name[i] in ['a'..'z','A'..'Z','0'..'9']) then
      raise EConfigException.CreateFmt('Invalid Value Name: %s',[Name]);
  FValues.AddObject(Name, Val);
end;

procedure TkcfSection.SetValue(Name: string; Value: Int64);
var
  i: integer;
begin
  i:= FValues.IndexOf(Name);
  if i < 0 then
    AddValueChecked(Name, TkcfValue.Create(Value))
  else
    TkcfValue(FValues.Objects[i]).Value:= Value;
end;

procedure TkcfSection.SetValue(Name: string; Value: Double);
var
  i: integer;
begin
  i:= FValues.IndexOf(Name);
  if i < 0 then
    AddValueChecked(Name, TkcfValue.Create(Value))
  else
    TkcfValue(FValues.Objects[i]).Value:= Value;
end;

procedure TkcfSection.SetValue(Name: string; Value: WideString);
var
  i: integer;
begin
  i:= FValues.IndexOf(Name);
  if i < 0 then
    AddValueChecked(Name, TkcfValue.Create(Value))
  else
    TkcfValue(FValues.Objects[i]).Value:= Value;
end;

procedure TkcfSection.SetValue(Name: string; Value: Boolean);
var
  i: integer;
begin
  i:= FValues.IndexOf(Name);
  if i < 0 then
    AddValueChecked(Name, TkcfValue.Create(Value))
  else
    TkcfValue(FValues.Objects[i]).Value:= Value;
end;

procedure TkcfSection.UnsetValue(Name: string);
var
  i: integer;
begin
  i:= FValues.IndexOf(Name);
  if i >= 0 then begin
    FValues.Objects[i].Free;
    FValues.Delete(i);
  end;
end;

{ TkcfConfigFile }

constructor TkcfConfigFile.Create(Data: TStream);
begin
  inherited Create;
  if Assigned(Data) then
    LoadFromStream(Data);
end;

procedure TkcfConfigFile.LoadFromStream(Stream: TStream);
begin
  ClearSections;
  ClearValues;
  LoadData(Stream);
end;

procedure TkcfSection.LoadData(Data: TStream);
var
  EOF: boolean;
  function ReadLine: AnsiString;
  var
    c: AnsiChar;
  begin
    c:= #0;
    Result:= '';
    repeat
      EOF:= Data.Read(c, sizeof(c)) <> sizeof(c);
      if not EOF and not (Ord(c) in [13, 10]) then
        Result:= Result + c;
    until EOF or (Ord(c) in [13, 10]);
  end;
var
  l, sn, vn, vs: string;
  se: TkcfSection;
  va: TkcfValue;
begin
  repeat
    l:= ReadLine;
    l:= trim(l);
    if l = '' then
      continue;
    if l = sSectionEnd then
      exit;
    if AnsiStartsStr(sComment, l) then
      continue;
    if AnsiEndsStr(sSectionMarker, l) then begin
      sn:= trim(Copy(l, 1, length(l) - length(sSectionMarker)));
      if SectionExists(sn) then
        raise EConfigException.Create('Redeclared Section');
      se:= Section(sn);
      try
        se.LoadData(Data);
      except
        DeleteSection(sn);
        FreeAndNil(se);
      end;
    end else if Pos(sValueDelim, l) > 0 then begin
      vn:= trim(Copy(l, 1, Pos(sValueDelim, l) - 1));
      vs:= trim(Copy(l, Pos(sValueDelim, l) + 1, Maxint));
      if ValueExists(vn) then
        raise EConfigException.Create('Redeclared Value');
      va:= TkcfValue.Create('');
      try
        va.LoadData(vs);
      except
        FreeAndNil(va);
      end;
      if Assigned(va) then
        AddValueChecked(vn, va);
    end else
      raise EConfigException.Create('Cannot Parse Line: '+l);
  until eof;
end;

procedure TkcfConfigFile.SaveToStream(Stream: TStream);
begin
  SaveData(Stream, '');
end;

procedure TkcfSection.SaveData(Stream: TStream; Indent: string);
var
  i: integer;
  s: AnsiString;
begin
  for i:= 0 to FValues.Count - 1 do begin
    s:= Indent + FValues[i] + ' ' + sValueDelim + ' ' + TkcfValue(FValues.Objects[i]).SaveData + sLineBreak;
    Stream.Write(S[1], Length(S) * Sizeof(AnsiChar));
  end;

  for i:= 0 to FSections.Count - 1 do begin
    s:= Indent + FSections[i] + sSectionMarker + sLineBreak;
    Stream.Write(S[1], Length(S) * Sizeof(AnsiChar));
    TkcfSection(FSections.Objects[i]).SaveData(Stream, Indent + #9);
    s:= Indent + sSectionEnd + sLineBreak;
    Stream.Write(S[1], Length(S) * Sizeof(AnsiChar));
  end;
end;

end.

