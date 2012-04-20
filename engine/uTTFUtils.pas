{
Kali Engine (C) Martok
https://github.com/martok/kali/
-----------------------------------------------------------------------
For copyright information see file license.txt.
}

{
uTTFUtils is a stripped down version of TextSuite's TextSuiteTTFUtils,
originally licensed as:
TextSuite Copyright (C) 2007-2009 Steffen Xonna (aka Lossy eX)
http://www.opengl24.de/

Original license:

This software is provided 'as-is', without any express or implied warranty. In
no event will the authors be held liable for any damages arising from the use
of this software.

Permission is granted to anyone to use this software for any purpose, including
commercial applications, and to alter it and redistribute it freely, subject to
the following restrictions:

1. The origin of this software must not be misrepresented; you must not claim
   that you wrote the original software. If you use this software in a product,
   an acknowledgment in the product documentation would be appreciated but is
   not required.
2. Altered source versions must be plainly marked as such, and must not be
   misrepresented as being the original software.
3. This notice may not be removed or altered from any source distribution.

}

unit uTTFUtils;

interface

uses Classes;

const
  NAME_ID_COPYRIGHT  = 0;
  NAME_ID_FACE_NAME  = 1;
  NAME_ID_STYLE_NAME = 2;
  NAME_ID_FULL_NAME  = 4;



  function MakeTTTableName(ch1, ch2, ch3, ch4: Char): Cardinal;
  function GetTTString(pBuffer: Pointer; BufferSize: Integer; NameID, LanguageID: Cardinal; var Text: AnsiString): Boolean;

  function GetTTFontFullNameFromStream(Stream: TStream; LanguageID: Cardinal): AnsiString;
  function GetTTFontFullNameFromFile(Filename: AnsiString; LanguageID: Cardinal): AnsiString;

  function GetCurrentLocale: Cardinal;

implementation


uses
  SysUtils, Windows;


function SWAPWORD(x: Word): Word;
asm
  mov dl, al
  mov al, ah
  mov ah, dl
end;


function SWAPLONG(x: Cardinal): Cardinal;
asm
  mov dx, ax
  shr eax, 16
  mov cx, ax
  mov al, dh
  mov ah, dl
  shl eax, 16
  mov al, ch
  mov ah, cl
end;


function MakeTTTableName(ch1, ch2, ch3, ch4: Char): Cardinal;
begin
  Result := ord(ch4) shl 24 or ord(ch3) shl 16 or ord(ch2) shl 8 or ord(ch1); 
end;


type
  TT_OFFSET_TABLE = packed record
  	uMajorVersion: Word;
  	uMinorVersion: Word;
	  uNumOfTables: Word;
  	uSearchRange: Word;
  	uEntrySelector: Word;
	  uRangeShift: Word;
  end;


  TT_TABLE_DIRECTORY = packed record
  	TableName: Cardinal;     // table name
  	uCheckSum: Cardinal;  // Check sum
	  uOffset: Cardinal;    // Offset from beginning of file
	  uLength: Cardinal;    // length of the table in bytes
  end;


  TT_NAME_TABLE_HEADER = packed record
  	uFSelector: Word;     //format selector. Always 0
	  uNRCount: Word;       //Name Records count
	  uStorageOffset: Word; //Offset for strings storage, from start of the table
  end;

  TT_NAME_RECORD = packed record
  	uPlatformID: Word;
	  uEncodingID: Word;
	  uLanguageID: Word;
	  uNameID: Word;
	  uStringLength: Word;
	  uStringOffset: Word;  //from start of storage area
  end;


const
  PLATFORM_ID_APPLE_UNICODE = 0;
  PLATFORM_ID_MACINTOSH     = 1;
  PLATFORM_ID_MICROSOFT     = 3;


function GetTTTableData(Stream: TStream; TableName: Cardinal; pBuff: Pointer; var Size: Integer): Boolean;
var
  Pos: Int64;
  OffsetTable: TT_OFFSET_TABLE;
  TableDir: TT_TABLE_DIRECTORY;
  Idx: Integer;
begin
  Result := False;

  Pos := Stream.Position;

  // Reading table header
  Stream.Read(OffsetTable, sizeof(TT_OFFSET_TABLE));
  OffsetTable.uNumOfTables := SWAPWORD(OffsetTable.uNumOfTables);
  OffsetTable.uMajorVersion := SWAPWORD(OffsetTable.uMajorVersion);
  OffsetTable.uMinorVersion := SWAPWORD(OffsetTable.uMinorVersion);

  //check is this is a true type font and the version is 1.0
  if (OffsetTable.uMajorVersion <> 1) or (OffsetTable.uMinorVersion <> 0) then
    Exit;

  // seaching table with name
  for Idx := 0 to OffsetTable.uNumOfTables -1 do begin
    Stream.Read(TableDir, sizeof(TT_TABLE_DIRECTORY));

    if (TableName = TableDir.TableName) then begin
      TableDir.uOffset := SWAPLONG(TableDir.uOffset);
      TableDir.uLength := SWAPLONG(TableDir.uLength);

      // copying tabledata
      if (pBuff <> nil) and (Size >= Integer(TableDir.uLength)) then begin
        Stream.Seek(TableDir.uOffset, soBeginning);
        Size := Stream.Read(pBuff^, TableDir.uLength);

        Result := Size = Integer(TableDir.uLength);
      end else

      begin
        // restoring streamposition
        Stream.Position := Pos;

        Size := TableDir.uLength;
        Result := True;
      end;

      break;
    end;
  end;
end;


function GetTTString(pBuffer: Pointer; BufferSize: Integer; NameID, LanguageID: Cardinal; var Text: AnsiString): Boolean;
var
  pActBuffer: pByte; 
  ttNTHeader: TT_NAME_TABLE_HEADER;
  ttRecord: TT_NAME_RECORD;
  Idx: Integer;
  Prio: Integer;

  procedure ExtractName;
  var
    pTempBuffer: pByte;
    pTemp: pWideChar;
    uStringLengthH2: Word;

    procedure SwapText(pText: pWideChar; Length: Word);
    begin
      while Length > 0 do begin
        pWord(pText)^ := SWAPWORD(pWord(pText)^);
        Inc(pText);
        Dec(Length);
      end;
    end;

  begin
    Result := True;

    ttRecord.uStringLength := SWAPWORD(ttRecord.uStringLength);
    ttRecord.uStringOffset := SWAPWORD(ttRecord.uStringOffset);

    uStringLengthH2 := ttRecord.uStringLength shr 1;

    pTempBuffer := pBuffer;
    Inc(pTempBuffer, ttNTHeader.uStorageOffset + ttRecord.uStringOffset);

    // Unicode
    if ((ttRecord.uPlatformID = PLATFORM_ID_MICROSOFT) and (ttRecord.uEncodingID in [0, 1])) or
       ((ttRecord.uPlatformID = PLATFORM_ID_APPLE_UNICODE) and (ttRecord.uEncodingID > 0)) then begin
      GetMem(pTemp, (uStringLengthH2+1) shl 1);
      try
        // uStringLengthH2 * 2 because possible buffer overrun
        Move(pTempBuffer^, pTemp^, uStringLengthH2 * 2);

        SwapText(pTemp, uStringLengthH2);

        WideCharLenToStrVar(pTemp, uStringLengthH2, Text);
      finally
        FreeMem(pTemp);
      end;
    end else

    // none unicode
    begin
      SetLength(Text, ttRecord.uStringLength);
      Move(pTempBuffer^, Text[1], ttRecord.uStringLength);
    end;
  end;

begin
  Result := False;

  pActBuffer := pBuffer;

  Move(pActBuffer^, ttNTHeader, sizeof(TT_NAME_TABLE_HEADER));
  inc(pActBuffer, sizeof(TT_NAME_TABLE_HEADER));

  ttNTHeader.uNRCount := SWAPWORD(ttNTHeader.uNRCount);
  ttNTHeader.uStorageOffset := SWAPWORD(ttNTHeader.uStorageOffset);

  Prio := -1;

  for Idx := 0 to ttNTHeader.uNRCount -1 do begin
    Move(pActBuffer^, ttRecord, sizeof(TT_NAME_RECORD));
    Inc(pActBuffer, sizeof(TT_NAME_RECORD));

    ttRecord.uNameID := SWAPWORD(ttRecord.uNameID);

    if ttRecord.uNameID = NameID then begin
      ttRecord.uPlatformID := SWAPWORD(ttRecord.uPlatformID);
      ttRecord.uEncodingID := SWAPWORD(ttRecord.uEncodingID);
      ttRecord.uLanguageID := SWAPWORD(ttRecord.uLanguageID);

      // highest priority
      if (ttRecord.uPlatformID = PLATFORM_ID_MICROSOFT) then begin
        // system language
        if (ttRecord.uLanguageID = languageID) then begin
          if Prio <= 7 then begin
            ExtractName;

            Prio := 7;
          end;
        end else

        // english
        if (ttRecord.uLanguageID = 1033) then begin
          if Prio <= 6 then begin
            ExtractName;

            Prio := 6;
          end;
        end else

        // all else
        if Prio <= 5 then begin
          ExtractName;

          Prio := 5;
        end;
      end else

      // apple unicode
      if (ttRecord.uPlatformID = PLATFORM_ID_APPLE_UNICODE) then begin
        ExtractName;

        Prio := 4;
      end else

      // macintosh
      if (ttRecord.uPlatformID = PLATFORM_ID_MACINTOSH) then begin
        // english
        if (ttRecord.uLanguageID = 0) then begin
          if Prio <= 3 then begin
            ExtractName;

            Prio := 3;
          end;
        end else

        // all other
        begin
          ExtractName;

          Prio := 2;
        end;
      end else

      begin
        if Prio <= 1 then begin
          ExtractName;

          Prio := 1;
        end;
      end;
    end;
  end;
end;

function GetTTFontFullNameFromStream(Stream: TStream; LanguageID: Cardinal): AnsiString;
var
  TableName: Cardinal;
  Buffer: Pointer;
  BufferSize: Integer;
begin
  TableName := MakeTTTableName('n', 'a', 'm', 'e');

  if GetTTTableData(Stream, TableName, nil, BufferSize) then begin
    GetMem(Buffer, BufferSize);
    try
      if GetTTTableData(Stream, TableName, Buffer, BufferSize) then begin
        if not GetTTString(Buffer, BufferSize, NAME_ID_FULL_NAME, LanguageID, Result) then
          if not GetTTString(Buffer, BufferSize, NAME_ID_FACE_NAME, LanguageID, Result) then
            Result := '';
      end;
    finally
      FreeMem(Buffer);
    end;
  end;
end;

function GetTTFontFullNameFromFile(Filename: AnsiString; LanguageID: Cardinal): AnsiString;
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(String(Filename), fmOpenRead or fmShareDenyWrite);
  try
    GetTTFontFullNameFromStream(fs, LanguageID);
  finally
    fs.Free;
  end;
end;

function GetCurrentLocale: Cardinal;
var
  Lang: AnsiString;
begin
  SetLength(Lang, 4);
  GetLocaleInfoA(LOCALE_USER_DEFAULT, LOCALE_ILANGUAGE, @Lang[1], 4);
  Result:= StrToInt('$' + String(Lang));
end;

end.
