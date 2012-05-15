{
Kali Engine (C) Martok
https://github.com/martok/kali/
-----------------------------------------------------------------------
For copyright information see file license.txt.
}

unit uUtil;

interface

uses Types, Variants;

type
  PrecisionTime = Int64;
  TTimer = object
    Total, Current: Double;
    Periodic: boolean;
    function Step(DT: single): integer;
  end;

var
  PerformanceFrequency: Int64;

procedure CenterRectInRect(Outer: TRect; var Inner: TRect);
function Timer(const WaitTime: Double; Periodic: boolean = false): TTimer;
function TryTrunc(x: Extended; out Val: Int64): boolean;
function VarCompress(const Val: Variant; SinglePrecision: boolean): Variant;

implementation

uses Windows, Math;

procedure CenterRectInRect(Outer: TRect; var Inner: TRect);
var
  iw, ih: integer;
begin
  iw:= Inner.Right - Inner.Left;
  ih:= Inner.Bottom - Inner.Top;
  Inner:= Bounds((Outer.Right + Outer.Left - iw) div 2,
    (Outer.Bottom + Outer.Top - ih) div 2,
    iw, ih);
end;

function Timer(const WaitTime: Double; Periodic: boolean): TTimer;
begin
  Result.Total:= WaitTime;
  Result.Current:= 0;
  Result.Periodic:= Periodic;
end;

function TryTrunc(x: Extended; out Val: Int64): boolean;
var
  oldcw, cw, sw: word;
asm
    fld      x
    fwait

    fnstcw   oldcw
    fnstcw   cw
    fwait
    or       cw, $0f00             // towards zero, full precision
    fldcw    cw
    mov      eax, dword ptr val
    fistp    qword ptr [eax]

    fnstsw   sw
    mov      ax, sw
    not      ax
    and      ax, 1                 // IE = 1
    mov      Result, al
    test     al,al
    jnz      @noexception
    fnclex                         // if IE, eat exception
@noexception:
    fwait                          // raise all others

    FLDCW    oldcw
end;

function VarCompress(const Val: Variant; SinglePrecision: boolean): Variant;
const
  eps: array[Boolean] of Extended = (1E-15 * 1000, 1E-7 * 1000);
var
  i: int64;
begin
  Result:= Val;
  //only numbers.
  if not VarIsNumeric(Result) then
    exit;
  if VarIsType(Result, [varDouble, varSingle, varCurrency]) and
    (abs(Frac(Result)) < eps[SinglePrecision]) then begin
    // representable as fixed, is it in range for anything?
    if TryTrunc(Result, i) then
      Result:= i;
  end;
  // now it's either fixed or not representable as such
  case VarType(Result) of
    varDouble: if SinglePrecision then
        Result:= VarAsType(Result, varSingle);
    varCurrency,
      varSingle: ;
  else begin
      // find smallest possible representation according to signedness
      if Result < 0 then begin
        if Result >= Low(ShortInt) then
          Result:= VarAsType(Result, varShortInt)
        else if Result >= Low(SmallInt) then
          Result:= VarAsType(Result, varSmallint)
        else if Result >= Low(Integer) then
          Result:= VarAsType(Result, varInteger);
      end else if Result > 0 then begin
        if Result <= High(Byte) then
          Result:= VarAsType(Result, varByte)
        else if Result <= High(Word) then
          Result:= VarAsType(Result, varWord)
        else if Result <= High(Cardinal) then
          Result:= VarAsType(Result, varLongWord);
      end else
        Result:= VarAsType(0, varByte);
    end
  end;
end;

{ TTimer }

function TTimer.Step(DT: single): integer;
begin
  Current:= Current + DT;
  if IsZero(Total) then
    Result:= -1
  else
    Result:= trunc(Current / Total);
  if Periodic and (Result > 0) then
    Current:= Current - Result * Total;
end;

initialization
  QueryPerformanceFrequency(PerformanceFrequency);
end.

