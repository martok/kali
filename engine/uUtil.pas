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

  TFPoint = record
    X, Y: Double;
  end;

  TPolygon = array of TPoint;

var
  PerformanceFrequency: Int64;

procedure CenterRectInRect(Outer: TRect; var Inner: TRect);
function Timer(const WaitTime: Double; Periodic: boolean = false): TTimer;
function TryTrunc(x: Extended; out Val: Int64): boolean;
function VarCompress(const Val: Variant; SinglePrecision: boolean): Variant;

function Polygon(Points: array of TPoint): TPolygon;
function RectToPolygon(Rect: TRect): TPolygon;
function BoundingRectangle(Poly: TPolygon): TRect;
function PolyPolyIntersect(A, B: TPolygon): Boolean;
procedure OffsetPolygon(var Poly: TPolygon; dx, dy: integer);

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

function Polygon(Points: array of TPoint): TPolygon;
var
  i: integer;
begin
  SetLength(Result, Length(Points));
  for i:= 0 to high(Points) do
    Result[i]:= Points[i];
end;

function RectToPolygon(Rect: TRect): TPolygon;
begin
  SetLength(Result, 4);
  Result[0]:= Point(Rect.Left, Rect.Top);
  Result[1]:= Point(Rect.Right, Rect.Top);
  Result[2]:= Point(Rect.Right, Rect.Bottom);
  Result[3]:= Point(Rect.Left, Rect.Bottom);
end;

function BoundingRectangle(Poly: TPolygon): TRect;
var
  i: integer;
begin
  Result.Top:= MaxInt;
  Result.Left:= MaxInt;
  Result.Bottom:= 1 - MaxInt;
  Result.Right:= 1 - MaxInt;
  for i:= 0 to high(Poly) do begin
    Result.Left:= min(Result.Left, Poly[i].X);
    Result.Right:= max(Result.Right, Poly[i].X);
    Result.Top:= min(Result.Top, Poly[i].Y);
    Result.Bottom:= max(Result.Bottom, Poly[i].Y);
  end;
end;

function PolyPolyIntersect(A, B: TPolygon): Boolean;
var
  i, j, k: integer;
  ab, av, bb, bv: TFPoint;
  d, e: double;
begin
  Result:= false;
  for i:= 0 to high(A) do begin
    ab.X:= A[i].X;
    ab.Y:= A[i].Y;
    k:= (i + 1) mod Length(A);
    av.X:= A[k].X - ab.x;
    av.Y:= A[k].Y - ab.Y;
    for j:= 0 to high(B) do begin
      bb.X:= B[j].X;
      bb.Y:= B[j].Y;
      k:= (j + 1) mod Length(B);
      bv.X:= B[k].X - bb.x;
      bv.Y:= B[k].Y - bb.Y;

      e:= (av.y * bv.x - av.x * bv.y);
      if not IsZero(e) then begin
        d:= (-ab.y * bv.x + bb.y * bv.x + (ab.x - bb.x) * bv.y) / e;
        if (d >= 0) and (d <= 1) then begin
          e:= (av.x * bv.y - av.y * bv.x);
          if not IsZero(e) then begin
            d:= (ab.y * av.x - ab.x * av.y + av.Y * bb.x - av.X * bb.y) / e;
            if (d >= 0) and (d <= 1) then begin
              Result:= true;
              exit;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure OffsetPolygon(var Poly: TPolygon; dx, dy: integer);
var
  i: integer;
begin
  for i:= 0 to high(Poly) do begin
    inc(Poly[i].X, dx);
    inc(Poly[i].Y, dy);
  end;
end;

initialization
  QueryPerformanceFrequency(PerformanceFrequency);
end.

