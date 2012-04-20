{
Kali Engine (C) Martok
https://github.com/martok/kali/
-----------------------------------------------------------------------
For copyright information see file license.txt.
}

unit uUtil;

interface

uses Types;

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

