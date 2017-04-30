program Bits2Byte;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp
  { you can add units after this };

type

  { TBits2Byte }

  TBits2Byte = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

{ TBits2Byte }

procedure TBits2Byte.DoRun;
var
  InCnt, i, j: Integer;
  OutFS: TFileStream;
  InFS: array[0..7] of TFileStream;
  InByte: array[0..7] of Byte;
  OutByte: Byte;
  Interlace: Boolean;
begin
  OutFS := TFileStream.Create(ParamStr(1), fmCreate);
  InCnt := ParamCount - 1;
  for i := 0 to InCnt - 1 do
    InFS[i] := TFileStream.Create(ParamStr(i + 2), fmOpenRead);

  while InFS[0].Position < InFS[0].Size do
  begin
    for i := 0 to InCnt - 1 do
      InByte[i] := InFS[i].ReadByte;

    for j := 0 to 7 do
    begin
      OutByte := 0;

      for i := 0 to InCnt - 1 do
      begin
        OutByte := (OutByte shl 1) or (InByte[i] and 1);
        InByte[i] := InByte[i] shr 1;
      end;

      OutFS.WriteByte(OutByte);
    end;
  end;

  OutFS.Free;
  for i := 0 to InCnt - 1 do
    InFS[i].Free;


  // stop program loop
  Terminate;
end;

constructor TBits2Byte.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException := True;
end;

destructor TBits2Byte.Destroy;
begin
  inherited Destroy;
end;

var
  Application: TBits2Byte;
begin
  Application := TBits2Byte.Create(nil);
  Application.Run;
  Application.Free;
end.

