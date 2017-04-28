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
  InFS: array[0..15] of TFileStream;
  InWord: array[0..15] of Word;
  OutWord: Word;
begin
  OutFS := TFileStream.Create(ParamStr(1), fmCreate);
  InCnt := ParamCount - 1;
  for i := 0 to InCnt - 1 do
    InFS[i] := TFileStream.Create(ParamStr(i + 2), fmOpenRead);

  while InFS[0].Position < InFS[0].Size do
  begin
    for i := 0 to InCnt - 1 do
      InWord[i] := InFS[i].ReadWord;

    for j := 0 to High(InWord) do
    begin
      OutWord := 0;

      for i := 0 to InCnt - 1 do
      begin
        OutWord := (OutWord shl 1) or (InWord[i] and 1);
        InWord[i] := InWord[i] shr 1;
      end;

      OutWord := SwapEndian(OutWord);

      OutFS.WriteWord(OutWord);
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

