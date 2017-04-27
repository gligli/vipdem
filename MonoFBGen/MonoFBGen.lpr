program MonoFBGen;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp
  { you can add units after this };

type

  { TMonoFBGen }

  TMonoFBGen = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

const
  cTileCount = 256;
  cTileWidth = 8;

{ TMonoFBGen }

procedure TMonoFBGen.DoRun;
var
  tile, y, x, idx: Integer;
  tilesPlanes: array[0..cTileWidth - 1, 0..3] of Byte;
  pixels: array[0..cTileCount-1, 0..cTileWidth*cTileWidth-1] of Byte;
  fs: TFileStream;
begin
  for tile := 0 to cTileCount - 1 do
  begin
    for y := 0 to cTileWidth - 1 do
      for x := 0 to cTileWidth - 1 do
      begin
        case y of
          0, 4: pixels[tile, cTileWidth * y + x] := (tile shr (x shr 1)) and 1;
          1, 5: pixels[tile, cTileWidth * y + x] := ((tile shr (x shr 1)) and 1) + 2;
          2, 6: pixels[tile, cTileWidth * y + x] := (tile shr (7 - (x shr 1))) and 1;
          3, 7: pixels[tile, cTileWidth * y + x] := ((tile shr (7 - (x shr 1))) and 1) + 2;
        end;
      end;
  end;

  fs := TFileStream.Create(ParamStr(1), fmCreate);
  try
    for tile := 0 to cTileCount - 1 do
    begin
      FillChar(tilesPlanes, Sizeof(tilesPlanes), 0);
      for y := 0 to cTileWidth - 1 do
      begin
        for x := 0 to cTileWidth - 1 do
        begin
          idx := pixels[tile, cTileWidth * y + x];
          tilesPlanes[y, 0] := tilesPlanes[y, 0] or (((idx and 1) shl 7) shr x);
          tilesPlanes[y, 1] := tilesPlanes[y, 1] or (((idx and 2) shl 6) shr x);
          tilesPlanes[y, 2] := tilesPlanes[y, 2] or (((idx and 4) shl 5) shr x);
          tilesPlanes[y, 3] := tilesPlanes[y, 3] or (((idx and 8) shl 4) shr x);
        end;
        for x := 0 to 3 do
          fs.WriteByte(tilesPlanes[y, x]);
      end;
    end;
  finally
    fs.Free;
  end;

  // stop program loop
  Terminate;
end;

constructor TMonoFBGen.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException := True;
end;

destructor TMonoFBGen.Destroy;
begin
  inherited Destroy;
end;

var
  Application: TMonoFBGen;
begin
  Application := TMonoFBGen.Create(nil);
  Application.Run;
  Application.Free;
end.

