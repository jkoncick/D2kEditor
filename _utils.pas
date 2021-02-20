unit _utils;

interface

uses
  Controls, Messages, ExtCtrls, SysUtils;

const CNT_PLAYERS = 8;

type
  TByteArray = array[0..0] of byte;
  TByteArrayPtr = ^TByteArray;
  TWordArray = array[0..0] of word;
  TWordArrayPtr = ^TWordArray;
  TCardinalArray = array[0..0] of cardinal;
  TCardinalArrayPtr = ^TCardinalArray;

  TDummyStringRec = record
    str: array[0..49] of char;
  end;

  TDummyStringRecPtr = ^TDummyStringRec;

type
  TImage = class(ExtCtrls.TImage)
    protected
      procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  end;

function get_integer_value(var arr: array of byte; pos, bytes: integer): integer;
function get_float_value  (var arr: array of byte; pos: integer): single;
procedure set_integer_value(var arr: array of byte; pos, bytes: integer; value: Integer);
procedure set_float_value  (var arr: array of byte; pos: integer; value: single);
procedure store_c_string(source: String; target_ptr: TByteArrayPtr; target_size: integer);
function IntToBin(val: integer; width: integer): string;
function BinToInt(bin: string): integer;
function find_file(name_pattern: String; description: String): String;
procedure load_binary_file(filename: String; var data; size: integer);
procedure save_binary_file(filename: String; var data; size: integer);

var
  current_dir: String;
  performance_frequency: Int64;

implementation

uses
  Forms, Windows, _settings, main, _missionini;

procedure TImage.CMMouseLeave(var Message: TMessage);
begin
  MainWindow.ImageMouseLeave(self);
end;

function get_integer_value(var arr: array of byte; pos, bytes: integer): integer;
var
  b: integer;
begin
  result := 0;
  for b := 0 to bytes - 1 do
    result := result + (arr[pos + b] shl (8 * b));
end;

function get_float_value(var arr: array of byte; pos: integer): single;
var
  f_ptr: ^single;
begin
  f_ptr := Addr(arr[pos]);
  result := f_ptr^;
end;

procedure set_integer_value(var arr: array of byte; pos, bytes: integer; value: Integer);
var
  b: integer;
begin
  for b := 0 to bytes - 1 do
    arr[pos + b] := (value shr (8 * b)) and 255;
end;

procedure set_float_value(var arr: array of byte; pos: integer; value: single);
var
  f_ptr: ^single;
begin
  f_ptr := Addr(arr[pos]);
  f_ptr^ := value;
end;

procedure store_c_string(source: String; target_ptr: TByteArrayPtr; target_size: integer);
begin
  FillChar(target_ptr^, target_size, 0);
  Move(source[1], target_ptr^, Length(source));
end;

function IntToBin(val: integer; width: integer): string;
begin
  result := '';
  while val > 0 do
    begin
      result := IntToStr(val and 1) + result;
      val := val shr 1;
    end;
  while Length(result) < width do
    result := '0' + result;
end;

function BinToInt(bin: string): integer;
var
  i: integer;
begin
  result := 0;
  for i := 1 to Length(bin) do
    begin
      result := result shl 1;
      result := result or StrToInt(bin[i]);
    end;
end;

function find_file(name_pattern: String; description: String): String;
var
  tmp_filename, tmp_filename2: String;
begin
  // 1st try: editor's folder
  tmp_filename := current_dir + name_pattern;
  // 2nd try: game's folder
  tmp_filename2 := Settings.GamePath + '\' + name_pattern;
  if FileExists(tmp_filename2) then
    tmp_filename := tmp_filename2;
  // 3rd try: CustomCampaignData folder
  tmp_filename2 := Settings.GamePath + '\CustomCampaignData\' + MissionIni.CampaignFolder + '\' + MissionIni.ModsFolder + '\' + name_pattern;
  if FileExists(tmp_filename2) then
    tmp_filename := tmp_filename2;
  // Check if file exists
  if not FileExists(tmp_filename) then
  begin
    if description <> '' then
      Application.MessageBox(PChar('Could not find file ' + name_pattern), PChar('Error loading ' + description + ' file'), MB_OK or MB_ICONERROR);
    tmp_filename := '';
  end;
  result := tmp_filename;
end;

procedure load_binary_file(filename: String; var data; size: integer);
var
  f: file of byte;
begin
  AssignFile(f, filename);
  Reset(f);
  BlockRead(f, data, size);
  CloseFile(f);
end;

procedure save_binary_file(filename: String; var data; size: integer);
var
  f: file of byte;
begin
  AssignFile(f, filename);
  Rewrite(f);
  BlockWrite(f, data, size);
  CloseFile(f);
end;

end.

