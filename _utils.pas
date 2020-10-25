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

type
  TImage = class(ExtCtrls.TImage)
    protected
      procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  end;

procedure store_c_string(source: String; target_ptr: TByteArrayPtr; target_size: integer);
function IntToBin(val: integer; width: integer): string;
function BinToInt(bin: string): integer;
function find_file(pattern: String): String;
procedure load_binary_file(filename: String; var data; size: integer);
procedure save_binary_file(filename: String; var data; size: integer);

var
  current_dir: String;

implementation

uses
  Forms, Windows, _settings, main, mission_dialog;

procedure TImage.CMMouseLeave(var Message: TMessage);
begin
  MainWindow.ImageMouseLeave(self);
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

function find_file(pattern: String): String;
var
  tmp_filename, tmp_filename2: String;
begin
  // 1st try: editor's folder
  tmp_filename := current_dir + pattern;
  // 2nd try: game's folder
  tmp_filename2 := Settings.GamePath + '\' + pattern;
  if FileExists(tmp_filename2) then
    tmp_filename := tmp_filename2;
  // 3rd try: CustomCampaignData folder
  tmp_filename2 := Settings.GamePath + '\CustomCampaignData\' + MissionDialog.cbCampaignFolder.Text + '\' + MissionDialog.cbModsFolder.Text + '\' + pattern;
  if FileExists(tmp_filename2) then
    tmp_filename := tmp_filename2;
  // Check if file exists
  if not FileExists(tmp_filename) then
  begin
    Application.MessageBox(PChar('Could not find file ' + pattern), 'Error loading configuration or graphics file', MB_OK or MB_ICONERROR);
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
 