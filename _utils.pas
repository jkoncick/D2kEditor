unit _utils;

interface

uses
  Controls, Messages, ExtCtrls, SysUtils, _dispatcher;

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
procedure load_binary_file(filename: String; var data; size: integer);
procedure save_binary_file(filename: String; var data; size: integer);

function find_file(name_pattern: String; description: String): String;
function manage_filesave(var filename: string; name_pattern: string; filename_change_event: TDispatcherRegisteredEvent): boolean;
function confirm_overwrite_original_file(actual_filename, orig_filename: string; mods_folder_allowed: boolean): boolean;


var
  current_dir: String;
  performance_frequency: Int64;
  confirm_overwrite_original_file_last_answer: integer;

implementation

uses
  Forms, Windows, StrUtils, Math, _settings, main, _mission, _missionini;

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

function manage_filesave(var filename: string; name_pattern: string; filename_change_event: TDispatcherRegisteredEvent): boolean;
var
  tmp_filename: string;
  tmp_dir: string;
begin
  result := true;
  if (MissionIni.CampaignFolder <> '') and (MissionIni.ModsFolder <> '') then
  begin
    tmp_filename := Settings.GamePath + '\CustomCampaignData\' + MissionIni.CampaignFolder + '\' + MissionIni.ModsFolder + '\' + name_pattern;
    if AnsiCompareText(filename, tmp_filename) <> 0 then
    begin
      Application.MessageBox(PChar(Format('Saved a modified copy of original game file ''%s'' into'#13'''%s''', [filename, tmp_filename])), 'Saving a new copy into CustomCampaignData', MB_ICONINFORMATION or MB_OK);
      filename := tmp_filename;
      Dispatcher.register_event(filename_change_event);
      // Create directories if they don't exist
      tmp_dir := ExtractFileDir(tmp_filename);
      if not DirectoryExists(tmp_dir) then
        ForceDirectories(tmp_dir);
    end;
  end;
  if not confirm_overwrite_original_file(filename, Settings.GamePath + '\' + name_pattern, Mission.mis_assigned) then
    result := false;
end;

function confirm_overwrite_original_file(actual_filename, orig_filename: string; mods_folder_allowed: boolean): boolean;
var
  modified_date: TDateTime;
  year, month, day: word;
begin
  result := true;
  if AnsiCompareText(actual_filename, orig_filename) <> 0 then
    exit;
  if not FileExists(actual_filename) then
    exit;
  modified_date := FileDateToDateTime(FileAge(actual_filename));
  DecodeDate(modified_date, year, month, day);
  if year <> 1998 then
    exit;
  if confirm_overwrite_original_file_last_answer = 0 then
    confirm_overwrite_original_file_last_answer := Application.MessageBox(
      PChar(
      'You are going to modify an original Dune 2000 game file, which can break your game!'#13+
      IfThen(mods_folder_allowed, 'If you want to make modifications, it is recommended to save the file under CustomCampaignData folder. Go to current mission settings and configure Campaign and Mods folder, then a copy will be created there automatically upon saving.'#13, '') +
      #13+
      'Do you '+IfThen(mods_folder_allowed, 'still', 'really')+' want to overwrite the '+IfThen(mods_folder_allowed, 'original ', '')+'file '''+actual_filename+'''?'#13+
      'If yes, make sure you have a backup!'),
      'Overwrite original game file warning', MB_YESNO or MB_ICONWARNING);
  result := confirm_overwrite_original_file_last_answer = IDYES;
end;

end.

