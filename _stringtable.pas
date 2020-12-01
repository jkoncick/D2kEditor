unit _stringtable;

interface

uses
  ValEdit, IniFiles, Classes;

type
  TStringTable = class

  public
    text_uib_filename: string;
    samples_uib_filename: string;

    text_uib: THashedStringList;
    samples_uib: TStringList;

  public
    procedure init;
    procedure load_text_uib(text_uib_name: string);
    procedure load_samples_uib;
  private
    procedure load_uib_file(filename: String; storage: TStringList);

  public
    function get_text(index: integer; accept_custom: boolean; var is_custom: boolean): String;
  end;

var
  StringTable: TStringTable;

implementation

uses SysUtils, _missionini, _utils, _dispatcher;

procedure TStringTable.init;
begin
  text_uib := THashedStringList.Create;
  samples_uib := TStringList.Create;
  load_text_uib('');
  load_samples_uib;
end;

procedure TStringTable.load_text_uib(text_uib_name: string);
var
  tmp_filename: String;
begin
  // Find TEXT.UIB file
  if text_uib_name = '' then
    text_uib_name := 'text.uib';
  tmp_filename := find_file('Data\UI_DATA\' + text_uib_name, '');
  if tmp_filename = '' then
    tmp_filename := find_file('Data\UI_DATA\text.uib', 'string table');
  if (tmp_filename = '') or (tmp_filename = text_uib_filename) then
    exit;
  text_uib_filename := tmp_filename;
  // Load TEXT.UIB file
  load_uib_file(tmp_filename, text_uib);
  // Register event in dispatcher
  Dispatcher.register_event(evFLTextUib);
end;

procedure TStringTable.load_samples_uib;
var
  tmp_filename: String;
begin
  // Find SAMPLES.UIB file
  tmp_filename := find_file('Data\UI_DATA\samples.uib', 'sound string table');
  if (tmp_filename = '') or (tmp_filename = samples_uib_filename) then
    exit;
  samples_uib_filename := tmp_filename;
  // Load SAMPLES.UIB file
  load_uib_file(tmp_filename, samples_uib);
  // Register event in dispatcher
  Dispatcher.register_event(evFLSamplesUib);
end;

procedure TStringTable.load_uib_file(filename: String; storage: TStringList);
var
  uib_file: file of byte;
  buffer: array of byte;
  file_size: integer;
  num_entries: integer;
  pos: integer;
  len: integer;
  index: integer;
  key, value: string;
begin
  AssignFile(uib_file, filename);
  Reset(uib_file);
  file_size := FileSize(uib_file);
  SetLength(buffer, file_size);
  BlockRead(uib_file, buffer[0], file_size);
  CloseFile(uib_file);
  storage.Clear;
  num_entries := buffer[0] + buffer[1] * 256;
  pos := 4;
  index := 0;
  while (pos < file_size) and (index < num_entries) do
  begin
    len := buffer[pos] + buffer[pos+1] * 256;
    SetString(key, PChar(Addr(buffer[pos+2])), len-1);
    pos := pos + len + 2;
    len := buffer[pos] + buffer[pos+1] * 256;
    SetString(value, PChar(Addr(buffer[pos+2])), len-1);
    pos := pos + len + 2;
    inc(index);
    storage.Add(Format('%s=%s', [key, value]));
  end;
end;

function TStringTable.get_text(index: integer; accept_custom: boolean; var is_custom: boolean): String;
begin
  is_custom := false;
  if accept_custom and MissionIni.get_custom_text(index, result) then
    is_custom := true
  else if (index >= text_uib.Count) or (index < 0) then
    result := '(undefined)'
  else
    result := text_uib.ValueFromIndex[index];
end;

end.
