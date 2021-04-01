unit _stringtable;

interface

uses
  ValEdit, IniFiles, Classes, Grids;

type
  TStringTable = class

  public
    text_uib_filename: string;
    samples_uib_filename: string;

    samples_uib_modified: boolean;

    text_uib: THashedStringList;
    samples_uib: TStringList;

  public
    procedure init;
    procedure load_text_uib(text_uib_name: string);
    procedure load_samples_uib(force: boolean);
    procedure save_samples_uib;
  private
    procedure load_uib_file(filename: String; storage: TStringList);
    procedure save_uib_file(filename: String; storage: TStringList);

  public
    function get_text(index: integer; accept_custom: boolean; var is_custom: boolean): String;
    function find_samples_uib_value(value: string; hint_index: integer): integer;
    function add_samples_uib_entry(key, value: string): integer;
    procedure store_samples_uib_data_from_stringgrid(sg: TStringGrid);
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
  load_samples_uib(false);
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

procedure TStringTable.load_samples_uib(force: boolean);
var
  tmp_filename: String;
begin
  // Find SAMPLES.UIB file
  tmp_filename := find_file('Data\UI_DATA\samples.uib', 'sound string table');
  if (tmp_filename = '') or ((tmp_filename = samples_uib_filename) and not force) then
    exit;
  samples_uib_filename := tmp_filename;
  // Load SAMPLES.UIB file
  load_uib_file(tmp_filename, samples_uib);
  samples_uib_modified := false;
  // Register event in dispatcher
  Dispatcher.register_event(evFLSamplesUib);
end;

procedure TStringTable.save_samples_uib;
begin
  if (samples_uib_filename = '') or not samples_uib_modified then
    exit;
  if not manage_filesave(samples_uib_filename, 'Data\UI_DATA\samples.uib', evStructuresFilenameChange) then
    exit;
  save_uib_file(samples_uib_filename, samples_uib);
  samples_uib_modified := false;
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

procedure TStringTable.save_uib_file(filename: String; storage: TStringList);
var
  file_size: integer;
  i: integer;
  buffer: array of byte;
  pos, len: integer;
begin
  // Compute final file size first
  file_size := 4;
  for i := 0 to storage.Count - 1 do
    inc(file_size, 6 + Length(storage.Names[i]) + Length(storage.ValueFromIndex[i]));
  // Fill buffer with data
  SetLength(buffer, file_size);
  set_integer_value(buffer, 0, 4, storage.Count);
  pos := 4;
  for i := 0 to storage.Count - 1 do
  begin
    len := Length(storage.Names[i]);
    set_integer_value(buffer, pos, 2, len + 1);
    store_c_string(storage.Names[i], Addr(buffer[pos+2]), len + 1);
    inc(pos, len + 3);
    len := Length(storage.ValueFromIndex[i]);
    set_integer_value(buffer, pos, 2, len + 1);
    store_c_string(storage.ValueFromIndex[i], Addr(buffer[pos+2]), len + 1);
    inc(pos, len + 3);
  end;
  // Save buffer into file
  save_binary_file(filename, buffer[0], file_size);
  SetLength(buffer, 0);
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

function TStringTable.find_samples_uib_value(value: string; hint_index: integer): integer;
var
  i: integer;
begin
  result := -1;
  // First try to find at given hint index
  if (hint_index < samples_uib.Count) and (samples_uib.ValueFromIndex[hint_index] = value) then
  begin
    result := hint_index;
    exit;
  end;
  // Then search through all values
  for i := 0 to samples_uib.Count - 1 do
    if samples_uib.ValueFromIndex[i] = value then
    begin
      result := i;
      break;
    end;
end;

function TStringTable.add_samples_uib_entry(key, value: string): integer;
begin
  result := samples_uib.Count;
  samples_uib.Add(Format('%s=%s', [key, value]));
  samples_uib_modified := true;
  Dispatcher.register_event(evSamplesUibModify);
end;

procedure TStringTable.store_samples_uib_data_from_stringgrid(sg: TStringGrid);
var
  i: integer;
begin
  samples_uib.Clear;
  for i := 0 to sg.RowCount - 2 do
    samples_uib.Add(Format('%s=%s', [sg.Cells[1, i + 1], sg.Cells[2, i + 1]]));
  samples_uib_modified := true;
  Dispatcher.register_event(evSamplesUibModify);
end;

end.
