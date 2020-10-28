unit _stringtable;

interface

uses
  ValEdit, IniFiles, Classes;

type
  TStringTable = class

  public
    text_uib_filename: string;
    samples_uib_filename: string;

    text_uib: TStringList;
    samples_uib: TStringList;
    custom_text_value_list: TValueListEditor;

  public
    procedure init;
    procedure init_value_list(value_list: TValueListEditor);
    procedure load_text_uib(text_uib_name: string);
    procedure load_samples_uib;
    procedure load_uib_file(filename: String; storage: TStringList);
    function get_text(index: integer; accept_custom: boolean; var is_custom: boolean): String;
    procedure set_custom_text(index: integer; text: String);
    procedure remove_custom_text(index: integer);
    // Loading and saving custom texts
    procedure load_custom_texts_from_ini(ini: TMemIniFile);
    procedure save_custom_texts_to_ini(ini: TMemIniFile);
    procedure clear_custom_texts;

  end;

var
  StringTable: TStringTable;

implementation

uses SysUtils, _utils, event_dialog, tileatr_editor, structures_editor;

procedure TStringTable.init;
begin
  text_uib := TStringList.Create;
  samples_uib := TStringList.Create;
  load_text_uib('');
  load_samples_uib;
end;

procedure TStringTable.init_value_list(value_list: TValueListEditor);
begin
  custom_text_value_list := value_list;
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
  // Update all occurences
  TileAtrEditor.update_tile_hint_text_list;
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
  // Update all occurences
  EventDialog.update_sound_names;
  StructuresEditor.update_sound_names;
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
var
  row: integer;
begin
  is_custom := false;
  if (custom_text_value_list <> nil) and accept_custom and custom_text_value_list.FindRow(inttostr(index), row) then
  begin
    result := custom_text_value_list.Cells[1,row];
    is_custom := true;
  end else
  if (index >= text_uib.Count) or (index < 0) then
    result := '(undefined)'
  else
    result := text_uib.ValueFromIndex[index];
end;

procedure TStringTable.set_custom_text(index: integer; text: String);
begin
  custom_text_value_list.Values[inttostr(index)] := text;
end;

procedure TStringTable.remove_custom_text(index: integer);
var
  row: integer;
begin
   if custom_text_value_list.FindRow(inttostr(index), row) then
     custom_text_value_list.DeleteRow(row);
end;

procedure TStringTable.load_custom_texts_from_ini(ini: TMemIniFile);
var
  tmp_strings: TStringList;
  i: integer;
begin
  tmp_strings := TStringList.Create;
  ini.ReadSection('Text', tmp_strings);
  for i := 0 to tmp_strings.Count-1 do
  begin
    tmp_strings[i] := tmp_strings[i]+'='+ini.ReadString('Text',tmp_strings[i],'');
  end;
  custom_text_value_list.Strings := tmp_strings;
  tmp_strings.Destroy;
end;

procedure TStringTable.save_custom_texts_to_ini(ini: TMemIniFile);
var
  i: integer;
begin
  ini.EraseSection('Text');
  for i := 0 to custom_text_value_list.Strings.Count - 1 do
    ini.WriteString('Text', custom_text_value_list.Cells[0,i+1], custom_text_value_list.Cells[1,i+1]);
end;

procedure TStringTable.clear_custom_texts;
begin
  custom_text_value_list.Strings.Clear;
end;

end.
