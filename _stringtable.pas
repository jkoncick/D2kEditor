unit _stringtable;

interface

uses
  ValEdit, IniFiles, Classes;

type
  TStringTableEntry = record
    key: String;
    text: String;
  end;

type
  TStringTable = class

  private
    entries: array of TStringTableEntry;
    num_entries: integer;
    custom_text_value_list: TValueListEditor;

  public
    procedure init_value_list(value_list: TValueListEditor);
    procedure load_from_file(filename: String);
    function get_table_size: integer;
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
  SoundStringTable: TStringTable;

implementation

uses SysUtils;

procedure TStringTable.init_value_list(value_list: TValueListEditor);
begin
  custom_text_value_list := value_list;
end;

procedure TStringTable.load_from_file(filename: String);
var
  string_table_file: file of byte;
  buffer: array of byte;
  file_size: integer;
  pos: integer;
  len: integer;
  index: integer;
begin
  if not FileExists(filename) then
    exit;
  AssignFile(string_table_file, filename);
  Reset(string_table_file);
  file_size := FileSize(string_table_file);
  SetLength(buffer, file_size);
  BlockRead(string_table_file, buffer[0], file_size);
  CloseFile(string_table_file);
  num_entries := buffer[0] + buffer[1] * 256;
  SetLength(entries, num_entries);
  pos := 4;
  index := 0;
  while (pos < file_size) and (index < num_entries) do
  begin
    len := buffer[pos] + buffer[pos+1] * 256;
    SetString(entries[index].key, PChar(Addr(buffer[pos+2])), len-1);
    pos := pos + len + 2;
    len := buffer[pos] + buffer[pos+1] * 256;
    SetString(entries[index].text, PChar(Addr(buffer[pos+2])), len-1);
    pos := pos + len + 2;
    inc(index);
  end;
end;

function TStringTable.get_table_size: integer;
begin
  result := num_entries;
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
  if (index >= num_entries) or (index < 0) then
    result := '(undefined)'
  else
    result := entries[index].text;
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
