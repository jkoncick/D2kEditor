unit stringtable;

interface

type
  TStringTableEntry = record
    key: String;
    text: String;
  end;

var
  string_table: array of TStringTableEntry;
  string_table_size: integer;

procedure load_string_table(filename: String);

implementation

uses SysUtils;

procedure load_string_table(filename: String);
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
  string_table_size := buffer[0] + buffer[1] * 256;
  SetLength(string_table, string_table_size);
  pos := 4;
  index := 0;
  while (pos < file_size) and (index < string_table_size) do
  begin
    len := buffer[pos] + buffer[pos+1] * 256;
    SetString(string_table[index].key, PChar(Addr(buffer[pos+2])), len-1);
    pos := pos + len + 2;
    len := buffer[pos] + buffer[pos+1] * 256;
    SetString(string_table[index].text, PChar(Addr(buffer[pos+2])), len-1);
    pos := pos + len + 2;
    inc(index);
  end;
end;

end.
