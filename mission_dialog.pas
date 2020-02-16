unit mission_dialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Spin,  Buttons, ComCtrls, Grids,
  ValEdit, IniFiles, Clipbrd, _map, _mission;

const player_names_shorter: array[0..7] of string =
  ('Atreides', 'Harkon.', 'Ordos', 'Emperor', 'Fremen', 'Smuggl.', 'Mercen.', 'Sandworm');

type
  TMisAIProperty = record
    name: String;
    data_type: char;
    position: integer;
  end;

type
  TRuleDefinition = record
    name: String;
    default_value: String;
  end;

type
  TAIClipboard = record
    ai_data: array[0..7607] of byte;
  end;

type
  TMissionDialog = class(TForm)
    lblTechLevel: TLabel;
    lblStartingMoney: TLabel;
    SettingsPanel: TPanel;
    lblAllocIndex: TLabel;
    PlayerSettingsPanel: TPanel;
    RulesAndStringsPanel: TPanel;
    AITabControl: TTabControl;
    lblSetToAll: TLabel;
    seTechLevelAll: TSpinEdit;
    edStartingMoneyAll: TEdit;
    btnAllocIndexReset: TButton;
    btnAllegianceReset: TButton;
    lblTimeLimit: TLabel;
    edTimeLimit: TEdit;
    cbSetBothSides: TCheckBox;
    AIValueList: TValueListEditor;
    AIOptionsPanel: TPanel;
    btnImportAI: TButton;
    btnExportAI: TButton;
    btnCopyAI: TButton;
    StringValueList: TValueListEditor;
    RuleValueList: TValueListEditor;
    cbUseINI: TCheckBox;
    btnRefreshStrings: TButton;
    Bevel1: TBevel;
    btnResetToDefaults: TButton;
    lblMapName: TLabel;
    lblMapAuthor: TLabel;
    lblMapMusic: TLabel;
    edMapName: TEdit;
    edMapAuthor: TEdit;
    btnPasteAI: TButton;
    lblMapSideId: TLabel;
    lblMapMissionNumber: TLabel;
    cbMapSideId: TComboBox;
    seMapMissionNumber: TSpinEdit;
    StringsPanel: TPanel;
    StringsSplitter: TSplitter;
    MapBriefing: TMemo;
    lblMapBriefing: TLabel;
    ExportAIDialog: TSaveDialog;
    ImportAIDialog: TOpenDialog;
    lblTimeLimitHelp: TLabel;
    cbDiffMode: TCheckBox;
    cbMapMusic: TComboBox;
    edTilesetName: TEdit;
    edTileatrName: TEdit;
    lblTilesetName: TLabel;
    lblTileatrName: TLabel;
    lblTextUib: TLabel;
    edTextUib: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure seTechLevelAllChange(Sender: TObject);
    procedure edStartingMoneyAllChange(Sender: TObject);
    procedure btnAllocIndexResetClick(Sender: TObject);
    procedure btnAllegianceResetClick(Sender: TObject);
    procedure tech_level_change(Sender: TObject);
    procedure starting_money_change(Sender: TObject);
    procedure alloc_index_change(Sender: TObject);
    procedure allegiance_btn_click(Sender: TObject);
    procedure time_limit_change(Sender: TObject);
    procedure edTilesetNameChange(Sender: TObject);
    procedure edTileatrNameChange(Sender: TObject);
    procedure AITabControlChange(Sender: TObject);
    procedure AIValueListStringsChange(Sender: TObject);
    procedure cbUseINIClick(Sender: TObject);
    procedure btnResetToDefaultsClick(Sender: TObject);
    procedure btnRefreshStringsClick(Sender: TObject);
    procedure btnExportAIClick(Sender: TObject);
    procedure btnImportAIClick(Sender: TObject);
    procedure btnCopyAIClick(Sender: TObject);
    procedure btnPasteAIClick(Sender: TObject);
    procedure cbDiffModeClick(Sender: TObject);
    procedure cbMapSideIdChange(Sender: TObject);
    procedure seMapMissionNumberChange(Sender: TObject);
  private
    player_label: array[0..cnt_mis_players-1] of TLabel;
    tech_level: array[0..cnt_mis_players-1] of TSpinEdit;
    starting_money: array[0..cnt_mis_players-1] of TEdit;
    alloc_index: array[0..cnt_mis_players-1] of TSpinEdit;
    player_label_alleg: array[0..cnt_mis_players-1] of TLabel;
    allegiance_btn: array[0..cnt_mis_players-1, 0..cnt_mis_players-1] of TBitBtn;
    rule_definitions: array of TRuleDefinition;
    misai_properties: array of TMisAIProperty;
    ai_clipboard_format: cardinal;
  public
    procedure fill_data;
    procedure tileset_changed;

    function get_integer_value(source: array of byte; pos, bytes: integer): integer;
    function get_float_value(source: array of byte; pos: integer): single;
    procedure fill_ai_values;

    function get_ini_filename(map_filename: String): String;
    procedure load_ini_fields;
    procedure empty_ini_fields;
    procedure save_ini_fields(map_filename: String);
  end;

var
  MissionDialog: TMissionDialog;

implementation

uses
  _stringtable, _settings, _tileset, event_dialog, main, tileatr_editor;

{$R *.dfm}

procedure TMissionDialog.FormCreate(Sender: TObject);
var
  i, j: integer;
  ini: TMemIniFile;
  tmp_strings: TStringList;
begin
  StringTable.init_value_list(StringValueList);
  for i := 0 to cnt_mis_players-1 do
  begin
    // Initialize player labels
    player_label[i] := TLabel.Create(self);
    player_label[i].Left := 8;
    player_label[i].Top := 28 + i * 24;
    player_label[i].Caption := player_names[i];
    player_label[i].Parent := PlayerSettingsPanel;
    // Initialize tech levels
    tech_level[i] := TSpinEdit.Create(self);
    tech_level[i].Left := 72;
    tech_level[i].Top := 24 + i * 24;
    tech_level[i].Width := 56;
    tech_level[i].Height := 22;
    tech_level[i].MinValue := 0;
    tech_level[i].MaxValue := 255;
    tech_level[i].Value := 0;
    tech_level[i].Tag := i;
    tech_level[i].Parent := PlayerSettingsPanel;
    tech_level[i].OnChange := tech_level_change;
    // Initialize Starting money
    starting_money[i] := TEdit.Create(self);
    starting_money[i].Left := 136;
    starting_money[i].Top := 24 + i * 24;
    starting_money[i].Width := 56;
    starting_money[i].Height := 22;
    starting_money[i].Text := '0';
    starting_money[i].Tag := i;
    starting_money[i].Parent := PlayerSettingsPanel;
    starting_money[i].OnChange := starting_money_change;
    // Initialize allocation indexes
    alloc_index[i] := TSpinEdit.Create(self);
    alloc_index[i].Left := 200;
    alloc_index[i].Top := 24 + i * 24;
    alloc_index[i].Width := 56;
    alloc_index[i].Height := 22;
    alloc_index[i].MinValue := 0;
    alloc_index[i].MaxValue := 255;
    alloc_index[i].Value := 0;
    alloc_index[i].Tag := i;
    alloc_index[i].Parent := PlayerSettingsPanel;
    alloc_index[i].OnChange := alloc_index_change;
    // Initialize allegiance labels
    player_label_alleg[i] := TLabel.Create(self);
    player_label_alleg[i].Left := 266 + i * 52;
    player_label_alleg[i].Top := 8;
    player_label_alleg[i].Caption := player_names_shorter[i];
    player_label_alleg[i].Parent := PlayerSettingsPanel;
    // Initialize allegiance buttons
    for j := 0 to cnt_mis_players-1 do
    begin
      allegiance_btn[i,j] := TBitBtn.Create(self);
      allegiance_btn[i,j].Left := 264 + j * 52;
      allegiance_btn[i,j].Top := 24 + i * 24;
      allegiance_btn[i,j].Width := 52;
      allegiance_btn[i,j].Height := 22;
      allegiance_btn[i,j].Font.Style := [fsBold];
      allegiance_btn[i,j].Tag := i * 8 + j;
      allegiance_btn[i,j].Parent := PlayerSettingsPanel;
      allegiance_btn[i,j].OnClick := allegiance_btn_click;
    end;
    // Initialize AI PageControl
    AITabControl.Tabs.Add(player_names_short[i]);
    // Initialize Play as list
    cbMapSideId.Items.Add(inttostr(i) + ' - ' + player_names[i]);
  end;
  // Load rule definitions from ini file
  tmp_strings := TStringList.Create;
  ini := TMemIniFile.Create(current_dir + 'config/rules.ini');
  ini.ReadSection('Vars',tmp_strings);
  SetLength(rule_definitions, tmp_strings.Count);
  for i := 0 to tmp_strings.Count - 1 do
  begin
    rule_definitions[i].name := tmp_strings[i];
    rule_definitions[i].default_value := ini.ReadString('Vars',tmp_strings[i],'');
  end;
  ini.Destroy;
  tmp_strings.Clear;
  // Load misai properties from ini file
  ini := TMemIniFile.Create(current_dir + 'config/mis_ai.ini');
  ini.ReadSection('AI',tmp_strings);
  SetLength(misai_properties, tmp_strings.Count);
  for i := 0 to tmp_strings.Count - 1 do
  begin
    misai_properties[i].name := ini.ReadString('AI',tmp_strings[i],'');
    misai_properties[i].data_type := tmp_strings[i][1];
    misai_properties[i].position := strtoint(Copy(tmp_strings[i], 3, Length(tmp_strings[i]) - 2));
  end;
  ini.Destroy;
  tmp_strings.Destroy;
  cbMapMusic.Items := EventDialog.cbMusicName.Items;
  // Register AI clipboard format
  ai_clipboard_format := RegisterClipboardFormat('D2kEditorAISegment');
end;

procedure TMissionDialog.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key = 27 then
    Close;
  if key = 123 then
    MainWindow.Show;
end;

procedure TMissionDialog.fill_data;
var
  i, j: integer;
begin
  for i := 0 to cnt_mis_players-1 do
  begin
    tech_level[i].Value := Mission.mis_data.tech_level[i];
    starting_money[i].Text := inttostr(Mission.mis_data.starting_money[i]);
    alloc_index[i].Value := Mission.mis_data.allocation_index[i];
    for j := 0 to cnt_mis_players-1 do
    begin
      allegiance_btn[i,j].Caption := allegiance_type[Mission.mis_data.allegiance[i,j]];
      allegiance_btn[i,j].Font.Color := allegiance_type_color[Mission.mis_data.allegiance[i,j]];
    end;
  end;
  edTimeLimit.Text := inttostr(Mission.mis_data.time_limit);
  edTilesetName.Text := Mission.mis_data.tileset;
  edTileatrName.Text := Mission.mis_data.tileatr;
  fill_ai_values;
  cbUseINI.Tag := 1;
  if FileExists(get_ini_filename(Map.filename)) then
  begin
    load_ini_fields;
    cbUseINI.Checked := true;
  end else
  begin
    empty_ini_fields;
    cbUseINI.Checked := false;
  end;
  cbUseINI.Tag := 0;
end;

procedure TMissionDialog.tileset_changed;
begin
  edTilesetName.Text := Tileset.tileset_name;
  edTileatrName.Text := Tileset.tileatr_name;
end;

function TMissionDialog.get_integer_value(source: array of byte; pos, bytes: integer): integer;
var
  b: integer;
begin
  result := 0;
  for b := 0 to bytes - 1 do
    result := result + (source[pos + b] shl (8 * b));
end;

function TMissionDialog.get_float_value(source: array of byte; pos: integer): single;
var
  f_ptr: ^single;
begin
  f_ptr := Addr(source[pos]);
  result := f_ptr^;
end;

procedure TMissionDialog.fill_ai_values;
var
  i: integer;
  bytes: integer;
  i_val: integer;
  f_val: single;
  tmp_strings: TStringList;
begin
  tmp_strings := TStringList.Create();
  for i := 0 to Length(misai_properties) -1 do
  begin
    bytes := 1;
    case misai_properties[i].data_type of
      'b': bytes := 1;
      'w': bytes := 2;
      'd': bytes := 4;
      'f':
        begin
          f_val := get_float_value(Mission.mis_data.ai_segments[AITabControl.TabIndex], misai_properties[i].position);
          if cbDiffMode.Checked and (f_val = get_float_value(Mission.default_ai, misai_properties[i].position)) then
            tmp_strings.Add(misai_properties[i].name + '=')
          else
            tmp_strings.Add(misai_properties[i].name + '=' + floattostrf(f_val, ffFixed, 8, 3));
          continue;
        end;
      end;
    i_val := get_integer_value(Mission.mis_data.ai_segments[AITabControl.TabIndex], misai_properties[i].position, bytes);
    if cbDiffMode.Checked and (i_val = get_integer_value(Mission.default_ai, misai_properties[i].position, bytes)) then
      tmp_strings.Add(misai_properties[i].name + '=')
    else
      tmp_strings.Add(misai_properties[i].name + '=' + inttostr(i_val));
  end;
  AIValueList.Strings := tmp_strings;
  tmp_strings.Destroy;
end;

function TMissionDialog.get_ini_filename(map_filename: String): String;
begin
  result := ChangeFileExt(map_filename,'.ini');
end;

procedure TMissionDialog.load_ini_fields;
var
  i: integer;
  tmp_strings: TStringList;
  ini: TMemIniFile;
begin
  ini := TMemIniFile.Create(get_ini_filename(Map.filename));
  btnResetToDefaults.Enabled := true;
  btnRefreshStrings.Enabled := true;
  // Load basic map settings
  edMapName.Enabled := true;
  edMapName.Text := ini.ReadString('Basic','Name','');
  edMapAuthor.Enabled := true;
  edMapAuthor.Text := ini.ReadString('Basic','Author','');
  cbMapMusic.Enabled := true;
  cbMapMusic.Text := ini.ReadString('Basic','Music','');
  cbMapSideId.Enabled := true;
  cbMapSideId.ItemIndex := ini.ReadInteger('Basic','SideId',-1);
  seMapMissionNumber.Enabled := true;
  seMapMissionNumber.Value := ini.ReadInteger('Basic','MissionNumber',0);
  edTextUib.Enabled := true;
  edTextUib.Text := ini.ReadString('Basic','TextUib','');
  cbMapSideIdChange(nil);
  seMapMissionNumberChange(nil);
  // Load rules
  RuleValueList.Enabled := true;
  tmp_strings := TStringList.Create;
  for i := 0 to Length(rule_definitions) - 1 do
  begin
    tmp_strings.Add(rule_definitions[i].name+'='+ini.ReadString('Vars',rule_definitions[i].name,rule_definitions[i].default_value));
  end;
  RuleValueList.Strings := tmp_strings;
  tmp_strings.Clear;
  // Load strings
  StringValueList.Enabled := true;
  StringTable.load_custom_texts_from_ini(ini);
  // Load briefing
  MapBriefing.Enabled := true;
  tmp_strings.Delimiter := '_';
  tmp_strings.DelimitedText := StringReplace(ini.ReadString('Basic','Briefing',''),' ','^',[rfReplaceAll]);
  for i := 0 to tmp_strings.Count-1 do
  begin
    tmp_strings[i] := StringReplace(tmp_strings[i],'^',' ',[rfReplaceAll]);
  end;
  MapBriefing.Lines := tmp_strings;
  // Remove additional trailing newline from the Memo
  MapBriefing.Text := Copy(MapBriefing.Text, 1, Length(MapBriefing.Text) - 2);
  tmp_strings.Destroy;
  // Load event/condition notes
  Mission.load_notes_from_ini(ini);
  EventDialog.enable_map_ini_features(true);
  ini.Destroy;
end;

procedure TMissionDialog.empty_ini_fields;
begin
  btnResetToDefaults.Enabled := false;
  btnRefreshStrings.Enabled := false;
  edMapName.Enabled := false;
  edMapName.Clear;
  edMapAuthor.Enabled := false;
  edMapAuthor.Clear;
  cbMapMusic.Enabled := false;
  cbMapMusic.Text := '';
  cbMapSideId.Enabled := false;
  cbMapSideId.ItemIndex := -1;
  seMapMissionNumber.Enabled := false;
  seMapMissionNumber.Value := 0;
  edTextUib.Enabled := false;
  edTextUib.Clear;
  RuleValueList.Enabled := false;
  RuleValueList.Strings.Clear;
  StringValueList.Enabled := false;
  StringTable.clear_custom_texts;
  MapBriefing.Enabled := false;
  MapBriefing.Lines.Clear;
  Mission.clear_notes;
  EventDialog.enable_map_ini_features(false);
end;

procedure TMissionDialog.save_ini_fields(map_filename: String);
var
  i: integer;
  tmp_string: String;
  ini: TMemIniFile;
begin
  if not cbUseINI.Checked then
    exit;
  ini := TMemIniFile.Create(get_ini_filename(map_filename));
  // Save basic map settings
  if edMapName.Text = '' then
    ini.DeleteKey('Basic','Name')
  else
    ini.WriteString('Basic','Name',edMapName.Text);
  if edMapAuthor.Text = '' then
    ini.DeleteKey('Basic','Author')
  else
    ini.WriteString('Basic','Author',edMapAuthor.Text);
  if cbMapMusic.Text = '' then
    ini.DeleteKey('Basic','Music')
  else
    ini.WriteString('Basic','Music',cbMapMusic.Text);
  if cbMapSideId.ItemIndex = -1 then
    ini.DeleteKey('Basic','SideId')
  else
    ini.WriteInteger('Basic','SideId',cbMapSideId.ItemIndex);
  if seMapMissionNumber.Value = 0 then
    ini.DeleteKey('Basic','MissionNumber')
  else
    ini.WriteInteger('Basic','MissionNumber',seMapMissionNumber.Value);
  if edTextUib.Text = '' then
    ini.DeleteKey('Basic','TextUib')
  else
    ini.WriteString('Basic','TextUib',edTextUib.Text);
  // Save rules
  for i := 0 to Length(rule_definitions) - 1 do
  begin
    if RuleValueList.Cells[1,i+1] = rule_definitions[i].default_value then
      ini.DeleteKey('Vars',rule_definitions[i].name)
    else
      ini.WriteString('Vars',rule_definitions[i].name,RuleValueList.Cells[1,i+1]);
  end;
  // Save strings
  StringTable.save_custom_texts_to_ini(ini);
  // Save briefing
  if MapBriefing.Lines.Count = 0 then
    ini.DeleteKey('Basic','Briefing')
  else begin
    tmp_string := StringReplace(MapBriefing.Text,chr(13)+chr(10),'_',[rfReplaceAll]);
    ini.WriteString('Basic','Briefing',tmp_string);
  end;
  // Save event/condition notes
  Mission.save_notes_to_ini(ini);
  ini.UpdateFile;
  ini.Destroy;
end;

procedure TMissionDialog.seTechLevelAllChange(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to cnt_mis_players-1 do
    tech_level[i].Value := StrToIntDef(seTechLevelAll.Text,0);
end;

procedure TMissionDialog.edStartingMoneyAllChange(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to cnt_mis_players-1 do
    starting_money[i].Text := IntToStr(StrToIntDef(edStartingMoneyAll.Text,0));
end;

procedure TMissionDialog.btnAllocIndexResetClick(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to cnt_mis_players-1 do
    alloc_index[i].Value := i;
end;

procedure TMissionDialog.btnAllegianceResetClick(Sender: TObject);
var
  i, j: integer;
begin
  for i := 0 to cnt_mis_players-1 do
    for j := 0 to cnt_mis_players-1 do
    begin
      if i = j then
        Mission.mis_data.allegiance[i,j] := 0
      else
        Mission.mis_data.allegiance[i,j] := 1;
      allegiance_btn[i,j].Caption := allegiance_type[Mission.mis_data.allegiance[i,j]];
      allegiance_btn[i,j].Font.Color := allegiance_type_color[Mission.mis_data.allegiance[i,j]];
    end;
end;

procedure TMissionDialog.tech_level_change(Sender: TObject);
begin
  Mission.mis_data.tech_level[(Sender as TSpinEdit).Tag] := StrToIntDef((Sender as TSpinEdit).Text, 0);
end;

procedure TMissionDialog.starting_money_change(Sender: TObject);
begin
  Mission.mis_data.starting_money[(Sender as TEdit).Tag] := StrToIntDef((Sender as TEdit).Text, 0);
end;

procedure TMissionDialog.alloc_index_change(Sender: TObject);
begin
  Mission.mis_data.allocation_index[(Sender as TSpinEdit).Tag] := StrToIntDef((Sender as TSpinEdit).Text, 0);
end;

procedure TMissionDialog.allegiance_btn_click(Sender: TObject);
var
  i, j: integer;
  new_allegiance: byte;
begin
  i := (Sender as TBitBtn).Tag div cnt_mis_players;
  j := (Sender as TBitBtn).Tag mod cnt_mis_players;
  new_allegiance := (Mission.mis_data.allegiance[i,j] + 1) mod Length(allegiance_type);
  Mission.mis_data.allegiance[i,j] := new_allegiance;
  allegiance_btn[i,j].Caption := allegiance_type[new_allegiance];
  allegiance_btn[i,j].Font.Color := allegiance_type_color[new_allegiance];
  if cbSetBothSides.Checked and (i <> j) then
  begin
    Mission.mis_data.allegiance[j,i] := new_allegiance;
    allegiance_btn[j,i].Caption := allegiance_type[new_allegiance];
    allegiance_btn[j,i].Font.Color := allegiance_type_color[new_allegiance];
  end;
end;

procedure TMissionDialog.time_limit_change(Sender: TObject);
begin
  Mission.mis_data.time_limit := StrToIntDef(edTimeLimit.Text, -1);
end;

procedure TMissionDialog.edTilesetNameChange(Sender: TObject);
begin
  FillChar(Mission.mis_data.tileset, Length(Mission.mis_data.tileset), 0);
  Move(edTilesetName.Text[1], Mission.mis_data.tileset, Length(edTilesetName.Text));
end;

procedure TMissionDialog.edTileatrNameChange(Sender: TObject);
begin
  FillChar(Mission.mis_data.tileatr, Length(Mission.mis_data.tileatr), 0);
  Move(edTileatrName.Text[1], Mission.mis_data.tileatr, Length(edTileatrName.Text));
end;

procedure TMissionDialog.AITabControlChange(Sender: TObject);
begin
  fill_ai_values;
end;

procedure TMissionDialog.AIValueListStringsChange(Sender: TObject);
var
  prop: ^TMisAIProperty;
  i_val: integer;
  f_val: single;
  f_ptr: ^single;
  bytes: integer;
  b: integer;
begin
  prop := Addr(misai_properties[AIValueList.Row-1]);
  bytes := 1;
  case prop.data_type of
    'b': bytes := 1;
    'w': bytes := 2;
    'd': bytes := 4;
    'f':
      begin
        f_val := StrToFloatDef(AIValueList.Cells[1,AIValueList.Row], get_float_value(Mission.default_ai, prop.position));
        f_ptr := Addr(Mission.mis_data.ai_segments[AITabControl.TabIndex, prop.position]);
        f_ptr^ := f_val;
        exit;
      end;
  end;
  i_val := strtointdef(AIValueList.Cells[1,AIValueList.Row], get_integer_value(Mission.default_ai, prop.position, bytes));
  for b := 0 to bytes - 1 do
    Mission.mis_data.ai_segments[AITabControl.TabIndex, prop.position+b] := (i_val shr (8 * b)) and 255;
end;

procedure TMissionDialog.cbUseINIClick(Sender: TObject);
var
  ini_filename: string;
begin
  if cbUseINI.Tag = 1 then
    exit;
  if cbUseINI.Checked then
  begin
    load_ini_fields;
  end else
  begin
    ini_filename := get_ini_filename(Map.filename);
    if FileExists(ini_filename) and (Application.MessageBox('Do you want to delete ini file on disk?', 'Do not use ini file', MB_YESNO or MB_ICONQUESTION) = IDYES) then
      DeleteFile(ini_filename);
    empty_ini_fields;
    // Need to update strings because custom texts are unloaded
    EventDialog.update_contents;
  end;
end;

procedure TMissionDialog.btnResetToDefaultsClick(Sender: TObject);
var
  i: integer;
  tmp_strings: TStringList;
begin
  if not cbUseINI.Checked then
    exit;
  tmp_strings := TStringList.Create;
  for i := 0 to Length(rule_definitions) - 1 do
  begin
    tmp_strings.Add(rule_definitions[i].name+'='+rule_definitions[i].default_value);
  end;
  RuleValueList.Strings := tmp_strings;
  tmp_strings.Destroy;
end;

procedure TMissionDialog.btnRefreshStringsClick(Sender: TObject);
begin
  EventDialog.update_contents;
end;

procedure TMissionDialog.btnExportAIClick(Sender: TObject);
var
  ai_file: file of byte;
begin
  if ExportAIDialog.Execute then
  begin
    AssignFile(ai_file, ExportAIDialog.FileName);
    ReWrite(ai_file);
    BlockWrite(ai_file, Mission.mis_data.ai_segments[AITabControl.TabIndex][1], Length(Mission.mis_data.ai_segments[0])-1);
    CloseFile(ai_file);
  end;
end;

procedure TMissionDialog.btnImportAIClick(Sender: TObject);
var
  ai_file: file of byte;
begin
  if ImportAIDialog.Execute then
  begin
    AssignFile(ai_file, ImportAIDialog.FileName);
    Reset(ai_file);
    BlockRead(ai_file, Mission.mis_data.ai_segments[AITabControl.TabIndex][1], Length(Mission.mis_data.ai_segments[0])-1);
    CloseFile(ai_file);
    fill_ai_values;
  end;
end;

procedure TMissionDialog.btnCopyAIClick(Sender: TObject);
var
  handle: THandle;
  pointer: ^TAIClipboard;
begin
  OpenClipboard(Application.Handle);
  EmptyClipboard;

  handle := GlobalAlloc(GMEM_DDESHARE or GMEM_MOVEABLE, sizeof(TAIClipboard));
  pointer := GlobalLock(handle);

  Move(Mission.mis_data.ai_segments[AITabControl.TabIndex][1], pointer.ai_data, 7607);

  GlobalUnLock(handle);
  SetClipboardData(ai_clipboard_format, handle);
  CloseClipboard;
end;

procedure TMissionDialog.btnPasteAIClick(Sender: TObject);
var
  handle: THandle;
  pointer: ^TAIClipboard;
begin
  if not Clipboard.HasFormat(ai_clipboard_format) then
    exit;
  OpenClipboard(Application.Handle);
  handle := GetClipboardData(ai_clipboard_format);
  pointer := GlobalLock(handle);

  Move(pointer.ai_data, Mission.mis_data.ai_segments[AITabControl.TabIndex][1], 7607);

  fill_ai_values;

  GlobalUnLock(handle);
  CloseClipboard;
end;

procedure TMissionDialog.cbDiffModeClick(Sender: TObject);
begin
  fill_ai_values;
end;

procedure TMissionDialog.cbMapSideIdChange(Sender: TObject);
begin
  if cbMapSideId.ItemIndex <> -1 then
    Settings.MySideID := cbMapSideId.ItemIndex;
end;

procedure TMissionDialog.seMapMissionNumberChange(Sender: TObject);
begin
  if seMapMissionNumber.Value <> 0 then
    Settings.MissionNumber := seMapMissionNumber.Value;
end;

end.
