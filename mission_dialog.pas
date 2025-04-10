unit mission_dialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Spin, Buttons, ComCtrls, Grids,
  ValEdit, IniFiles, _map, _mission, _structures, _misai, _utils;

type
  TMissionDialog = class(TForm)
    lblTechLevel: TLabel;
    lblStartingMoney: TLabel;
    SettingsPanel: TPanel;
    lblHouseID: TLabel;
    SideSettingsPanel: TPanel;
    RulesAndStringsPanel: TPanel;
    AITabControl: TTabControl;
    lblSetToAll: TLabel;
    seTechLevelAll: TSpinEdit;
    edStartingMoneyAll: TEdit;
    btnHouseIDReset: TButton;
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
    cbTextUib: TComboBox;
    pnSelectDefenceAreaFromMap: TPanel;
    btnSelectDefenceAreaFromMap: TButton;
    lblModsFolder: TLabel;
    cbModsFolder: TComboBox;
    lblColoursBin: TLabel;
    cbColoursBin: TComboBox;
    lblCampaignFolder: TLabel;
    cbCampaignFolder: TComboBox;
    lblMapIntelId: TLabel;
    edMapIntelId: TEdit;
    lblPlayersIni: TLabel;
    cbPlayersIni: TComboBox;
    btnModsFolderOpen: TButton;
    sbShowAIHelp: TSpeedButton;
    pnAIHelp: TPanel;
    lblAIHelp: TLabel;
    // Form events
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    // Mission data editor events
    procedure seTechLevelAllChange(Sender: TObject);
    procedure edStartingMoneyAllChange(Sender: TObject);
    procedure btnHouseIDResetClick(Sender: TObject);
    procedure btnAllegianceResetClick(Sender: TObject);
    procedure tech_level_change(Sender: TObject);
    procedure starting_money_change(Sender: TObject);
    procedure house_id_change(Sender: TObject);
    procedure allegiance_btn_click(Sender: TObject);
    procedure time_limit_change(Sender: TObject);
    procedure edTilesetNameChange(Sender: TObject);
    procedure edTileatrNameChange(Sender: TObject);
    // AI value editor events
    procedure sbShowAIHelpClick(Sender: TObject);
    procedure AITabControlChange(Sender: TObject);
    procedure AIValueListStringsChange(Sender: TObject);
    procedure AIValueListSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure btnExportAIClick(Sender: TObject);
    procedure btnImportAIClick(Sender: TObject);
    procedure btnCopyAIClick(Sender: TObject);
    procedure btnPasteAIClick(Sender: TObject);
    procedure cbDiffModeClick(Sender: TObject);
    procedure btnSelectDefenceAreaFromMapClick(Sender: TObject);
    // Mission ini data editor events
    procedure cbUseINIClick(Sender: TObject);
    procedure btnResetToDefaultsClick(Sender: TObject);
    procedure btnRefreshStringsClick(Sender: TObject);
    procedure MissionIniPropertyChange(Sender: TObject);
    procedure cbMapSideIdChange(Sender: TObject);
    procedure seMapMissionNumberChange(Sender: TObject);
    procedure cbCampaignFolderChange(Sender: TObject);
    procedure cbModsFolderChange(Sender: TObject);
    procedure cbColoursBinChange(Sender: TObject);
    procedure cbPlayersIniChange(Sender: TObject);
    procedure cbTextUibChange(Sender: TObject);
    procedure btnModsFolderOpenClick(Sender: TObject);
    procedure MapBriefingEnter(Sender: TObject);
    procedure MapBriefingExit(Sender: TObject);
  private
    // Dynamic controls
    side_label: array[0..CNT_SIDES-1] of TLabel;
    tech_level: array[0..CNT_SIDES-1] of TSpinEdit;
    starting_money: array[0..CNT_SIDES-1] of TEdit;
    house_id: array[0..CNT_SIDES-1] of TSpinEdit;
    color_marker: array[0..CNT_SIDES-1] of TPanel;
    side_label_alleg: array[0..CNT_SIDES-1] of TLabel;
    allegiance_btn: array[0..CNT_SIDES-1, 0..CNT_SIDES-1] of TBitBtn;
    // Misc. variables
    defence_area_num: integer;
    loading: boolean;
  public
    // Dispatcher procedures
    procedure update_side_list(side_list: TStringList);
    procedure update_side_colors;
    procedure update_tileset;
    procedure update_mission_data;
    procedure update_mis_ai_values;
    procedure update_mission_ini_data;

    // Procedures called from different forms
    procedure finish_defence_area_position_selection(min_x, max_x, min_y, max_y: integer);
  end;

var
  MissionDialog: TMissionDialog;

implementation

uses
  Math, StrUtils, ShellApi, _missionini, _graphics, _stringtable, _settings, _tileset, _launcher, event_dialog, main, _dispatcher, _gamestructs;

{$R *.dfm}

procedure TMissionDialog.FormCreate(Sender: TObject);
var
  i, j: integer;
  tmp_strings: TStringList;
  SR: TSearchRec;
begin
  MissionIni.init_controls(MapBriefing, RuleValueList, StringValueList);
  for i := 0 to CNT_SIDES-1 do
  begin
    // Initialize side labels
    side_label[i] := TLabel.Create(self);
    side_label[i].Left := 8;
    side_label[i].Top := 28 + i * 24;
    side_label[i].Parent := SideSettingsPanel;
    // Initialize tech levels
    tech_level[i] := TSpinEdit.Create(self);
    tech_level[i].Left := 80;
    tech_level[i].Top := 24 + i * 24;
    tech_level[i].Width := 48;
    tech_level[i].Height := 22;
    tech_level[i].MinValue := 0;
    tech_level[i].MaxValue := 255;
    tech_level[i].Value := 0;
    tech_level[i].Tag := i;
    tech_level[i].Parent := SideSettingsPanel;
    tech_level[i].OnChange := tech_level_change;
    // Initialize Starting money
    starting_money[i] := TEdit.Create(self);
    starting_money[i].Left := 132;
    starting_money[i].Top := 24 + i * 24;
    starting_money[i].Width := 56;
    starting_money[i].Height := 22;
    starting_money[i].Text := '0';
    starting_money[i].Tag := i;
    starting_money[i].Parent := SideSettingsPanel;
    starting_money[i].OnChange := starting_money_change;
    // Initialize house ID
    house_id[i] := TSpinEdit.Create(self);
    house_id[i].Left := 192;
    house_id[i].Top := 24 + i * 24;
    house_id[i].Width := 48;
    house_id[i].Height := 22;
    house_id[i].MinValue := 0;
    house_id[i].MaxValue := 255;
    house_id[i].Value := 0;
    house_id[i].Tag := i;
    house_id[i].Parent := SideSettingsPanel;
    house_id[i].OnChange := house_id_change;
    // Initialize side color markers
    color_marker[i] := TPanel.Create(self);
    color_marker[i].Left := 240;
    color_marker[i].Top := 24 + i * 24;
    color_marker[i].Width := 16;
    color_marker[i].Height := 22;
    color_marker[i].BevelOuter := bvNone;
    color_marker[i].ParentBackground := False;
    color_marker[i].Parent := SideSettingsPanel;
    // Initialize allegiance labels
    side_label_alleg[i] := TLabel.Create(self);
    side_label_alleg[i].Left := 266 + i * 52;
    side_label_alleg[i].Top := 8;
    side_label_alleg[i].Parent := SideSettingsPanel;
    // Initialize allegiance buttons
    for j := 0 to CNT_SIDES-1 do
    begin
      allegiance_btn[i,j] := TBitBtn.Create(self);
      allegiance_btn[i,j].Left := 264 + j * 52;
      allegiance_btn[i,j].Top := 24 + i * 24;
      allegiance_btn[i,j].Width := 52;
      allegiance_btn[i,j].Height := 22;
      allegiance_btn[i,j].Font.Style := [fsBold];
      allegiance_btn[i,j].Tag := i * 8 + j;
      allegiance_btn[i,j].Parent := SideSettingsPanel;
      allegiance_btn[i,j].OnClick := allegiance_btn_click;
    end;
    // Initialize AI PageControl
    AITabControl.Tabs.Add('');
  end;
  cbMapMusic.Items := EventDialog.cbMusicName.Items;
  pnAIHelp.BringToFront;
  // Initialize list of TEXT.UIB files
  tmp_strings := TStringList.Create;
  if FindFirst(Settings.GamePath + '\Data\UI_DATA\*.UIB', 0, SR) = 0 then
  begin
    repeat
      if (SR.Name <> 'campaign.uib') and (SR.Name <> 'colours.uib') and (SR.Name <> 'menus.uib') and (SR.Name <> 'samples.uib') then
        tmp_strings.Add(SR.Name);
    until FindNext(SR) <> 0;
      FindClose(SR);
  end;
  cbTextUib.Items := tmp_strings;
  tmp_strings.Clear;
  // Initialize list of Campaign folders
  if FindFirst(Settings.GamePath + '\CustomCampaignData\*', faDirectory, SR) = 0 then
  begin
    repeat
      if ((SR.Attr and faDirectory) <> 0) and (SR.Name <> '.') and (SR.Name <> '..') then
        tmp_strings.Add(SR.Name);
    until FindNext(SR) <> 0;
      FindClose(SR);
  end;
  cbCampaignFolder.Items := tmp_strings;
  tmp_strings.Destroy;
end;

procedure TMissionDialog.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key = 27 then
    Close;
  if key = 123 then
    MainWindow.Show;
end;

procedure TMissionDialog.seTechLevelAllChange(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to CNT_SIDES-1 do
    tech_level[i].Value := StrToIntDef(seTechLevelAll.Text,0);
end;

procedure TMissionDialog.edStartingMoneyAllChange(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to CNT_SIDES-1 do
    starting_money[i].Text := IntToStr(StrToIntDef(edStartingMoneyAll.Text,0));
end;

procedure TMissionDialog.btnHouseIDResetClick(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to CNT_SIDES-1 do
    house_id[i].Value := i;
end;

procedure TMissionDialog.btnAllegianceResetClick(Sender: TObject);
var
  i, j: integer;
begin
  for i := 0 to CNT_SIDES-1 do
    for j := 0 to CNT_SIDES-1 do
    begin
      if i = j then
        Mission.allegiance[i,j] := 0
      else
        Mission.allegiance[i,j] := 1;
      allegiance_btn[i,j].Caption := allegiance_type[Mission.allegiance[i,j]];
      allegiance_btn[i,j].Font.Color := allegiance_type_color[Mission.allegiance[i,j]];
    end;
end;

procedure TMissionDialog.tech_level_change(Sender: TObject);
begin
  Mission.tech_level[(Sender as TSpinEdit).Tag] := StrToIntDef((Sender as TSpinEdit).Text, 0);
end;

procedure TMissionDialog.starting_money_change(Sender: TObject);
begin
  Mission.starting_money[(Sender as TEdit).Tag] := StrToIntDef((Sender as TEdit).Text, 0);
end;

procedure TMissionDialog.house_id_change(Sender: TObject);
begin
  if loading then
    exit;
  Mission.house_id[(Sender as TSpinEdit).Tag] := StrToIntDef((Sender as TSpinEdit).Text, 0);
  color_marker[(Sender as TSpinEdit).Tag].Color := StructGraphics.house_colors_inv[Mission.house_id[(Sender as TSpinEdit).Tag]];
  Dispatcher.register_event(evMisHouseIDChange);
end;

procedure TMissionDialog.allegiance_btn_click(Sender: TObject);
var
  i, j: integer;
  new_allegiance: byte;
begin
  i := (Sender as TBitBtn).Tag div CNT_SIDES;
  j := (Sender as TBitBtn).Tag mod CNT_SIDES;
  new_allegiance := IfThen((Mission.allegiance[i,j] - 1) < 0, Length(allegiance_type)-1, Mission.allegiance[i,j] - 1);
  Mission.allegiance[i,j] := new_allegiance;
  allegiance_btn[i,j].Caption := allegiance_type[new_allegiance];
  allegiance_btn[i,j].Font.Color := allegiance_type_color[new_allegiance];
  if cbSetBothSides.Checked and (i <> j) then
  begin
    Mission.allegiance[j,i] := new_allegiance;
    allegiance_btn[j,i].Caption := allegiance_type[new_allegiance];
    allegiance_btn[j,i].Font.Color := allegiance_type_color[new_allegiance];
  end;
end;

procedure TMissionDialog.time_limit_change(Sender: TObject);
begin
  Mission.time_limit := StrToIntDef(edTimeLimit.Text, -1);
end;

procedure TMissionDialog.edTilesetNameChange(Sender: TObject);
begin
  if not loading then
    store_c_string(edTilesetName.Text, Addr(Mission.tileset_name), Length(Mission.tileset_name));
end;

procedure TMissionDialog.edTileatrNameChange(Sender: TObject);
begin
  if not loading then
    store_c_string(edTileatrName.Text, Addr(Mission.tileatr_name), Length(Mission.tileatr_name));
end;

procedure TMissionDialog.sbShowAIHelpClick(Sender: TObject);
var
  i: integer;
  b: boolean;
begin
  pnAIHelp.Visible := sbShowAIHelp.Down;
  for i := 0 to Length(side_label_alleg) - 1 do
    side_label_alleg[i].Visible := not sbShowAIHelp.Down;
  lblTimeLimitHelp.Visible := not sbShowAIHelp.Down;
  lblTilesetName.Visible := not sbShowAIHelp.Down;
  lblTileatrName.Visible := not sbShowAIHelp.Down;
  AIValueListSelectCell(Sender, AIValueList.Col, AIValueList.Row, b);
end;

procedure TMissionDialog.AITabControlChange(Sender: TObject);
begin
  update_mis_ai_values;
end;

procedure TMissionDialog.AIValueListStringsChange(Sender: TObject);
var
  i: integer;
  prop: TGameStructMemberPtr;
  i_val: integer;
  f_val: single;
  bytes: integer;
begin
  if loading then
    exit;
  loading := true;
  // Range selection (if one value selected, loop goes only once)
  for i := AIValueList.Selection.Top to AIValueList.Selection.Bottom do
  begin
    // Set same value to all rows within selected range
    if i <> AIValueList.Row then
      AIValueList.Cells[1,i] := AIValueList.Cells[1,AIValueList.Row];
    prop := GameStructs.get_misai_property(i-1);
    // Check if defence area was affected
    if (prop.offset >= DEFENCE_AREAS_COUNT_BYTE) and (prop.offset <= DEFENCE_AREAS_END_BYTE) then
      Dispatcher.register_event(evMisDefenceAreaChange);
    // Determine data type
    bytes := game_struct_data_type_size[Ord(prop.data_type)];
    if prop.data_type = dtFloat then
    begin
      f_val := StrToFloatDef(AIValueList.Cells[1,AIValueList.Row], get_float_value(Addr(MisAI.default_ai), prop.offset));
      set_float_value(Addr(Mission.ai_segments[AITabControl.TabIndex]), prop.offset, f_val);
      continue;
    end;
    i_val := strtointdef(AIValueList.Cells[1,AIValueList.Row], get_integer_value(Addr(MisAI.default_ai), prop.offset, bytes));
    set_integer_value(Addr(Mission.ai_segments[AITabControl.TabIndex]), prop.offset, bytes, i_val);
  end;
  loading := false;
end;

procedure TMissionDialog.AIValueListSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
var
  prop: TGameStructMemberPtr;
  help_text: string;
  i: integer;
begin
  prop := GameStructs.get_misai_property(ARow-1);
  help_text := AIValueList.Cells[0, ARow] + #13;
  for i := 0 to Length(MisAI.ai_hint_entries) - 1 do
    if (prop.offset >= MisAI.ai_hint_entries[i].min_offset) and (prop.offset <= MisAI.ai_hint_entries[i].max_offset) then
    begin
      help_text := help_text + MisAI.ai_hint_entries[i].hint;
      break;
    end;
  lblAIHelp.Caption := help_text;
  if (prop.offset < DEFENCE_AREAS_START_BYTE) or (prop.offset > DEFENCE_AREAS_END_BYTE) then
  begin
    pnSelectDefenceAreaFromMap.Visible := false;
    exit;
  end;
  defence_area_num := (prop.offset - DEFENCE_AREAS_START_BYTE) div DEFENCE_AREA_SIZE;
  pnSelectDefenceAreaFromMap.Visible := true;
  btnSelectDefenceAreaFromMap.Caption := 'Select defence area '+ inttostr(defence_area_num+1) +' from map';
end;

procedure TMissionDialog.btnExportAIClick(Sender: TObject);
begin
  if ExportAIDialog.Execute then
    MisAI.save_misai_segment(ExportAIDialog.FileName, Mission.ai_segments[AITabControl.TabIndex]);
end;

procedure TMissionDialog.btnImportAIClick(Sender: TObject);
begin
  if ImportAIDialog.Execute then
  begin
    MisAI.load_misai_segment(ImportAIDialog.FileName, Mission.ai_segments[AITabControl.TabIndex]);
    update_mis_ai_values;
  end;
end;

procedure TMissionDialog.btnCopyAIClick(Sender: TObject);
begin
  MisAI.copy_misai_segment_to_clipboard(Mission.ai_segments[AITabControl.TabIndex]);
end;

procedure TMissionDialog.btnPasteAIClick(Sender: TObject);
begin
  if MisAI.paste_misai_segment_from_clipboard(Mission.ai_segments[AITabControl.TabIndex]) then
    update_mis_ai_values;
end;

procedure TMissionDialog.cbDiffModeClick(Sender: TObject);
begin
  update_mis_ai_values;
end;

procedure TMissionDialog.btnSelectDefenceAreaFromMapClick(Sender: TObject);
var
  x, y: integer;
  defence_area: TDefenceAreaPtr;
begin
  if defence_area_num >= Mission.ai_segments[AITabControl.TabIndex, DEFENCE_AREAS_COUNT_BYTE] then
  begin
    Application.MessageBox('Increase number of Defence Areas first.', 'Cannot select Defence Area', MB_OK or MB_ICONWARNING);
    exit;
  end;
  defence_area := MisAI.get_defence_area(Mission.ai_segments[AITabControl.TabIndex], defence_area_num);
  x := defence_area.MinX;
  y := defence_area.MinY;
  MainWindow.start_position_selection(x, y, psmDefenceArea);
  close;
end;

procedure TMissionDialog.cbUseINIClick(Sender: TObject);
var
  msg: string;
begin
  if loading then
    exit;
  if cbUseINI.Checked then
  begin
    MissionIni.assign_mission_ini;
  end else
  begin
    msg := 'Warning: This action will erase all Mission ini file data.'#13;
    if MissionIni.mission_ini_filename <> '' then
      msg := msg + 'When you save this map, the file ' + ExtractFileName(MissionIni.mission_ini_filename) + ' will be deleted and data will be lost!'#13;
    msg := msg + 'Do you want to continue?'#13;
    if Application.MessageBox(PChar(msg), 'Do not use ini file', MB_YESNO or MB_ICONWARNING) = IDNO then
    begin
      loading := true;
      cbUseINI.Checked := true;
      loading := false;
      exit;
    end;
    MissionIni.unload_mission_ini(true);
  end;
end;

procedure TMissionDialog.btnResetToDefaultsClick(Sender: TObject);
begin
  MissionIni.reset_rules_to_defaults;
end;

procedure TMissionDialog.btnRefreshStringsClick(Sender: TObject);
begin
  Dispatcher.register_event(evMissionIniCustomTextChange);
end;

procedure TMissionDialog.MissionIniPropertyChange(Sender: TObject);
begin
  if loading then
    exit;
  MissionIni.Name := edMapName.Text;
  MissionIni.Author := edMapAuthor.Text;
  MissionIni.Music := cbMapMusic.Text;
  MissionIni.IntelId := edMapIntelId.Text;
end;

procedure TMissionDialog.cbMapSideIdChange(Sender: TObject);
begin
  if cbMapSideId.ItemIndex <> -1 then
    MissionIni.set_side_id(cbMapSideId.ItemIndex);
end;

procedure TMissionDialog.seMapMissionNumberChange(Sender: TObject);
begin
  if not loading then
    MissionIni.set_mission_number(seMapMissionNumber.Value);
end;

procedure TMissionDialog.cbCampaignFolderChange(Sender: TObject);
var
  tmp_strings: TStringList;
  SR: TSearchRec;
begin
  // Load list of Mods folders
  tmp_strings := TStringList.Create;
  if FindFirst(Settings.GamePath + '\CustomCampaignData\' + cbCampaignFolder.Text + '\*', faDirectory, SR) = 0 then
  begin
    repeat
      if ((SR.Attr and faDirectory) <> 0) and (SR.Name <> '.') and (SR.Name <> '..') and (AnsiCompareText(SR.Name,'colours') <> 0) and (AnsiCompareText(SR.Name,'intel') <> 0) and (AnsiCompareText(SR.Name,'players') <> 0) then
        tmp_strings.Add(SR.Name);
    until FindNext(SR) <> 0;
      FindClose(SR);
  end;
  cbModsFolder.Items := tmp_strings;
  // Load list of Colours.bin files
  tmp_strings.Clear;
  if FindFirst(Settings.GamePath + '\CustomCampaignData\' + cbCampaignFolder.Text + '\Colours\*.BIN', 0, SR) = 0 then
  begin
    repeat
      tmp_strings.Add(SR.Name);
    until FindNext(SR) <> 0;
      FindClose(SR);
  end;
  cbColoursBin.Items := tmp_strings;
  // Load list of Players.ini files
  tmp_strings.Clear;
  if FindFirst(Settings.GamePath + '\CustomCampaignData\' + cbCampaignFolder.Text + '\Players\*.ini', 0, SR) = 0 then
  begin
    repeat
      tmp_strings.Add(SR.Name);
    until FindNext(SR) <> 0;
      FindClose(SR);
  end;
  cbPlayersIni.Items := tmp_strings;
  tmp_strings.Destroy;

  if not loading then
    MissionIni.set_campaign_folder(cbCampaignFolder.Text);
end;

procedure TMissionDialog.cbModsFolderChange(Sender: TObject);
begin
  MissionIni.set_mods_folder(cbModsFolder.Text);
end;

procedure TMissionDialog.cbColoursBinChange(Sender: TObject);
begin
  MissionIni.set_colours_file(cbColoursBin.Text);
end;

procedure TMissionDialog.cbPlayersIniChange(Sender: TObject);
begin
  MissionIni.set_players_file(cbPlayersIni.Text);
end;

procedure TMissionDialog.cbTextUibChange(Sender: TObject);
begin
  MissionIni.set_text_uib(cbTextUib.Text);
end;

procedure TMissionDialog.btnModsFolderOpenClick(Sender: TObject);
begin
  if (cbCampaignFolder.Text <> '') then
    ShellExecute(0, 'open', PChar(Settings.GamePath + '\CustomCampaignData\' + cbCampaignFolder.Text + '\' + cbModsFolder.Text), nil, nil, SW_SHOWNORMAL);
end;

procedure TMissionDialog.MapBriefingEnter(Sender: TObject);
begin
  MainWindow.Structureseditor1.ShortCut := 0;
end;

procedure TMissionDialog.MapBriefingExit(Sender: TObject);
begin
  MainWindow.Structureseditor1.ShortCut := 16472;
end;

procedure TMissionDialog.update_side_list(side_list: TStringList);
var
  i: integer;
  prev_index: integer;
begin
  prev_index := cbMapSideId.ItemIndex;
  cbMapSideId.Items := side_list;
  cbMapSideId.ItemIndex := prev_index;
  for i := 0 to CNT_SIDES-1 do
  begin
    side_label[i].Caption := Structures.side_names[i];
    side_label_alleg[i].Caption := IfThen(Length(Structures.side_names[i]) <= 8, Structures.side_names[i], Copy(Structures.side_names[i], 0, 6)+'.');
    AITabControl.Tabs[i] := Structures.side_names_short[i];
  end;
end;

procedure TMissionDialog.update_side_colors;
var
  i: integer;
begin
  for i := 0 to CNT_SIDES-1 do
    color_marker[i].Color := StructGraphics.house_colors_inv[Mission.house_id[i]];
end;

procedure TMissionDialog.update_tileset;
begin
  edTilesetName.Text := Tileset.tileset_name;
  edTileatrName.Text := Tileset.tileatr_name;
end;

procedure TMissionDialog.update_mission_data;
var
  i, j: integer;
begin
  loading := true;
  for i := 0 to CNT_SIDES-1 do
  begin
    tech_level[i].Value := Mission.tech_level[i];
    starting_money[i].Text := inttostr(Mission.starting_money[i]);
    house_id[i].Value := Mission.house_id[i];
    for j := 0 to CNT_SIDES-1 do
    begin
      allegiance_btn[i,j].Caption := allegiance_type[Mission.allegiance[i,j]];
      allegiance_btn[i,j].Font.Color := allegiance_type_color[Mission.allegiance[i,j]];
    end;
  end;
  update_side_colors;
  edTimeLimit.Text := inttostr(Mission.time_limit);
  edTilesetName.Text := Mission.tileset_name;
  edTileatrName.Text := Mission.tileatr_name;
  loading := false;
end;

procedure TMissionDialog.update_mis_ai_values;
var
  i: integer;
  bytes: integer;
  i_val: integer;
  f_val: single;
  prop: TGameStructMemberPtr;
  prop_name: string;
  tmp_strings: TStringList;
begin
  if not Mission.mis_assigned then
    exit;
  if GameStructs.ai_struct_index = -1 then
    exit;
  loading := true;
  tmp_strings := TStringList.Create();
  for i := 0 to GameStructs.struct_member_count[GameStructs.ai_struct_index] -1 do
  begin
    prop := GameStructs.get_misai_property(i);
    prop_name := GameStructs.struct_member_names[GameStructs.ai_struct_index][i];
    bytes := game_struct_data_type_size[Ord(prop.data_type)];
    if prop.data_type = dtFloat then
    begin
      f_val := get_float_value(Addr(Mission.ai_segments[AITabControl.TabIndex]), prop.offset);
      if cbDiffMode.Checked and (f_val = get_float_value(Addr(MisAI.default_ai), prop.offset)) then
        tmp_strings.Add(prop_name + '=')
      else
        tmp_strings.Add(prop_name + '=' + floattostrf(f_val, ffFixed, 8, 3));
      continue;
    end;
    i_val := get_integer_value(Addr(Mission.ai_segments[AITabControl.TabIndex]), prop.offset, bytes);
    if cbDiffMode.Checked and (i_val = get_integer_value(Addr(MisAI.default_ai), prop.offset, bytes)) then
      tmp_strings.Add(prop_name + '=')
    else
      tmp_strings.Add(prop_name + '=' + inttostr(i_val));
  end;
  AIValueList.Strings := tmp_strings;
  tmp_strings.Destroy;
  loading := false;
end;

procedure TMissionDialog.update_mission_ini_data;
var
  status: boolean;
begin
  loading := true;
  status := MissionIni.mission_ini_assigned;

  cbUseINI.Checked := status;
  btnResetToDefaults.Enabled := status;
  btnRefreshStrings.Enabled := status;
  edMapName.Enabled := status;
  edMapName.Text := MissionIni.Name;
  edMapAuthor.Enabled := status;
  edMapAuthor.Text := MissionIni.Author;
  cbMapMusic.Enabled := status;
  cbMapMusic.Text := MissionIni.Music;
  cbMapSideId.Enabled := status;
  cbMapSideId.ItemIndex := MissionIni.SideId;
  seMapMissionNumber.Enabled := status;
  seMapMissionNumber.Value := MissionIni.MissionNumber;
  cbTextUib.Enabled := status;
  cbTextUib.Text := MissionIni.TextUib;
  MapBriefing.Enabled := status;
  cbCampaignFolder.Enabled := status;
  cbCampaignFolder.Text := MissionIni.CampaignFolder;
  cbCampaignFolderChange(nil);
  cbModsFolder.Enabled := status;
  cbModsFolder.Text := MissionIni.ModsFolder;
  cbColoursBin.Enabled := status;
  cbColoursBin.Text := MissionIni.ColoursFile;
  cbPlayersIni.Enabled := status;
  cbPlayersIni.Text := MissionIni.PlayersFile;
  edMapIntelId.Enabled := status;
  edMapIntelId.Text := MissionIni.IntelId;
  RuleValueList.Enabled := status;
  StringValueList.Enabled := status;

  loading := false;
end;

procedure TMissionDialog.finish_defence_area_position_selection(min_x, max_x, min_y, max_y: integer);
var
  defence_area: TDefenceAreaPtr;
begin
  Show;
  if (min_x = -1) or (min_y = -1) then
    exit;
  defence_area := MisAI.get_defence_area(Mission.ai_segments[AITabControl.TabIndex], defence_area_num);
  defence_area.MinX := min_x;
  defence_area.MaxX := max_x;
  defence_area.MinY := min_y;
  defence_area.MaxY := max_y;
  update_mis_ai_values;
  Dispatcher.register_event(evMisDefenceAreaChange);
end;

end.
