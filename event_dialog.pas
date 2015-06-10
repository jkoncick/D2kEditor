unit event_dialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, ExtCtrls, StdCtrls, CheckLst, Spin, mis_file;

type
  TEventDialog = class(TForm)
    EventGrid: TStringGrid;
    LowerPanel: TPanel;
    ConditionGrid: TStringGrid;
    Splitter1: TSplitter;
    EventPropertiesPanel: TPanel;
    EventUnitListPanel: TPanel;
    EventConditionListPanel: TPanel;
    ConditionPropertiesPanel: TPanel;
    EventConditionList: TCheckListBox;
    UnitSelectionList: TListBox;
    EventUnitList: TListBox;
    EventUnitListLabelPanel: TPanel;
    lblUnitSelection: TLabel;
    lblUnitList: TLabel;
    EventConditionListLabelPanel: TPanel;
    lblEventConditions: TLabel;
    EventUnitListPaddingPanel: TPanel;
    btnAddUnit: TButton;
    btnDeleteUnit: TButton;
    btnDeleteLastUnit: TButton;
    btnDeleteAllUnits: TButton;
    btnAddCondition: TButton;
    btnDeleteCondition: TButton;
    btnDeleteLastCondition: TButton;
    btnDeleteAllConditions: TButton;
    lblEventProperties: TLabel;
    lblConditionProperties: TLabel;
    lblEventType: TLabel;
    cbEventType: TComboBox;
    cbConditionType: TComboBox;
    lblConditionType: TLabel;
    epEventPlayer: TPanel;
    epEventPosition: TPanel;
    epDeployAction: TPanel;
    epAllegiance: TPanel;
    lblEventPlayer: TLabel;
    cbEventPlayer: TComboBox;
    lblEventPosition: TLabel;
    seEventPositionX: TSpinEdit;
    seEventPositionY: TSpinEdit;
    btnEventPositionGotoMap: TButton;
    lblDeployAction: TLabel;
    cbDeployAction: TComboBox;
    cbAllegianceSource: TComboBox;
    cbAllegianceTarget: TComboBox;
    lblAllegiance: TLabel;
    cbAllegianceType: TComboBox;
    lblAllegianceTo: TLabel;
    epSetFlag: TPanel;
    lblSetFlag: TLabel;
    seFlagNumber: TSpinEdit;
    rbFlagTrue: TRadioButton;
    rbFlagFalse: TRadioButton;
    epRadius: TPanel;
    lblRadius: TLabel;
    seRadius: TSpinEdit;
    epEventValue: TPanel;
    lblEventValue: TLabel;
    edEventValue: TEdit;
    cpConditionPlayer: TPanel;
    lblConditionPlayer: TLabel;
    cbConditionPlayer: TComboBox;
    cpConditionPosition: TPanel;
    lblConditionPosition: TLabel;
    seConditionPositionX: TSpinEdit;
    seConditionPositionY: TSpinEdit;
    btnConditionPositionGotoMap: TButton;
    cpBuildingType: TPanel;
    lblBuildingType: TLabel;
    cbBuildingType: TComboBox;
    cpUnitType: TPanel;
    lblUnitType: TLabel;
    cbUnitType: TComboBox;
    cpTimer: TPanel;
    lblTimer: TLabel;
    cbTimerCompareFunc: TComboBox;
    edTimerTime: TEdit;
    cpInterval: TPanel;
    lblInterval: TLabel;
    edStartDelay: TEdit;
    edInterval: TEdit;
    cpConditionValue: TPanel;
    lblConditionValue: TLabel;
    seConditionValue: TSpinEdit;
    cpCasualties: TPanel;
    lblCasualtyThreshold: TLabel;
    edCasualtyThreshold1: TEdit;
    edCasualtyThreshold2: TEdit;
    cpCreditsAmount: TPanel;
    lblCreditsAmount: TLabel;
    edCreditsAmount: TEdit;
    epMessage: TPanel;
    lblMessage: TLabel;
    seMessageId: TSpinEdit;
    edMessageText: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure seMessageIdChange(Sender: TObject);
    procedure EventGridSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure ConditionGridSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure cbEventTypeChange(Sender: TObject);
    procedure cbConditionTypeChange(Sender: TObject);
    procedure btnAddUnitClick(Sender: TObject);
    procedure btnDeleteUnitClick(Sender: TObject);
    procedure UnitSelectionListDblClick(Sender: TObject);
    procedure EventUnitListDblClick(Sender: TObject);
    procedure btnDeleteLastUnitClick(Sender: TObject);
    procedure btnDeleteAllUnitsClick(Sender: TObject);
  private
    tmp_event: TEvent;
    tmp_condition: TCondition;
  public
    procedure fill_grids;

    procedure select_event(index: integer);
    procedure fill_event_ui(event_valid: boolean);
    procedure fill_event_ui_panel(panel: TPanel; var panel_top: integer);
    procedure fill_event_unit_list;
    procedure fill_event_condition_list;
    procedure change_event_type(event_type: integer);

    procedure select_condition(index: integer);
    procedure fill_condition_ui(condition_valid: boolean);
    procedure fill_condition_ui_panel(panel: TPanel; var panel_top: integer);
    procedure change_condition_type(condition_type: integer);
  end;

var
  EventDialog: TEventDialog;

implementation

uses stringtable;

{$R *.dfm}

{ TEventDialog }

procedure TEventDialog.FormCreate(Sender: TObject);
var
  i: integer;
begin
  // Initialize window size and position
  Left := 0;
  Top := 0;
  Width := 1280;
  Height := 720;
  // Initialize event grid
  EventGrid.Cells[0,0] := '#';
  EventGrid.ColWidths[0] := 20;
  EventGrid.Cells[1,0] := 'Event type';
  EventGrid.ColWidths[1] := 100;
  EventGrid.Cells[2,0] := 'Position';
  EventGrid.ColWidths[2] := 50;
  EventGrid.Cells[3,0] := 'Player';
  EventGrid.ColWidths[3] := 64;
  EventGrid.Cells[4,0] := 'Contents';
  EventGrid.ColWidths[4] := 500;
  EventGrid.Cells[5,0] := 'Conditions';
  EventGrid.ColWidths[5] := 1140;
  for i:= 1 to 64 do
  begin
    EventGrid.Cells[0,i] := inttostr(i-1);
  end;
  // Initialize condition grid
  ConditionGrid.Cells[0,0] := '#';
  ConditionGrid.ColWidths[0] := 20;
  ConditionGrid.Cells[1,0] := 'Condition type';
  ConditionGrid.ColWidths[1] := 92;
  ConditionGrid.Cells[2,0] := 'Player';
  ConditionGrid.ColWidths[2] := 64;
  ConditionGrid.Cells[3,0] := 'Contents';
  ConditionGrid.ColWidths[3] := 120;
  for i:= 1 to 48 do
  begin
    ConditionGrid.Cells[0,i] := inttostr(i-1);
  end;
  // Initialize event type list
  for i:= 0 to Length(event_type_info)-1 do
    cbEventType.Items.Add(inttostr(i) + ' - ' + event_type_info[i].name);
  // Initialize condition type list
  for i:= 0 to Length(condition_type_info)-1 do
    cbConditionType.Items.Add(inttostr(i) + ' - ' + condition_type_info[i].name);
  // Initialize unit selection list
  for i:= 0 to Length(unit_names)-1 do
    UnitSelectionList.Items.Add(inttostr(i) + ' - ' + unit_names[i]);
  // Initialize player list
  for i:= 0 to Length(player_names)-1 do
    cbEventPlayer.Items.Add(inttostr(i) + ' - ' + player_names[i]);
  // Initialize deploy action list
  for i:= 0 to Length(deploy_action)-1 do
    cbDeployAction.Items.Add(inttostr(i) + ' - ' + deploy_action[i]);
  // Initialize allegiance type list
  cbAllegianceSource.Items := cbEventPlayer.Items;
  cbAllegianceTarget.Items := cbEventPlayer.Items;
  for i:= 0 to Length(allegiance_type)-1 do
    cbAllegianceType.Items.Add(inttostr(i) + ' - ' + allegiance_type[i]);
  cbConditionPlayer.Items := cbEventPlayer.Items;
  // Initialize max message ID
  seMessageId.MaxValue := string_table_size;
  // Initialize building types
  for i:= 0 to Length(building_names)-1 do
    cbBuildingType.Items.Add(inttostr(i) + ' - ' + building_names[i]);
  // Initialize unit types
  for i:= 0 to Length(unit_names)-1 do
    cbUnitType.Items.Add(inttostr(i) + ' - ' + unit_names[i]);
  // Initialize comparison functions
  for i:= 0 to Length(comparison_function)-1 do
    cbTimerCompareFunc.Items.Add(comparison_function[i]);
  select_event(0);
  select_condition(0);
end;

procedure TEventDialog.FormShow(Sender: TObject);
begin
  fill_grids;
end;

procedure TEventDialog.fill_grids;
var
  i: integer;
  event: ^TEvent;
  et_info: ^TEventTypeInfo;
  cond: ^TCondition;
  ct_info: ^TConditionTypeInfo;
begin
  // Fill events
  for i := 1 to mis_data.num_events do
  begin
    event := Addr(mis_data.events[i-1]);
    et_info := Addr(event_type_info[event.event_type]);
    // Basic information
    EventGrid.Cells[1,i] := et_info.name;
    if et_info.use_map_position then
      EventGrid.Cells[2,i] := inttostr(event.map_pos_x) + ' , ' + inttostr(event.map_pos_y)
    else
      EventGrid.Cells[2,i] := '';
    if et_info.use_player_index then
      EventGrid.Cells[3,i] := player_names[event.player]
    else
      EventGrid.Cells[3,i] := '';
    // Contents
    EventGrid.Cells[4,i] := get_event_contents(i-1);
    // Conditions
    EventGrid.Cells[5,i] := get_event_conditions(i-1);
  end;
  // Clear unused event rows
  for i := mis_data.num_events + 1 to 64 do
  begin
    EventGrid.Rows[i].Clear;
    EventGrid.Cells[0,i] := inttostr(i-1);
  end;
  // Fill conditions
  for i := 1 to mis_data.num_conditions do
  begin
    cond := Addr(mis_data.conditions[i-1]);
    ct_info := Addr(condition_type_info[cond.condition_type]);
    // Basic information
    ConditionGrid.Cells[1,i] := ct_info.name;
    if ct_info.use_player_index then
      ConditionGrid.Cells[2,i] := player_names[cond.player]
    else
      ConditionGrid.Cells[2,i] := '';
    // Contents
    ConditionGrid.Cells[3,i] := get_condition_contents(i-1, false);
  end;
  // Clear unused condition rows
  for i := mis_data.num_conditions + 1 to 48 do
  begin
    ConditionGrid.Rows[i].Clear;
    ConditionGrid.Cells[0,i] := inttostr(i-1);
  end;
end;

procedure TEventDialog.select_event(index: integer);
begin
  lblEventProperties.Caption := 'Event properties (event ' + inttostr(index) + ')';
  if index >= mis_data.num_events then
  begin
    cbEventType.ItemIndex := -1;
    cbEventType.Enabled := false;
  end else
  begin
    Move(mis_data.events[index], tmp_event, sizeof(TEvent));
    cbEventType.ItemIndex := tmp_event.event_type;
    cbEventType.Enabled := true;
  end;
  fill_event_ui(index < mis_data.num_events);
end;

procedure TEventDialog.fill_event_ui(event_valid: boolean);
var
  panel_top: integer;
begin
  epEventPlayer.Visible := event_valid and event_type_info[tmp_event.event_type].use_player_index;
  epEventPosition.Visible := event_valid and event_type_info[tmp_event.event_type].use_map_position;
  epDeployAction.Visible := false;
  epAllegiance.Visible := false;
  epSetFlag.Visible := false;
  epRadius.Visible := false;
  epEventValue.Visible := false;
  epMessage.Visible := false;
  EventUnitList.Items.Clear;
  EventConditionList.Items.Clear;
  UnitSelectionList.Enabled := event_valid and event_type_info[tmp_event.event_type].use_unit_list;
  if not event_valid then
    exit;
  panel_top := 52;
  if event_type_info[tmp_event.event_type].use_player_index then
    fill_event_ui_panel(epEventPlayer, panel_top);
  if event_type_info[tmp_event.event_type].use_map_position then
    fill_event_ui_panel(epEventPosition, panel_top);
  case EventType(tmp_event.event_type) of
    etReinforcement:  fill_event_ui_panel(epDeployAction, panel_top);
    etAllegiance:     fill_event_ui_panel(epAllegiance, panel_top);
    etRevealMap:      fill_event_ui_panel(epRadius, panel_top);
    etShowMessage:    fill_event_ui_panel(epMessage, panel_top);
    etUnitSpawn:      fill_event_ui_panel(epDeployAction, panel_top);
    etSetFlag:        fill_event_ui_panel(epSetFlag, panel_top);
  end;
  if event_type_info[tmp_event.event_type].value_name <> '' then
    fill_event_ui_panel(epEventValue, panel_top);
  // Fill unit list
  fill_event_unit_list;
  // Fill condition list
  fill_event_condition_list;
end;

procedure TEventDialog.fill_event_ui_panel(panel: TPanel; var panel_top: integer);
begin
  if panel = epEventPlayer then
    cbEventPlayer.ItemIndex := tmp_event.player;
  if panel = epEventPosition then
  begin
    seEventPositionX.Value := tmp_event.map_pos_x;
    seEventPositionY.Value := tmp_event.map_pos_y;
  end;
  if panel = epDeployAction then
    cbDeployAction.ItemIndex := tmp_event.deploy_action;
  if panel = epAllegiance then
  begin
    cbAllegianceSource.ItemIndex := tmp_event.player;
    cbAllegianceTarget.ItemIndex := tmp_event.allegiance_target;
    cbAllegianceType.ItemIndex := tmp_event.allegiance_type;
  end;
  if panel = epSetFlag then
  begin
    seFlagNumber.Value := tmp_event.player;
    if tmp_event.value = 1 then
      rbFlagTrue.Checked := true
    else
      rbFlagFalse.Checked := true;
  end;
  if panel = epRadius then
    seRadius.Value := tmp_event.num_units;
  if panel = epEventValue then
  begin
    lblEventValue.Caption := event_type_info[tmp_event.event_type].value_name + ':';
    edEventValue.Text := inttostr(tmp_event.value);
  end;
  if panel = epMessage then
    seMessageId.Value := tmp_event.message_index;
  panel.Visible := true;
  panel.Top := panel_top;
  panel_top := panel_top + panel.Height + 2;
end;

procedure TEventDialog.fill_event_unit_list;
var
  i: integer;
begin
  for i := 0 to tmp_event.num_units - 1 do
    EventUnitList.Items.Add(unit_names[tmp_event.units[i]]);
end;

procedure TEventDialog.fill_event_condition_list;
var
  i: integer;
  cond_index: integer;
begin
  for i := 0 to tmp_event.num_conditions - 1 do
  begin
    cond_index := tmp_event.condition_index[i];
    EventConditionList.Items.Add(inttostr(cond_index) + ' - ' + condition_type_info[mis_data.conditions[cond_index].condition_type].name + '(' + get_condition_contents(tmp_event.condition_index[i], true) + ')');
    EventConditionList.Checked[i] := tmp_event.condition_not[i] = 1;
  end;
end;

procedure TEventDialog.change_event_type(event_type: integer);
begin
  if tmp_event.event_type = event_type then
    exit;
  tmp_event.event_type := event_type;
  if not event_type_info[event_type].use_player_index then
    tmp_event.player := 0;
  if not event_type_info[event_type].use_map_position then
  begin
    tmp_event.map_pos_x := 0;
    tmp_event.map_pos_y := 0;
  end;
  if not event_type_info[event_type].use_unit_list then
  begin
    tmp_event.num_units := 0;
    FillChar(tmp_event.units, Length(tmp_event.units) + 4, 0);
  end;
  tmp_event.value := 0;
  tmp_event.allegiance_target := 0;
  tmp_event.allegiance_type := 0;
  if (event_type <> Byte(etReinforcement)) and (event_type <> Byte(etUnitSpawn)) then
    tmp_event.deploy_action := 0;
  fill_event_ui(true);
end;

procedure TEventDialog.select_condition(index: integer);
begin
  lblConditionProperties.Caption := 'Condition properties (condition ' + inttostr(index) + ')';
  if index >= mis_data.num_conditions then
  begin
    cbConditionType.ItemIndex := -1;
    cbConditionType.Enabled := false;
  end else
  begin
    Move(mis_data.conditions[index], tmp_condition, sizeof(TCondition));
    cbConditionType.ItemIndex := tmp_condition.condition_type;
    cbConditionType.Enabled := true;
  end;
  fill_condition_ui(index < mis_data.num_conditions);
end;

procedure TEventDialog.fill_condition_ui(condition_valid: boolean);
var
  panel_top: integer;
begin
  cpConditionPlayer.Visible := condition_valid and condition_type_info[tmp_condition.condition_type].use_player_index;
  cpConditionPosition.Visible := false;
  cpBuildingType.Visible := false;
  cpUnitType.Visible := false;
  cpTimer.Visible := false;
  cpInterval.Visible := false;
  cpConditionValue.Visible := false;
  cpCasualties.Visible := false;
  cpCreditsAmount.Visible := false;
  if not condition_valid then
    exit;
  panel_top := 52;
  if condition_type_info[tmp_condition.condition_type].use_player_index then
    fill_condition_ui_panel(cpConditionPlayer, panel_top);
  case ConditionType(tmp_condition.condition_type) of
    ctBuildingExists: fill_condition_ui_panel(cpBuildingType, panel_top);
    ctUnitExists:     fill_condition_ui_panel(cpUnitType, panel_top);
    ctInterval:       fill_condition_ui_panel(cpInterval, panel_top);
    ctTimer:          fill_condition_ui_panel(cpTimer, panel_top);
    ctCasualties:     fill_condition_ui_panel(cpCasualties, panel_top);
    ctTileRevealed:   fill_condition_ui_panel(cpConditionPosition, panel_top);
    ctSpiceHarvested: fill_condition_ui_panel(cpCreditsAmount, panel_top);
  end;
  if condition_type_info[tmp_condition.condition_type].value_name <> '' then
    fill_condition_ui_panel(cpConditionValue, panel_top);
end;

procedure TEventDialog.fill_condition_ui_panel(panel: TPanel;
  var panel_top: integer);
begin
  if panel = cpConditionPlayer then
    cbConditionPlayer.ItemIndex := tmp_condition.player;
  if panel = cpConditionPosition then
  begin
    seConditionPositionX.Value := tmp_condition.map_pos_x;
    seConditionPositionY.Value := tmp_condition.map_pos_y;
  end;
  if panel = cpBuildingType then
    cbBuildingType.ItemIndex := tmp_condition.building_type;
  if panel = cpUnitType then
    cbUnitType.ItemIndex := tmp_condition.unit_type_or_comparison_function;
  if panel = cpTimer then
  begin
    cbTimerCompareFunc.ItemIndex := tmp_condition.unit_type_or_comparison_function;
    edTimerTime.Text := inttostr(tmp_condition.time_amount);
  end;
  if panel = cpInterval then
  begin
    edStartDelay.Text := inttostr(tmp_condition.start_delay);
    edInterval.Text := inttostr(tmp_condition.time_amount);
  end;
  if panel = cpConditionValue then
  begin
    lblConditionValue.Caption := condition_type_info[tmp_condition.condition_type].value_name + ':';
    seConditionValue.Value := tmp_condition.value;
  end;
  if panel = cpCasualties then
  begin
    edCasualtyThreshold1.Text := inttostr(tmp_condition.value);
    edCasualtyThreshold2.Text := inttohex(tmp_condition.casualty_flags, 8);
  end;
  if panel = cpCreditsAmount then
    edCreditsAmount.Text := inttostr(tmp_condition.value);
  panel.Visible := true;
  panel.Top := panel_top;
  panel_top := panel_top + panel.Height + 2;
end;

procedure TEventDialog.change_condition_type(condition_type: integer);
begin
  if tmp_condition.condition_type = condition_type then
    exit;
  tmp_condition.condition_type := condition_type;
  if not condition_type_info[condition_type].use_player_index then
    tmp_condition.player := 0;
  tmp_condition.time_amount := 0;
  tmp_condition.start_delay := 0;
  tmp_condition.value := 0;
  tmp_condition.map_pos_x := 0;
  tmp_condition.map_pos_y := 0;
  tmp_condition.casualty_flags := 0;
  tmp_condition.building_type := 0;
  tmp_condition.unit_type_or_comparison_function := 0;
  fill_condition_ui(true);
end;

procedure TEventDialog.seMessageIdChange(Sender: TObject);
begin
  if StrToIntDef(seMessageId.Text,0) < string_table_size then
    edMessageText.Text := string_table[StrToIntDef(seMessageId.Text,0)].text;
end;

procedure TEventDialog.EventGridSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  select_event(ARow-1);
end;

procedure TEventDialog.ConditionGridSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  select_condition(ARow-1);
end;

procedure TEventDialog.cbEventTypeChange(Sender: TObject);
begin
  change_event_type(cbEventType.ItemIndex);
end;

procedure TEventDialog.cbConditionTypeChange(Sender: TObject);
begin
  change_condition_type(cbConditionType.ItemIndex);
end;

procedure TEventDialog.btnAddUnitClick(Sender: TObject);
begin
  if tmp_event.num_units = (Length(tmp_event.units) + 4) then
  begin
    beep;
    exit;
  end;
  tmp_event.units[tmp_event.num_units] := UnitSelectionList.ItemIndex;
  inc(tmp_event.num_units);
  EventUnitList.Items.Add(unit_names[UnitSelectionList.ItemIndex]);
end;

procedure TEventDialog.btnDeleteUnitClick(Sender: TObject);
var
  i: integer;
begin
  if EventUnitList.ItemIndex = -1 then
    exit;
  for i := EventUnitList.ItemIndex to tmp_event.num_units - 2 do
    tmp_event.units[i] := tmp_event.units[i+1];
  tmp_event.units[tmp_event.num_units - 1] := 0;
  dec(tmp_event.num_units);
  EventUnitList.Items.Delete(EventUnitList.ItemIndex);
end;

procedure TEventDialog.UnitSelectionListDblClick(Sender: TObject);
begin
  btnAddUnitClick(Sender);
end;

procedure TEventDialog.EventUnitListDblClick(Sender: TObject);
begin
  btnDeleteUnitClick(Sender);
end;

procedure TEventDialog.btnDeleteLastUnitClick(Sender: TObject);
begin
  if tmp_event.num_units = 0 then
  begin
    beep;
    exit;
  end;
  tmp_event.units[tmp_event.num_units-1] := 0;
  dec(tmp_event.num_units);
  EventUnitList.Items.Delete(tmp_event.num_units);
end;

procedure TEventDialog.btnDeleteAllUnitsClick(Sender: TObject);
begin
  tmp_event.num_units := 0;
  FillChar(tmp_event.units, Length(tmp_event.units) + 4, 0);
  EventUnitList.Items.Clear;
end;

end.
