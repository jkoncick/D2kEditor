unit event_dialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, ExtCtrls, StdCtrls, CheckLst, Spin, mis_file, Buttons,
  Menus;

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
    cpCasualties: TPanel;
    lblCasualtyFlags: TLabel;
    edCasualtyFlags: TEdit;
    epMessage: TPanel;
    lblMessage: TLabel;
    seMessageId: TSpinEdit;
    edMessageText: TEdit;
    btnApplyEventChanges: TBitBtn;
    btnApplyConditionChanges: TBitBtn;
    edConditionValue: TEdit;
    EventGridPopupMenu: TPopupMenu;
    Addevent1: TMenuItem;
    Deleteselectedevent1: TMenuItem;
    Deletelastevent1: TMenuItem;
    ConditionGridPopupMenu: TPopupMenu;
    Addcondition1: TMenuItem;
    Deleteselectedcondition1: TMenuItem;
    Deletelastcondition1: TMenuItem;
    MoveUp1: TMenuItem;
    MoveDown1: TMenuItem;
    Insertevent1: TMenuItem;
    Duplicateevent1: TMenuItem;
    Duplicatecondition1: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
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
    procedure EventConditionListClickCheck(Sender: TObject);
    procedure btnAddConditionClick(Sender: TObject);
    procedure btnDeleteConditionClick(Sender: TObject);
    procedure ConditionGridDblClick(Sender: TObject);
    procedure btnDeleteLastConditionClick(Sender: TObject);
    procedure btnDeleteAllConditionsClick(Sender: TObject);
    procedure EventGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure EventGridMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure EventGridMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure ConditionGridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure ConditionGridMouseWheelDown(Sender: TObject;
      Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure ConditionGridMouseWheelUp(Sender: TObject;
      Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure btnApplyEventChangesClick(Sender: TObject);
    procedure btnApplyConditionChangesClick(Sender: TObject);
    procedure EventConditionListDblClick(Sender: TObject);
    procedure btnEventPositionGotoMapClick(Sender: TObject);
    procedure btnConditionPositionGotoMapClick(Sender: TObject);
    procedure Addevent1Click(Sender: TObject);
    procedure Insertevent1Click(Sender: TObject);
    procedure Duplicateevent1Click(Sender: TObject);
    procedure Deleteselectedevent1Click(Sender: TObject);
    procedure Deletelastevent1Click(Sender: TObject);
    procedure MoveUp1Click(Sender: TObject);
    procedure MoveDown1Click(Sender: TObject);
    procedure Addcondition1Click(Sender: TObject);
    procedure Duplicatecondition1Click(Sender: TObject);
    procedure Deleteselectedcondition1Click(Sender: TObject);
    procedure Deletelastcondition1Click(Sender: TObject);
    procedure seFlagNumberChange(Sender: TObject);
  private
    tmp_event: TEvent;
    tmp_condition: TCondition;
    selected_event: integer;
    selected_condition: integer;

    condition_position: boolean;
  public
    procedure update_contents;
    procedure fill_grids;

    procedure select_event(index: integer);
    procedure fill_event_ui(event_valid: boolean);
    procedure fill_event_ui_panel(panel: TPanel; var panel_top: integer);
    procedure fill_event_unit_list;
    procedure fill_event_condition_list;
    procedure change_event_type(event_type: integer);
    procedure apply_event_changes;

    procedure select_condition(index: integer);
    procedure fill_condition_ui(condition_valid: boolean);
    procedure fill_condition_ui_panel(panel: TPanel; var panel_top: integer);
    procedure change_condition_type(condition_type: integer);
    procedure apply_condition_changes;

    procedure finish_event_position_selection(x, y: integer);
  end;

var
  EventDialog: TEventDialog;

implementation

uses main, string_table;

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

procedure TEventDialog.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key = 27 then
    Close;
  if key = 123 then
    MainWindow.Show;
end;

procedure TEventDialog.update_contents;
begin
  fill_grids;
  select_event(selected_event);
  select_condition(selected_condition);
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
  selected_event := index;
  lblEventProperties.Caption := 'Event properties (event ' + inttostr(index) + ')';
  if index >= mis_data.num_events then
  begin
    FillChar(tmp_event, sizeof(TEvent), 0);
    cbEventType.ItemIndex := -1;
    cbEventType.Enabled := false;
    btnApplyEventChanges.Enabled := false;
  end else
  begin
    tmp_event := mis_data.events[index];
    cbEventType.ItemIndex := tmp_event.event_type;
    cbEventType.Enabled := true;
    btnApplyEventChanges.Enabled := true;
  end;
  fill_event_ui(index < mis_data.num_events);
end;

procedure TEventDialog.fill_event_ui(event_valid: boolean);
var
  panel_top: integer;
begin
  epEventPlayer.Visible := event_valid and event_type_info[tmp_event.event_type].use_player_index;
  epEventPosition.Visible := event_valid and event_type_info[tmp_event.event_type].use_map_position;
  epDeployAction.Visible := event_valid and ((tmp_event.event_type = Byte(etReinforcement)) or (tmp_event.event_type = Byte(etUnitSpawn)));
  epAllegiance.Visible := tmp_event.event_type = Byte(etAllegiance);
  epSetFlag.Visible := tmp_event.event_type = Byte(etSetFlag);
  epRadius.Visible := tmp_event.event_type = Byte(etRevealMap);
  epEventValue.Visible := event_valid and (event_type_info[tmp_event.event_type].value_name <> '');
  epMessage.Visible := tmp_event.event_type = Byte(etShowMessage);
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
  if event_type_info[mis_data.events[selected_event].event_type].use_unit_list then
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

procedure TEventDialog.apply_event_changes;
var
  event_type: integer;
begin
  event_type := tmp_event.event_type;
  if event_type_info[event_type].use_player_index then
    tmp_event.player := cbEventPlayer.ItemIndex;
  if event_type_info[event_type].use_map_position then
  begin
    tmp_event.map_pos_x := seEventPositionX.Value;
    tmp_event.map_pos_y := seEventPositionY.Value;
  end;
  if event_type_info[event_type].value_name <> '' then
    tmp_event.value := strtoint(edEventValue.Text);
  case EventType(event_type) of
    etReinforcement: tmp_event.deploy_action := cbDeployAction.ItemIndex;
    etAllegiance:
    begin
      tmp_event.player := cbAllegianceSource.ItemIndex;
      tmp_event.allegiance_target := cbAllegianceTarget.ItemIndex;
      tmp_event.allegiance_type := cbAllegianceType.ItemIndex;
    end;
    etRevealMap: tmp_event.num_units := seRadius.Value;
    etShowMessage: tmp_event.message_index := seMessageId.Value;
    etUnitSpawn: tmp_event.deploy_action := cbDeployAction.ItemIndex;
    etSetFlag:
    begin
      tmp_event.player := seFlagNumber.Value;
      if rbFlagTrue.Checked then
        tmp_event.value := 1
      else
        tmp_event.value := 0;
    end;
  end;
  mis_data.events[selected_event] := tmp_event;
  fill_grids;
  // Update event markers on map if event has position
  if event_type_info[event_type].use_map_position then
  begin
    process_event_markers;
    MainWindow.render_map;
  end;
end;

procedure TEventDialog.select_condition(index: integer);
begin
  selected_condition := index;
  lblConditionProperties.Caption := 'Condition properties (condition ' + inttostr(index) + ')';
  if index >= mis_data.num_conditions then
  begin
    FillChar(tmp_condition, sizeof(TCondition), 0);
    cbConditionType.ItemIndex := -1;
    cbConditionType.Enabled := false;
    btnApplyConditionChanges.Enabled := false;
  end else
  begin
    tmp_condition := mis_data.conditions[index];
    cbConditionType.ItemIndex := tmp_condition.condition_type;
    cbConditionType.Enabled := true;
    btnApplyConditionChanges.Enabled := true;
  end;
  fill_condition_ui(index < mis_data.num_conditions);
end;

procedure TEventDialog.fill_condition_ui(condition_valid: boolean);
var
  panel_top: integer;
begin
  cpConditionPlayer.Visible := condition_valid and condition_type_info[tmp_condition.condition_type].use_player_index;
  cpConditionPosition.Visible := tmp_condition.condition_type = Byte(ctTileRevealed);
  cpBuildingType.Visible := condition_valid and (tmp_condition.condition_type = Byte(ctBuildingExists));
  cpUnitType.Visible := tmp_condition.condition_type = Byte(ctUnitExists);
  cpTimer.Visible := tmp_condition.condition_type = Byte(ctTimer);
  cpInterval.Visible := tmp_condition.condition_type = Byte(ctInterval);
  cpConditionValue.Visible := condition_valid and (condition_type_info[tmp_condition.condition_type].value_name <> '');
  cpCasualties.Visible := tmp_condition.condition_type = Byte(ctCasualties);
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
    edConditionValue.Text := inttostr(tmp_condition.value);
  end;
  if panel = cpCasualties then
    edCasualtyFlags.Text := inttohex(tmp_condition.casualty_flags, 8);
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
  if (condition_type = Byte(ctTileRevealed)) or (condition_type = Byte(ctBaseDestroyed)) or (condition_type = Byte(ctUnitsDestroyed)) then
    tmp_condition.value := 1;
  fill_condition_ui(true);
end;

procedure TEventDialog.apply_condition_changes;
var
  condition_type: integer;
begin
  condition_type := tmp_condition.condition_type;
  if condition_type_info[condition_type].use_player_index then
    tmp_condition.player := cbConditionPlayer.ItemIndex;
  if condition_type_info[condition_type].value_name <> '' then
    tmp_condition.value := strtoint(edConditionValue.Text);
  case ConditionType(condition_type) of
    ctBuildingExists: tmp_condition.building_type := cbBuildingType.ItemIndex;
    ctUnitExists: tmp_condition.unit_type_or_comparison_function := cbUnitType.ItemIndex;
    ctInterval:
    begin
      tmp_condition.time_amount := strtoint(edInterval.Text);
      tmp_condition.start_delay := strtoint(edStartDelay.Text);
    end;
    ctTimer:
    begin
      tmp_condition.time_amount := strtoint(edTimerTime.Text);
      tmp_condition.unit_type_or_comparison_function := cbTimerCompareFunc.ItemIndex;
    end;
    ctCasualties:
      tmp_condition.casualty_flags := strtoint('$'+edCasualtyFlags.Text);
    ctTileRevealed:
    begin
      tmp_condition.map_pos_x := seConditionPositionX.Value;
      tmp_condition.map_pos_y := seConditionPositionY.Value;
    end;
  end;
  mis_data.conditions[selected_condition] := tmp_condition;
  fill_grids;
  EventConditionList.Clear;
  fill_event_condition_list;
  // Update event markers on map if condition has position
  if condition_type = Byte(ctTileRevealed) then
  begin
    process_event_markers;
    MainWindow.render_map;
  end;
end;

procedure TEventDialog.finish_event_position_selection(x, y: integer);
begin
  if not condition_position then
  begin
    seEventPositionX.Value := x;
    seEventPositionY.Value := y;
  end else
  begin
    seConditionPositionX.Value := x;
    seConditionPositionY.Value := y;
  end;
end;

procedure TEventDialog.seMessageIdChange(Sender: TObject);
begin
  edMessageText.Text := StringTable.get_text(StrToIntDef(seMessageId.Text,0));
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
  if UnitSelectionList.ItemIndex = -1 then
    exit;
  if selected_event >= mis_data.num_events then
    exit;
  if not event_type_info[mis_data.events[selected_event].event_type].use_unit_list then
    exit;
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

procedure TEventDialog.EventConditionListClickCheck(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to tmp_event.num_conditions -1 do
    if EventConditionList.Checked[i] then
      tmp_event.condition_not[i] := 1
    else
      tmp_event.condition_not[i] := 0;
end;

procedure TEventDialog.btnAddConditionClick(Sender: TObject);
var
  i: integer;
begin
  // Check if selected event is valid
  if selected_event >= mis_data.num_events then
    exit;
  // Check if selected condition is valid
  if selected_condition >= mis_data.num_conditions then
    exit;
  // Check if condition already exists in list
  for i := 0 to tmp_event.num_conditions -1 do
    if tmp_event.condition_index[i] = selected_condition then
      exit;
  // Check if reached maximum number of conditions in event
  if tmp_event.num_conditions = Length(tmp_event.condition_index) then
  begin
    beep;
    exit;
  end;
  tmp_event.condition_index[tmp_event.num_conditions] := selected_condition;
  tmp_event.condition_not[tmp_event.num_conditions] := 0;
  inc(tmp_event.num_conditions);
  EventConditionList.Items.Add(inttostr(selected_condition) + ' - ' + condition_type_info[mis_data.conditions[selected_condition].condition_type].name + '(' + get_condition_contents(selected_condition, true) + ')');
end;

procedure TEventDialog.btnDeleteConditionClick(Sender: TObject);
var
  i: integer;
begin
  if EventConditionList.ItemIndex = -1 then
    exit;
  for i := EventConditionList.ItemIndex to tmp_event.num_conditions - 2 do
  begin
    tmp_event.condition_index[i] := tmp_event.condition_index[i+1];
    tmp_event.condition_not[i] := tmp_event.condition_not[i+1];
  end;
  tmp_event.condition_index[tmp_event.num_conditions - 1] := 0;
  tmp_event.condition_not[tmp_event.num_conditions - 1] := 0;
  dec(tmp_event.num_conditions);
  EventConditionList.Items.Delete(EventConditionList.ItemIndex);
end;

procedure TEventDialog.ConditionGridDblClick(Sender: TObject);
begin
  btnAddConditionClick(Sender);
end;

procedure TEventDialog.btnDeleteLastConditionClick(Sender: TObject);
begin
  if tmp_event.num_conditions = 0 then
  begin
    beep;
    exit;
  end;
  tmp_event.condition_index[tmp_event.num_conditions-1] := 0;
  tmp_event.condition_not[tmp_event.num_conditions-1] := 0;
  dec(tmp_event.num_conditions);
  EventConditionList.Items.Delete(tmp_event.num_conditions);
end;

procedure TEventDialog.btnDeleteAllConditionsClick(Sender: TObject);
begin
  tmp_event.num_conditions := 0;
  FillChar(tmp_event.condition_index, Length(tmp_event.condition_index), 0);
  FillChar(tmp_event.condition_not, Length(tmp_event.condition_not), 0);
  EventConditionList.Items.Clear;
end;

procedure TEventDialog.EventGridMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  row: integer;
begin
  if Button = mbRight then
  begin
    EventGrid.MouseToCell(X,Y,X,row);
    if row > 0 then
      EventGrid.Row := row;
  end;
end;

procedure TEventDialog.EventGridMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if EventGrid.TopRow < (Length(mis_data.events) + 1 - EventGrid.VisibleRowCount) then
    EventGrid.TopRow := EventGrid.TopRow + 1;
  Handled := true;
end;

procedure TEventDialog.EventGridMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if EventGrid.TopRow > 1 then
    EventGrid.TopRow := EventGrid.TopRow - 1;
  Handled := true;
end;

procedure TEventDialog.ConditionGridMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  row: integer;
begin
  if Button = mbRight then
  begin
    ConditionGrid.MouseToCell(X,Y,X,row);
    if row > 0 then
      ConditionGrid.Row := row;
  end;
end;

procedure TEventDialog.ConditionGridMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if ConditionGrid.TopRow < (Length(mis_data.conditions) + 1 - ConditionGrid.VisibleRowCount) then
    ConditionGrid.TopRow := ConditionGrid.TopRow + 1;
  Handled := true;
end;

procedure TEventDialog.ConditionGridMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if ConditionGrid.TopRow > 1 then
    ConditionGrid.TopRow := ConditionGrid.TopRow - 1;
  Handled := true;
end;

procedure TEventDialog.btnApplyEventChangesClick(Sender: TObject);
begin
  apply_event_changes;
end;

procedure TEventDialog.btnApplyConditionChangesClick(Sender: TObject);
begin
  apply_condition_changes;
end;

procedure TEventDialog.EventConditionListDblClick(Sender: TObject);
begin
  ConditionGrid.Row := tmp_event.condition_index[EventConditionList.ItemIndex] + 1;
end;

procedure TEventDialog.btnEventPositionGotoMapClick(Sender: TObject);
begin
  MainWindow.start_event_position_selection(seEventPositionX.Value, seEventPositionY.Value);
  condition_position := false;
  close;
end;

procedure TEventDialog.btnConditionPositionGotoMapClick(Sender: TObject);
begin
  MainWindow.start_event_position_selection(seConditionPositionX.Value, seConditionPositionY.Value);
  condition_position := true;
  close;
end;

procedure TEventDialog.Addevent1Click(Sender: TObject);
begin
  if mis_data.num_events = Length(mis_data.events) then
  begin
    beep;
    exit;
  end;
  inc(mis_data.num_events);
  fill_grids;
  if EventGrid.Row = mis_data.num_events then
    select_event(EventGrid.Row-1)
  else
    EventGrid.Row := mis_data.num_events;
end;

procedure TEventDialog.Insertevent1Click(Sender: TObject);
var
  i: integer;
begin
  if mis_data.num_events = Length(mis_data.events) then
  begin
    beep;
    exit;
  end;
  if selected_event >= mis_data.num_events then
    exit;
  for i := mis_data.num_events downto selected_event + 1 do
    mis_data.events[i] := mis_data.events[i-1];
  FillChar(mis_data.events[selected_event], sizeof(TEvent), 0);
  inc(mis_data.num_events);
  fill_grids;
  select_event(selected_event);
end;

procedure TEventDialog.Duplicateevent1Click(Sender: TObject);
var
  i: integer;
begin
  if mis_data.num_events = Length(mis_data.events) then
  begin
    beep;
    exit;
  end;
  if selected_event >= mis_data.num_events then
    exit;
  for i := mis_data.num_events downto selected_event + 1 do
    mis_data.events[i] := mis_data.events[i-1];
  inc(mis_data.num_events);
  fill_grids;
  EventGrid.Row := selected_event + 2;
end;

procedure TEventDialog.Deleteselectedevent1Click(Sender: TObject);
var
  i: integer;
begin
  if selected_event >= mis_data.num_events then
    exit;
  for i := selected_event to mis_data.num_events - 2 do
    mis_data.events[i] := mis_data.events[i+1];
  FillChar(mis_data.events[mis_data.num_events - 1], sizeof(TEvent), 0);
  dec(mis_data.num_events);
  fill_grids;
  select_event(selected_event);
end;

procedure TEventDialog.Deletelastevent1Click(Sender: TObject);
begin
  if mis_data.num_events = 0 then
  begin
    beep;
    exit;
  end;
  FillChar(mis_data.events[mis_data.num_events - 1], sizeof(TEvent), 0);
  dec(mis_data.num_events);
  fill_grids;
  select_event(selected_event);
end;

procedure TEventDialog.MoveUp1Click(Sender: TObject);
begin
  if selected_event = 0 then
    exit;
  tmp_event := mis_data.events[selected_event];
  mis_data.events[selected_event] := mis_data.events[selected_event - 1];
  mis_data.events[selected_event - 1] := tmp_event;
  fill_grids;
  EventGrid.Row := selected_event;
end;

procedure TEventDialog.MoveDown1Click(Sender: TObject);
begin
  if selected_event >= mis_data.num_events - 1 then
    exit;
  tmp_event := mis_data.events[selected_event];
  mis_data.events[selected_event] := mis_data.events[selected_event + 1];
  mis_data.events[selected_event + 1] := tmp_event;
  fill_grids;
  EventGrid.Row := selected_event + 2;
end;

procedure TEventDialog.Addcondition1Click(Sender: TObject);
begin
  if mis_data.num_conditions = Length(mis_data.conditions) then
  begin
    beep;
    exit;
  end;
  inc(mis_data.num_conditions);
  fill_grids;
  if ConditionGrid.Row = mis_data.num_conditions then
    select_condition(ConditionGrid.Row-1)
  else
    ConditionGrid.Row := mis_data.num_conditions;
end;

procedure TEventDialog.Duplicatecondition1Click(Sender: TObject);
begin
  if mis_data.num_conditions = Length(mis_data.conditions) then
  begin
    beep;
    exit;
  end;
  Move(mis_data.conditions[selected_condition], mis_data.conditions[mis_data.num_conditions], sizeof(TCondition));
  inc(mis_data.num_conditions);
  fill_grids;
  ConditionGrid.Row := mis_data.num_conditions;
end;

procedure TEventDialog.Deleteselectedcondition1Click(Sender: TObject);
begin
  if selected_condition >= mis_data.num_conditions then
    exit;
  if Application.MessageBox('Do you really want to delete condition? All condition references will be updated.', 'Delete condition', MB_YESNO or MB_ICONQUESTION) = IDNO then
    exit;
  delete_condition(selected_condition);
  update_contents;
end;

procedure TEventDialog.Deletelastcondition1Click(Sender: TObject);
begin
  if selected_condition >= mis_data.num_conditions then
    exit;
  if Application.MessageBox('Do you really want to delete condition? All condition references will be updated.', 'Delete condition', MB_YESNO or MB_ICONQUESTION) = IDNO then
    exit;
  delete_condition(mis_data.num_conditions - 1);
  update_contents;
end;

procedure TEventDialog.seFlagNumberChange(Sender: TObject);
begin
  if mis_data.conditions[StrToIntDef(seFlagNumber.Text,0)].condition_type = Byte(ctFlag) then
    seFlagNumber.Color := clWhite
  else
    seFlagNumber.Color := clRed;
end;

end.
