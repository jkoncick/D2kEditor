unit event_dialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, ExtCtrls, StdCtrls, CheckLst, Spin, Buttons, Menus,
  _mission, _eventconfig, _utils;

type
  CreateEventType = (ceUnitSpawn, ceHarvRepl, ceAnnihMsg);

type
  TCoordControlGroup = record
    coorddef: TCoordDefinitionPtr;
    data_ptr: Pointer;
    struct_def: TStructDefinitionPtr;
    offset_x: integer;
    offset_y: integer;
    container: TPanel;
    caption: TLabel;
    spin_x: TSpinEdit;
    spin_y: TSpinEdit;
    btn_select: TButton;
  end;

  TCoordControlGroupPtr = ^TCoordControlGroup;

type
  TArgControlGroup = record
    argdef: TArgDefinitionPtr;
    data_ptr: Pointer;
    struct_def: TStructDefinitionPtr;
    struct_member: integer;
    container: TPanel;
    caption: TLabel;
    text_edit: TEdit;
    spin_edit: TSpinEdit;
    combo_box: TCombobox;
    check_box: TCheckBox;
    radio_false: TRadioButton;
    radio_true: TRadioButton;
  end;

  TArgControlGroupPtr = ^TArgControlGroup;

type
  TEventDialog = class(TForm)
    EventGrid: TStringGrid;
    LowerPanel: TPanel;
    ConditionGrid: TStringGrid;
    Splitter1: TSplitter;
    EventPropertiesPanel: TPanel;
    EventDataPanel: TPanel;
    EventConditionListPanel: TPanel;
    ConditionPropertiesPanel: TPanel;
    EventConditionList: TCheckListBox;
    EventConditionListLabelPanel: TPanel;
    lblEventConditions: TLabel;
    btnAddCondition: TButton;
    btnDeleteCondition: TButton;
    btnDeleteLastCondition: TButton;
    btnDeleteAllConditions: TButton;
    lblEventNumber: TLabel;
    lblConditionProperties: TLabel;
    lblEventType: TLabel;
    cbxEventType: TComboBox;
    cbxConditionType: TComboBox;
    lblConditionType: TLabel;
    edpMessage: TPanel;
    lblMessage: TLabel;
    seMessageId: TSpinEdit;
    edMessageText: TEdit;
    btnApplyEventChanges: TBitBtn;
    btnApplyConditionChanges: TBitBtn;
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
    N1: TMenuItem;
    Createevent1: TMenuItem;
    Unitspawn1: TMenuItem;
    Harvesterreplacement1: TMenuItem;
    Annihilatemessage1: TMenuItem;
    Createrunonceflag1: TMenuItem;
    CreateEventsPanel: TPanel;
    lblCreateEvents: TLabel;
    btnCreateEventsCancel: TBitBtn;
    btnCreateEventsOk: TBitBtn;
    lblCreateEventsPlayer: TLabel;
    cbCreateEventsPlayer: TComboBox;
    lblCreateEventsCount: TLabel;
    seCreateEventsNum: TSpinEdit;
    cbCreateEventsAllocIndex: TCheckBox;
    btnPlusCondition: TButton;
    edEventNote: TEdit;
    lblEventNote: TLabel;
    lblConditionNote: TLabel;
    edConditionNote: TEdit;
    btnCustomMsgText: TButton;
    EventConditionListButtonPanel: TPanel;
    btnEventConditionListCopy: TButton;
    btnEventConditionListPaste: TButton;
    edpMusic: TPanel;
    lblMusic: TLabel;
    BevelSizeHolder: TBevel;
    cbMusicName: TComboBox;
    cbMarkEventsHavingCondition: TCheckBox;
    MoveUp2: TMenuItem;
    MoveDown2: TMenuItem;
    btnMoveConditionUp: TButton;
    btnMoveConditionDown: TButton;
    N2: TMenuItem;
    Showkeyshortcuts1: TMenuItem;
    N3: TMenuItem;
    Showkeyshortcuts2: TMenuItem;
    edpUnitList: TPanel;
    UnitSelectionList: TListBox;
    EventUnitListPaddingPanel: TPanel;
    EventUnitListLabelPanel: TPanel;
    lblUnitSelection: TLabel;
    lblUnitList: TLabel;
    EventUnitList: TListBox;
    btnMoveUnitUp: TButton;
    btnMoveUnitDown: TButton;
    btnDeleteUnit: TButton;
    btnDeleteLastUnit: TButton;
    btnDeleteAllUnits: TButton;
    btnAddUnit: TButton;
    edpByteValues: TPanel;
    sgEventByteValues: TStringGrid;
    lblEventByteValues: TLabel;
    UpperPanel: TPanel;
    lbEventTypeList: TListBox;
    lbConditionTypeList: TListBox;
    edpTileBlock: TPanel;
    imgTileBlock: TImage;
    cbEventAutoBlock: TCheckBox;
    cbEventBlocked: TCheckBox;
    edpTilePairs: TPanel;
    lblTilePairs: TLabel;
    sgTilePairs: TStringGrid;
    imgTilePairs: TImage;
    // Form actions
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShortCut(var Msg: TWMKey; var Handled: Boolean);
    // Event type list actions
    procedure lbEventTypeListDblClick(Sender: TObject);
    // Event grid actions
    procedure EventGridSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure EventGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EventGridMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EventGridMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure EventGridMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure EventGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    // Event grid popup menu actions
    procedure Addevent1Click(Sender: TObject);
    procedure Insertevent1Click(Sender: TObject);
    procedure Duplicateevent1Click(Sender: TObject);
    procedure Deleteselectedevent1Click(Sender: TObject);
    procedure Deletelastevent1Click(Sender: TObject);
    procedure MoveUp1Click(Sender: TObject);
    procedure MoveDown1Click(Sender: TObject);
    procedure Unitspawn1Click(Sender: TObject);
    procedure Harvesterreplacement1Click(Sender: TObject);
    procedure Annihilatemessage1Click(Sender: TObject);
    procedure Createrunonceflag1Click(Sender: TObject);
    procedure Showkeyshortcuts1Click(Sender: TObject);
    // Create events panel actions
    procedure btnCreateEventsCancelClick(Sender: TObject);
    procedure cbCreateEventsPlayerChange(Sender: TObject);
    procedure btnCreateEventsOkClick(Sender: TObject);
    // Event properties panel actions
    procedure cbxEventTypeChange(Sender: TObject);
    procedure cbEventBlockedClick(Sender: TObject);
    procedure seFlagNumberChange(Sender: TObject);
    procedure btnApplyEventChangesClick(Sender: TObject);
    // Event data panel actions
    // -- Unit List
    procedure btnAddUnitClick(Sender: TObject);
    procedure btnDeleteUnitClick(Sender: TObject);
    procedure btnDeleteLastUnitClick(Sender: TObject);
    procedure btnDeleteAllUnitsClick(Sender: TObject);
    procedure btnMoveUnitUpClick(Sender: TObject);
    procedure btnMoveUnitDownClick(Sender: TObject);
    // -- Byte values
    procedure sgEventByteValuesSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: String);
    // -- Message
    procedure seMessageIdChange(Sender: TObject);
    procedure btnCustomMsgTextClick(Sender: TObject);
    // -- Music
    procedure cbMusicNameChange(Sender: TObject);
    // -- Tile block
    procedure imgTileBlockMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    // -- Tile pairs
    procedure sgTilePairsSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: String);
    // Event condition list panel actions
    procedure EventConditionListClickCheck(Sender: TObject);
    procedure EventConditionListDblClick(Sender: TObject);
    procedure btnPlusConditionClick(Sender: TObject);
    procedure btnAddConditionClick(Sender: TObject);
    procedure btnDeleteConditionClick(Sender: TObject);
    procedure btnDeleteLastConditionClick(Sender: TObject);
    procedure btnDeleteAllConditionsClick(Sender: TObject);
    procedure btnMoveConditionUpClick(Sender: TObject);
    procedure btnMoveConditionDownClick(Sender: TObject);
    procedure btnEventConditionListCopyClick(Sender: TObject);
    procedure btnEventConditionListPasteClick(Sender: TObject);
    // Condition type list actions
    procedure lbConditionTypeListDblClick(Sender: TObject);
    // Condition grid actions
    procedure ConditionGridSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure ConditionGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ConditionGridDblClick(Sender: TObject);
    procedure ConditionGridMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ConditionGridMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure ConditionGridMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    // Condition grid popup menu actions
    procedure Addcondition1Click(Sender: TObject);
    procedure Duplicatecondition1Click(Sender: TObject);
    procedure Deleteselectedcondition1Click(Sender: TObject);
    procedure Deletelastcondition1Click(Sender: TObject);
    procedure MoveUp2Click(Sender: TObject);
    procedure MoveDown2Click(Sender: TObject);
    procedure Showkeyshortcuts2Click(Sender: TObject);
    // Condition properties panel actions
    procedure cbxConditionTypeChange(Sender: TObject);
    procedure btnApplyConditionChangesClick(Sender: TObject);
    procedure cbMarkEventsHavingConditionClick(Sender: TObject);
    // Miscellaneous
    procedure PopupMenuPopup(Sender: TObject);
    // Coord control group actions
    procedure CCGCoordinateChange(Sender: TObject);
    procedure CCGBtnSelectClick(Sender: TObject);
    // Arg control group actions
    procedure ACGValueChange(Sender: TObject);
  private
    tmp_event: TEvent;
    tmp_condition: TCondition;
    selected_event: integer;
    selected_condition: integer;

    loading: boolean;
    selected_coord_index: integer;
    create_event_type: CreateEventType;
    notes_enabled: boolean;
    msg_text_is_custom: boolean;
    copy_conditions_from: integer;

    pending_update_contents: boolean;

    cached_lists: array[2..7] of TStringList;
    ccgs: array[0..7] of TCoordControlGroup;
    acgs: array[0..12] of TArgControlGroup;

  public
    // Dispatcher events
    procedure update_event_type_configuration;
    procedure update_contents;
    procedure update_player_list(player_list: TStringList);
    procedure update_structures_list;
    procedure update_sound_list;
    procedure update_tileset;
  private
    procedure enable_mission_ini_features;
    // Fill data procedures
    procedure fill_grids;
    // Event-related procedures
    procedure fill_event_grid_row(index: integer);
    procedure select_event(index: integer);
    procedure fill_event_ui;
    procedure fill_event_data_panel(panel: TPanel; active: boolean);
    procedure fill_event_condition_list;
    procedure change_event_type(new_event_type: integer);
    procedure apply_event_changes;
    // Event data-related procedures
    procedure draw_tile_block;
    procedure draw_tile_pairs;
    // Condition-related prodcedures
    procedure fill_condition_grid_row(index: integer);
    procedure select_condition(index: integer);
    procedure fill_condition_ui;
    procedure change_condition_type(new_condition_type: integer);
    procedure apply_condition_changes;
    // Coord control group procedures
    procedure create_coord_control_group(index: integer; data_ptr: Pointer; struct_def: TStructDefinitionPtr; offset_x, offset_y: integer; parent_panel: TPanel);
    procedure fill_coord_control_group(ccg: TCoordControlGroupPtr; coorddef: TCoordDefinitionPtr; var panel_top: integer);
    // Coord control group procedures
    procedure create_arg_control_group(index: integer; data_ptr: Pointer; struct_def: TStructDefinitionPtr; struct_member: integer; parent_panel: TPanel);
    procedure fill_arg_control_group(acg: TArgControlGroupPtr; argdef: TArgDefinitionPtr; var panel_top: integer);
    procedure fill_arg_combo_box(acg: TArgControlGroupPtr; value: integer; force_update_list: boolean);
  public
    // Procedures called from different forms
    procedure finish_point_selection(x, y: integer);
    procedure finish_area_selection(min_x, max_x, min_y, max_y: integer);
    procedure apply_changes;
  end;

var
  EventDialog: TEventDialog;

implementation

uses Math, main, _missionini, _stringtable, _settings, _structures, _dispatcher, _tileset, _map,
  StrUtils;

{$R *.dfm}

{ TEventDialog }

procedure TEventDialog.FormCreate(Sender: TObject);
var
  i: integer;
  SR: TSearchRec;
  StringList: TStrings;
begin
  // Initialize window size and position
  Left := 0;
  Top := 0;
  Width := 1280;
  Height := 720;
  // Initialize event grid
  EventGrid.RowCount := MAX_EVENTS + 1;
  EventGrid.Cells[0,0] := '#';
  EventGrid.ColWidths[0] := 20;
  EventGrid.Cells[1,0] := 'Event type';
  EventGrid.ColWidths[1] := 90;
  EventGrid.Cells[2,0] := 'Bl.';
  EventGrid.ColWidths[2] := 20;
  EventGrid.Cells[3,0] := 'Position';
  EventGrid.ColWidths[3] := 50;
  EventGrid.Cells[4,0] := 'Player';
  EventGrid.ColWidths[4] := 72;
  EventGrid.Cells[5,0] := 'Contents';
  EventGrid.ColWidths[5] := 400;
  EventGrid.Cells[6,0] := 'Conditions';
  EventGrid.ColWidths[6] := 400;
  EventGrid.Cells[7,0] := 'Note';
  EventGrid.ColWidths[7] := 1140;
  // Initialize condition grid
  ConditionGrid.RowCount := MAX_CONDITIONS + 1;
  ConditionGrid.Cells[0,0] := '#';
  ConditionGrid.ColWidths[0] := 20;
  ConditionGrid.Cells[1,0] := 'Condition type';
  ConditionGrid.ColWidths[1] := 84;
  ConditionGrid.Cells[2,0] := 'Player';
  ConditionGrid.ColWidths[2] := 72;
  ConditionGrid.Cells[3,0] := 'Contents';
  ConditionGrid.ColWidths[3] := 112;
  StringList := TStringList.Create;
  // Initialize music names
  if FindFirst(Settings.GamePath + '\Data\Music\*.AUD', 0, SR) = 0 then
  begin
    repeat
      StringList.Add(SR.Name);
    until FindNext(SR) <> 0;
      FindClose(SR);
  end;
  cbMusicName.Items := StringList;
  StringList.Destroy;
  // Initialize cached lists
  for i := Low(cached_lists) to High(cached_lists) do
    cached_lists[i] := TStringList.Create;
  // Initialize coord control groups
  for i := 0 to High(ccgs) do
  begin
    if i < 4 then
      create_coord_control_group(i, Addr(tmp_event), Addr(event_args_struct_members), i, i + 4, EventPropertiesPanel)
    else
      create_coord_control_group(i, Addr(tmp_condition), Addr(condition_args_struct_members), i - 4 + 12, i - 4 + 16, ConditionPropertiesPanel);
  end;
  // Initialize arg control groups
  for i := 0 to High(acgs) do
  begin
    if i < Length(event_args_struct_members) then
      create_arg_control_group(i, Addr(tmp_event), Addr(event_args_struct_members), i, EventPropertiesPanel)
    else
      create_arg_control_group(i, Addr(tmp_condition), Addr(condition_args_struct_members), i - Length(event_args_struct_members), ConditionPropertiesPanel);
  end;
  //select_event(0);
  select_condition(0);
end;

procedure TEventDialog.FormShow(Sender: TObject);
begin
  if pending_update_contents then
    update_contents;
end;

procedure TEventDialog.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  apply_changes;
end;

procedure TEventDialog.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (key = 13) and CreateEventsPanel.Visible then
    btnCreateEventsOkClick(Sender);
  if (key = 13) and not CreateEventsPanel.Visible then
  begin
    if (ActiveControl = ConditionGrid) then
    begin
      btnApplyConditionChangesClick(nil);
      btnApplyEventChangesClick(nil);
    end
    else if (ActiveControl.Parent = ConditionPropertiesPanel) or
     ((ActiveControl.Parent.Parent <> Nil) and (ActiveControl.Parent.Parent = ConditionPropertiesPanel)) then
      btnApplyConditionChangesClick(nil)
    else
      btnApplyEventChangesClick(nil);
  end;
  if (key = 27) and CreateEventsPanel.Visible then
    btnCreateEventsCancelClick(Sender);
  if (key = 27) and not CreateEventsPanel.Visible then
    Close;
  if key = 123 then // F2
    MainWindow.Show;
  if (key = 107) and (ActiveControl <> edMessageText)  then // Num +
    btnPlusConditionClick(nil);
end;

procedure TEventDialog.FormShortCut(var Msg: TWMKey; var Handled: Boolean);
var
  key: word;
begin
  if Msg.CharCode = 13 then
  begin
    key := 13;
    FormKeyDown(nil, key, []);
    Handled := true;
  end;
end;

procedure TEventDialog.lbEventTypeListDblClick(Sender: TObject);
var
  newpos: integer;
begin
  newpos := Mission.add_event(selected_event + 1, EventConfig.event_type_mapping[lbEventTypeList.ItemIndex]);
  if newpos <> -1 then
  begin
    fill_grids;
    if EventGrid.Row = newpos + 1 then
      select_event(newpos)
    else
      EventGrid.Row := newpos + 1;
  end;
end;

procedure TEventDialog.EventGridSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
begin
  apply_changes;
  if selected_event <> ARow-1 then
    select_event(ARow-1);
end;

procedure TEventDialog.EventGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if EventGrid.Row > Mission.mis_data.num_events then
    exit;
  if (key = ord('C')) then
    btnEventConditionListCopyClick(nil);
  if (key = ord('P')) then
    btnEventConditionListPasteClick(nil);
  // Num0 - Num7 = Set Eevent Player
  //**if eppPlayer.Visible and (key >= 96) and (key <= 103) then
  //**begin
  //**  cbEventPlayer.ItemIndex := key - 96;
  //**end;
  // Space = Go to map
  //**if eppMapPos.Visible and (key = 32) then
  //**begin
  //**  btnEventMapPosGotoMapClick(nil);
  //**end;
end;

procedure TEventDialog.EventGridMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
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

procedure TEventDialog.EventGridMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if EventGrid.TopRow < (Length(Mission.mis_data.events) + 1 - EventGrid.VisibleRowCount) then
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

procedure TEventDialog.EventGridDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  if not cbMarkEventsHavingCondition.Checked then
    exit;
  if (ARow = 0) or (ACol = 0) or (ARow - 1 = selected_event) then
    exit;
  if Mission.check_event_has_condition(ARow - 1, selected_condition) then
  begin
    EventGrid.Canvas.Brush.Color := clYellow;
    EventGrid.Canvas.FillRect(Rect);
    EventGrid.Canvas.TextRect(Rect,Rect.Left+2,Rect.Top+2,EventGrid.Cells[ACol,ARow]);
  end;
end;

procedure TEventDialog.Addevent1Click(Sender: TObject);
begin
  if Mission.add_event(Mission.mis_data.num_events, 0) <> -1 then
  begin
    fill_grids;
    if EventGrid.Row = Mission.mis_data.num_events then
      select_event(EventGrid.Row-1)
    else
      EventGrid.Row := Mission.mis_data.num_events;
  end;
end;

procedure TEventDialog.Insertevent1Click(Sender: TObject);
var
  newpos: integer;
begin
  newpos := Mission.add_event(selected_event, 0);
  if newpos <> -1 then
  begin
    fill_grids;
    if EventGrid.Row = newpos + 1 then
      select_event(newpos)
    else
      EventGrid.Row := newpos + 1;
  end;
end;

procedure TEventDialog.Duplicateevent1Click(Sender: TObject);
begin
  if Mission.add_event(selected_event + 1, 0) <> -1 then
  begin
    Mission.mis_data.events[selected_event + 1] := Mission.mis_data.events[selected_event];
    MissionIni.event_notes[selected_event + 1] := MissionIni.event_notes[selected_event];
    fill_grids;
    EventGrid.Row := selected_event + 2;
  end;
end;

procedure TEventDialog.Deleteselectedevent1Click(Sender: TObject);
begin
  if selected_event >= Mission.mis_data.num_events then
    exit;
  Mission.delete_event(selected_event);
  fill_grids;
  select_event(selected_event);
end;

procedure TEventDialog.Deletelastevent1Click(Sender: TObject);
begin
  if Mission.mis_data.num_events = 0 then
    exit;
  Mission.delete_event(Mission.mis_data.num_events - 1);
  fill_grids;
  select_event(selected_event);
end;

procedure TEventDialog.MoveUp1Click(Sender: TObject);
begin
  if selected_event = 0 then
    exit;
  Mission.swap_events(selected_event, selected_event-1);
  Dec(selected_event);
  select_event(selected_event);
  EventGrid.Row := selected_event+1;
  fill_grids;
end;

procedure TEventDialog.MoveDown1Click(Sender: TObject);
begin
  if selected_event >= Mission.mis_data.num_events - 1 then
    exit;
  Mission.swap_events(selected_event, selected_event+1);
  Inc(selected_event);
  select_event(selected_event);
  EventGrid.Row := selected_event+1;
  fill_grids;
end;

procedure TEventDialog.Unitspawn1Click(Sender: TObject);
begin
  CreateEventsPanel.Visible := true;
  lblCreateEvents.Caption := 'Create Unit spawn events';
  seCreateEventsNum.Visible := true;
  lblCreateEventsCount.Visible := true;
  cbCreateEventsAllocIndex.Visible := false;
  create_event_type := ceUnitSpawn;
end;

procedure TEventDialog.Harvesterreplacement1Click(Sender: TObject);
begin
  CreateEventsPanel.Visible := true;
  lblCreateEvents.Caption := 'Create Harvester replacement';
  seCreateEventsNum.Visible := false;
  lblCreateEventsCount.Visible := false;
  cbCreateEventsAllocIndex.Visible := false;
  create_event_type := ceHarvRepl;
end;

procedure TEventDialog.Annihilatemessage1Click(Sender: TObject);
begin
  CreateEventsPanel.Visible := true;
  lblCreateEvents.Caption := 'Create Side annihilated message';
  seCreateEventsNum.Visible := true;
  lblCreateEventsCount.Visible := false;
  cbCreateEventsAllocIndex.Visible := true;
  create_event_type := ceAnnihMsg;
  seCreateEventsNum.Value := cbCreateEventsPlayer.ItemIndex;
end;

procedure TEventDialog.Createrunonceflag1Click(Sender: TObject);
begin
  Mission.add_run_once_flag(selected_event);
  update_contents;
end;

procedure TEventDialog.Showkeyshortcuts1Click(Sender: TObject);
var
  msg: string;
begin
  msg := 'Enter = Apply changes';
  ShowMessage(msg);
end;

procedure TEventDialog.btnCreateEventsCancelClick(Sender: TObject);
begin
  CreateEventsPanel.Visible := false;
  EventGrid.SetFocus;
end;

procedure TEventDialog.cbCreateEventsPlayerChange(Sender: TObject);
begin
  if create_event_type = ceAnnihMsg then
    seCreateEventsNum.Value := cbCreateEventsPlayer.ItemIndex;
end;

procedure TEventDialog.btnCreateEventsOkClick(Sender: TObject);
begin
  EventGrid.Row := Mission.mis_data.num_events + 1;
  case create_event_type of
    ceUnitSpawn: Mission.create_unit_spawn(cbCreateEventsPlayer.ItemIndex, seCreateEventsNum.Value);
    ceHarvRepl: Mission.create_harvester_replacement(cbCreateEventsPlayer.ItemIndex);
    ceAnnihMsg: Mission.create_annihilated_message(cbCreateEventsPlayer.ItemIndex, cbCreateEventsAllocIndex.Checked, seCreateEventsNum.Value);
  end;
  CreateEventsPanel.Visible := false;
  EventGrid.SetFocus;
  fill_grids;
  select_event(EventGrid.Row - 1);
  select_condition(ConditionGrid.Row - 1);
end;

procedure TEventDialog.cbxEventTypeChange(Sender: TObject);
begin
  change_event_type(EventConfig.event_type_mapping[cbxEventType.ItemIndex]);
end;

procedure TEventDialog.cbEventBlockedClick(Sender: TObject);
begin
  if loading then
    exit;
  tmp_event.blocked_flags := 0;
  if cbEventAutoBlock.Checked then
    tmp_event.blocked_flags := tmp_event.blocked_flags or 1;
  if cbEventBlocked.Checked then
    tmp_event.blocked_flags := tmp_event.blocked_flags or 2;
end;

procedure TEventDialog.seFlagNumberChange(Sender: TObject);
begin
  //**if Mission.mis_data.conditions[StrToIntDef(seFlagNumber.Text,0)].condition_type = Byte(ctFlag) then
  //**  seFlagNumber.Color := clWhite
  //**else
  //**  seFlagNumber.Color := clRed;
end;

procedure TEventDialog.btnApplyEventChangesClick(Sender: TObject);
begin
  apply_event_changes;
  EventGrid.SetFocus;
end;

procedure TEventDialog.btnAddUnitClick(Sender: TObject);
begin
  if UnitSelectionList.ItemIndex = -1 then
    exit;
  if selected_event >= Mission.mis_data.num_events then
    exit;
  if tmp_event.amount = Length(tmp_event.data) then
  begin
    beep;
    exit;
  end;
  tmp_event.data[tmp_event.amount] := UnitSelectionList.ItemIndex;
  acgs[1].spin_edit.Value := tmp_event.amount + 1;
  EventUnitList.Items.Add(Structures.get_unit_name_str(UnitSelectionList.ItemIndex));
end;

procedure TEventDialog.btnDeleteUnitClick(Sender: TObject);
var
  i: integer;
begin
  if EventUnitList.ItemIndex = -1 then
    exit;
  for i := EventUnitList.ItemIndex to tmp_event.amount - 2 do
    tmp_event.data[i] := tmp_event.data[i+1];
  tmp_event.data[tmp_event.amount - 1] := 0;
  acgs[1].spin_edit.Value := tmp_event.amount - 1;
  EventUnitList.Items.Delete(EventUnitList.ItemIndex);
end;

procedure TEventDialog.btnDeleteLastUnitClick(Sender: TObject);
begin
  if tmp_event.amount = 0 then
  begin
    beep;
    exit;
  end;
  tmp_event.data[tmp_event.amount-1] := 0;
  acgs[1].spin_edit.Value := tmp_event.amount - 1;
  EventUnitList.Items.Delete(tmp_event.amount);
end;

procedure TEventDialog.btnDeleteAllUnitsClick(Sender: TObject);
begin
  acgs[1].spin_edit.Value := 0;
  FillChar(tmp_event.data, Length(tmp_event.data), 0);
  EventUnitList.Items.Clear;
end;

procedure TEventDialog.btnMoveUnitUpClick(Sender: TObject);
var
  index: integer;
  tmp_unit_name: String;
begin
  index := EventUnitList.ItemIndex;
  if (index = -1) or (index = 0) then
    exit;
  swap_byte(Addr(tmp_event.data[index]), Addr(tmp_event.data[index-1]));
  tmp_unit_name := EventUnitList.Items[index];
  EventUnitList.Items[index] := EventUnitList.Items[index-1];
  EventUnitList.Items[index-1] := tmp_unit_name;
  EventUnitList.ItemIndex := EventUnitList.ItemIndex - 1;
end;

procedure TEventDialog.btnMoveUnitDownClick(Sender: TObject);
var
  index: integer;
  tmp_unit_name: String;
begin
  index := EventUnitList.ItemIndex;
  if (index = -1) or (index = (tmp_event.amount - 1)) then
    exit;
  swap_byte(Addr(tmp_event.data[index]), Addr(tmp_event.data[index+1]));
  tmp_unit_name := EventUnitList.Items[index];
  EventUnitList.Items[index] := EventUnitList.Items[index+1];
  EventUnitList.Items[index+1] := tmp_unit_name;
  EventUnitList.ItemIndex := EventUnitList.ItemIndex + 1;
end;

procedure TEventDialog.sgEventByteValuesSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: String);
var
  index: integer;
begin
  index := ARow * 8 + ACol - 7;
  if index >= 0 then
    tmp_event.data[index] := StrToIntDef(sgEventByteValues.Cells[ACol, ARow], 0);
end;

procedure TEventDialog.seMessageIdChange(Sender: TObject);
var
  value: integer;
begin
  value := StrToIntDef(seMessageId.Text,0);
  set_integer_value(Addr(tmp_event.data), 21, 4, value);
  edMessageText.Text := StringTable.get_text(value, true, msg_text_is_custom);
  edMessageText.ReadOnly := not msg_text_is_custom;
  if not btnCustomMsgText.Visible then
    exit;
  if msg_text_is_custom then
    btnCustomMsgText.Caption := 'Original text'
  else
    btnCustomMsgText.Caption := 'Custom text'
end;

procedure TEventDialog.btnCustomMsgTextClick(Sender: TObject);
begin
  if not msg_text_is_custom then
  begin
    btnCustomMsgText.Caption := 'Original text';
    edMessageText.Text := '';
    edMessageText.ReadOnly := false;
    edMessageText.SetFocus;
    msg_text_is_custom := true;
  end else
  begin
    btnCustomMsgText.Caption := 'Custom text';
    MissionIni.remove_custom_text(seMessageId.Value);
    edMessageText.Text := StringTable.get_text(seMessageId.Value, true, msg_text_is_custom);
    edMessageText.ReadOnly := true;
    fill_grids;
  end;
end;

procedure TEventDialog.cbMusicNameChange(Sender: TObject);
begin
  FillChar(tmp_event.data[0], Length(tmp_event.data), 0);
  Move(cbMusicName.Text[1], tmp_event.data[0], Length(cbMusicName.Text));
end;

procedure TEventDialog.imgTileBlockMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button <> mbRight then
    exit;
  set_integer_value(Addr(tmp_event.data[1]), ((Y div 32) * (tmp_event.coord_x[1]) + (X div 32)) * 2, 2, 65535);
  draw_tile_block;
end;

procedure TEventDialog.sgTilePairsSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: String);
begin
  set_integer_value(Addr(tmp_event.data[1]), (ARow * 2 + ACol) * 2, 2, StrToIntDef(sgTilePairs.Cells[ACol, ARow], 0));
  draw_tile_pairs;
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

procedure TEventDialog.EventConditionListDblClick(Sender: TObject);
begin
  ConditionGrid.Row := tmp_event.condition_index[EventConditionList.ItemIndex] + 1;
  ConditionGrid.SetFocus;
end;

procedure TEventDialog.btnPlusConditionClick(Sender: TObject);
begin
  Addcondition1Click(Sender);
  btnAddConditionClick(Sender);
  ConditionGrid.SetFocus;
end;

procedure TEventDialog.btnAddConditionClick(Sender: TObject);
var
  i: integer;
begin
  // Check if selected event is valid
  if selected_event >= Mission.mis_data.num_events then
    exit;
  // Check if selected condition is valid
  if selected_condition >= Mission.mis_data.num_conditions then
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
  EventConditionList.Items.Add(inttostr(selected_condition) + ' - ' + EventConfig.condition_types[Mission.mis_data.conditions[selected_condition].condition_type].name + '(' + Mission.get_condition_contents(selected_condition, true) + ')');
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

procedure TEventDialog.btnMoveConditionUpClick(Sender: TObject);
var
  index: integer;
  tmp_condition_desc: String;
begin
  index := EventConditionList.ItemIndex;
  if (index = -1) or (index = 0) then
    exit;
  swap_byte(Addr(tmp_event.condition_index[index]), Addr(tmp_event.condition_index[index-1]));
  swap_byte(Addr(tmp_event.condition_not[index]), Addr(tmp_event.condition_not[index-1]));
  tmp_condition_desc := EventConditionList.Items[index];
  EventConditionList.Items[index] := EventConditionList.Items[index-1];
  EventConditionList.Items[index-1] := tmp_condition_desc;
  EventConditionList.Checked[index] := tmp_event.condition_not[index] = 1;
  EventConditionList.Checked[index-1] := tmp_event.condition_not[index-1] = 1;
  EventConditionList.ItemIndex := EventConditionList.ItemIndex - 1;
end;

procedure TEventDialog.btnMoveConditionDownClick(Sender: TObject);
var
  index: integer;
  tmp_condition_desc: String;
begin
  index := EventConditionList.ItemIndex;
  if (index = -1) or (index = (tmp_event.num_conditions - 1)) then
    exit;
  swap_byte(Addr(tmp_event.condition_index[index]), Addr(tmp_event.condition_index[index+1]));
  swap_byte(Addr(tmp_event.condition_not[index]), Addr(tmp_event.condition_not[index+1]));
  tmp_condition_desc := EventConditionList.Items[index];
  EventConditionList.Items[index] := EventConditionList.Items[index+1];
  EventConditionList.Items[index+1] := tmp_condition_desc;
  EventConditionList.Checked[index] := tmp_event.condition_not[index] = 1;
  EventConditionList.Checked[index+1] := tmp_event.condition_not[index+1] = 1;
  EventConditionList.ItemIndex := EventConditionList.ItemIndex + 1;
end;

procedure TEventDialog.btnEventConditionListCopyClick(Sender: TObject);
begin
  copy_conditions_from := selected_event;
  EventGrid.SetFocus;
end;

procedure TEventDialog.btnEventConditionListPasteClick(Sender: TObject);
begin
  tmp_event.num_conditions := Mission.mis_data.events[copy_conditions_from].num_conditions;
  Move(Mission.mis_data.events[copy_conditions_from].condition_index, tmp_event.condition_index, Length(tmp_event.condition_index));
  Move(Mission.mis_data.events[copy_conditions_from].condition_not, tmp_event.condition_not, Length(tmp_event.condition_not));
  EventConditionList.Clear;
  fill_event_condition_list;
  EventGrid.SetFocus;
end;

procedure TEventDialog.lbConditionTypeListDblClick(Sender: TObject);
begin
  if Mission.add_condition(EventConfig.condition_type_mapping[lbConditionTypeList.ItemIndex]) then
  begin
    fill_grids;
    if ConditionGrid.Row = Mission.mis_data.num_conditions then
      select_condition(ConditionGrid.Row-1)
    else
      ConditionGrid.Row := Mission.mis_data.num_conditions;
  end;
end;

procedure TEventDialog.ConditionGridSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  apply_changes;
  if selected_condition <> ARow-1 then
    select_condition(ARow-1);
end;

procedure TEventDialog.ConditionGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if ConditionGrid.Row > Mission.mis_data.num_conditions then
    exit;
  // Num0 - Num7 = Set Condition Player
  //**if cppPlayer.Visible and (key >= 96) and (key <= 103) then
  //**begin
  //**  cbConditionPlayer.SetFocus;
  //**  cbConditionPlayer.ItemIndex := key - 96;
  //**end;
  // Space = Go to map
  //**if cppMapPosition.Visible and (key = 32) then
  //**begin
    //**btnConditionPositionGotoMap.SetFocus;
    //**btnConditionPositionGotoMapClick(nil);
  //**end;
end;

procedure TEventDialog.ConditionGridDblClick(Sender: TObject);
begin
  btnAddConditionClick(Sender);
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
  if ConditionGrid.TopRow < (Length(Mission.mis_data.conditions) + 1 - ConditionGrid.VisibleRowCount) then
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

procedure TEventDialog.Addcondition1Click(Sender: TObject);
begin
  if Mission.add_condition(0) then
  begin
    fill_grids;
    if ConditionGrid.Row = Mission.mis_data.num_conditions then
      select_condition(ConditionGrid.Row-1)
    else
      ConditionGrid.Row := Mission.mis_data.num_conditions;
  end;
end;

procedure TEventDialog.Duplicatecondition1Click(Sender: TObject);
begin
  if Mission.add_condition(-1) then
  begin
    Mission.mis_data.conditions[Mission.mis_data.num_conditions-1] := Mission.mis_data.conditions[selected_condition];
    MissionIni.condition_notes[Mission.mis_data.num_conditions-1] := MissionIni.condition_notes[selected_condition];
    fill_grids;
    ConditionGrid.Row := Mission.mis_data.num_conditions;
  end;
end;

procedure TEventDialog.Deleteselectedcondition1Click(Sender: TObject);
begin
  if selected_condition >= Mission.mis_data.num_conditions then
    exit;
  if Mission.condition_is_used(selected_condition) and
    (Application.MessageBox('Do you really want to delete condition? All condition references will be updated.', 'Delete condition', MB_YESNO or MB_ICONQUESTION) = IDNO) then
    exit;
  Mission.delete_condition(selected_condition);
  update_contents;
end;

procedure TEventDialog.Deletelastcondition1Click(Sender: TObject);
begin
  if Mission.mis_data.num_conditions = 0 then
    exit;
  if Mission.condition_is_used(Mission.mis_data.num_conditions - 1) and
    (Application.MessageBox('Do you really want to delete condition? All condition references will be updated.', 'Delete condition', MB_YESNO or MB_ICONQUESTION) = IDNO) then
    exit;
  Mission.delete_condition(Mission.mis_data.num_conditions - 1);
  update_contents;
end;

procedure TEventDialog.MoveUp2Click(Sender: TObject);
begin
  if selected_condition = 0 then
    exit;
  Mission.swap_conditions(selected_condition, selected_condition - 1);
  select_event(selected_event);
  dec(selected_condition);
  ConditionGrid.Row := selected_condition + 1;
  update_contents;
end;

procedure TEventDialog.MoveDown2Click(Sender: TObject);
begin
  if selected_condition >= Mission.mis_data.num_conditions - 1 then
    exit;
  Mission.swap_conditions(selected_condition, selected_condition + 1);
  select_event(selected_event);
  inc(selected_condition);
  ConditionGrid.Row := selected_condition + 1;
  update_contents;
end;

procedure TEventDialog.Showkeyshortcuts2Click(Sender: TObject);
var
  msg: string;
begin
  msg := 'Enter = Apply changes';
  ShowMessage(msg);
end;

procedure TEventDialog.cbxConditionTypeChange(Sender: TObject);
begin
  change_condition_type(EventConfig.condition_type_mapping[cbxConditionType.ItemIndex]);
end;

procedure TEventDialog.btnApplyConditionChangesClick(Sender: TObject);
begin
  apply_condition_changes;
  ConditionGrid.SetFocus;
end;

procedure TEventDialog.cbMarkEventsHavingConditionClick(Sender: TObject);
begin
  EventGrid.Invalidate;
end;

procedure TEventDialog.PopupMenuPopup(Sender: TObject);
begin
  apply_changes;
end;

procedure TEventDialog.CCGCoordinateChange(Sender: TObject);
var
  ccg: TCoordControlGroupPtr;
begin
  if loading then
    exit;
  ccg := Addr(ccgs[(Sender as TControl).Tag]);
  set_integer_value(ccg.data_ptr, ccg.offset_x, 1, StrToIntDef(ccg.spin_x.Text, 0));
  set_integer_value(ccg.data_ptr, ccg.offset_y, 1, StrToIntDef(ccg.spin_y.Text, 0));
end;

procedure TEventDialog.CCGBtnSelectClick(Sender: TObject);
var
  ccg_index: integer;
  ccg: TCoordControlGroupPtr;
begin
  ccg_index := (Sender as TControl).Tag;
  ccg := Addr(ccgs[ccg_index]);
  MainWindow.start_position_selection(ccg.spin_x.Value, ccg.spin_y.Value, PositionSelectionMode(ccg.coorddef.coord_type));
  selected_coord_index := ccg_index;
  close;
end;

procedure TEventDialog.ACGValueChange(Sender: TObject);
var
  acg: TArgControlGroupPtr;
  value: integer;
  i: integer;
begin
  if loading then
    exit;
  acg := Addr(acgs[(Sender as TControl).Tag]);
  if acg.argdef.arg_type = atNone then
    exit;
  value := 0;
  case acg.argdef.arg_type of
    atNumber: value := StrToIntDef(acg.spin_edit.Text, 0);
    atBigNumber: value := StrToIntDef(acg.text_edit.Text, 0);
    atHexNumber: value := StrToIntDef('$' + acg.text_edit.Text, 0);
    atFloat:
      begin
        set_float_struct_member(acg.data_ptr, acg.struct_def, acg.struct_member, StrToFloatDef(acg.text_edit.Text, 0.0));
        exit;
      end;
    atList: value := acg.combo_box.ItemIndex;
    atBool: value := IfThen(acg.check_box.Checked, 1, 0);
    atSwitch: value := IfThen(acg.radio_true.Checked, 1, 0);
  end;
  set_integer_struct_member(acg.data_ptr, acg.struct_def, acg.struct_member, value);
  // Update visibility of other control groups
  for i := 0 to High(ccgs) do
    ccgs[i].container.Visible := (ccgs[i].coorddef.name <> '') and evaluate_show_if(Addr(ccgs[i].coorddef.show_if), ccgs[i].data_ptr, ccgs[i].struct_def);
  for i := 0 to High(acgs) do
    acgs[i].container.Visible := (acgs[i].argdef.arg_type <> atNone) and evaluate_show_if(Addr(acgs[i].argdef.show_if), acgs[i].data_ptr, acgs[i].struct_def);
  // Update amount of tile pairs
  if EventConfig.event_types[tmp_event.event_type].event_data = edTilePairs then
    fill_event_data_panel(edpTilePairs, true);
end;

procedure TEventDialog.update_event_type_configuration;
var
  i: integer;
  tmp_strings: TStringList;
begin
  tmp_strings := TStringList.Create;
  // Initialize event type list
  for i := 0 to EventConfig.cnt_valid_event_types - 1 do
    tmp_strings.Add(inttostr(EventConfig.event_type_mapping[i]) + ' - ' + EventConfig.event_types[EventConfig.event_type_mapping[i]].name);
  cbxEventType.Items := tmp_strings;
  lbEventTypeList.Items := tmp_strings;
  tmp_strings.Clear;
  // Initialize condition type list
  for i := 0 to EventConfig.cnt_valid_condition_types - 1 do
    tmp_strings.Add(inttostr(EventConfig.condition_type_mapping[i]) + ' - ' + EventConfig.condition_types[EventConfig.condition_type_mapping[i]].name);
  cbxConditionType.Items := tmp_strings;
  lbConditionTypeList.Items := tmp_strings;
  tmp_strings.Destroy;
end;

procedure TEventDialog.update_contents;
begin
  if not Visible then
  begin
    pending_update_contents := true;
    exit;
  end;
  pending_update_contents := false;
  if not Mission.mis_assigned then
    exit;
  enable_mission_ini_features;
  fill_grids;
  select_event(selected_event);
  select_condition(selected_condition);
end;

procedure TEventDialog.update_player_list(player_list: TStringList);
var
  prev_index: integer;
  i: integer;
begin
  prev_index := cbCreateEventsPlayer.ItemIndex;
  cbCreateEventsPlayer.Items := player_list;
  cbCreateEventsPlayer.ItemIndex := Max(prev_index, 0);
  cached_lists[Byte(ltPlayers)].Assign(player_list);
  // Update currently visible arg combo boxes
  for i := 0 to High(acgs) do
    if (acgs[i].argdef <> nil) and (acgs[i].argdef.arg_type = atList) and (acgs[i].argdef.list_type = ltPlayers) then
      fill_arg_combo_box(Addr(acgs[i]), acgs[i].combo_box.ItemIndex, true);
end;

procedure TEventDialog.update_structures_list;
var
  i: integer;
begin
  // Building list
  cached_lists[Byte(ltBuildings)].Clear;
  for i:= 0 to Structures.templates.BuildingCount -1 do
    cached_lists[Byte(ltBuildings)].Add(inttostr(i) + ' - ' + Structures.get_building_name_str(i));
  // Unit list
  cached_lists[Byte(ltUnits)].Clear;
  for i:= 0 to Structures.templates.UnitCount -1 do
    cached_lists[Byte(ltUnits)].Add(inttostr(i) + ' - ' + Structures.get_unit_name_str(i));
  UnitSelectionList.Items := cached_lists[Byte(ltUnits)];
  // Weapon list
  cached_lists[Byte(ltWeapons)].Clear;
  for i:= 0 to Structures.templates.WeaponCount -1 do
    cached_lists[Byte(ltWeapons)].Add(inttostr(i) + ' - ' + Structures.templates.WeaponStrings[i]);
  // Explosion list
  cached_lists[Byte(ltExplosions)].Clear;
  for i:= 0 to Structures.templates.ExplosionCount -1 do
    cached_lists[Byte(ltExplosions)].Add(inttostr(i) + ' - ' + Structures.templates.ExplosionStrings[i]);
  // Update currently visible arg combo boxes
  for i := 0 to High(acgs) do
    if (acgs[i].argdef <> nil) and (acgs[i].argdef.arg_type = atList) and (acgs[i].argdef.list_type >= ltBuildings) then
      fill_arg_combo_box(Addr(acgs[i]), acgs[i].combo_box.ItemIndex, true);
end;

procedure TEventDialog.update_sound_list;
var
  i: integer;
begin
  cached_lists[Byte(ltSounds)].Clear;
  for i := 0 to StringTable.samples_uib.Count - 1 do
    cached_lists[Byte(ltSounds)].Add(inttostr(i) + ' - ' + StringTable.samples_uib.ValueFromIndex[i]);
  // Update currently visible arg combo boxes
  for i := 0 to High(acgs) do
    if (acgs[i].argdef <> nil) and (acgs[i].argdef.arg_type = atList) and (acgs[i].argdef.list_type = ltSounds) then
      fill_arg_combo_box(Addr(acgs[i]), acgs[i].combo_box.ItemIndex, true);
end;

procedure TEventDialog.update_tileset;
begin
  if EventConfig.event_types[tmp_event.event_type].event_data = edTileBlock then
    draw_tile_block;
  if EventConfig.event_types[tmp_event.event_type].event_data = edTilePairs then
    draw_tile_pairs;
end;

procedure TEventDialog.enable_mission_ini_features;
begin
  notes_enabled := MissionIni.mission_ini_assigned;
  lblEventNote.Visible := notes_enabled;
  lblConditionNote.Visible := notes_enabled;
  edEventNote.Visible := notes_enabled;
  edConditionNote.Visible := notes_enabled;
  btnCustomMsgText.Visible := MissionIni.mission_ini_assigned;
end;

procedure TEventDialog.fill_grids;
var
  i: integer;
begin
  // Fill events
  for i := 0 to MAX_EVENTS - 1 do
    fill_event_grid_row(i);
  // Fill conditions
  for i := 0 to MAX_CONDITIONS - 1 do
    fill_condition_grid_row(i);
  // All event grid must be redrawn
  if cbMarkEventsHavingCondition.Checked then
    EventGrid.Invalidate;
end;

procedure TEventDialog.fill_event_grid_row(index: integer);
var
  row: integer;
  event: ^TEvent;
  et: TEventTypeDefinitionPtr;
begin
  row := index + 1;
  if index >= Mission.mis_data.num_events then
  begin
    EventGrid.Rows[row].Clear;
    EventGrid.Cells[0,row] := inttostr(index);
    exit;
  end;
  event := Addr(Mission.mis_data.events[index]);
  et := Addr(EventConfig.event_types[event.event_type]);
  EventGrid.Cells[0,row] := inttostr(index);
  // Basic information
  EventGrid.Cells[1,row] := et.name;
  EventGrid.Cells[2,row] := IfThen((event.blocked_flags and 1) = 1, 'A', '') + IfThen((event.blocked_flags and 2) = 2, 'B', '');
  if et.has_map_pos and evaluate_show_if(Addr(et.coords[0].show_if), event, Addr(event_args_struct_members)) then
    EventGrid.Cells[3,row] := inttostr(event.coord_x[0]) + ' , ' + inttostr(event.coord_y[0])
  else
    EventGrid.Cells[3,row] := '';
  if et.has_player then
    EventGrid.Cells[4,row] := Structures.player_names[event.player]
  else
    EventGrid.Cells[4,row] := '';
  // Contents
  EventGrid.Cells[5,row] := Mission.get_event_contents(index);
  // Conditions
  EventGrid.Cells[6,row] := Mission.get_event_conditions(index);
  // Note
  EventGrid.Cells[7,row] := MissionIni.event_notes[index];
end;

procedure TEventDialog.select_event(index: integer);
var
  item_index, i: integer;
  event_valid: boolean;
begin
  selected_event := index;
  tmp_event := Mission.mis_data.events[index];
  lblEventNumber.Caption := 'Event ' + inttostr(index);
  event_valid := index < Mission.mis_data.num_events;
  item_index := -1;
  if event_valid then
    for i := 0 to High(EventConfig.event_type_mapping) do
      if EventConfig.event_type_mapping[i] = tmp_event.event_type then
      begin
        item_index := i;
        break;
      end;
  cbxEventType.ItemIndex := item_index;
  cbxEventType.Enabled := event_valid;
  loading := true;
  cbEventAutoBlock.Checked := event_valid and ((tmp_event.blocked_flags and 1) = 1);
  cbEventBlocked.Checked := event_valid and ((tmp_event.blocked_flags and 2) = 2);
  loading := false;
  btnApplyEventChanges.Enabled := event_valid;
  edEventNote.Enabled := event_valid;
  fill_event_ui;
end;

procedure TEventDialog.fill_event_ui;
var
  i: integer;
  panel_top: integer;
  et: TEventTypeDefinitionPtr;
  ed: EventData;
begin
  et := Addr(EventConfig.event_types[IfThen(selected_event < Mission.mis_data.num_events, tmp_event.event_type, -1)]);
  ed := et.event_data;
  panel_top := 48;
  // Fill event coordinates
  for i := 0 to High(et.coords) do
    fill_coord_control_group(Addr(ccgs[i]), Addr(et.coords[i]), panel_top);
  // Fill event arguments
  for i := 0 to High(et.args) do
    fill_arg_control_group(Addr(acgs[i]), Addr(et.args[i]), panel_top);
  // Fill event data UI
  fill_event_data_panel(edpUnitList,     ed = edUnitList);
  fill_event_data_panel(edpByteValues,   ed = edByteValues);
  fill_event_data_panel(edpMessage,      ed = edMessage);
  fill_event_data_panel(edpMusic,        ed = edMusic);
  fill_event_data_panel(edpTileBlock,    ed = edTileBlock);
  fill_event_data_panel(edpTilePairs,    ed = edTilePairs);
  // Fill event note
  if notes_enabled then
    edEventNote.Text := MissionIni.event_notes[selected_event];
  // Fill condition list
  fill_event_condition_list;
end;

procedure TEventDialog.fill_event_data_panel(panel: TPanel; active: boolean);
var
  i, j: integer;
  tmp: string;
begin
  panel.Visible := active;
  if not active then
    exit;
  if panel = edpUnitList then
  begin
    EventUnitList.Items.Clear;
    for i := 0 to tmp_event.amount - 1 do
      EventUnitList.Items.Add(Structures.get_unit_name_str(tmp_event.data[i]));
  end;
  if panel = edpByteValues then
  begin
    for j := 0 to 3 do
      for i := 0 to 7 do
        if (j * 8 + i - 7) >= 0 then
          sgEventByteValues.Cells[i, j] := IntToStr(tmp_event.data[j * 8 + i - 7])
        else
          sgEventByteValues.Cells[i, j] := '';
  end;
  if panel = edpMessage then
  begin
    if seMessageId.Value = get_integer_value(Addr(tmp_event.data), 21, 4) then
      seMessageIdChange(nil)
    else
      seMessageId.Value := get_integer_value(Addr(tmp_event.data), 21, 4);
  end;
  if panel = edpMusic then
  begin
    SetString(tmp, PChar(Addr(tmp_event.data[0])), StrLen(PChar(Addr(tmp_event.data[0]))));
    cbMusicName.Text := tmp;
  end;
  if panel = edpTileBlock then
    draw_tile_block;
  if panel = edpTilePairs then
  begin
    sgTilePairs.RowCount := tmp_event.amount;
    sgTilePairs.Height := tmp_event.amount * 32 + 3;
    for i := 0 to tmp_event.amount - 1 do
    begin
      sgTilePairs.Cells[0, i] := IntToStr(get_integer_value(Addr(tmp_event.data[1]), i * 4, 2));
      sgTilePairs.Cells[1, i] := IntToStr(get_integer_value(Addr(tmp_event.data[1]), i * 4 + 2, 2));
    end;
    draw_tile_pairs;
  end;
end;

procedure TEventDialog.fill_event_condition_list;
var
  i: integer;
  cond_index: integer;
begin
  EventConditionList.Items.Clear;
  for i := 0 to tmp_event.num_conditions - 1 do
  begin
    cond_index := tmp_event.condition_index[i];
    EventConditionList.Items.Add(inttostr(cond_index) + ' - ' + EventConfig.condition_types[Mission.mis_data.conditions[cond_index].condition_type].name + '(' + Mission.get_condition_contents(tmp_event.condition_index[i], true) + ')');
    EventConditionList.Checked[i] := tmp_event.condition_not[i] = 1;
  end;
end;

procedure TEventDialog.change_event_type(new_event_type: integer);
var
  i: integer;
  old_et, new_et: TEventTypeDefinitionPtr;
begin
  if tmp_event.event_type = new_event_type then
    exit;
  old_et := Addr(EventConfig.event_types[tmp_event.event_type]);
  new_et := Addr(EventConfig.event_types[new_event_type]);
  // Change type
  tmp_event.event_type := new_event_type;
  // Reset args
  for i := 0 to High(new_et.args) do
    if old_et.args[i].name <> new_et.args[i].name then
      set_integer_struct_member(Addr(tmp_event), Addr(event_args_struct_members), i, new_et.args[i].default);
  // Reset coords
  for i := 0 to High(new_et.coords) do
    if old_et.coords[i].name <> new_et.coords[i].name then
    begin
      tmp_event.coord_x[i] := new_et.coords[i].default;
      tmp_event.coord_y[i] := new_et.coords[i].default;
    end;
  // Reset data
  if (old_et.event_data <> new_et.event_data) then
    FillChar(tmp_event.data, Length(tmp_event.data), 0);
  fill_event_ui;
end;

procedure TEventDialog.apply_event_changes;
var
  et: TEventTypeDefinitionPtr;
  old_event_used_position: boolean;
begin
  if selected_event >= Mission.mis_data.num_events then
    exit;
  et := Addr(EventConfig.event_types[tmp_event.event_type]);
  old_event_used_position := EventConfig.event_types[Mission.mis_data.events[selected_event].event_type].has_map_pos;
  // Check if existing and new data differ
  if CompareMem(Addr(Mission.mis_data.events[selected_event]), Addr(tmp_event), sizeof(TEvent)) and (MissionIni.event_notes[selected_event] = edeventNote.Text) and not ((et.event_data = edMessage) and msg_text_is_custom) then
    exit;
  // Save event data
  Mission.mis_data.events[selected_event] := tmp_event;
  Mission.mis_modified := true;
  // Save event note
  if notes_enabled then
    MissionIni.event_notes[selected_event] := edeventNote.Text;
  // Save custom message text
  if (et.event_data = edMessage) and msg_text_is_custom then
    MissionIni.set_custom_text(get_integer_value(Addr(tmp_event.data), 21, 4), edMessageText.Text);
  // Update GUI
  fill_event_grid_row(selected_event);
  // Update event markers on map if old or new event has position
  if old_event_used_position or (et.has_map_pos) then
    Dispatcher.register_event(evMisEventPositionChange);
end;

procedure TEventDialog.draw_tile_block;
var
  i, j: integer;
  tile_index, tile_x, tile_y: integer;
begin
  imgTileBlock.Width := tmp_event.coord_x[1] * 32;
  imgTileBlock.Height := tmp_event.coord_y[1] * 32;
  imgTileBlock.Picture.Bitmap.Width := tmp_event.coord_x[1] * 32;
  imgTileBlock.Picture.Bitmap.Height := tmp_event.coord_y[1] * 32;
  for j := 0 to tmp_event.coord_y[1] - 1 do
    for i := 0 to tmp_event.coord_x[1] - 1 do
      begin
        tile_index := get_integer_value(Addr(tmp_event.data[1]), (j * tmp_event.coord_x[1] + i) * 2, 2);
        tile_x := (tile_index mod 20) * 32;
        tile_y := (tile_index div 20) * 32;
        if tile_index = 65535 then
        begin
          imgTileBlock.Canvas.Pen.Color := clSilver;
          imgTileBlock.Canvas.Brush.Color := clSilver;
          imgTileBlock.Canvas.Brush.Style := bsSolid;
          imgTileBlock.Canvas.Rectangle(i * 32, j * 32, i * 32 + 32, j * 32 + 32);
        end else
          imgTileBlock.Canvas.CopyRect(Rect(i * 32, j * 32, i * 32 + 32, j * 32 + 32), Tileset.tileimage.Canvas, Rect(tile_x, tile_y, tile_x + 32, tile_y + 32));
      end;
end;

procedure TEventDialog.draw_tile_pairs;
var
  i: integer;
  tile_index, tile_x, tile_y: integer;
begin
  imgTilePairs.Height := tmp_event.amount * 32;
  imgTilePairs.Picture.Bitmap.Width := 64;
  imgTilePairs.Picture.Bitmap.Height := tmp_event.amount * 32;
  for i := 0 to tmp_event.amount - 1 do
  begin
    tile_index := get_integer_value(Addr(tmp_event.data[1]), i * 4, 2);
    tile_x := (tile_index mod 20) * 32;
    tile_y := (tile_index div 20) * 32;
    imgTilePairs.Canvas.CopyRect(Rect(0, i * 32, 32, i * 32 + 32), Tileset.tileimage.Canvas, Rect(tile_x, tile_y, tile_x + 32, tile_y + 32));
    tile_index := get_integer_value(Addr(tmp_event.data[1]), i * 4 + 2, 2);
    tile_x := (tile_index mod 20) * 32;
    tile_y := (tile_index div 20) * 32;
    imgTilePairs.Canvas.CopyRect(Rect(32, i * 32, 64, i * 32 + 32), Tileset.tileimage.Canvas, Rect(tile_x, tile_y, tile_x + 32, tile_y + 32));
  end;
end;

procedure TEventDialog.fill_condition_grid_row(index: integer);
var
  row: integer;
  cond: ^TCondition;
  ct: TConditionTypeDefinitionPtr;
begin
  row := index + 1;
  if index >= Mission.mis_data.num_conditions then
  begin
    ConditionGrid.Rows[row].Clear;
    ConditionGrid.Cells[0,row] := inttostr(index);
    exit;
  end;
  cond := Addr(Mission.mis_data.conditions[index]);
  ct := Addr(EventConfig.condition_types[cond.condition_type]);
  ConditionGrid.Cells[0,row] := inttostr(index);
  // Basic information
  ConditionGrid.Cells[1,row] := ct.name;
  if ct.has_player then
    ConditionGrid.Cells[2,row] := Structures.player_names[cond.player]
  else
    ConditionGrid.Cells[2,row] := '';
  // Contents
  ConditionGrid.Cells[3,row] := Mission.get_condition_contents(index, false);
end;

procedure TEventDialog.select_condition(index: integer);
var
  item_index, i: integer;
  condition_valid: boolean;
begin
  selected_condition := index;
  tmp_condition := Mission.mis_data.conditions[index];
  lblConditionProperties.Caption := 'Condition properties (condition ' + inttostr(index) + ')';
  condition_valid := index < Mission.mis_data.num_conditions;
  item_index := -1;
  if condition_valid then
    for i := 0 to High(EventConfig.condition_type_mapping) do
      if EventConfig.condition_type_mapping[i] = tmp_condition.condition_type then
      begin
        item_index := i;
        break;
      end;
  cbxConditionType.ItemIndex := item_index;
  cbxConditionType.Enabled := condition_valid;
  btnApplyConditionChanges.Enabled := condition_valid;
  edConditionNote.Enabled := condition_valid;
  fill_condition_ui;
  if cbMarkEventsHavingCondition.Checked then
    EventGrid.Invalidate;
end;

procedure TEventDialog.fill_condition_ui;
var
  i: integer;
  panel_top: integer;
  ct: TConditionTypeDefinitionPtr;
begin
  ct := Addr(EventConfig.condition_types[IfThen(selected_condition < Mission.mis_data.num_conditions, tmp_condition.condition_type, -1)]);
  panel_top := 48;
  // Fill event coordinates
  for i := 0 to High(ct.coords) do
    fill_coord_control_group(Addr(ccgs[i+4]), Addr(ct.coords[i]), panel_top);
  // Fill event arguments
  for i := 0 to High(ct.args) do
    fill_arg_control_group(Addr(acgs[i+Length(event_args_struct_members)]), Addr(ct.args[i]), panel_top);
  // Fill condition note
  if notes_enabled then
    edConditionNote.Text := MissionIni.condition_notes[selected_condition];
end;

procedure TEventDialog.change_condition_type(new_condition_type: integer);
var
  i: integer;
  old_ct, new_ct: TConditionTypeDefinitionPtr;
begin
  if tmp_condition.condition_type = new_condition_type then
    exit;
  old_ct := Addr(EventConfig.condition_types[tmp_condition.condition_type]);
  new_ct := Addr(EventConfig.condition_types[new_condition_type]);
  // Change type
  tmp_condition.condition_type := new_condition_type;
  // Reset args
  for i := 0 to High(new_ct.args) do
    if old_ct.args[i].name <> new_ct.args[i].name then
      set_integer_struct_member(Addr(tmp_condition), Addr(condition_args_struct_members), i, new_ct.args[i].default);
  // Reset coords
  for i := 0 to High(new_ct.coords) do
    if old_ct.coords[i].name <> new_ct.coords[i].name then
    begin
      tmp_condition.coord_x[i] := new_ct.coords[i].default;
      tmp_condition.coord_y[i] := new_ct.coords[i].default;
    end;
  fill_condition_ui;
end;

procedure TEventDialog.apply_condition_changes;
var
  ct: TConditionTypeDefinitionPtr;
  old_condition_used_position: boolean;
  i: integer;
begin
  if selected_condition >= Mission.mis_data.num_conditions then
    exit;
  ct := Addr(EventConfig.condition_types[tmp_condition.condition_type]);
  old_condition_used_position := EventConfig.condition_types[Mission.mis_data.conditions[selected_condition].condition_type].has_map_pos;
  // Check if existing and new data differ
  if CompareMem(Addr(Mission.mis_data.conditions[selected_condition]), Addr(tmp_condition), sizeof(TCondition)) and (MissionIni.condition_notes[selected_condition] = edConditionNote.Text) then
    exit;
  // Save condition data
  Mission.mis_data.conditions[selected_condition] := tmp_condition;
  Mission.mis_modified := true;
  // Save condition note
  if notes_enabled then
    MissionIni.condition_notes[selected_condition] := edConditionNote.Text;
  // Update GUI
  fill_condition_grid_row(selected_condition);
  for i := 0 to Mission.mis_data.num_events - 1 do
    if Mission.check_event_has_condition(i, selected_condition) then
      fill_event_grid_row(i);
  if Mission.check_event_has_condition(selected_event, selected_condition) then
    fill_event_condition_list;
  // Update event markers on map if condition has position
  if old_condition_used_position or (ct.has_map_pos) then
    Dispatcher.register_event(evMisEventPositionChange);
end;

procedure TEventDialog.create_coord_control_group(index: integer; data_ptr: Pointer; struct_def: TStructDefinitionPtr; offset_x, offset_y: integer; parent_panel: TPanel);
begin
  ccgs[index].data_ptr := data_ptr;
  ccgs[index].struct_def := struct_def;
  ccgs[index].offset_x := offset_x;
  ccgs[index].offset_y := offset_y;
  with ccgs[index] do
  begin
    container := TPanel.Create(self);
    container.Width := parent_panel.Width;
    container.Height := 28;
    container.BevelOuter := bvNone;
    container.Visible := false;
    container.Parent := parent_panel;
    caption := TLabel.Create(self);
    caption.Left := 4;
    caption.Top := 8;
    caption.Parent := container;
    spin_x := TSpinEdit.Create(self);
    spin_x.Top := 4;
    spin_x.Left := parent_panel.Width - 160;
    spin_x.Width := 49;
    spin_x.MaxValue := 127;
    spin_x.Tag := index;
    spin_x.OnChange := CCGCoordinateChange;
    spin_x.Parent := container;
    spin_y := TSpinEdit.Create(self);
    spin_y.Top := 4;
    spin_y.Left := parent_panel.Width - 104;
    spin_y.Width := 49;
    spin_y.MaxValue := 127;
    spin_y.Tag := index;
    spin_y.OnChange := CCGCoordinateChange;
    spin_y.Parent := container;
    btn_select := TButton.Create(self);
    btn_select.Top := 4;
    btn_select.Left := parent_panel.Width - 48;
    btn_select.Width := 40;
    btn_select.Height := 21;
    btn_select.Caption := 'Select';
    btn_select.Tag := index;
    btn_select.OnClick := CCGBtnSelectClick;
    btn_select.Parent := container;
  end;
end;

procedure TEventDialog.fill_coord_control_group(ccg: TCoordControlGroupPtr; coorddef: TCoordDefinitionPtr; var panel_top: integer);
var
  show: boolean;
begin
  ccg.coorddef := coorddef;
  show := coorddef.name <> '';
  ccg.container.Visible := show and evaluate_show_if(Addr(coorddef.show_if), ccg.data_ptr, ccg.struct_def);;
  if not show then
    exit;
  ccg.container.Top := panel_top;
  inc(panel_top, ccg.container.Height);
  ccg.caption.Caption := coorddef.name + ':';
  loading := true;
  ccg.spin_x.Value := get_integer_value(ccg.data_ptr, ccg.offset_x, 1);
  ccg.spin_y.Value := get_integer_value(ccg.data_ptr, ccg.offset_y, 1);
  ccg.spin_x.ReadOnly := coorddef.readonly;
  ccg.spin_y.ReadOnly := coorddef.readonly;
  ccg.btn_select.Visible := coorddef.coord_type <> ctNone;
  loading := false;
end;

procedure TEventDialog.create_arg_control_group(index: integer; data_ptr: Pointer; struct_def: TStructDefinitionPtr; struct_member: integer; parent_panel: TPanel);
begin
  acgs[index].data_ptr := data_ptr;
  acgs[index].struct_def := struct_def;
  acgs[index].struct_member := struct_member;
  with acgs[index] do
  begin
    container := TPanel.Create(self);
    container.Width := parent_panel.Width;
    container.Height := 28;
    container.BevelOuter := bvNone;
    container.Visible := false;
    container.Parent := parent_panel;
    caption := TLabel.Create(self);
    caption.Left := 4;
    caption.Top := 8;
    caption.Parent := container;
    text_edit := TEdit.Create(self);
    text_edit.Top := 4;
    text_edit.Left := parent_panel.Width - 160;
    text_edit.Width := 128;
    text_edit.Visible := False;
    text_edit.Tag := index;
    text_edit.OnChange := ACGValueChange;
    text_edit.Parent := container;
    spin_edit := TSpinEdit.Create(self);
    spin_edit.Top := 4;
    spin_edit.Left := parent_panel.Width - 160;
    spin_edit.Width := 60;
    spin_edit.Visible := False;
    spin_edit.Tag := index;
    spin_edit.OnChange := ACGValueChange;
    spin_edit.Parent := container;
    combo_box := TComboBox.Create(self);
    combo_box.Top := 4;
    combo_box.Left := parent_panel.Width - 160;
    combo_box.Width := 128;
    combo_box.Visible := False;
    combo_box.Tag := index;
    combo_box.OnChange := ACGValueChange;
    combo_box.Parent := container;
    check_box := TCheckBox.Create(self);
    check_box.Top := 4;
    check_box.Left := parent_panel.Width - 160;
    check_box.Visible := False;
    check_box.Tag := index;
    check_box.OnClick := ACGValueChange;
    check_box.Parent := container;
    radio_false := TRadioButton.Create(self);
    radio_false.Top := 4;
    radio_false.Left := parent_panel.Width - 160;
    radio_false.Visible := False;
    radio_false.Tag := index;
    radio_false.OnClick := ACGValueChange;
    radio_false.Parent := container;
    radio_true := TRadioButton.Create(self);
    radio_true.Top := 4;
    radio_true.Left := parent_panel.Width - 96;
    radio_true.Visible := False;
    radio_true.Tag := index;
    radio_true.OnClick := ACGValueChange;
    radio_true.Parent := container;
  end;
end;

procedure TEventDialog.fill_arg_control_group(acg: TArgControlGroupPtr; argdef: TArgDefinitionPtr; var panel_top: integer);
var
  show: boolean;
  value: integer;
begin
  acg.argdef := argdef;
  show := argdef.arg_type <> atNone;
  acg.container.Visible := show and evaluate_show_if(Addr(argdef.show_if), acg.data_ptr, acg.struct_def);
  if not show then
    exit;
  acg.container.Top := panel_top;
  inc(panel_top, acg.container.Height);
  acg.caption.Caption := argdef.name + ':';
  // Manage visibility of datatype-specific controls
  acg.text_edit.Visible := (argdef.arg_type = atBigNumber) or (argdef.arg_type = atHexNumber) or (argdef.arg_type = atFloat);
  acg.spin_edit.Visible := argdef.arg_type = atNumber;
  acg.combo_box.Visible := argdef.arg_type = atList;
  acg.check_box.Visible := argdef.arg_type = atBool;
  acg.radio_false.Visible := argdef.arg_type = atSwitch;
  acg.radio_true.Visible := argdef.arg_type = atSwitch;
  // Manage read-only
  acg.text_edit.ReadOnly := argdef.readonly;
  acg.spin_edit.ReadOnly := argdef.readonly;
  acg.combo_box.Enabled := not argdef.readonly;
  acg.check_box.Enabled := not argdef.readonly;
  acg.radio_false.Enabled := not argdef.readonly;
  acg.radio_true.Enabled := not argdef.readonly;
  // Get event arg value
  value := get_integer_struct_member(acg.data_ptr, acg.struct_def, acg.struct_member);
  // Set up datatype-specific controls
  loading := true;
  case argdef.arg_type of
    atNumber:
      begin
        acg.spin_edit.Value := value;
        acg.spin_edit.MaxValue := argdef.maxval;
      end;
    atBigNumber: acg.text_edit.Text := inttostr(value);
    atHexNumber: acg.text_edit.Text := IntToHex(value, 8);
    atFloat:
      begin
        acg.text_edit.Text := floattostrf(get_float_struct_member(acg.data_ptr, acg.struct_def, acg.struct_member), ffFixed, 8, 3);
        loading := false;
        exit;
      end;
    atList: fill_arg_combo_box(acg, value, false);
    atBool: acg.check_box.Checked := value <> 0;
    atSwitch:
      begin
        if argdef.values.Count = 2 then
        begin
          acg.radio_false.Caption := argdef.values[0];
          acg.radio_true.Caption := argdef.values[1];
        end;
        if value <> 0 then
          acg.radio_true.Checked := true
        else
          acg.radio_false.Checked := true;
      end;
  end;
  loading := false;
end;

procedure TEventDialog.fill_arg_combo_box(acg: TArgControlGroupPtr; value: integer; force_update_list: boolean);
begin
  if acg.argdef.list_type = ltNone then
    exit;
  if acg.argdef.list_type = ltCustom then
    acg.combo_box.Items := acg.argdef.values
  else
    acg.combo_box.Items := cached_lists[Byte(acg.argdef.list_type)];
  acg.combo_box.ItemIndex := value;
end;

procedure TEventDialog.finish_point_selection(x, y: integer);
var
  ccg: TCoordControlGroupPtr;
begin
  Show;
  if (x = -1) and (y = -1) then
    exit;
  ccg := Addr(ccgs[selected_coord_index]);
  ccg.spin_x.Value := x;
  ccg.spin_y.Value := y;
end;

procedure TEventDialog.finish_area_selection(min_x, max_x, min_y, max_y: integer);
var
  ccg: TCoordControlGroupPtr;
  width, height, tiles: integer;
  i, j: integer;
begin
  Show;
  if (min_x = -1) and (min_y = -1) then
    exit;
  width := max_x - min_x + 1;
  height := max_y - min_y + 1;
  tiles := width * height;
  // Check for size of tile block
  if (EventConfig.event_types[tmp_event.event_type].event_data = edTileBlock) and (tiles > 12) then
  begin
    Application.MessageBox(PChar(Format('You selected area which consists of %d tiles. Maximum allowed number of tiles is 12.', [tiles])), 'Cannot select area', MB_OK or MB_ICONERROR);
    exit;
  end;
  // Set up spin edits
  ccg := Addr(ccgs[selected_coord_index]);
  ccg.spin_x.Value := min_x;
  ccg.spin_y.Value := min_y;
  ccg := Addr(ccgs[selected_coord_index + 1]);
  ccg.spin_x.Value := width;
  ccg.spin_y.Value := height;
  // Copy tile block from map
  if EventConfig.event_types[tmp_event.event_type].event_data = edTileBlock then
  begin
    FillChar(tmp_event.data[0], Length(tmp_event.data), 0);
    for j := 0 to max_y - min_y do
      for i := 0 to max_x - min_x do
        set_integer_value(Addr(tmp_event.data[1]), (j * (width) + i) * 2, 2, Map.data[i + min_x, j + min_y].tile);
    draw_tile_block;
  end;
end;

procedure TEventDialog.apply_changes;
begin
  if pending_update_contents then
    exit;
  apply_condition_changes;
  apply_event_changes;
end;

end.
