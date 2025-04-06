unit event_dialog;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, ExtCtrls, StdCtrls, CheckLst, Spin, Buttons, Menus,
  _mission, _eventconfig, _utils;

type
  CreateEventType = (ceUnitSpawn, ceHarvRepl, ceAnnihMsg);

type
  VariableSelectionType = (vsCoord, vsArg, vsVarArg, vsEventMessageVar, vsCondExprVar, vsCondExprValue, vsEventFilterSkip, vsEventFilterLimit, vsEventObjectIndex, vsConditionFilterAmount, vsFilter);

type
  TCoordControlGroup = record
    coorddef: TCoordDefinitionPtr;
    is_event: boolean;
    data_ptr: Pointer;
    var_flag_ptr: PByte;
    struct_def: TStructDefinitionPtr;
    coord_index: integer;
    offset_x: integer;
    offset_y: integer;
    container: TPanel;
    caption: TLabel;
    spin_x: TSpinEdit;
    spin_y: TSpinEdit;
    edit_var_x: TEdit;
    edit_var_y: TEdit;
    btn_var_toggle_x: TButton;
    btn_var_toggle_y: TButton;
    btn_select: TButton;
  end;

  TCoordControlGroupPtr = ^TCoordControlGroup;

type
  TArgControlGroup = record
    argdef: TArgDefinitionPtr;
    is_event: boolean;
    data_ptr: Pointer;
    var_flag_ptr: PByte;
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
    btn_var_toggle: TButton;
  end;

  TArgControlGroupPtr = ^TArgControlGroup;

type
  TFilterControlGroup = record
    object_type: integer;
    is_event: boolean;
    filter_ptr: TObjectFilterPtr;
    cb_check_position: TCheckBox;
    cb_position_negation: TCheckBox;
    cbx_position_type: TComboBox;
    lbl_position: array[0..3] of TLabel;
    se_position: array[0..3] of TSpinEdit;
    btn_position_select: TButton;
    cbx_criteria: array[0..7] of TComboBox;
    criteria_type: array[0..7] of integer;
    cbx_operation: array[0..7] of TComboBox;
    se_value: array[0..7] of TSpinEdit;
    cbx_value: array[0..7] of TComboBox;
    cbx_and_or: array[0..6] of TComboBox;
    btn_var_toggle: array[0..15] of TButton;
    edit_var_name: array[0..15] of TEdit;
  end;

  TFilterControlGroupPtr = ^TFilterControlGroup;

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
    lblCreateEventsSide: TLabel;
    cbCreateEventsSide: TComboBox;
    lblCreateEventsCount: TLabel;
    seCreateEventsNum: TSpinEdit;
    cbCreateEventsUseHouseID: TCheckBox;
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
    MoveUp2: TMenuItem;
    MoveDown2: TMenuItem;
    btnMoveConditionUp: TButton;
    btnMoveConditionDown: TButton;
    N2: TMenuItem;
    edpValueList: TPanel;
    EventValueSelectionList: TListBox;
    pnEventValueListPadding: TPanel;
    pnEventValueListHeader: TPanel;
    lblEventValueSelectionList: TLabel;
    lblEventValueList: TLabel;
    EventValueList: TListBox;
    btnMoveValueUp: TButton;
    btnMoveValueDown: TButton;
    btnDeleteValue: TButton;
    btnDeleteLastValue: TButton;
    btnDeleteAllValues: TButton;
    btnAddValue: TButton;
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
    rbEventConditionsAnd: TRadioButton;
    rbEventConditionsOr: TRadioButton;
    Exportevents1: TMenuItem;
    Importevents1: TMenuItem;
    lblEventExportMarker: TLabel;
    pnEventExportMarker: TPanel;
    ExportEventsDialog: TSaveDialog;
    ImportEventsDialog: TOpenDialog;
    edpFilter: TPanel;
    lblEventFilterLimit: TLabel;
    seEventFilterLimit: TSpinEdit;
    pnConditionFilter: TPanel;
    lblConditionFilterAmount: TLabel;
    seConditionFilterAmount: TSpinEdit;
    rbConditionFilterAmoutGtEq: TRadioButton;
    rbConditionFilterAmoutEq: TRadioButton;
    N3: TMenuItem;
    Markcounterpart1: TMenuItem;
    Markselcondition1: TMenuItem;
    Markseltype1: TMenuItem;
    N4: TMenuItem;
    Marknothing2: TMenuItem;
    Markseltype2: TMenuItem;
    lblEventFilterSkip: TLabel;
    seEventFilterSkip: TSpinEdit;
    cbxEventGameStructMember: TComboBox;
    lblEventGameStructMember: TLabel;
    lblConditionGameStructMember: TLabel;
    cbxConditionGameStructMember: TComboBox;
    SelectVariablePanel: TPanel;
    lbSelectVariableList: TListBox;
    pnSelectVariableBottomPanel: TPanel;
    edSelectVariableName: TEdit;
    lblSelectVariableName: TLabel;
    btnSelectVariableOk: TBitBtn;
    btnSelectVariableCancel: TBitBtn;
    lblSelectVariableList: TLabel;
    btnEventFilterIndexToggle: TButton;
    pnEventFilterLimitSkip: TPanel;
    pnEventFilterBody: TPanel;
    pnConditionFilterBody: TPanel;
    edEventFilterIndexVar: TEdit;
    lblEventFilterIndexVar: TLabel;
    btnEventFilterSkipVarToggle: TButton;
    btnEventFilterLimitVarToggle: TButton;
    edEventFilterSkipVar: TEdit;
    edEventFilterLimitVar: TEdit;
    btnConditionFilterAmountVarToggle: TButton;
    edConditionFilterAmountVar: TEdit;
    lblMessageVarDatatype: TLabel;
    lblMessageVariable: TLabel;
    pnEventValueListCoords: TPanel;
    lblEventValueListXCoord: TLabel;
    lblEventValueListYCoord: TLabel;
    seEventValueListXCoord1: TSpinEdit;
    seEventValueListYCoord1: TSpinEdit;
    seEventValueListXCoord2: TSpinEdit;
    seEventValueListYCoord2: TSpinEdit;
    btnEventValueListCoordsSelect: TButton;
    edpCondExpr: TPanel;
    lblCondExprAndOr: TLabel;
    lblCondExprVariable: TLabel;
    lblCondExprValue: TLabel;
    btnCondExprPlus: TButton;
    btnCondExprMinus: TButton;
    pnEventTypeList: TPanel;
    edEventTypeFilter: TEdit;
    pnEventTypeFilter: TPanel;
    lblEventTypeFilter: TLabel;
    pnConditionTypeList: TPanel;
    pnConditionTypeFilter: TPanel;
    lblConditionTypeFilter: TLabel;
    edConditionTypeFilter: TEdit;
    pnEventHelp: TPanel;
    lblEventHelp: TLabel;
    sbShowEventHelp: TSpeedButton;
    sbShowConditionHelp: TSpeedButton;
    // Form actions
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormResize(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShortCut(var Msg: TWMKey; var Handled: Boolean);
    // Event type list actions
    procedure lbEventTypeListClick(Sender: TObject);
    procedure lbEventTypeListDblClick(Sender: TObject);
    procedure edEventTypeFilterChange(Sender: TObject);
    // Event grid actions
    procedure EventGridSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure EventGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EventGridMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EventGridMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
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
    procedure Exportevents1Click(Sender: TObject);
    procedure Importevents1Click(Sender: TObject);
    procedure MarkEventsClick(Sender: TObject);
    // Create events panel actions
    procedure btnCreateEventsCancelClick(Sender: TObject);
    procedure cbCreateEventsSideChange(Sender: TObject);
    procedure btnCreateEventsOkClick(Sender: TObject);
    // Event properties panel actions
    procedure cbxEventTypeChange(Sender: TObject);
    procedure EventFlagsClick(Sender: TObject);
    procedure sbShowEventHelpClick(Sender: TObject);
    procedure cbxEventGameStructMemberChange(Sender: TObject);
    // Event data panel actions
    // -- Value List
    procedure btnAddValueClick(Sender: TObject);
    procedure btnDeleteValueClick(Sender: TObject);
    procedure btnDeleteLastValueClick(Sender: TObject);
    procedure btnDeleteAllValuesClick(Sender: TObject);
    procedure btnMoveValueUpClick(Sender: TObject);
    procedure btnMoveValueDownClick(Sender: TObject);
    procedure btnEventValueListCoordsSelectClick(Sender: TObject);
    // -- Byte values
    procedure sgEventByteValuesSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: String);
    // -- Message
    procedure seMessageIdChange(Sender: TObject);
    procedure btnCustomMsgTextClick(Sender: TObject);
    procedure cbxMessageVarDataTypeChange(Sender: TObject);
    procedure edMessageVariableClick(Sender: TObject);
    // -- Music
    procedure cbMusicNameChange(Sender: TObject);
    // -- Tile block
    procedure imgTileBlockMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    // -- Tile pairs
    procedure sgTilePairsSetEditText(Sender: TObject; ACol, ARow: Integer; const Value: String);
    // -- Conditional expression
    procedure btnCondExprPlusClick(Sender: TObject);
    procedure btnCondExprMinusClick(Sender: TObject);
    procedure cbxCondExprAndOrChange(Sender: TObject);
    procedure edCondExprVariableClick(Sender: TObject);
    procedure cbxCondExprOperatorChange(Sender: TObject);
    procedure edCondExprValueChange(Sender: TObject);
    procedure edCondExprValueClick(Sender: TObject);
    procedure btnCondExprVarBtnClick(Sender: TObject);
    // -- Object filter
    procedure seEventFilterSkipChange(Sender: TObject);
    procedure seEventFilterLimitChange(Sender: TObject);
    procedure btnEventFilterSkipVarToggleClick(Sender: TObject);
    procedure btnEventFilterLimitVarToggleClick(Sender: TObject);
    procedure edEventFilterSkipVarClick(Sender: TObject);
    procedure edEventFilterLimitVarClick(Sender: TObject);
    procedure btnEventFilterIndexToggleClick(Sender: TObject);
    procedure edEventFilterIndexVarClick(Sender: TObject);
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
    procedure lbConditionTypeListClick(Sender: TObject);
    procedure lbConditionTypeListDblClick(Sender: TObject);
    procedure edConditionTypeFilterChange(Sender: TObject);
    // Condition grid actions
    procedure ConditionGridSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure ConditionGridDblClick(Sender: TObject);
    procedure ConditionGridMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ConditionGridMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure ConditionGridMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure ConditionGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    // Condition grid popup menu actions
    procedure Addcondition1Click(Sender: TObject);
    procedure Duplicatecondition1Click(Sender: TObject);
    procedure Deleteselectedcondition1Click(Sender: TObject);
    procedure Deletelastcondition1Click(Sender: TObject);
    procedure MoveUp2Click(Sender: TObject);
    procedure MoveDown2Click(Sender: TObject);
    procedure MarkConditionsClick(Sender: TObject);
    // Condition properties panel actions
    procedure cbxConditionTypeChange(Sender: TObject);
    procedure sbShowConditionHelpClick(Sender: TObject);
    procedure cbxConditionGameStructMemberChange(Sender: TObject);
    // Condition filter panel actions
    procedure seConditionFilterAmountChange(Sender: TObject);
    procedure btnConditionFilterAmountVarToggleClick(Sender: TObject);
    procedure edConditionFilterAmountVarClick(Sender: TObject);
    // Miscellaneous
    procedure PopupMenuPopup(Sender: TObject);
    // Coord control group actions
    procedure CCGCoordinateChange(Sender: TObject);
    procedure CCGBtnSelectClick(Sender: TObject);
    procedure CCGVariableToggle(Sender: TObject);
    procedure CCGTextEditClick(Sender: TObject);
    // Arg control group actions
    procedure ACGValueChange(Sender: TObject);
    procedure ACGVariableToggle(Sender: TObject);
    procedure ACGTextEditClick(Sender: TObject);
    // Filter control group actions
    procedure FCGPositionTypeChange(Sender: TObject);
    procedure FCGValueChange(Sender: TObject);
    procedure FCGCriteriaChange(Sender: TObject);
    procedure FCGBtnSelectClick(Sender: TObject);
    procedure FCGVariableToggle(Sender: TObject);
    procedure FCGTextEditClick(Sender: TObject);
    // Select variable panel actions
    procedure btnSelectVariableOkClick(Sender: TObject);
    procedure btnSelectVariableCancelClick(Sender: TObject);
    procedure lbSelectVariableListClick(Sender: TObject);
    procedure edSelectVariableNameChange(Sender: TObject);
  private
    event_type_list_mapping: array[0..255] of integer;
    event_category_first_type: integer;
    event_category_last_type: integer;
    condition_type_list_mapping: array[0..255] of integer;
    condition_category_first_type: integer;
    condition_category_last_type: integer;
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
    event_value_list_type: EventData;
    variable_selection_last_hook_type: integer;
    variable_selection_type: VariableSelectionType;
    variable_selection_index: integer;
    variable_name_changed: boolean;
    selected_event_had_counterpart: boolean;

    pending_update_contents: boolean;
    pending_update_variable_list: boolean;

    cached_lists: array[-2..12] of TStringList;
    ccgs: array[0..5] of TCoordControlGroup;
    acgs: array[0..13] of TArgControlGroup;
    fcgs: array[0..1] of TFilterControlGroup;
    event_gamestruct_value_arg_def: TArgDefinition;
    condition_gamestruct_value_arg_def: TArgDefinition;
    event_message_var_datatype: array[0..7] of TComboBox;
    event_message_variable: array[0..7] of TEdit;
    cond_expr_and_or: array[0..6] of TComboBox;
    cond_expr_variable: array[0..7] of TEdit;
    cond_expr_operator: array[0..7] of TComboBox;
    cond_expr_value: array[0..7] of TEdit;
    cond_expr_var_btn: array[0..7] of TButton;

  public
    // Dispatcher events
    procedure update_event_type_configuration;
    procedure update_contents;
    procedure update_side_list(side_list: TStringList);
    procedure update_structures_list;
    procedure update_sound_list;
    procedure update_tileset;
    procedure update_variable_names;
  private
    procedure enable_mission_ini_features;
    // Fill data procedures
    procedure fill_grids;
    procedure fill_event_help_text;
    // Event-related procedures
    procedure fill_event_type_list;
    procedure fill_event_grid_row(index: integer);
    procedure select_event(index: integer);
    procedure fill_event_ui;
    function  get_event_arg_def(et: TEventTypeDefinitionPtr; arg_num: integer): TArgDefinitionPtr;
    procedure fill_event_gamestruct_member_combo(et: TEventTypeDefinitionPtr; var panel_top: integer);
    procedure fill_event_data_panel(panel: TPanel; active: boolean; object_type: integer);
    procedure fill_event_condition_list;
    procedure change_event_type(new_event_type: integer);
    procedure apply_event_changes;
    // Event data-related procedures
    function get_event_value_list_item_str(index: integer): String;
    procedure draw_tile_block;
    procedure draw_tile_pairs;
    procedure create_message_var_controls;
    procedure create_cond_expr_controls;
    // Condition-related prodcedures
    procedure fill_condition_type_list;
    procedure fill_condition_grid_row(index: integer);
    procedure select_condition(index: integer);
    procedure fill_condition_ui;
    function  get_condition_arg_def(ct: TConditionTypeDefinitionPtr; arg_num: integer): TArgDefinitionPtr;
    procedure fill_condition_gamestruct_member_combo(ct: TConditionTypeDefinitionPtr; var panel_top: integer);
    procedure change_condition_type(new_condition_type: integer);
    procedure apply_condition_changes;
    // Coord control group procedures
    procedure create_coord_control_group(index: integer; is_event: boolean; data_ptr: Pointer; var_flag_ptr: PByte; struct_def: TStructDefinitionPtr; coord_index, offset_x, offset_y: integer; parent_panel: TPanel);
    procedure fill_coord_control_group(ccg: TCoordControlGroupPtr; coorddef: TCoordDefinitionPtr);
    // Arg control group procedures
    procedure create_arg_control_group(index: integer; is_event: boolean; data_ptr: Pointer; var_flag_ptr: PByte; struct_def: TStructDefinitionPtr; struct_member: integer; parent_panel: TPanel);
    procedure fill_arg_control_group(acg: TArgControlGroupPtr; argdef: TArgDefinitionPtr);
    procedure fill_arg_combo_box(acg: TArgControlGroupPtr; value: integer; force_update_list: boolean);
    // Filter control group procedures
    procedure create_filter_control_group(index: integer; is_event: boolean; filter_ptr: Pointer; parent_panel: TPanel);
    procedure fill_filter_control_group(fcg: TFilterControlGroupPtr; object_type: integer);
    // Select variable procedures
    procedure start_variable_selection(selecion_type: VariableSelectionType; index: integer; default_var: integer; is_event: boolean);
    procedure end_variable_selection(status: boolean);
  public
    // Procedures called from different forms
    procedure finish_point_selection(x, y: integer);
    procedure finish_area_selection(min_x, max_x, min_y, max_y: integer);
    procedure finish_point_and_size_selection(x, y, width, height: integer);
    procedure apply_changes;
  end;

var
  EventDialog: TEventDialog;

implementation

uses Math, main, _missionini, _stringtable, _settings, _structures, _dispatcher, _tileset, _map, _gamelists, _gamestructs,
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
  EventGrid.Cells[0,0] := '#';
  EventGrid.ColWidths[0] := 30;
  EventGrid.Cells[1,0] := 'Bl.';
  EventGrid.ColWidths[1] := 20;
  EventGrid.Cells[2,0] := 'Event type';
  EventGrid.ColWidths[2] := 90;
  EventGrid.Cells[3,0] := 'Position';
  EventGrid.ColWidths[3] := 50;
  EventGrid.Cells[4,0] := 'Side';
  EventGrid.ColWidths[4] := 72;
  EventGrid.Cells[5,0] := 'Contents';
  EventGrid.ColWidths[5] := 400;
  EventGrid.Cells[6,0] := 'Conditions';
  EventGrid.ColWidths[6] := 400;
  EventGrid.Cells[7,0] := 'Note';
  EventGrid.ColWidths[7] := 1140;
  // Initialize condition grid
  ConditionGrid.Cells[0,0] := '#';
  ConditionGrid.ColWidths[0] := 24;
  ConditionGrid.Cells[1,0] := 'Condition type';
  ConditionGrid.ColWidths[1] := 84;
  ConditionGrid.Cells[2,0] := 'Side';
  ConditionGrid.ColWidths[2] := 72;
  ConditionGrid.Cells[3,0] := 'Contents';
  ConditionGrid.ColWidths[3] := 108;
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
  for i := 0 to 255 do
    cached_lists[ord(ilNone)].Add(IntToStr(i));
  for i := 0 to High(object_filter_comp_operation) do
    cached_lists[-1].Add(object_filter_comp_operation[i]);
  cached_lists[-2].Add(object_filter_comp_operation[0]);
  cached_lists[-2].Add(object_filter_comp_operation[1]);
  // Initialize value list
  event_value_list_type := edNone;
  // Initialize coord control groups
  for i := 0 to High(ccgs) do
  begin
    if i < 4 then
      create_coord_control_group(i, true, Addr(tmp_event), Addr(tmp_event.coord_var_flags), Addr(event_args_struct_members), i, i, i + 4, EventPropertiesPanel)
    else
      create_coord_control_group(i, false, Addr(tmp_condition), Addr(tmp_condition.coord_var_flags), Addr(condition_args_struct_members), i - 4, i - 4 + 12, i - 4 + 16, ConditionPropertiesPanel);
  end;
  // Initialize arg control groups
  for i := 0 to High(acgs) do
  begin
    if i < Length(event_args_struct_members) then
      create_arg_control_group(i, true, Addr(tmp_event), Addr(tmp_event.arg_var_flags), Addr(event_args_struct_members), i, EventPropertiesPanel)
    else
      create_arg_control_group(i, false, Addr(tmp_condition), Addr(tmp_condition.arg_var_flags), Addr(condition_args_struct_members), i - Length(event_args_struct_members), ConditionPropertiesPanel);
  end;
  // Initialize filter control groups
  create_filter_control_group(0, true, Addr(tmp_event.data[1]), pnEventFilterBody);
  create_filter_control_group(1, false, Addr(tmp_condition), pnConditionFilterBody);
  // Initialize event message controls
  create_message_var_controls;
  // Initialize conditional expression controls
  create_cond_expr_controls;
end;

procedure TEventDialog.FormShow(Sender: TObject);
begin
  if pending_update_contents then
    update_contents;
end;

procedure TEventDialog.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  end_variable_selection(false);
  apply_changes;
end;

procedure TEventDialog.FormResize(Sender: TObject);
begin
  if ClientWidth >= (pnConditionTypeList.Left + pnConditionTypeList.Width + pnConditionFilter.Width) then
  begin
    pnConditionFilter.Parent := LowerPanel;
    pnConditionFilter.Top := 0;
    pnConditionFilter.Left := pnConditionTypeList.Left + pnConditionTypeList.Width;
  end else
  begin
    pnConditionFilter.Parent := EventDialog;
    pnConditionFilter.Top := Splitter1.Top - pnConditionFilter.Height;
    pnConditionFilter.Left := ClientWidth - pnConditionFilter.Width;
  end;
  SelectVariablePanel.Height := EventGrid.Height;
  lbSelectVariableList.Height := SelectVariablePanel.Height - pnSelectVariableBottomPanel.Height - lbSelectVariableList.Top - 4;
  pnEventHelp.Top := EventGrid.Height - pnEventHelp.Height;
end;

procedure TEventDialog.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if key = 13 then // Enter
  begin
    if CreateEventsPanel.Visible then
      btnCreateEventsOkClick(Sender)
    else if SelectVariablePanel.Visible then
      end_variable_selection(true)
    else
      apply_changes;
  end;
  if key = 27 then // Escape
  begin
    if CreateEventsPanel.Visible then
      btnCreateEventsCancelClick(Sender)
    else if SelectVariablePanel.Visible then
      end_variable_selection(false)
    else if pnEventExportMarker.Visible then
    begin
      pnEventExportMarker.Visible := false;
      EventGrid.Options := EventGrid.Options - [goRangeSelect];
    end else
      Close;
  end;
  if key = 123 then // F2
    MainWindow.Show;
  if (key = 107) and (ActiveControl <> edMessageText) then // Num +
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

procedure TEventDialog.lbEventTypeListClick(Sender: TObject);
var
  category_num: integer;
begin
  if event_type_list_mapping[lbEventTypeList.ItemIndex] < 0 then
  begin
    category_num := event_type_list_mapping[lbEventTypeList.ItemIndex] * -1 - 1;
    event_category_first_type := EventConfig.event_type_categories[category_num].first;
    if (category_num + 1) < EventConfig.cnt_valid_event_type_categories then
      event_category_last_type := EventConfig.event_type_categories[category_num + 1].first - 1
    else
      event_category_last_type := 255;
  end else
  begin
    event_category_first_type := -1;
    event_category_last_type := -1;
  end;
  if Markseltype1.Checked then
    EventGrid.Invalidate;
end;

procedure TEventDialog.lbEventTypeListDblClick(Sender: TObject);
var
  newpos: integer;
begin
  if event_type_list_mapping[lbEventTypeList.ItemIndex] < 0 then
    exit;
  newpos := Mission.add_event(selected_event + 1, event_type_list_mapping[lbEventTypeList.ItemIndex]);
  if newpos <> -1 then
  begin
    fill_grids;
    if EventGrid.Row = newpos + 1 then
      select_event(newpos)
    else
      EventGrid.Row := newpos + 1;
  end;
end;

procedure TEventDialog.edEventTypeFilterChange(Sender: TObject);
begin
  fill_event_type_list;
end;

procedure TEventDialog.EventGridSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
begin
  if loading then
    exit;
  apply_changes;
  if selected_event <> ARow-1 then
  begin
    if Markcounterpart1.Checked and (selected_event_had_counterpart or ((ARow-1 < Mission.num_events) and (Mission.event_indentation[ARow-1].counterpart_event <> -1))) then
      EventGrid.Invalidate;
    select_event(ARow-1);
  end;
end;

procedure TEventDialog.EventGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if EventGrid.Row > Mission.num_events then
    exit;
  if (key = ord('C')) then
    btnEventConditionListCopyClick(nil);
  if (key = ord('P')) then
    btnEventConditionListPasteClick(nil);
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

procedure TEventDialog.EventGridMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  first_event, last_event: integer;
begin
  if not pnEventExportMarker.Visible then
    exit;
  first_event := EventGrid.Selection.Top - 1;
  last_event := Min(EventGrid.Selection.Bottom - 1, Mission.num_events - 1);
  pnEventExportMarker.Visible := false;
  EventGrid.Options := EventGrid.Options - [goRangeSelect];
  if (first_event = Mission.num_events) or (Mission.num_events = 0) then
    exit;
  ExportEventsDialog.Title := Format('Export events (%d - %d)', [first_event, last_event]);
  if ExportEventsDialog.Execute then
    Mission.export_events(first_event, last_event, ExportEventsDialog.FileName);
end;

procedure TEventDialog.EventGridMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if EventGrid.TopRow < (Min(Mission.num_events + IfThen(Settings.EventGridShowEmptyLines, EventGrid.Height div EventGrid.RowHeights[1], 2), MAX_EVENTS + 1) - EventGrid.VisibleRowCount) then
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

procedure TEventDialog.EventGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
var
  i: integer;
  is_separator: boolean;
  event_note: string;
  separator_color: integer;
  negation: boolean;
begin
  if (ARow = 0) or (ACol = 0) or (ARow - 1 = selected_event) or (ARow - 1 >= Mission.num_events) then
    exit;
  if (goRangeSelect in EventGrid.Options) and (ARow >= EventGrid.Selection.Top) and (ARow <= EventGrid.Selection.Bottom) then
    exit;
  event_note := IfThen(MissionIni.mission_ini_assigned, MissionIni.event_notes[ARow - 1], '');
  is_separator := Length(event_note) > 0;
  separator_color := 0;
  for i := 1 to Length(event_note) do
    if event_note[i] <> '=' then
    begin
      if (i > 1) and (event_note[i] = '#') then
      begin
        event_note := Copy(event_note, i+1, Length(event_note) - i);
        separator_color := StrToIntDef('$' + event_note, 0);
        break;
      end else
        is_separator := false;
    end;
  if ((Mission.event_data[ARow - 1].event_flags and 2) <> 0) and is_separator then
  begin
    EventGrid.Canvas.Brush.Color := ((separator_color and $ff) shl 16) or (separator_color and $ff00) or ((separator_color and $ff0000) shr 16);
    EventGrid.Canvas.FillRect(Rect);
    exit;
  end
  else if Markselcondition1.Checked and Mission.check_event_has_condition(ARow - 1, selected_condition, negation) then
  begin
    if not negation then
      EventGrid.Canvas.Brush.Color := clYellow
    else
      EventGrid.Canvas.Brush.Color := $00E0E0;
  end
  else if Markseltype1.Checked and ((Mission.event_data[ARow - 1].event_type = event_type_list_mapping[lbEventTypeList.ItemIndex]) or ((Mission.event_data[ARow - 1].event_type >= event_category_first_type) and (Mission.event_data[ARow - 1].event_type <= event_category_last_type))) and (ARow - 1 < Mission.num_events) then
  begin
    EventGrid.Canvas.Brush.Color := clYellow;
  end
  else if Mission.event_indentation[ARow - 1].invalid then
  begin
    EventGrid.Canvas.Brush.Color := clRed;
  end
  else if Markcounterpart1.Checked and (Mission.event_indentation[selected_event].counterpart_event = ARow - 1) then
  begin
    EventGrid.Canvas.Brush.Color := clYellow;
  end
  else if EventConfig.event_types[Mission.event_data[ARow - 1].event_type].color <> -1 then
  begin
    EventGrid.Canvas.Brush.Color := EventConfig.event_types[Mission.event_data[ARow - 1].event_type].color;
  end else
    exit;
  EventGrid.Canvas.FillRect(Rect);
  EventGrid.Canvas.TextRect(Rect,Rect.Left+2,Rect.Top+2,EventGrid.Cells[ACol,ARow]);
end;

procedure TEventDialog.Addevent1Click(Sender: TObject);
begin
  if Mission.add_event(Mission.num_events, 0) <> -1 then
  begin
    fill_grids;
    if EventGrid.Row = Mission.num_events then
      select_event(EventGrid.Row-1)
    else
      EventGrid.Row := Mission.num_events;
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
    Mission.event_data[selected_event + 1] := Mission.event_data[selected_event];
    MissionIni.event_notes[selected_event + 1] := MissionIni.event_notes[selected_event];
    fill_grids;
    EventGrid.Row := selected_event + 2;
  end;
end;

procedure TEventDialog.Deleteselectedevent1Click(Sender: TObject);
begin
  if selected_event >= Mission.num_events then
    exit;
  Mission.delete_event(selected_event);
  fill_grids;
  select_event(selected_event);
end;

procedure TEventDialog.Deletelastevent1Click(Sender: TObject);
begin
  if Mission.num_events = 0 then
    exit;
  Mission.delete_event(Mission.num_events - 1);
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
  if selected_event >= Mission.num_events - 1 then
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
  cbCreateEventsUseHouseID.Visible := false;
  create_event_type := ceUnitSpawn;
end;

procedure TEventDialog.Harvesterreplacement1Click(Sender: TObject);
begin
  CreateEventsPanel.Visible := true;
  lblCreateEvents.Caption := 'Create Harvester replacement';
  seCreateEventsNum.Visible := false;
  lblCreateEventsCount.Visible := false;
  cbCreateEventsUseHouseID.Visible := false;
  create_event_type := ceHarvRepl;
end;

procedure TEventDialog.Annihilatemessage1Click(Sender: TObject);
begin
  CreateEventsPanel.Visible := true;
  lblCreateEvents.Caption := 'Create Side annihilated message';
  seCreateEventsNum.Visible := true;
  lblCreateEventsCount.Visible := false;
  cbCreateEventsUseHouseID.Visible := true;
  create_event_type := ceAnnihMsg;
  seCreateEventsNum.Value := cbCreateEventsSide.ItemIndex;
end;

procedure TEventDialog.Createrunonceflag1Click(Sender: TObject);
begin
  Mission.add_run_once_flag(selected_event);
  update_contents;
end;

procedure TEventDialog.Exportevents1Click(Sender: TObject);
begin
  pnEventExportMarker.Visible := true;
  EventGrid.Options := EventGrid.Options + [goRangeSelect];
end;

procedure TEventDialog.Importevents1Click(Sender: TObject);
begin
  if ImportEventsDialog.Execute then
    Mission.import_events(ImportEventsDialog.FileName);
end;

procedure TEventDialog.MarkEventsClick(Sender: TObject);
begin
  EventGrid.Invalidate;
end;

procedure TEventDialog.btnCreateEventsCancelClick(Sender: TObject);
begin
  CreateEventsPanel.Visible := false;
  EventGrid.SetFocus;
end;

procedure TEventDialog.cbCreateEventsSideChange(Sender: TObject);
begin
  if create_event_type = ceAnnihMsg then
    seCreateEventsNum.Value := cbCreateEventsSide.ItemIndex;
end;

procedure TEventDialog.btnCreateEventsOkClick(Sender: TObject);
begin
  EventGrid.Row := Mission.num_events + 1;
  case create_event_type of
    ceUnitSpawn: Mission.create_unit_spawn(cbCreateEventsSide.ItemIndex, seCreateEventsNum.Value);
    ceHarvRepl: Mission.create_harvester_replacement(cbCreateEventsSide.ItemIndex);
    ceAnnihMsg: Mission.create_annihilated_message(cbCreateEventsSide.ItemIndex, cbCreateEventsUseHouseID.Checked, seCreateEventsNum.Value);
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

procedure TEventDialog.EventFlagsClick(Sender: TObject);
begin
  if loading then
    exit;
  tmp_event.event_flags := tmp_event.event_flags and (not 7);
  if cbEventAutoBlock.Checked then
    tmp_event.event_flags := tmp_event.event_flags or 1;
  if cbEventBlocked.Checked then
    tmp_event.event_flags := tmp_event.event_flags or 2;
  if rbEventConditionsOr.Checked then
    tmp_event.event_flags := tmp_event.event_flags or 4;
end;

procedure TEventDialog.sbShowEventHelpClick(Sender: TObject);
begin
  sbShowConditionHelp.Down := false;
  pnEventHelp.Visible := sbShowEventHelp.Down;
  fill_event_help_text;
end;

procedure TEventDialog.cbxEventGameStructMemberChange(Sender: TObject);
var
  et: TEventTypeDefinitionPtr;
  member: TGameStructMemberPtr;
begin
  if cbxEventGameStructMember.ItemIndex = -1 then
    exit;
  et := Addr(EventConfig.event_types[IfThen(selected_event < Mission.num_events, tmp_event.event_type, -1)]);
  if et.gamestruct_index = -1 then
    exit;
  member := GameStructs.get_struct_member(et.gamestruct_index, cbxEventGameStructMember.ItemIndex);
  if member = nil then
    exit;
  set_integer_struct_member(Addr(tmp_event), Addr(event_args_struct_members), et.gamestruct_datatype_arg, Ord(member.data_type));
  set_integer_struct_member(Addr(tmp_event), Addr(event_args_struct_members), et.gamestruct_offset_arg, member.offset);
  fill_event_ui;
end;

procedure TEventDialog.btnAddValueClick(Sender: TObject);
begin
  if selected_event >= Mission.num_events then
    exit;
  if (event_value_list_type = edUnitList) or (event_value_list_type = edValueList) then
  begin
    if EventValueSelectionList.ItemIndex = -1 then
      exit;
    if tmp_event.amount = Length(tmp_event.data) then
      exit;
    tmp_event.data[tmp_event.amount] := EventValueSelectionList.ItemIndex;
  end;
  if event_value_list_type = edCoordList then
  begin
    if tmp_event.amount = (Length(tmp_event.data) div 2) then
      exit;
    tmp_event.data[tmp_event.amount * 2 + 1] := seEventValueListXCoord1.Value;
    tmp_event.data[tmp_event.amount * 2 + 2] := seEventValueListYCoord1.Value;
  end;
  if event_value_list_type = edAreaList then
  begin
    if tmp_event.amount = (Length(tmp_event.data) div 4) then
      exit;
    tmp_event.data[tmp_event.amount * 4 + 1] := seEventValueListXCoord1.Value;
    tmp_event.data[tmp_event.amount * 4 + 2] := seEventValueListYCoord1.Value;
    tmp_event.data[tmp_event.amount * 4 + 3] := seEventValueListXCoord2.Value;
    tmp_event.data[tmp_event.amount * 4 + 4] := seEventValueListYCoord2.Value;
  end;
  EventValueList.Items.Add(get_event_value_list_item_str(tmp_event.amount));
  acgs[1].spin_edit.Value := tmp_event.amount + 1;
end;

procedure TEventDialog.btnDeleteValueClick(Sender: TObject);
var
  i: integer;
begin
  if EventValueList.ItemIndex = -1 then
    exit;
  if (event_value_list_type = edUnitList) or (event_value_list_type = edValueList) then
  begin
    for i := EventValueList.ItemIndex to tmp_event.amount - 2 do
      tmp_event.data[i] := tmp_event.data[i+1];
    tmp_event.data[tmp_event.amount - 1] := 0;
  end;
  if event_value_list_type = edCoordList then
  begin
    for i := EventValueList.ItemIndex to tmp_event.amount - 2 do
    begin
      tmp_event.data[i * 2 + 1] := tmp_event.data[i * 2 + 3];
      tmp_event.data[i * 2 + 2] := tmp_event.data[i * 2 + 4];
    end;
    tmp_event.data[(tmp_event.amount - 1) * 2 + 1] := 0;
    tmp_event.data[(tmp_event.amount - 1) * 2 + 2] := 0;
  end;
  if event_value_list_type = edAreaList then
  begin
    for i := EventValueList.ItemIndex to tmp_event.amount - 2 do
    begin
      tmp_event.data[i * 4 + 1] := tmp_event.data[i * 4 + 5];
      tmp_event.data[i * 4 + 2] := tmp_event.data[i * 4 + 6];
      tmp_event.data[i * 4 + 3] := tmp_event.data[i * 4 + 7];
      tmp_event.data[i * 4 + 4] := tmp_event.data[i * 4 + 8];
    end;
    tmp_event.data[(tmp_event.amount - 1) * 4 + 1] := 0;
    tmp_event.data[(tmp_event.amount - 1) * 4 + 2] := 0;
    tmp_event.data[(tmp_event.amount - 1) * 4 + 3] := 0;
    tmp_event.data[(tmp_event.amount - 1) * 4 + 4] := 0;
  end;
  acgs[1].spin_edit.Value := tmp_event.amount - 1;
  EventValueList.Items.Delete(EventValueList.ItemIndex);
end;

procedure TEventDialog.btnDeleteLastValueClick(Sender: TObject);
begin
  if tmp_event.amount = 0 then
    exit;
  if (event_value_list_type = edUnitList) or (event_value_list_type = edValueList) then
  begin
    tmp_event.data[tmp_event.amount - 1] := 0;
  end;
  if event_value_list_type = edCoordList then
  begin
    tmp_event.data[(tmp_event.amount - 1) * 2 + 1] := 0;
    tmp_event.data[(tmp_event.amount - 1) * 2 + 2] := 0;
  end;
  if event_value_list_type = edAreaList then
  begin
    tmp_event.data[(tmp_event.amount - 1) * 4 + 1] := 0;
    tmp_event.data[(tmp_event.amount - 1) * 4 + 2] := 0;
    tmp_event.data[(tmp_event.amount - 1) * 4 + 3] := 0;
    tmp_event.data[(tmp_event.amount - 1) * 4 + 4] := 0;
  end;
  acgs[1].spin_edit.Value := tmp_event.amount - 1;
  EventValueList.Items.Delete(tmp_event.amount);
end;

procedure TEventDialog.btnDeleteAllValuesClick(Sender: TObject);
begin
  acgs[1].spin_edit.Value := 0;
  FillChar(tmp_event.data, Length(tmp_event.data), 0);
  EventValueList.Items.Clear;
end;

procedure TEventDialog.btnMoveValueUpClick(Sender: TObject);
var
  index: integer;
  tmp_value_str: String;
begin
  index := EventValueList.ItemIndex;
  if (index = -1) or (index = 0) then
    exit;
  if (event_value_list_type = edUnitList) or (event_value_list_type = edValueList) then
  begin
    swap_byte(Addr(tmp_event.data[index]), Addr(tmp_event.data[index-1]));
  end;
  if event_value_list_type = edCoordList then
  begin
    swap_byte(Addr(tmp_event.data[index * 2 + 1]), Addr(tmp_event.data[(index-1) * 2 + 1]));
    swap_byte(Addr(tmp_event.data[index * 2 + 2]), Addr(tmp_event.data[(index-1) * 2 + 2]));
  end;
  if event_value_list_type = edAreaList then
  begin
    swap_byte(Addr(tmp_event.data[index * 4 + 1]), Addr(tmp_event.data[(index-1) * 4 + 1]));
    swap_byte(Addr(tmp_event.data[index * 4 + 2]), Addr(tmp_event.data[(index-1) * 4 + 2]));
    swap_byte(Addr(tmp_event.data[index * 4 + 3]), Addr(tmp_event.data[(index-1) * 4 + 3]));
    swap_byte(Addr(tmp_event.data[index * 4 + 4]), Addr(tmp_event.data[(index-1) * 4 + 4]));
  end;
  tmp_value_str := EventValueList.Items[index];
  EventValueList.Items[index] := EventValueList.Items[index-1];
  EventValueList.Items[index-1] := tmp_value_str;
  EventValueList.ItemIndex := EventValueList.ItemIndex - 1;
end;

procedure TEventDialog.btnMoveValueDownClick(Sender: TObject);
var
  index: integer;
  tmp_value_str: String;
begin
  index := EventValueList.ItemIndex;
  if (index = -1) or (index = (tmp_event.amount - 1)) then
    exit;
  if (event_value_list_type = edUnitList) or (event_value_list_type = edValueList) then
  begin
    swap_byte(Addr(tmp_event.data[index]), Addr(tmp_event.data[index+1]));
  end;
  if event_value_list_type = edCoordList then
  begin
    swap_byte(Addr(tmp_event.data[index * 2 + 1]), Addr(tmp_event.data[(index+1) * 2 + 1]));
    swap_byte(Addr(tmp_event.data[index * 2 + 2]), Addr(tmp_event.data[(index+1) * 2 + 2]));
  end;
  if event_value_list_type = edAreaList then
  begin
    swap_byte(Addr(tmp_event.data[index * 4 + 1]), Addr(tmp_event.data[(index+1) * 4 + 1]));
    swap_byte(Addr(tmp_event.data[index * 4 + 2]), Addr(tmp_event.data[(index+1) * 4 + 2]));
    swap_byte(Addr(tmp_event.data[index * 4 + 3]), Addr(tmp_event.data[(index+1) * 4 + 3]));
    swap_byte(Addr(tmp_event.data[index * 4 + 4]), Addr(tmp_event.data[(index+1) * 4 + 4]));
  end;
  tmp_value_str := EventValueList.Items[index];
  EventValueList.Items[index] := EventValueList.Items[index+1];
  EventValueList.Items[index+1] := tmp_value_str;
  EventValueList.ItemIndex := EventValueList.ItemIndex + 1;
end;

procedure TEventDialog.btnEventValueListCoordsSelectClick(Sender: TObject);
begin
  selected_coord_index := -3;
  if event_value_list_type = edCoordList then
    MainWindow.start_position_selection(seEventValueListXCoord1.Value, seEventValueListYCoord1.Value, psmEventPoint);
  if event_value_list_type = edAreaList then
    MainWindow.start_position_selection(seEventValueListXCoord1.Value, seEventValueListYCoord1.Value, psmEventArea);
  close;
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

procedure TEventDialog.cbxMessageVarDataTypeChange(Sender: TObject);
var
  i: integer;
begin
  i := (Sender as TControl).Tag;
  if (Sender as TComboBox).ItemIndex = 0 then
  begin
    tmp_event.data[5 + i] := 0;
    event_message_variable[i].Text := '';
    event_message_variable[i].Enabled := False;
  end else
  begin
    if tmp_event.data[5 + i] = 0 then
      start_variable_selection(vsEventMessageVar, i, IfThen((i > 0) and (tmp_event.data[13 + i] = 0), tmp_event.data[12 + i] + 1, tmp_event.data[13 + i]), true);
    tmp_event.data[5 + i] := (Sender as TComboBox).ItemIndex;
    event_message_variable[i].Text := Mission.get_variable_name(tmp_event.data[13 + i], 1, selected_event);
    event_message_variable[i].Enabled := True;
  end;
end;

procedure TEventDialog.edMessageVariableClick(Sender: TObject);
var
  i: integer;
begin
  i := (Sender as TControl).Tag;
  start_variable_selection(vsEventMessageVar, i, tmp_event.data[13 + i], true);
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

procedure TEventDialog.btnCondExprPlusClick(Sender: TObject);
var
  cond_expr: TCondExprPtr;
begin
  cond_expr := Addr(tmp_event.data[1]);
  if cond_expr.num_operations < 8 then
  begin
    Inc(cond_expr.num_operations);
    fill_event_data_panel(edpCondExpr, true, 0);
  end;
end;

procedure TEventDialog.btnCondExprMinusClick(Sender: TObject);
var
  cond_expr: TCondExprPtr;
begin
  cond_expr := Addr(tmp_event.data[1]);
  if cond_expr.num_operations > 0 then
  begin
    Dec(cond_expr.num_operations);
    fill_event_data_panel(edpCondExpr, true, 0);
  end;
end;

procedure TEventDialog.cbxCondExprAndOrChange(Sender: TObject);
var
  cond_expr: TCondExprPtr;
  index: integer;
begin
  cond_expr := Addr(tmp_event.data[1]);
  index := (Sender as TControl).Tag;
  cond_expr.and_or := (cond_expr.and_or and not(3 shl (index * 2))) or ((Sender as TComboBox).ItemIndex shl (index * 2));
end;

procedure TEventDialog.edCondExprVariableClick(Sender: TObject);
var
  cond_expr: TCondExprPtr;
  index: integer;
begin
  cond_expr := Addr(tmp_event.data[1]);
  index := (Sender as TControl).Tag;
  start_variable_selection(vsCondExprVar, index, cond_expr.variable[index], true);
end;

procedure TEventDialog.cbxCondExprOperatorChange(Sender: TObject);
var
  cond_expr: TCondExprPtr;
  index: integer;
begin
  cond_expr := Addr(tmp_event.data[1]);
  index := (Sender as TControl).Tag;
  cond_expr.operator := (cond_expr.operator and not(15 shl (index * 4))) or (Cardinal((Sender as TComboBox).ItemIndex) shl (index * 4));
end;

procedure TEventDialog.edCondExprValueChange(Sender: TObject);
var
  cond_expr: TCondExprPtr;
  index: integer;
begin
  if loading then
    exit;
  cond_expr := Addr(tmp_event.data[1]);
  index := (Sender as TControl).Tag;
  cond_expr.value[index] := StrToIntDef((Sender as TEdit).Text, 0);
end;

procedure TEventDialog.edCondExprValueClick(Sender: TObject);
var
  cond_expr: TCondExprPtr;
  index: integer;
begin
  cond_expr := Addr(tmp_event.data[1]);
  index := (Sender as TControl).Tag;
  if (cond_expr.value_var_flags and (1 shl index)) <> 0 then
    start_variable_selection(vsCondExprValue, index, cond_expr.value[index], true);
end;

procedure TEventDialog.btnCondExprVarBtnClick(Sender: TObject);
var
  cond_expr: TCondExprPtr;
  index: integer;
begin
  cond_expr := Addr(tmp_event.data[1]);
  index := (Sender as TControl).Tag;
  if (cond_expr.value_var_flags and (1 shl index)) = 0 then
    start_variable_selection(vsCondExprValue, index, 0, true)
  else
  begin
    cond_expr.value_var_flags := cond_expr.value_var_flags and not (1 shl index);
    cond_expr.value[index] := 0;
    cond_expr_value[index].Text := IntToStr(cond_expr.value[index]);
    cond_expr_var_btn[index].Caption := 'V';
  end;
end;

procedure TEventDialog.seEventFilterSkipChange(Sender: TObject);
begin
  tmp_event.filter_skip := StrToIntDef(seEventFilterSkip.Text, 0);
end;

procedure TEventDialog.seEventFilterLimitChange(Sender: TObject);
begin
  tmp_event.data[0] := StrToIntDef(seEventFilterLimit.Text, 0);
end;

procedure TEventDialog.btnEventFilterSkipVarToggleClick(Sender: TObject);
begin
  if (tmp_event.event_flags and 16) = 0 then
    start_variable_selection(vsEventFilterSkip, 0, 0, true)
  else
  begin
    tmp_event.event_flags := tmp_event.event_flags and (not 16);
    tmp_event.filter_skip := 0;
    fill_event_data_panel(edpFilter, true, fcgs[0].object_type);
  end;
end;

procedure TEventDialog.btnEventFilterLimitVarToggleClick(Sender: TObject);
begin
  if (tmp_event.event_flags and 32) = 0 then
    start_variable_selection(vsEventFilterLimit, 0, 0, true)
  else
  begin
    tmp_event.event_flags := tmp_event.event_flags and (not 32);
    tmp_event.data[0] := 0;
    fill_event_data_panel(edpFilter, true, fcgs[0].object_type);
  end;
end;

procedure TEventDialog.edEventFilterSkipVarClick(Sender: TObject);
begin
  start_variable_selection(vsEventFilterSkip, 0, tmp_event.filter_skip, true);
end;

procedure TEventDialog.edEventFilterLimitVarClick(Sender: TObject);
begin
  start_variable_selection(vsEventFilterLimit, 0, tmp_event.data[0], true);
end;

procedure TEventDialog.btnEventFilterIndexToggleClick(Sender: TObject);
begin
  tmp_event.event_flags := (tmp_event.event_flags and (not 8)) or ((not (tmp_event.event_flags and 8)) and 8);
  tmp_event.filter_skip := 0;
  fill_event_data_panel(edpFilter, true, fcgs[0].object_type);
  if (tmp_event.event_flags and 8) <> 0 then
    start_variable_selection(vsEventObjectIndex, 0, 0, true);
end;

procedure TEventDialog.edEventFilterIndexVarClick(Sender: TObject);
begin
  start_variable_selection(vsEventObjectIndex, 0, tmp_event.filter_skip, true);
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
  if selected_event >= Mission.num_events then
    exit;
  // Check if selected condition is valid
  if selected_condition >= Mission.num_conditions then
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
  EventConditionList.Items.Add(inttostr(selected_condition) + ' - ' + EventConfig.condition_types[Mission.condition_data[selected_condition].condition_type].name + '(' + Mission.get_condition_contents(selected_condition, true) + ')');
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
  tmp_event.num_conditions := Mission.event_data[copy_conditions_from].num_conditions;
  Move(Mission.event_data[copy_conditions_from].condition_index, tmp_event.condition_index, Length(tmp_event.condition_index));
  Move(Mission.event_data[copy_conditions_from].condition_not, tmp_event.condition_not, Length(tmp_event.condition_not));
  EventConditionList.Clear;
  fill_event_condition_list;
  EventGrid.SetFocus;
end;

procedure TEventDialog.lbConditionTypeListClick(Sender: TObject);
var
  category_num: integer;
begin
  if condition_type_list_mapping[lbConditionTypeList.ItemIndex] < 0 then
  begin
    category_num := condition_type_list_mapping[lbConditionTypeList.ItemIndex] * -1 - 1;
    condition_category_first_type := EventConfig.condition_type_categories[category_num].first;
    if (category_num + 1) < EventConfig.cnt_valid_condition_type_categories then
      condition_category_last_type := EventConfig.condition_type_categories[category_num + 1].first - 1
    else
      condition_category_last_type := 255;
  end else
  begin
    condition_category_first_type := -1;
    condition_category_last_type := -1;
  end;
  if Markseltype2.Checked then
    ConditionGrid.Invalidate;
end;

procedure TEventDialog.lbConditionTypeListDblClick(Sender: TObject);
begin
  if condition_type_list_mapping[lbConditionTypeList.ItemIndex] < 0 then
    exit;
  if Mission.add_condition(condition_type_list_mapping[lbConditionTypeList.ItemIndex]) then
  begin
    fill_grids;
    if ConditionGrid.Row = Mission.num_conditions then
      select_condition(ConditionGrid.Row-1)
    else
      ConditionGrid.Row := Mission.num_conditions;
  end;
end;

procedure TEventDialog.edConditionTypeFilterChange(Sender: TObject);
begin
  fill_condition_type_list;
end;

procedure TEventDialog.ConditionGridSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
begin
  if loading then
    exit;
  apply_changes;
  if selected_condition <> ARow-1 then
    select_condition(ARow-1);
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
  if ConditionGrid.TopRow < (Min(Mission.num_conditions + IfThen(Settings.EventGridShowEmptyLines, ConditionGrid.Height div ConditionGrid.RowHeights[1], 2), MAX_CONDITIONS + 1) - ConditionGrid.VisibleRowCount) then
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

procedure TEventDialog.ConditionGridDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
  if Marknothing2.Checked then
    exit;
  if (ARow = 0) or (ACol = 0) or (ARow - 1 = selected_condition) then
    exit;
  if Markseltype2.Checked and ((Mission.condition_data[ARow - 1].condition_type = condition_type_list_mapping[lbConditionTypeList.ItemIndex]) or ((Mission.condition_data[ARow - 1].condition_type >= condition_category_first_type) and (Mission.condition_data[ARow - 1].condition_type <= condition_category_last_type))) and (ARow - 1 < Mission.num_conditions) then
  begin
    ConditionGrid.Canvas.Brush.Color := clYellow;
    ConditionGrid.Canvas.FillRect(Rect);
    ConditionGrid.Canvas.TextRect(Rect,Rect.Left+2,Rect.Top+2,ConditionGrid.Cells[ACol,ARow]);
  end;
end;

procedure TEventDialog.Addcondition1Click(Sender: TObject);
begin
  if Mission.add_condition(0) then
  begin
    fill_grids;
    if ConditionGrid.Row = Mission.num_conditions then
      select_condition(ConditionGrid.Row-1)
    else
      ConditionGrid.Row := Mission.num_conditions;
  end;
end;

procedure TEventDialog.Duplicatecondition1Click(Sender: TObject);
begin
  if Mission.add_condition(-1) then
  begin
    Mission.condition_data[Mission.num_conditions-1] := Mission.condition_data[selected_condition];
    MissionIni.condition_notes[Mission.num_conditions-1] := MissionIni.condition_notes[selected_condition];
    fill_grids;
    ConditionGrid.Row := Mission.num_conditions;
  end;
end;

procedure TEventDialog.Deleteselectedcondition1Click(Sender: TObject);
begin
  if selected_condition >= Mission.num_conditions then
    exit;
  if Mission.condition_is_used(selected_condition) and
    (Application.MessageBox('Do you really want to delete condition? All condition references will be updated.', 'Delete condition', MB_YESNO or MB_ICONQUESTION) = IDNO) then
    exit;
  Mission.delete_condition(selected_condition);
  update_contents;
end;

procedure TEventDialog.Deletelastcondition1Click(Sender: TObject);
begin
  if Mission.num_conditions = 0 then
    exit;
  if Mission.condition_is_used(Mission.num_conditions - 1) and
    (Application.MessageBox('Do you really want to delete condition? All condition references will be updated.', 'Delete condition', MB_YESNO or MB_ICONQUESTION) = IDNO) then
    exit;
  Mission.delete_condition(Mission.num_conditions - 1);
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
  if selected_condition >= Mission.num_conditions - 1 then
    exit;
  Mission.swap_conditions(selected_condition, selected_condition + 1);
  select_event(selected_event);
  inc(selected_condition);
  ConditionGrid.Row := selected_condition + 1;
  update_contents;
end;

procedure TEventDialog.MarkConditionsClick(Sender: TObject);
begin
  ConditionGrid.Invalidate;
end;

procedure TEventDialog.cbxConditionTypeChange(Sender: TObject);
begin
  change_condition_type(EventConfig.condition_type_mapping[cbxConditionType.ItemIndex]);
end;

procedure TEventDialog.sbShowConditionHelpClick(Sender: TObject);
begin
  sbShowEventHelp.Down := false;
  pnEventHelp.Visible := sbShowConditionHelp.Down;
  fill_event_help_text;
end;

procedure TEventDialog.cbxConditionGameStructMemberChange(Sender: TObject);
var
  ct: TConditionTypeDefinitionPtr;
  member: TGameStructMemberPtr;
begin
  if cbxConditionGameStructMember.ItemIndex = -1 then
    exit;
  ct := Addr(EventConfig.condition_types[IfThen(selected_condition < Mission.num_conditions, tmp_condition.condition_type, -1)]);
  if ct.gamestruct_index = -1 then
    exit;
  member := GameStructs.get_struct_member(ct.gamestruct_index, cbxConditionGameStructMember.ItemIndex);
  if member = nil then
    exit;
  set_integer_struct_member(Addr(tmp_condition), Addr(condition_args_struct_members), ct.gamestruct_datatype_arg, Ord(member.data_type));
  set_integer_struct_member(Addr(tmp_condition), Addr(condition_args_struct_members), ct.gamestruct_offset_arg, member.offset);
  fill_condition_ui;
end;

procedure TEventDialog.seConditionFilterAmountChange(Sender: TObject);
begin
  if loading then
    exit;
  tmp_condition.arg2 := StrToIntDef(seConditionFilterAmount.Text, 0);
  tmp_condition.arg1 := IfThen(rbConditionFilterAmoutEq.Checked, tmp_condition.arg1 or 1, tmp_condition.arg1 and (not 1));
end;

procedure TEventDialog.btnConditionFilterAmountVarToggleClick(Sender: TObject);
begin
  if (tmp_condition.arg1 and 2) = 0 then
    start_variable_selection(vsConditionFilterAmount, 0, 0, false)
  else
  begin
    tmp_condition.arg1 := tmp_condition.arg1 and (not 2);
    tmp_condition.arg2 := 0;
    fill_condition_ui;
  end;
end;

procedure TEventDialog.edConditionFilterAmountVarClick(Sender: TObject);
begin
  start_variable_selection(vsConditionFilterAmount, 0, tmp_condition.arg2, false)
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

procedure TEventDialog.CCGVariableToggle(Sender: TObject);
var
  ccg: TCoordControlGroupPtr;
  x_or_y: integer;
  flag_index: integer;
  is_var: boolean;
  default: integer;
begin
  ccg := Addr(ccgs[(Sender as TControl).Tag div 2]);
  x_or_y := (Sender as TControl).Tag mod 2;
  flag_index := ccg.coord_index * 2 + x_or_y;
  is_var := (ccg.var_flag_ptr^ and (1 shl flag_index)) <> 0;
  if is_var then
  begin
    ccg.var_flag_ptr^ := ccg.var_flag_ptr^ and (not (1 shl flag_index));
    set_integer_value(ccg.data_ptr, IfThen(x_or_y = 0, ccg.offset_x, ccg.offset_y), 1, ccg.coorddef.default);
    fill_coord_control_group(ccg, ccg.coorddef);
  end else
  begin
    default := 0;
    if (x_or_y = 1) and ((ccg.var_flag_ptr^ and (1 shl (ccg.coord_index * 2))) <> 0) then
      default := get_integer_value(ccg.data_ptr, ccg.offset_x, 1) + 1;
    start_variable_selection(vsCoord, (Sender as TControl).Tag, default, ccg.is_event);
  end;
end;

procedure TEventDialog.CCGTextEditClick(Sender: TObject);
var
  ccg: TCoordControlGroupPtr;
  x_or_y: integer;
begin
  ccg := Addr(ccgs[(Sender as TControl).Tag div 2]);
  x_or_y := (Sender as TControl).Tag mod 2;
  start_variable_selection(vsCoord, (Sender as TControl).Tag, get_integer_value(ccg.data_ptr, IfThen(x_or_y = 0, ccg.offset_x, ccg.offset_y), 1), ccg.is_event);
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
  // Update event gamestruct member combo
  if acg.argdef.is_gamestruct_arg then
  begin
    if (Sender as TControl).Tag < Length(event_args_struct_members) then
      fill_event_ui
    else
      fill_condition_ui;
  end;
  // Update amount of tile pairs
  if EventConfig.event_types[tmp_event.event_type].event_data = edTilePairs then
    fill_event_data_panel(edpTilePairs, true, 0);
  // Update value selection list
  if (EventConfig.event_types[tmp_event.event_type].event_data = edValueList) and ((Sender as TControl).Tag = 5) then
    fill_event_data_panel(edpValueList, true, Ord(edValueList));
  // Update condition expression
  if (EventConfig.event_types[tmp_event.event_type].event_data = edCondExpr) and ((Sender as TControl).Tag = 1) then
  begin
    FillChar(tmp_event.data[1], 24, 0);
    fill_event_data_panel(edpCondExpr, true, value);
  end;
end;

procedure TEventDialog.ACGVariableToggle(Sender: TObject);
var
  acg: TArgControlGroupPtr;
  is_var: boolean;
begin
  acg := Addr(acgs[(Sender as TControl).Tag]);
  is_var := (acg.var_flag_ptr^ and (1 shl acg.struct_member)) <> 0;
  if is_var then
  begin
    acg.var_flag_ptr^ := acg.var_flag_ptr^ and (not (1 shl acg.struct_member));
    set_integer_struct_member(acg.data_ptr, acg.struct_def, acg.struct_member, acg.argdef.default);
    fill_arg_control_group(acg, acg.argdef);
  end else
    start_variable_selection(vsArg, (Sender as TControl).Tag, acg.argdef.default, acg.is_event);
end;

procedure TEventDialog.ACGTextEditClick(Sender: TObject);
var
  acg: TArgControlGroupPtr;
begin
  acg := Addr(acgs[(Sender as TControl).Tag]);
  if (acg.var_flag_ptr^ and (1 shl acg.struct_member)) <> 0 then
    start_variable_selection(vsArg, (Sender as TControl).Tag, get_integer_struct_member(acg.data_ptr, acg.struct_def, acg.struct_member), acg.is_event)
  else if acg.argdef.arg_type = atVariable then
    start_variable_selection(vsVarArg, (Sender as TControl).Tag, get_integer_struct_member(acg.data_ptr, acg.struct_def, acg.struct_member), acg.is_event);
end;

procedure TEventDialog.FCGPositionTypeChange(Sender: TObject);
var
  fcg: TFilterControlGroupPtr;
  i: integer;
begin
  fcg := Addr(fcgs[(Sender as TControl).Tag]);
  for i := 0 to 3 do
  begin
    if fcg.cbx_position_type.ItemIndex = 0 then
    begin
      case i of
        0: fcg.lbl_position[i].Caption := 'Min X';
        1: fcg.lbl_position[i].Caption := 'Min Y';
        2: fcg.lbl_position[i].Caption := 'Max X';
        3: fcg.lbl_position[i].Caption := 'Max Y';
      end;
      fcg.lbl_position[3].Visible := True;
      fcg.se_position[3].Visible := (fcg.filter_ptr.pos_and_var_flags and 128) = 0;
      fcg.btn_var_toggle[7].Visible := True;
      fcg.edit_var_name[7].Visible := (fcg.filter_ptr.pos_and_var_flags and 128) <> 0;
    end else
    begin
      case i of
        0: fcg.lbl_position[i].Caption := 'Center X';
        1: fcg.lbl_position[i].Caption := 'Center Y';
        2: fcg.lbl_position[i].Caption := 'Radius';
        3: fcg.lbl_position[i].Caption := '';
      end;
      fcg.lbl_position[3].Visible := False;
      fcg.se_position[3].Visible := False;
      fcg.btn_var_toggle[7].Visible := False;
      fcg.edit_var_name[7].Visible := False;
    end;
  end;
  if loading then
    exit;
  fcg.filter_ptr.pos_and_var_flags := fcg.filter_ptr.pos_and_var_flags and (not 12);
  fcg.filter_ptr.pos_and_var_flags := fcg.filter_ptr.pos_and_var_flags or (fcg.cbx_position_type.ItemIndex shl 2);
end;

procedure TEventDialog.FCGValueChange(Sender: TObject);
var
  fcg: TFilterControlGroupPtr;
  i: integer;
begin
  if loading then
    exit;
  fcg := Addr(fcgs[(Sender as TControl).Tag]);
  fcg.filter_ptr.pos_and_var_flags := fcg.filter_ptr.pos_and_var_flags and (not 3);
  fcg.filter_ptr.pos_and_var_flags := fcg.filter_ptr.pos_and_var_flags or IfThen(fcg.cb_check_position.Checked, 1, 0);
  fcg.filter_ptr.pos_and_var_flags := fcg.filter_ptr.pos_and_var_flags or IfThen(fcg.cb_position_negation.Checked, 2, 0);
  for i := 0 to High(fcg.se_position) do
  begin
    fcg.se_position[i].Enabled := fcg.cb_check_position.Checked;
    fcg.btn_var_toggle[i+4].Enabled := fcg.cb_check_position.Checked;
    fcg.edit_var_name[i+4].Enabled := fcg.cb_check_position.Checked;
    if fcg.filter_ptr.pos_and_var_flags and (1 shl (i + 4)) <> 0 then
      continue;
    fcg.filter_ptr.pos_values[i] := StrToIntDef(fcg.se_position[i].Text, 0);
  end;
  fcg.cb_position_negation.Enabled := fcg.cb_check_position.Checked;
  fcg.cbx_position_type.Enabled := fcg.cb_check_position.Checked;
  fcg.btn_position_select.Enabled := fcg.cb_check_position.Checked;
  for i := 0 to High(fcg.cbx_criteria) do
  begin
    fcg.filter_ptr.criteria_type[i] := fcg.cbx_criteria[i].ItemIndex;
    fcg.filter_ptr.criteria_type[i] := fcg.filter_ptr.criteria_type[i] or (IfThen(fcg.cbx_operation[i].Visible, fcg.cbx_operation[i].ItemIndex shl 6, 0));
    if fcg.filter_ptr.pos_and_var_flags and (1 shl (i + 8)) <> 0 then
      continue;
    if EventConfig.filter_criteria[fcg.object_type, fcg.cbx_criteria[i].ItemIndex].list_type = ltNone then
      fcg.filter_ptr.criteria_value[i] := StrToIntDef(fcg.se_value[i].Text, 0)
    else
      fcg.filter_ptr.criteria_value[i] := fcg.cbx_value[i].ItemIndex;
  end;
  fcg.filter_ptr.criteria_and_or := 0;
  for i := 0 to High(fcg.cbx_and_or) do
    fcg.filter_ptr.criteria_and_or := fcg.filter_ptr.criteria_and_or or (fcg.cbx_and_or[i].ItemIndex shl (i * 2));
end;

procedure TEventDialog.FCGCriteriaChange(Sender: TObject);
var
  fcg: TFilterControlGroupPtr;
  fcd: TFilterCriteriaDefinitionPtr;
  skip_fill_lists: boolean;
  is_var: boolean;
  i: integer;
begin
  fcg := Addr(fcgs[(Sender as TControl).Tag]);
  for i := 0 to High(fcg.cbx_criteria) do
  begin
    skip_fill_lists := fcg.criteria_type[i] = fcg.cbx_criteria[i].ItemIndex;
    fcg.criteria_type[i] := fcg.cbx_criteria[i].ItemIndex;
    // None criteria - hide controls
    if fcg.cbx_criteria[i].ItemIndex = 0 then
    begin
      fcg.cbx_operation[i].Visible := false;
      fcg.se_value[i].Visible := false;
      fcg.cbx_value[i].Visible := false;
      fcg.edit_var_name[i+8].Visible := false;
      fcg.btn_var_toggle[i+8].Visible := false;
      continue;
    end;
    // Actual criteria
    fcd := Addr(EventConfig.filter_criteria[fcg.object_type, fcg.criteria_type[i]]);
    is_var := (fcg.filter_ptr.pos_and_var_flags and (1 shl (i + 8))) <> 0;
    fcg.cbx_operation[i].Visible := true;
    fcg.se_value[i].Visible := (fcd.list_type = ltNone) and not is_var;
    fcg.cbx_value[i].Visible := (fcd.list_type <> ltNone) and not is_var;
    fcg.edit_var_name[i+8].Visible := is_var;
    fcg.btn_var_toggle[i+8].Visible := true;
    fcg.btn_var_toggle[i+8].Caption := IfThen(is_var, 'C', 'V');
    if skip_fill_lists then
      continue;
    // Operation list
    if fcd.is_flag then
      fcg.cbx_operation[i].Items := cached_lists[-2]
    else
      fcg.cbx_operation[i].Items := cached_lists[-1];
    fcg.cbx_operation[i].ItemIndex := 0;
    // No list - use spin edit for numeric value
    if fcd.list_type = ltNone then
    begin
      fcg.se_value[i].MaxValue := fcd.maxval;
      fcg.se_value[i].Value := fcd.default;
      continue;
    end;
    // Actual list - initialize combo box
    case fcd.list_type of
      ltCustom: fcg.cbx_value[i].Items := fcd.values;
      ltGame: fcg.cbx_value[i].Items := GameLists.get_list_ref(fcd.game_list_type);
      ltItem: fcg.cbx_value[i].Items := cached_lists[Ord(fcd.item_list_type)];
    end;
    fcg.cbx_value[i].ItemIndex := 0;
  end;
  FCGValueChange(Sender);
end;

procedure TEventDialog.FCGBtnSelectClick(Sender: TObject);
var
  fcg_index: integer;
  fcg: TFilterControlGroupPtr;
begin
  fcg_index := (Sender as TControl).Tag;
  fcg := Addr(fcgs[fcg_index]);
  if (fcg.filter_ptr.pos_and_var_flags and 12) = 0 then
    MainWindow.start_position_selection(fcg.se_position[0].Value, fcg.se_position[1].Value, psmEventArea)
  else
    MainWindow.start_position_selection(fcg.se_position[0].Value, fcg.se_position[1].Value, psmEventPoint);
  selected_coord_index := not fcg_index;
  close;
end;

procedure TEventDialog.FCGVariableToggle(Sender: TObject);
var
  fcg: TFilterControlGroupPtr;
  flag_index: integer;
  is_var: boolean;
  default: integer;
begin
  fcg := Addr(fcgs[(Sender as TControl).Tag div 16]);
  flag_index := (Sender as TControl).Tag mod 16;
  is_var := (fcg.filter_ptr.pos_and_var_flags and (1 shl flag_index)) <> 0;
  if is_var then
  begin
    fcg.filter_ptr.pos_and_var_flags := fcg.filter_ptr.pos_and_var_flags and (not (1 shl flag_index));
    if flag_index < 8 then
      fcg.filter_ptr.pos_values[flag_index - 4] := 0
    else
      fcg.filter_ptr.criteria_value[flag_index - 8] := 0;
    fill_filter_control_group(fcg, fcg.object_type);
  end else
  begin
    default := 0;
    if (flag_index > 4) and (flag_index < 8) and ((fcg.filter_ptr.pos_and_var_flags and (1 shl (flag_index - 1))) <> 0) then
      default := fcg.filter_ptr.pos_values[flag_index - 5] + 1;
    start_variable_selection(vsFilter, (Sender as TControl).Tag, default, fcg.is_event);
  end;
end;

procedure TEventDialog.FCGTextEditClick(Sender: TObject);
var
  fcg: TFilterControlGroupPtr;
  flag_index: integer;
begin
  fcg := Addr(fcgs[(Sender as TControl).Tag div 16]);
  flag_index := (Sender as TControl).Tag mod 16;
  if flag_index < 8 then
    start_variable_selection(vsFilter, (Sender as TControl).Tag, fcg.filter_ptr.pos_values[flag_index - 4], fcg.is_event)
  else
    start_variable_selection(vsFilter, (Sender as TControl).Tag, fcg.filter_ptr.criteria_value[flag_index - 8], fcg.is_event);
end;

procedure TEventDialog.btnSelectVariableOkClick(Sender: TObject);
begin
  end_variable_selection(true);
end;

procedure TEventDialog.btnSelectVariableCancelClick(Sender: TObject);
begin
  end_variable_selection(false);
end;

procedure TEventDialog.lbSelectVariableListClick(Sender: TObject);
var
  var_index: integer;
begin
  var_index := lbSelectVariableList.ItemIndex;
  if (variable_selection_last_hook_type >= 0) and (var_index < Length(EventConfig.hook_vars[variable_selection_last_hook_type].var_name)) and (EventConfig.hook_vars[variable_selection_last_hook_type].var_name[var_index] <> '') then
  begin
    edSelectVariableName.Enabled := false;
    edSelectVariableName.Text := '#' + EventConfig.hook_vars[variable_selection_last_hook_type].var_name[var_index];
  end else
  begin
    edSelectVariableName.Enabled := true;
    edSelectVariableName.Text := MissionIni.variable_names[lbSelectVariableList.ItemIndex];
  end;
end;

procedure TEventDialog.edSelectVariableNameChange(Sender: TObject);
var
  index: integer;
begin
  if not edSelectVariableName.Enabled then
    exit;
  index := lbSelectVariableList.ItemIndex;
  if MissionIni.set_variable_name(index, edSelectVariableName.Text) then
    variable_name_changed := true;
  lbSelectVariableList.Items[index] := inttostr(index) + ': ' + Mission.get_variable_name(index, 0, -1);
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
  fill_event_type_list;
  tmp_strings.Clear;
  // Initialize condition type list
  for i := 0 to EventConfig.cnt_valid_condition_types - 1 do
    tmp_strings.Add(inttostr(EventConfig.condition_type_mapping[i]) + ' - ' + EventConfig.condition_types[EventConfig.condition_type_mapping[i]].name);
  cbxConditionType.Items := tmp_strings;
  fill_condition_type_list;
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
  end_variable_selection(false);
end;

procedure TEventDialog.update_side_list(side_list: TStringList);
var
  prev_index: integer;
  i: integer;
begin
  prev_index := cbCreateEventsSide.ItemIndex;
  cbCreateEventsSide.Items := side_list;
  cbCreateEventsSide.ItemIndex := Max(prev_index, 0);
  cached_lists[Byte(ilSides)].Assign(side_list);
  cached_lists[Byte(ilSidesAny)].Assign(side_list);
  cached_lists[Byte(ilSidesAny)].Add('Any');
  // Update currently visible arg combo boxes
  for i := 0 to High(acgs) do
    if (acgs[i].argdef <> nil) and (acgs[i].argdef.arg_type = atList) and (acgs[i].argdef.list_type = ltItem) and ((acgs[i].argdef.item_list_type = ilSides) or (acgs[i].argdef.item_list_type = ilSidesAny)) then
      fill_arg_combo_box(Addr(acgs[i]), acgs[i].combo_box.ItemIndex, true);
end;

procedure TEventDialog.update_structures_list;
var
  i: integer;
begin
  // Unit list
  cached_lists[Byte(ilUnits)].Clear;
  for i:= 0 to Structures.templates.UnitCount -1 do
    cached_lists[Byte(ilUnits)].Add(inttostr(i) + ' - ' + Structures.get_unit_name_str(i));
  // Unit group list
  cached_lists[Byte(ilUnitGroups)].Clear;
  for i:= 0 to Structures.templates.UnitGroupCount -1 do
    cached_lists[Byte(ilUnitGroups)].Add(inttostr(i) + ' - ' + Structures.get_unit_group_str(i));
  // Building list
  cached_lists[Byte(ilBuildings)].Clear;
  for i:= 0 to Structures.templates.BuildingCount -1 do
    cached_lists[Byte(ilBuildings)].Add(inttostr(i) + ' - ' + Structures.get_building_name_str(i));
  // Building group list
  cached_lists[Byte(ilBuildingGroups)].Clear;
  for i:= 0 to Structures.templates.BuildingGroupCount -1 do
    cached_lists[Byte(ilBuildingGroups)].Add(inttostr(i) + ' - ' + Structures.get_building_group_str(i));
  // Weapon list
  cached_lists[Byte(ilWeapons)].Clear;
  for i:= 0 to Structures.templates.WeaponCount -1 do
    cached_lists[Byte(ilWeapons)].Add(inttostr(i) + ' - ' + Structures.templates.WeaponStrings[i]);
  // Explosion list
  cached_lists[Byte(ilExplosions)].Clear;
  for i:= 0 to Structures.templates.ExplosionCount -1 do
    cached_lists[Byte(ilExplosions)].Add(inttostr(i) + ' - ' + Structures.templates.ExplosionStrings[i]);
  // Warhead list
  cached_lists[Byte(ilWarheads)].Clear;
  for i:= 0 to Structures.armour.WarheadCount-1 do
    cached_lists[Byte(ilWarheads)].Add(inttostr(i) + ' - ' + Structures.armour.WarheadStrings[i]);
  // Armour type list
  cached_lists[Byte(ilArmourTypes)].Clear;
  for i:= 0 to Structures.armour.ArmourTypeCount -1 do
    cached_lists[Byte(ilArmourTypes)].Add(inttostr(i) + ' - ' + Structures.armour.ArmourTypeStrings[i]);
  // Speed type list
  cached_lists[Byte(ilSpeedTypes)].Clear;
  for i:= 0 to High(Structures.speed.SpeedNameStrings) do
    cached_lists[Byte(ilSpeedTypes)].Add(inttostr(i) + ' - ' + Structures.speed.SpeedNameStrings[i]);
  // Update currently visible arg combo boxes
  for i := 0 to High(acgs) do
    if (acgs[i].argdef <> nil) and (acgs[i].argdef.arg_type = atList) and (acgs[i].argdef.list_type = ltItem) and (acgs[i].argdef.item_list_type >= ilBuildings) then
      fill_arg_combo_box(Addr(acgs[i]), acgs[i].combo_box.ItemIndex, true);
end;

procedure TEventDialog.update_sound_list;
var
  i: integer;
begin
  cached_lists[Byte(ilSounds)].Clear;
  for i := 0 to StringTable.samples_uib.Count - 1 do
    cached_lists[Byte(ilSounds)].Add(inttostr(i) + ' - ' + StringTable.samples_uib.ValueFromIndex[i]);
  // Update currently visible arg combo boxes
  for i := 0 to High(acgs) do
    if (acgs[i].argdef <> nil) and (acgs[i].argdef.arg_type = atList) and (acgs[i].argdef.list_type = ltItem) and (acgs[i].argdef.item_list_type = ilSounds) then
      fill_arg_combo_box(Addr(acgs[i]), acgs[i].combo_box.ItemIndex, true);
end;

procedure TEventDialog.update_tileset;
begin
  if EventConfig.event_types[tmp_event.event_type].event_data = edTileBlock then
    draw_tile_block;
  if EventConfig.event_types[tmp_event.event_type].event_data = edTilePairs then
    draw_tile_pairs;
end;

procedure TEventDialog.update_variable_names;
begin
  pending_update_variable_list := true;
end;

procedure TEventDialog.enable_mission_ini_features;
begin
  notes_enabled := MissionIni.mission_ini_assigned;
  lblEventNote.Visible := notes_enabled;
  lblConditionNote.Visible := notes_enabled;
  edEventNote.Visible := notes_enabled;
  edConditionNote.Visible := notes_enabled;
  btnCustomMsgText.Visible := MissionIni.mission_ini_assigned;
  lblSelectVariableName.Visible := MissionIni.mission_ini_assigned;
  edSelectVariableName.Visible := MissionIni.mission_ini_assigned;
end;

procedure TEventDialog.fill_grids;
var
  i: integer;
begin
  loading := true;
  // Fill events
  EventGrid.RowCount := Min(Mission.num_events + IfThen(Settings.EventGridShowEmptyLines, EventGrid.Height div EventGrid.RowHeights[1], 2), MAX_EVENTS + 1);
  for i := 0 to EventGrid.RowCount - 2 do
    fill_event_grid_row(i);
  // Fill conditions
  ConditionGrid.RowCount := Min(Mission.num_conditions + IfThen(Settings.EventGridShowEmptyLines, ConditionGrid.Height div ConditionGrid.RowHeights[1], 2), MAX_CONDITIONS + 1);
  for i := 0 to ConditionGrid.RowCount - 2 do
    fill_condition_grid_row(i);
  // All event grid must be redrawn
  if Markselcondition1.Checked or Markseltype1.Checked then
    EventGrid.Invalidate;
  if Markseltype2.Checked then
    ConditionGrid.Invalidate;
  loading := false;
end;

procedure TEventDialog.fill_event_help_text;
begin
  if sbShowEventHelp.Down  then
  begin
    if cbxEventType.Enabled then
      lblEventHelp.Caption := EventConfig.get_event_type_help_text(EventConfig.event_type_mapping[cbxEventType.ItemIndex])
    else
      lblEventHelp.Caption := '';
  end
  else if sbShowConditionHelp.Down then
  begin
    if cbxConditionType.Enabled then
      lblEventHelp.Caption := EventConfig.get_condition_type_help_text(EventConfig.condition_type_mapping[cbxConditionType.ItemIndex])
    else
      lblEventHelp.Caption := '';
  end;
end;

procedure TEventDialog.fill_event_type_list;
var
  i: integer;
  tmp_strings: TStringList;
  cnt_event_types, cnt_event_type_categories: integer;
begin
  tmp_strings := TStringList.Create;
  cnt_event_types := 0;
  cnt_event_type_categories := 0;
  for i := 0 to EventConfig.cnt_valid_event_types - 1 do
  begin
    if (edEventTypeFilter.Text = '') and (cnt_event_type_categories < EventConfig.cnt_valid_event_type_categories) and (EventConfig.event_type_categories[cnt_event_type_categories].first = EventConfig.event_type_mapping[i]) then
    begin
      event_type_list_mapping[cnt_event_types] := (cnt_event_type_categories + 1) * -1;
      Inc(cnt_event_types);
      Inc(cnt_event_type_categories);
    end;
    if (edEventTypeFilter.Text = '') or AnsiContainsText(EventConfig.event_types[EventConfig.event_type_mapping[i]].name, edEventTypeFilter.Text) then
    begin
      event_type_list_mapping[cnt_event_types] := EventConfig.event_type_mapping[i];
      Inc(cnt_event_types);
    end;
  end;
  for i := 0 to cnt_event_types - 1 do
    if event_type_list_mapping[i] >= 0 then
      tmp_strings.Add(inttostr(event_type_list_mapping[i]) + ' - ' + EventConfig.event_types[event_type_list_mapping[i]].name)
    else
      tmp_strings.Add('=== ' + EventConfig.event_type_categories[event_type_list_mapping[i] * -1 - 1].name + ' ===');
  lbEventTypeList.Items := tmp_strings;
  tmp_strings.Destroy;
end;

procedure TEventDialog.fill_event_grid_row(index: integer);
var
  row: integer;
  event: ^TEvent;
  et: TEventTypeDefinitionPtr;
  x_str, y_str: string;
  indent: string;
  i: integer;
begin
  row := index + 1;
  if index >= Mission.num_events then
  begin
    EventGrid.Rows[row].Clear;
    EventGrid.Cells[0,row] := inttostr(index);
    exit;
  end;
  event := Addr(Mission.event_data[index]);
  et := Addr(EventConfig.event_types[event.event_type]);
  EventGrid.Cells[0,row] := inttostr(index);
  // Basic information
  EventGrid.Cells[1,row] := IfThen((event.event_flags and 1) = 1, 'A', '') + IfThen((event.event_flags and 2) = 2, 'B', '');
  indent := '';
  for i := 0 to Mission.event_indentation[index].indent - 1 do
    indent := indent + '  ';
  EventGrid.Cells[2,row] := indent + et.name;
  if (et.coords[0].coord_type = ctPoint) and (et.coords[0].marker <> ' ') and evaluate_show_if(Addr(et.coords[0].show_if), event, Addr(event_args_struct_members)) then
  begin
    if (event.coord_var_flags and 1) <> 0 then
      x_str := Mission.get_variable_name(event.coord_x[0], 1, index)
    else
      x_str := inttostr(event.coord_x[0]);
    if (event.coord_var_flags and 2) <> 0 then
      y_str := Mission.get_variable_name(event.coord_y[0], 1, index)
    else
      y_str := inttostr(event.coord_y[0]);
    EventGrid.Cells[3,row] := x_str + ' , ' + y_str;
  end else
    EventGrid.Cells[3,row] := '';
  if et.has_side and evaluate_show_if(Addr(et.args[0].show_if), event, Addr(event_args_struct_members)) then
  begin
    if (event.arg_var_flags and 1) <> 0 then
      EventGrid.Cells[4,row] := Mission.get_variable_name(event.side, 1, index)
    else
      EventGrid.Cells[4,row] := IfThen(event.side < 8, Structures.side_names[event.side], 'Any')
  end else
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
  tmp_event := Mission.event_data[index];
  lblEventNumber.Caption := 'Event ' + inttostr(index);
  event_valid := index < Mission.num_events;
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
  cbEventAutoBlock.Checked := event_valid and ((tmp_event.event_flags and 1) = 1);
  cbEventBlocked.Checked := event_valid and ((tmp_event.event_flags and 2) = 2);
  rbEventConditionsOr.Checked := event_valid and ((tmp_event.event_flags and 4) = 4);
  rbEventConditionsAnd.Checked := not rbEventConditionsOr.Checked;
  loading := false;
  edEventNote.Enabled := event_valid;
  fill_event_ui;
  fill_event_help_text;
  selected_event_had_counterpart := event_valid and (Mission.event_indentation[index].counterpart_event <> -1);
end;

procedure TEventDialog.fill_event_ui;
var
  i: integer;
  panel_top: integer;
  et: TEventTypeDefinitionPtr;
  ed: EventData;
begin
  et := Addr(EventConfig.event_types[IfThen(selected_event < Mission.num_events, tmp_event.event_type, -1)]);
  ed := et.event_data;
  panel_top := 48;
  // Fill event coordinates
  for i := 0 to High(et.coords) do
  begin
    fill_coord_control_group(Addr(ccgs[i]), Addr(et.coords[i]));
    if ccgs[i].coorddef.name <> '' then
    begin
      ccgs[i].container.Top := panel_top;
      inc(panel_top, ccgs[i].container.Height);
    end;
  end;
  // Fill event arguments
  lblEventGameStructMember.Top := -20;
  for i := 0 to High(et.args) do
  begin
    fill_arg_control_group(Addr(acgs[i]), get_event_arg_def(et, i));
    if acgs[i].argdef.name <> '' then
    begin
      acgs[i].container.Top := panel_top;
      inc(panel_top, acgs[i].container.Height);
    end;
    // Fill gamestruct member combo box
    if i = et.gamestruct_offset_arg then
      fill_event_gamestruct_member_combo(et, panel_top);
  end;
  // Fill event data UI
  fill_event_data_panel(edpValueList,    (ed >= edUnitList) and (ed <= edAreaList), ord(ed));
  fill_event_data_panel(edpByteValues,   ed = edByteValues, 0);
  fill_event_data_panel(edpMessage,      ed = edMessage, 0);
  fill_event_data_panel(edpMusic,        ed = edMusic, 0);
  fill_event_data_panel(edpTileBlock,    ed = edTileBlock, 0);
  fill_event_data_panel(edpTilePairs,    ed = edTilePairs, 0);
  fill_event_data_panel(edpFilter,       ed >= edUnitFilter, ord(ed) - ord(edUnitFilter));
  fill_event_data_panel(edpCondExpr,     ed = edCondExpr, tmp_event.amount);
  // Fill event note
  if notes_enabled then
    edEventNote.Text := MissionIni.event_notes[selected_event];
  // Fill condition list
  fill_event_condition_list;
end;

function TEventDialog.get_event_arg_def(et: TEventTypeDefinitionPtr; arg_num: integer): TArgDefinitionPtr;
var
  data_type, offset: integer;
  gamestruct_member: TGameStructMemberPtr;
begin
  result := Addr(et.args[arg_num]);
  if (et.gamestruct_index = -1) or (arg_num <> et.gamestruct_value_arg) then
    exit;
  event_gamestruct_value_arg_def := et.args[arg_num];
  result := Addr(event_gamestruct_value_arg_def);
  data_type := get_integer_struct_member(Addr(tmp_event), Addr(event_args_struct_members), et.gamestruct_datatype_arg);
  if data_type = Ord(dtFloat) then
  begin
    event_gamestruct_value_arg_def.arg_type := atFloat;
    exit;
  end;
  offset := get_integer_struct_member(Addr(tmp_event), Addr(event_args_struct_members), et.gamestruct_offset_arg);
  gamestruct_member := GameStructs.get_struct_member(et.gamestruct_index, data_type, offset);
  if (gamestruct_member = nil) or (gamestruct_member.list_type = ltNone) then
    exit;
  event_gamestruct_value_arg_def.arg_type := atList;
  event_gamestruct_value_arg_def.list_type := gamestruct_member.list_type;
  event_gamestruct_value_arg_def.game_list_type := gamestruct_member.game_list_type;
  event_gamestruct_value_arg_def.item_list_type := gamestruct_member.item_list_type;
end;

procedure TEventDialog.fill_event_gamestruct_member_combo(et: TEventTypeDefinitionPtr; var panel_top: integer);
var
  data_type, offset: integer;
  list: TStringList;
begin
  lblEventGameStructMember.Visible := et.gamestruct_index <> -1;
  cbxEventGameStructMember.Visible := et.gamestruct_index <> -1;
  if et.gamestruct_index = -1 then
    exit;
  lblEventGameStructMember.Top := panel_top + 8;
  cbxEventGameStructMember.Top := panel_top + 4;
  inc(panel_top, 28);
  if (tmp_event.event_type <> cbxEventGameStructMember.Tag) then
  begin
    cbxEventGameStructMember.Tag := tmp_event.event_type;
    list := GameStructs.get_struct_member_name_list(et.gamestruct_index);
    if list <> nil then
      cbxEventGameStructMember.Items := list
    else
      cbxEventGameStructMember.Clear;
  end;
  data_type := get_integer_struct_member(Addr(tmp_event), Addr(event_args_struct_members), et.gamestruct_datatype_arg);
  offset := get_integer_struct_member(Addr(tmp_event), Addr(event_args_struct_members), et.gamestruct_offset_arg);
  cbxEventGameStructMember.ItemIndex := GameStructs.get_struct_member_index(et.gamestruct_index, data_type, offset);
end;

procedure TEventDialog.fill_event_data_panel(panel: TPanel; active: boolean; object_type: integer);
var
  i, j: integer;
  tmp: string;
  list_type: ItemListType;
  cond_expr: TCondExprPtr;
begin
  panel.Visible := active;
  if not active then
    exit;
  if panel = edpValueList then
  begin
    event_value_list_type := EventData(object_type);
    EventValueSelectionList.Visible := (event_value_list_type = edUnitList) or (event_value_list_type = edValueList);
    pnEventValueListCoords.Visible := (event_value_list_type = edCoordList) or (event_value_list_type = edAreaList);
    seEventValueListXCoord2.Visible := event_value_list_type = edAreaList;
    seEventValueListYCoord2.Visible := event_value_list_type = edAreaList;
    case event_value_list_type of
      edUnitList:
        begin
          lblEventValueSelectionList.Caption := 'Unit selection';
          lblEventValueList.Caption := 'Units in event';
          EventValueSelectionList.Items := cached_lists[Byte(ilUnits)];
        end;
      edValueList:
        begin
          lblEventValueSelectionList.Caption := 'Value selection';
          lblEventValueList.Caption := 'Values in event';
          list_type := ilNone;
          case tmp_event.value of
            1: list_type := ilUnits;
            2: list_type := ilBuildings;
          end;
          EventValueSelectionList.Items := cached_lists[Ord(list_type)];
        end;
      edCoordList:
        begin
          lblEventValueSelectionList.Caption := 'Coord selection';
          lblEventValueList.Caption := 'Coords in event';
        end;
      edAreaList:
        begin
          lblEventValueSelectionList.Caption := 'Area selection';
          lblEventValueList.Caption := 'Areas in event';
        end;
    end;
    EventValueList.Items.Clear;
    for i := 0 to tmp_event.amount - 1 do
      EventValueList.Items.Add(get_event_value_list_item_str(i));
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
    for i := 0 to Length(event_message_var_datatype) - 1 do
    begin
      event_message_var_datatype[i].ItemIndex := tmp_event.data[5 + i];
      if tmp_event.data[5 + i] = 0 then
      begin
        event_message_variable[i].Text := '';
        event_message_variable[i].Enabled := False;
      end else
      begin
        event_message_variable[i].Text := Mission.get_variable_name(tmp_event.data[13 + i], 1, selected_event);
        event_message_variable[i].Enabled := True;
      end;
    end;
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
  if panel = edpCondExpr then
  begin
    if object_type = 0 then
    begin
      edpFilter.Visible := False;
      loading := true;
      cond_expr := Addr(tmp_event.data[1]);
      for i := 0 to 6 do
      begin
        cond_expr_and_or[i].Visible := cond_expr.num_operations > (i + 1);
        cond_expr_and_or[i].ItemIndex := (cond_expr.and_or shr (i * 2)) and 3;
      end;
      for i := 0 to 7 do
      begin
        cond_expr_variable[i].Visible := cond_expr.num_operations > i;
        cond_expr_variable[i].Text := Mission.get_variable_name(cond_expr.variable[i], 1, selected_event);
        cond_expr_operator[i].Visible := cond_expr.num_operations > i;
        cond_expr_operator[i].ItemIndex := (cond_expr.operator shr (i * 4)) and 15;
        cond_expr_value[i].Visible := cond_expr.num_operations > i;
        cond_expr_var_btn[i].Visible := cond_expr.num_operations > i;
        if (cond_expr.value_var_flags shr i) and 1 = 1 then
        begin
          cond_expr_value[i].Text := Mission.get_variable_name(cond_expr.value[i], 1, selected_event);
          cond_expr_var_btn[i].Caption := 'C';
        end else
        begin
          cond_expr_value[i].Text := IntToStr(cond_expr.value[i]);
          cond_expr_var_btn[i].Caption := 'V';
        end;
      end;
      loading := false;
    end else
    begin
      panel.Visible := False;
      edpFilter.Visible := True;
      btnEventFilterIndexToggle.Visible := False;
      pnEventFilterLimitSkip.Visible := False;
      pnEventFilterBody.Visible := True;
      edEventFilterIndexVar.Visible := False;
      lblEventFilterIndexVar.Visible := False;
      fill_filter_control_group(Addr(fcgs[0]), object_type - 1);
    end;
  end;
  if panel = edpFilter then
  begin
    btnEventFilterIndexToggle.Visible := EventConfig.event_types[tmp_event.event_type].allow_obj_index;
    if (tmp_event.event_flags and 8) <> 0 then
    begin
      btnEventFilterIndexToggle.Caption := 'Filter';
      pnEventFilterBody.Visible := False;
      pnEventFilterLimitSkip.Visible := False;
      edEventFilterIndexVar.Visible := True;
      lblEventFilterIndexVar.Visible := True;
      edEventFilterIndexVar.Text := Mission.get_variable_name(tmp_event.filter_skip, 1, selected_event);
    end else
    begin
      btnEventFilterIndexToggle.Caption := 'Index';
      pnEventFilterBody.Visible := True;
      pnEventFilterLimitSkip.Visible := object_type < 7;
      edEventFilterIndexVar.Visible := False;
      lblEventFilterIndexVar.Visible := False;
      if (tmp_event.event_flags and 16) <> 0 then
      begin
        seEventFilterSkip.Visible := False;
        edEventFilterSkipVar.Visible := True;
        edEventFilterSkipVar.Text := Mission.get_variable_name(tmp_event.filter_skip, 1, selected_event);
        btnEventFilterSkipVarToggle.Caption := 'C';
      end else
      begin
        seEventFilterSkip.Visible := True;
        seEventFilterSkip.Value := tmp_event.filter_skip;
        edEventFilterSkipVar.Visible := False;
        btnEventFilterSkipVarToggle.Caption := 'V';
      end;
      if (tmp_event.event_flags and 32) <> 0 then
      begin
        seEventFilterLimit.Visible := False;
        edEventFilterLimitVar.Visible := True;
        edEventFilterLimitVar.Text := Mission.get_variable_name(tmp_event.data[0], 1, selected_event);
        btnEventFilterLimitVarToggle.Caption := 'C';
      end else
      begin
        seEventFilterLimit.Visible := True;
        seEventFilterLimit.Value := tmp_event.data[0];
        edEventFilterLimitVar.Visible := False;
        btnEventFilterLimitVarToggle.Caption := 'V';
      end;
      fill_filter_control_group(Addr(fcgs[0]), object_type);
    end;
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
    EventConditionList.Items.Add(inttostr(cond_index) + ' - ' + EventConfig.condition_types[Mission.condition_data[cond_index].condition_type].name + '(' + Mission.get_condition_contents(tmp_event.condition_index[i], true) + ')');
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
  fill_event_help_text;
end;

procedure TEventDialog.apply_event_changes;
var
  et: TEventTypeDefinitionPtr;
  old_event_used_position, old_event_used_area: boolean;
begin
  if selected_event >= Mission.num_events then
    exit;
  et := Addr(EventConfig.event_types[tmp_event.event_type]);
  old_event_used_position := EventConfig.event_types[Mission.event_data[selected_event].event_type].has_map_pos;
  old_event_used_area := EventConfig.event_types[Mission.event_data[selected_event].event_type].has_map_area;
  // Check if existing and new data differ
  if CompareMem(Addr(Mission.event_data[selected_event]), Addr(tmp_event), sizeof(TEvent)) and (MissionIni.event_notes[selected_event] = edeventNote.Text) and not ((et.event_data = edMessage) and msg_text_is_custom) then
    exit;
  // Save event data
  Mission.event_data[selected_event] := tmp_event;
  Mission.mis_modified := true;
  // Save event note
  if notes_enabled then
    MissionIni.event_notes[selected_event] := edeventNote.Text;
  // Save custom message text
  if (et.event_data = edMessage) and msg_text_is_custom then
    MissionIni.set_custom_text(get_integer_value(Addr(tmp_event.data), 21, 4), edMessageText.Text);
  // Compute event indentation
  Mission.compute_event_indentation;
  // Update GUI
  fill_grids;
  // Update event markers on map if old or new event has position
  if old_event_used_position or (et.has_map_pos) then
    Dispatcher.register_event(evMisEventPositionChange);
  if old_event_used_area or (et.has_map_area) then
    Dispatcher.register_event(evMisEventAreaChange);
end;

function TEventDialog.get_event_value_list_item_str(index: integer): String;
begin
  result := '';
  case event_value_list_type of
    edUnitList: result := Structures.get_unit_name_str(tmp_event.data[index]);
    edValueList: result := EventValueSelectionList.Items[tmp_event.data[index]];
    edCoordList: result := Format('%d , %d', [tmp_event.data[index * 2 + 1], tmp_event.data[index * 2 + 2]]);
    edAreaList: result := Format('%d , %d : %d , %d', [tmp_event.data[index * 4 + 1], tmp_event.data[index * 4 + 2], tmp_event.data[index * 4 + 3], tmp_event.data[index * 4 + 4]]);
  end;
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

procedure TEventDialog.create_message_var_controls;
var
  i: integer;
  lbl: TLabel;
  tmp_strings: TStringList;
begin
  tmp_strings := TStringList.Create;
  tmp_strings.Add('None');
  tmp_strings.Add('Number');
  tmp_strings.Add('Time');
  tmp_strings.Add('HexNumber');
  tmp_strings.Add('Float (1 decimal)');
  tmp_strings.Add('Float (2 decimals)');
  tmp_strings.Add('Float (3 decimals)');
  tmp_strings.Add('Float (4 decimals)');
  tmp_strings.Add('String from table');
  tmp_strings.Add('Unit name');
  tmp_strings.Add('Building name');
  tmp_strings.Add('Unit type');
  tmp_strings.Add('Building type');
  tmp_strings.Add('Unit group');
  tmp_strings.Add('Building group');
  tmp_strings.Add('Weapon name');
  tmp_strings.Add('Explosion name');
  tmp_strings.Add('Warhead name');
  tmp_strings.Add('Armour type');
  for i := 0 to 7 do
  begin
    lbl := TLabel.Create(Self);
    lbl.Left := 8;
    lbl.Top := 84 + i * 24;
    lbl.Caption := IntToStr(i+1);
    lbl.Parent := edpMessage;
    event_message_var_datatype[i] := TComboBox.Create(Self);
    event_message_var_datatype[i].Top := 80 + i * 24;
    event_message_var_datatype[i].Left := 20;
    event_message_var_datatype[i].Width := 116;
    event_message_var_datatype[i].Style := csDropDownList;
    event_message_var_datatype[i].Tag := i;
    event_message_var_datatype[i].OnChange := cbxMessageVarDataTypeChange;
    event_message_var_datatype[i].Parent := edpMessage;
    event_message_var_datatype[i].Items := tmp_strings;
    event_message_variable[i] := TEdit.Create(Self);
    event_message_variable[i].Top := 80 + i * 24;
    event_message_variable[i].Left := 140;
    event_message_variable[i].Width := 120;
    event_message_variable[i].ReadOnly := True;
    event_message_variable[i].Tag := i;
    event_message_variable[i].OnClick := edMessageVariableClick;
    event_message_variable[i].Parent := edpMessage;
  end;
  tmp_strings.Destroy;
end;

procedure TEventDialog.create_cond_expr_controls;
var
  i: integer;
  tmp_strings: TStringList;
begin
  tmp_strings := TStringList.Create;
  tmp_strings.Add('1&');
  tmp_strings.Add('2o');
  tmp_strings.Add('3&');
  tmp_strings.Add('4o');
  for i := 0 to Length(cond_expr_and_or) - 1 do
  begin
    cond_expr_and_or[i] := TComboBox.Create(Self);
    cond_expr_and_or[i].Top := 60 + i * 24;
    cond_expr_and_or[i].Left := 4;
    cond_expr_and_or[i].Width := 40;
    cond_expr_and_or[i].Style := csDropDownList;
    cond_expr_and_or[i].Tag := i;
    cond_expr_and_or[i].OnChange := cbxCondExprAndOrChange;
    cond_expr_and_or[i].Parent := edpCondExpr;
    cond_expr_and_or[i].Items := tmp_strings;
  end;
  tmp_strings.Clear;
  for i := 0 to Length(cond_expr_operator) - 1 do
    tmp_strings.Add(cond_expr_operator_str[i]);
  for i := 0 to 7 do
  begin
    cond_expr_variable[i] := TEdit.Create(Self);
    cond_expr_variable[i].Top := 48 + i * 24;
    cond_expr_variable[i].Left := 48;
    cond_expr_variable[i].Width := 88;
    cond_expr_variable[i].ReadOnly := True;
    cond_expr_variable[i].Tag := i;
    cond_expr_variable[i].OnClick := edCondExprVariableClick;
    cond_expr_variable[i].Parent := edpCondExpr;
    cond_expr_operator[i] := TComboBox.Create(Self);
    cond_expr_operator[i].Top := 48 + i * 24;
    cond_expr_operator[i].Left := 136;
    cond_expr_operator[i].Width := 40;
    cond_expr_operator[i].Style := csDropDownList;
    cond_expr_operator[i].Tag := i;
    cond_expr_operator[i].OnChange := cbxCondExprOperatorChange;
    cond_expr_operator[i].Parent := edpCondExpr;
    cond_expr_operator[i].Items := tmp_strings;
    cond_expr_value[i] := TEdit.Create(Self);
    cond_expr_value[i].Top := 48 + i * 24;
    cond_expr_value[i].Left := 176;
    cond_expr_value[i].Width := 88;
    cond_expr_value[i].Tag := i;
    cond_expr_value[i].OnChange := edCondExprValueChange;
    cond_expr_value[i].OnClick := edCondExprValueClick;
    cond_expr_value[i].Parent := edpCondExpr;
    cond_expr_var_btn[i] := TButton.Create(Self);
    cond_expr_var_btn[i].Top := 48 + i * 24;
    cond_expr_var_btn[i].Left := 266;
    cond_expr_var_btn[i].Width := 18;
    cond_expr_var_btn[i].Tag := i;
    cond_expr_var_btn[i].Height := 22;
    cond_expr_var_btn[i].Caption := 'V';
    cond_expr_var_btn[i].OnClick := btnCondExprVarBtnClick;
    cond_expr_var_btn[i].Parent := edpCondExpr;
  end;
  tmp_strings.Destroy;
end;

procedure TEventDialog.fill_condition_type_list;
var
  i: integer;
  tmp_strings: TStringList;
  cnt_condition_types, cnt_condition_type_categories: integer;
begin
  tmp_strings := TStringList.Create;
  cnt_condition_types := 0;
  cnt_condition_type_categories := 0;
  for i := 0 to EventConfig.cnt_valid_condition_types - 1 do
  begin
    if (edConditionTypeFilter.Text = '') and (cnt_condition_type_categories < EventConfig.cnt_valid_condition_type_categories) and (EventConfig.condition_type_categories[cnt_condition_type_categories].first = EventConfig.condition_type_mapping[i]) then
    begin
      condition_type_list_mapping[cnt_condition_types] := (cnt_condition_type_categories + 1) * -1;
      Inc(cnt_condition_types);
      Inc(cnt_condition_type_categories);
    end;
    if (edConditionTypeFilter.Text = '') or AnsiContainsText(EventConfig.condition_types[EventConfig.condition_type_mapping[i]].name, edConditionTypeFilter.Text) then
    begin
      condition_type_list_mapping[cnt_condition_types] := EventConfig.condition_type_mapping[i];
      Inc(cnt_condition_types);
    end;
  end;
  for i := 0 to cnt_condition_types - 1 do
    if condition_type_list_mapping[i] >= 0 then
      tmp_strings.Add(inttostr(condition_type_list_mapping[i]) + ' - ' + EventConfig.condition_types[condition_type_list_mapping[i]].name)
    else
      tmp_strings.Add('=== ' + EventConfig.condition_type_categories[condition_type_list_mapping[i] * -1 - 1].name + ' ===');
  lbConditionTypeList.Items := tmp_strings;
  tmp_strings.Destroy;
end;

procedure TEventDialog.fill_condition_grid_row(index: integer);
var
  row: integer;
  cond: ^TCondition;
  ct: TConditionTypeDefinitionPtr;
begin
  row := index + 1;
  if index >= Mission.num_conditions then
  begin
    ConditionGrid.Rows[row].Clear;
    ConditionGrid.Cells[0,row] := inttostr(index);
    exit;
  end;
  cond := Addr(Mission.condition_data[index]);
  ct := Addr(EventConfig.condition_types[cond.condition_type]);
  ConditionGrid.Cells[0,row] := inttostr(index);
  // Basic information
  ConditionGrid.Cells[1,row] := ct.name;
  if ct.has_side and evaluate_show_if(Addr(ct.args[0].show_if), cond, Addr(condition_args_struct_members)) then
  begin
    if (cond.arg_var_flags and 1) <> 0 then
      ConditionGrid.Cells[2,row] := Mission.get_variable_name(cond.side, 1, -1)
    else
      ConditionGrid.Cells[2,row] := IfThen(cond.side < 8, Structures.side_names[cond.side], 'Any')
  end else
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
  tmp_condition := Mission.condition_data[index];
  lblConditionProperties.Caption := 'Condition properties (condition ' + inttostr(index) + ')';
  condition_valid := index < Mission.num_conditions;
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
  edConditionNote.Enabled := condition_valid;
  fill_condition_ui;
  fill_event_help_text;
  if Markselcondition1.Checked then
    EventGrid.Invalidate;
end;

procedure TEventDialog.fill_condition_ui;
var
  i: integer;
  panel_top: integer;
  acg_index: integer;
  ct: TConditionTypeDefinitionPtr;
begin
  ct := Addr(EventConfig.condition_types[IfThen(selected_condition < Mission.num_conditions, tmp_condition.condition_type, -1)]);
  panel_top := 48;
  // Fill condition coordinates
  for i := 0 to High(ct.coords) do
  begin
    fill_coord_control_group(Addr(ccgs[i+4]), Addr(ct.coords[i]));
    if ccgs[i+4].coorddef.name <> '' then
    begin
      ccgs[i+4].container.Top := panel_top;
      inc(panel_top, ccgs[i+4].container.Height);
    end;
  end;
  // Fill event arguments
  lblConditionGameStructMember.Top := -20;
  for i := 0 to High(ct.args) do
  begin
    acg_index := i + Length(event_args_struct_members);
    fill_arg_control_group(Addr(acgs[acg_index]), get_condition_arg_def(ct, i));
    if acgs[acg_index].argdef.name <> '' then
    begin
      acgs[acg_index].container.Top := panel_top;
      inc(panel_top, acgs[acg_index].container.Height);
    end;
    // Fill gamestruct member combo box
    if i = ct.gamestruct_offset_arg then
      fill_condition_gamestruct_member_combo(ct, panel_top);
  end;
  // Fill condition data UI
  if ct.condition_data >= cdUnitFilter then
  begin
    pnConditionFilter.Visible := true;
    loading := true;
    if (tmp_condition.arg1 and 2) <> 0 then
    begin
      seConditionFilterAmount.Visible := False;
      edConditionFilterAmountVar.Visible := True;
      edConditionFilterAmountVar.Text := Mission.get_variable_name(tmp_condition.arg2, 1, -1);
      btnConditionFilterAmountVarToggle.Caption := 'C';
    end else
    begin
      seConditionFilterAmount.Visible := True;
      seConditionFilterAmount.Value := tmp_condition.arg2;
      edConditionFilterAmountVar.Visible := False;
      btnConditionFilterAmountVarToggle.Caption := 'V';
    end;
    if (tmp_condition.arg1 and 1) <> 0 then
      rbConditionFilterAmoutEq.Checked := true
    else
      rbConditionFilterAmoutGtEq.Checked := true;
    loading := false;
    fill_filter_control_group(Addr(fcgs[1]), Ord(ct.condition_data) - Ord(cdUnitFilter));
  end else
    pnConditionFilter.Visible := false;
  // Fill condition note
  if notes_enabled then
    edConditionNote.Text := MissionIni.condition_notes[selected_condition];
end;

function TEventDialog.get_condition_arg_def(ct: TConditionTypeDefinitionPtr; arg_num: integer): TArgDefinitionPtr;
var
  data_type, offset: integer;
  gamestruct_member: TGameStructMemberPtr;
begin
  result := Addr(ct.args[arg_num]);
  if (ct.gamestruct_index = -1) or (arg_num <> ct.gamestruct_value_arg) then
    exit;
  condition_gamestruct_value_arg_def := ct.args[arg_num];
  result := Addr(condition_gamestruct_value_arg_def);
  data_type := get_integer_struct_member(Addr(tmp_condition), Addr(condition_args_struct_members), ct.gamestruct_datatype_arg);
  if data_type = Ord(dtFloat) then
  begin
    condition_gamestruct_value_arg_def.arg_type := atFloat;
    exit;
  end;
  offset := get_integer_struct_member(Addr(tmp_condition), Addr(condition_args_struct_members), ct.gamestruct_offset_arg);
  gamestruct_member := GameStructs.get_struct_member(ct.gamestruct_index, data_type, offset);
  if (gamestruct_member = nil) or (gamestruct_member.list_type = ltNone) then
    exit;
  condition_gamestruct_value_arg_def.arg_type := atList;
  condition_gamestruct_value_arg_def.list_type := gamestruct_member.list_type;
  condition_gamestruct_value_arg_def.game_list_type := gamestruct_member.game_list_type;
  condition_gamestruct_value_arg_def.item_list_type := gamestruct_member.item_list_type;
end;

procedure TEventDialog.fill_condition_gamestruct_member_combo(ct: TConditionTypeDefinitionPtr; var panel_top: integer);
var
  data_type, offset: integer;
  list: TStringList;
begin
  lblConditionGameStructMember.Visible := ct.gamestruct_index <> -1;
  cbxConditionGameStructMember.Visible := ct.gamestruct_index <> -1;
  if ct.gamestruct_index = -1 then
    exit;
  lblConditionGameStructMember.Top := panel_top + 8;
  cbxConditionGameStructMember.Top := panel_top + 4;
  inc(panel_top, 28);
  if (tmp_condition.condition_type <> cbxConditionGameStructMember.Tag) then
  begin
    cbxConditionGameStructMember.Tag := tmp_condition.condition_type;
    list := GameStructs.get_struct_member_name_list(ct.gamestruct_index);
    if list <> nil then
      cbxConditionGameStructMember.Items := list
    else
      cbxConditionGameStructMember.Clear;
  end;
  data_type := get_integer_struct_member(Addr(tmp_condition), Addr(condition_args_struct_members), ct.gamestruct_datatype_arg);
  offset := get_integer_struct_member(Addr(tmp_condition), Addr(condition_args_struct_members), ct.gamestruct_offset_arg);
  cbxConditionGameStructMember.ItemIndex := GameStructs.get_struct_member_index(ct.gamestruct_index, data_type, offset);
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
  fill_event_help_text;
end;

procedure TEventDialog.apply_condition_changes;
var
  ct: TConditionTypeDefinitionPtr;
  old_condition_used_position, old_condition_used_area: boolean;
  i: integer;
  negation: boolean;
begin
  if selected_condition >= Mission.num_conditions then
    exit;
  ct := Addr(EventConfig.condition_types[tmp_condition.condition_type]);
  old_condition_used_position := EventConfig.condition_types[Mission.condition_data[selected_condition].condition_type].has_map_pos;
  old_condition_used_area := EventConfig.condition_types[Mission.condition_data[selected_condition].condition_type].has_map_area;
  // Check if existing and new data differ
  if CompareMem(Addr(Mission.condition_data[selected_condition]), Addr(tmp_condition), sizeof(TCondition)) and (MissionIni.condition_notes[selected_condition] = edConditionNote.Text) then
    exit;
  // Save condition data
  Mission.condition_data[selected_condition] := tmp_condition;
  Mission.mis_modified := true;
  // Save condition note
  if notes_enabled then
    MissionIni.condition_notes[selected_condition] := edConditionNote.Text;
  // Update GUI
  fill_condition_grid_row(selected_condition);
  for i := 0 to Mission.num_events - 1 do
    if Mission.check_event_has_condition(i, selected_condition, negation) then
      fill_event_grid_row(i);
  if Mission.check_event_has_condition(selected_event, selected_condition, negation) then
    fill_event_condition_list;
  // Update event markers on map if condition has position
  if old_condition_used_position or (ct.has_map_pos) then
    Dispatcher.register_event(evMisEventPositionChange);
  if old_condition_used_area or (ct.has_map_area) then
    Dispatcher.register_event(evMisEventAreaChange);
end;

procedure TEventDialog.create_coord_control_group(index: integer; is_event: boolean; data_ptr: Pointer; var_flag_ptr: PByte; struct_def: TStructDefinitionPtr; coord_index, offset_x, offset_y: integer; parent_panel: TPanel);
begin
  ccgs[index].is_event := is_event;
  ccgs[index].data_ptr := data_ptr;
  ccgs[index].var_flag_ptr := var_flag_ptr;
  ccgs[index].struct_def := struct_def;
  ccgs[index].coord_index := coord_index;
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
    spin_x.Width := 46;
    spin_x.MaxValue := 127;
    spin_x.Tag := index;
    spin_x.OnChange := CCGCoordinateChange;
    spin_x.Parent := container;
    spin_y := TSpinEdit.Create(self);
    spin_y.Top := 4;
    spin_y.Left := parent_panel.Width - 98;
    spin_y.Width := 46;
    spin_y.MaxValue := 127;
    spin_y.Tag := index;
    spin_y.OnChange := CCGCoordinateChange;
    spin_y.Parent := container;
    edit_var_x := TEdit.Create(self);
    edit_var_x.Top := 4;
    edit_var_x.Left := parent_panel.Width - 160;
    edit_var_x.Width := 46;
    edit_var_x.ReadOnly := true;
    edit_var_x.Tag := index * 2;
    edit_var_x.Visible := false;
    edit_var_x.OnClick := CCGTextEditClick;
    edit_var_x.Parent := container;
    edit_var_y := TEdit.Create(self);
    edit_var_y.Top := 4;
    edit_var_y.Left := parent_panel.Width - 98;
    edit_var_y.Width := 46;
    edit_var_y.ReadOnly := true;
    edit_var_y.Tag := index * 2 + 1;
    edit_var_y.Visible := false;
    edit_var_y.OnClick := CCGTextEditClick;
    edit_var_y.Parent := container;
    btn_var_toggle_x := TButton.Create(self);
    btn_var_toggle_x.Top := 4;
    btn_var_toggle_x.Left := parent_panel.Width - 114;
    btn_var_toggle_x.Width := 16;
    btn_var_toggle_x.Height := 21;
    btn_var_toggle_x.Caption := 'V';
    btn_var_toggle_x.Tag := index * 2;
    btn_var_toggle_x.OnClick := CCGVariableToggle;
    btn_var_toggle_x.Parent := container;
    btn_var_toggle_y := TButton.Create(self);
    btn_var_toggle_y.Top := 4;
    btn_var_toggle_y.Left := parent_panel.Width - 52;
    btn_var_toggle_y.Width := 16;
    btn_var_toggle_y.Height := 21;
    btn_var_toggle_y.Caption := 'V';
    btn_var_toggle_y.Tag := index * 2 + 1;
    btn_var_toggle_y.OnClick := CCGVariableToggle;
    btn_var_toggle_y.Parent := container;
    btn_select := TButton.Create(self);
    btn_select.Top := 4;
    btn_select.Left := parent_panel.Width - 36;
    btn_select.Width := 32;
    btn_select.Height := 21;
    btn_select.Caption := 'Sel';
    btn_select.Tag := index;
    btn_select.OnClick := CCGBtnSelectClick;
    btn_select.Parent := container;
  end;
end;

procedure TEventDialog.fill_coord_control_group(ccg: TCoordControlGroupPtr; coorddef: TCoordDefinitionPtr);
var
  show: boolean;
  is_var_x, is_var_y: boolean;
  value_x, value_y: integer;
begin
  ccg.coorddef := coorddef;
  show := coorddef.name <> '';
  ccg.container.Visible := show and evaluate_show_if(Addr(coorddef.show_if), ccg.data_ptr, ccg.struct_def);
  if not show then
    exit;
  ccg.caption.Caption := coorddef.name + ':';
  ccg.caption.Hint := StringReplace(coorddef.help_text, '_', #13, [rfReplaceAll, rfIgnoreCase]);
  ccg.caption.ShowHint := coorddef.help_text <> '';
  is_var_x := (ccg.var_flag_ptr^ and (1 shl (ccg.coord_index * 2))) <> 0;
  is_var_y := (ccg.var_flag_ptr^ and (1 shl (ccg.coord_index * 2 + 1))) <> 0;
  value_x := get_integer_value(ccg.data_ptr, ccg.offset_x, 1);
  value_y := get_integer_value(ccg.data_ptr, ccg.offset_y, 1);
  loading := true;
  ccg.spin_x.Visible := not is_var_x;
  ccg.edit_var_x.Visible := is_var_x;
  ccg.btn_var_toggle_x.Caption := IfThen(is_var_x, 'C', 'V');
  if is_var_x then
    ccg.edit_var_x.Text :=Mission.get_variable_name(value_x, 1, IfThen(ccg.is_event, selected_event, -1))
  else
  begin
    ccg.spin_x.Value := value_x;
    ccg.spin_x.MaxValue := coorddef.maxval;
    ccg.spin_x.ReadOnly := coorddef.readonly;
  end;
  ccg.spin_y.Visible := not is_var_y;
  ccg.edit_var_y.Visible := is_var_y;
  ccg.btn_var_toggle_y.Caption := IfThen(is_var_y, 'C', 'V');
  if is_var_y then
    ccg.edit_var_y.Text := Mission.get_variable_name(value_y, 1, IfThen(ccg.is_event, selected_event, -1))
  else
  begin
    ccg.spin_y.Value := value_y;
    ccg.spin_y.MaxValue := coorddef.maxval;
    ccg.spin_y.ReadOnly := coorddef.readonly;
  end;
  ccg.btn_select.Visible := coorddef.coord_type <> ctNone;
  loading := false;
end;

procedure TEventDialog.create_arg_control_group(index: integer; is_event: boolean; data_ptr: Pointer; var_flag_ptr: PByte; struct_def: TStructDefinitionPtr; struct_member: integer; parent_panel: TPanel);
begin
  acgs[index].is_event := is_event;
  acgs[index].data_ptr := data_ptr;
  acgs[index].var_flag_ptr := var_flag_ptr;
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
    text_edit.Width := 132;
    text_edit.Visible := False;
    text_edit.Tag := index;
    text_edit.OnChange := ACGValueChange;
    text_edit.OnClick := ACGTextEditClick;
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
    combo_box.Width := 132;
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
    btn_var_toggle := TButton.Create(self);
    btn_var_toggle.Top := 4;
    btn_var_toggle.Left := parent_panel.Width - 24;
    btn_var_toggle.Width := 20;
    btn_var_toggle.Height := 21;
    btn_var_toggle.Caption := 'V';
    btn_var_toggle.Tag := index;
    btn_var_toggle.OnClick := ACGVariableToggle;
    btn_var_toggle.Parent := container;
  end;
end;

procedure TEventDialog.fill_arg_control_group(acg: TArgControlGroupPtr; argdef: TArgDefinitionPtr);
var
  show: boolean;
  is_var: boolean;
  value: integer;
begin
  acg.argdef := argdef;
  show := argdef.arg_type <> atNone;
  acg.container.Visible := show and evaluate_show_if(Addr(argdef.show_if), acg.data_ptr, acg.struct_def);
  if not show then
    exit;
  acg.caption.Caption := argdef.name + ':';
  acg.caption.Hint := StringReplace(argdef.help_text, '_', #13, [rfReplaceAll, rfIgnoreCase]);
  acg.caption.ShowHint := argdef.help_text <> '';
  is_var := (acg.var_flag_ptr^ and (1 shl acg.struct_member)) <> 0;
  acg.btn_var_toggle.Caption := IfThen(is_var, 'C', 'V');
  // Manage visibility of datatype-specific controls
  acg.text_edit.Visible := is_var or (argdef.arg_type = atBigNumber) or (argdef.arg_type = atHexNumber) or (argdef.arg_type = atFloat) or (argdef.arg_type = atVariable);
  acg.spin_edit.Visible := (not is_var) and (argdef.arg_type = atNumber);
  acg.combo_box.Visible := (not is_var) and (argdef.arg_type = atList);
  acg.check_box.Visible := (not is_var) and (argdef.arg_type = atBool);
  acg.radio_false.Visible := (not is_var) and (argdef.arg_type = atSwitch);
  acg.radio_true.Visible := (not is_var) and (argdef.arg_type = atSwitch);
  // Manage read-only
  acg.text_edit.ReadOnly := is_var or argdef.readonly or (argdef.arg_type = atVariable);
  acg.spin_edit.ReadOnly := argdef.readonly;
  acg.combo_box.Enabled := not argdef.readonly;
  acg.check_box.Enabled := not argdef.readonly;
  acg.radio_false.Enabled := not argdef.readonly;
  acg.radio_true.Enabled := not argdef.readonly;
  // Get event arg value
  value := get_integer_struct_member(acg.data_ptr, acg.struct_def, acg.struct_member);
  // Set up datatype-specific controls
  loading := true;
  if is_var then
  begin
    acg.text_edit.Text := Mission.get_variable_name(value, IfThen(argdef.arg_type = atVariable, 2, 1), IfThen(acg.is_event, selected_event, -1));
    loading := false;
    exit;
  end;
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
    atVariable: acg.text_edit.Text := Mission.get_variable_name(value, 1, IfThen(acg.is_event, selected_event, -1));
  end;
  loading := false;
end;

procedure TEventDialog.fill_arg_combo_box(acg: TArgControlGroupPtr; value: integer; force_update_list: boolean);
begin
  case acg.argdef.list_type of
    ltNone: exit;
    ltCustom: acg.combo_box.Items := acg.argdef.values;
    ltGame: acg.combo_box.Items := GameLists.get_list_ref(acg.argdef.game_list_type);
    ltItem: acg.combo_box.Items := cached_lists[Ord(acg.argdef.item_list_type)];
  end;
  acg.combo_box.ItemIndex := value;
end;

procedure TEventDialog.create_filter_control_group(index: integer; is_event: boolean; filter_ptr: Pointer; parent_panel: TPanel);
var
  lbl: TLabel;
  i: integer;
  tmp_strings: TStringList;
begin
  fcgs[index].is_event := is_event;
  fcgs[index].object_type := -1;
  fcgs[index].filter_ptr := filter_ptr;
  tmp_strings := TStringList.Create;
  with fcgs[index] do
  begin
    cb_check_position := TCheckBox.Create(self);
    cb_check_position.Top := 0;
    cb_check_position.Caption := 'Check for position';
    cb_check_position.Width := 108;
    cb_check_position.Tag := index;
    cb_check_position.OnClick := FCGValueChange;
    cb_check_position.Parent := parent_panel;
    cb_position_negation := TCheckBox.Create(self);
    cb_position_negation.Top := 0;
    cb_position_negation.Left := 112;
    cb_position_negation.Caption := 'Negation';
    cb_position_negation.Tag := index;
    cb_position_negation.OnClick := FCGValueChange;
    cb_position_negation.Parent := parent_panel;
    cbx_position_type := TComboBox.Create(self);
    cbx_position_type.Top := 0;
    cbx_position_type.Left := 186;
    cbx_position_type.Width := 100;
    cbx_position_type.Style := csDropDownList;
    cbx_position_type.Tag := index;
    cbx_position_type.OnChange := FCGPositionTypeChange;
    cbx_position_type.Parent := parent_panel;
    tmp_strings.Add('Area');
    tmp_strings.Add('Square');
    tmp_strings.Add('Circle tiles');
    tmp_strings.Add('Circle pixels');
    cbx_position_type.Items := tmp_strings;
    for i := 0 to 3 do
    begin
      lbl_position[i] := TLabel.Create(self);
      lbl_position[i].Left := i * 62;
      lbl_position[i].Top := 22;
      lbl_position[i].Parent := parent_panel;
      se_position[i] := TSpinEdit.Create(self);
      se_position[i].Top := 36;
      se_position[i].Left := i * 62;
      se_position[i].Width := 46;
      se_position[i].MaxValue := 127;
      se_position[i].Tag := index;
      se_position[i].OnChange := FCGValueChange;
      se_position[i].Parent := parent_panel;
      btn_var_toggle[i+4] := TButton.Create(self);
      btn_var_toggle[i+4].Top := 36;
      btn_var_toggle[i+4].Left := i * 62 + 46;
      btn_var_toggle[i+4].Width := 16;
      btn_var_toggle[i+4].Height := 21;
      btn_var_toggle[i+4].Caption := 'V';
      btn_var_toggle[i+4].Tag := index * 16 + i + 4;
      btn_var_toggle[i+4].OnClick := FCGVariableToggle;
      btn_var_toggle[i+4].Parent := parent_panel;
      edit_var_name[i+4] := TEdit.Create(self);
      edit_var_name[i+4].Top := 36;
      edit_var_name[i+4].Left := i * 62;
      edit_var_name[i+4].Width := 46;
      edit_var_name[i+4].ReadOnly := true;
      edit_var_name[i+4].Visible := false;
      edit_var_name[i+4].OnClick := FCGTextEditClick;
      edit_var_name[i+4].Tag := index * 16 + i + 4;
      edit_var_name[i+4].Parent := parent_panel;
    end;
    btn_position_select := TButton.Create(self);
    btn_position_select.Left := 248;
    btn_position_select.Top := 36;
    btn_position_select.Width := 40;
    btn_position_select.Height := 22;
    btn_position_select.Caption := 'Select';
    btn_position_select.Tag := index;
    btn_position_select.OnClick := FCGBtnSelectClick;
    btn_position_select.Parent := parent_panel;
    lbl := TLabel.Create(self);
    lbl.Left := 0;
    lbl.Top := 72;
    lbl.Caption := 'And/Or';
    lbl.Parent := parent_panel;
    lbl := TLabel.Create(self);
    lbl.Left := 40;
    lbl.Top := 62;
    lbl.Caption := 'Criteria';
    lbl.Parent := parent_panel;
    lbl := TLabel.Create(self);
    lbl.Left := 117;
    lbl.Top := 62;
    lbl.Caption := 'Comp.';
    lbl.Parent := parent_panel;
    lbl := TLabel.Create(self);
    lbl.Left := 152;
    lbl.Top := 62;
    lbl.Caption := 'Value';
    lbl.Parent := parent_panel;
    for i := 0 to 7 do
    begin
      cbx_criteria[i] := TComboBox.Create(self);
      cbx_criteria[i].Top := 74 + i * 24;
      cbx_criteria[i].Left := 40;
      cbx_criteria[i].Width := 78;
      cbx_criteria[i].Style := csDropDownList;
      cbx_criteria[i].Tag := index;
      cbx_criteria[i].OnChange := FCGCriteriaChange;
      cbx_criteria[i].Parent := parent_panel;
      criteria_type[i] := -1;
      cbx_operation[i] := TComboBox.Create(self);
      cbx_operation[i].Top := 74 + i * 24;
      cbx_operation[i].Left := 117;
      cbx_operation[i].Width := 36;
      cbx_operation[i].Style := csDropDownList;
      cbx_operation[i].Tag := index;
      cbx_operation[i].OnChange := FCGValueChange;
      cbx_operation[i].Parent := parent_panel;
      se_value[i] := TSpinEdit.Create(self);
      se_value[i].Top := 74 + i * 24;
      se_value[i].Left := 152;
      se_value[i].Width := 64;
      se_value[i].Tag := index;
      se_value[i].OnChange := FCGValueChange;
      se_value[i].Parent := parent_panel;
      cbx_value[i] := TComboBox.Create(self);
      cbx_value[i].Top := 74 + i * 24;
      cbx_value[i].Left := 152;
      cbx_value[i].Width := 120;
      cbx_value[i].Style := csDropDownList;
      cbx_value[i].Tag := index;
      cbx_value[i].OnChange := FCGValueChange;
      cbx_value[i].Parent := parent_panel;
      btn_var_toggle[i+8] := TButton.Create(self);
      btn_var_toggle[i+8].Top := 74 + i * 24;
      btn_var_toggle[i+8].Left := 272;
      btn_var_toggle[i+8].Width := 16;
      btn_var_toggle[i+8].Height := 21;
      btn_var_toggle[i+8].Caption := 'V';
      btn_var_toggle[i+8].Visible := false;
      btn_var_toggle[i+8].Tag := index * 16 + i + 8;
      btn_var_toggle[i+8].OnClick := FCGVariableToggle;
      btn_var_toggle[i+8].Parent := parent_panel;
      edit_var_name[i+8] := TEdit.Create(self);
      edit_var_name[i+8].Top := 74 + i * 24;
      edit_var_name[i+8].Left := 152;
      edit_var_name[i+8].Width := 120;
      edit_var_name[i+8].ReadOnly := true;
      edit_var_name[i+8].Visible := false;
      edit_var_name[i+8].Tag := index * 16 + i + 8;
      edit_var_name[i+8].OnClick := FCGTextEditClick;
      edit_var_name[i+8].Parent := parent_panel;
    end;
    tmp_strings.Clear;
    tmp_strings.Add('1&');
    tmp_strings.Add('2o');
    tmp_strings.Add('3&');
    tmp_strings.Add('4o');
    for i := 0 to 6 do
    begin
      cbx_and_or[i] := TComboBox.Create(self);
      cbx_and_or[i].Top := 84 + i * 24;
      cbx_and_or[i].Left := 0;
      cbx_and_or[i].Width := 40;
      cbx_and_or[i].Style := csDropDownList;
      cbx_and_or[i].Tag := index;
      cbx_and_or[i].OnChange := FCGValueChange;
      cbx_and_or[i].Parent := parent_panel;
      cbx_and_or[i].Items := tmp_strings;
    end;
    tmp_strings.Destroy;
  end;
end;

procedure TEventDialog.fill_filter_control_group(fcg: TFilterControlGroupPtr; object_type: integer);
var
  tmp_strings: TStringList;
  i: integer;
begin
  // Fill criteria type combo boxes
  if fcg.object_type <> object_type then
  begin
    tmp_strings := TStringList.Create;
    for i := 0 to Length(EventConfig.filter_criteria[object_type]) - 1 do
      tmp_strings.Add(EventConfig.filter_criteria[object_type, i].name);
    for i := 0 to High(fcg.cbx_criteria) do
    begin
      fcg.cbx_criteria[i].Items := tmp_strings;
      fcg.criteria_type[i] := -1;
    end;
    tmp_strings.Destroy;
    fcg.object_type := object_type;
  end;
  // Fill data fields
  loading := true;
  fcg.cb_check_position.Enabled := object_type < 6;
  fcg.cb_check_position.Checked := (fcg.filter_ptr.pos_and_var_flags and 1) <> 0;
  fcg.cb_position_negation.Checked := (fcg.filter_ptr.pos_and_var_flags and 2) <> 0;
  fcg.cbx_position_type.ItemIndex := (fcg.filter_ptr.pos_and_var_flags shr 2) and 3;
  for i := 0 to High(fcg.se_position) do
  begin
    if (fcg.filter_ptr.pos_and_var_flags and (1 shl (i + 4))) <> 0 then
    begin
      fcg.se_position[i].Visible := false;
      fcg.edit_var_name[i + 4].Text := Mission.get_variable_name(fcg.filter_ptr.pos_values[i], 1, IfThen(fcg.is_event, selected_event, -1));
      fcg.edit_var_name[i + 4].Visible := true;
      fcg.btn_var_toggle[i + 4].Caption := 'C';
    end else
    begin
      fcg.se_position[i].Visible := true;
      fcg.se_position[i].Value := fcg.filter_ptr.pos_values[i];
      fcg.edit_var_name[i + 4].Visible := false;
      fcg.btn_var_toggle[i + 4].Caption := 'V';
    end;
    fcg.btn_var_toggle[i+4].Enabled := fcg.cb_check_position.Checked;
    fcg.edit_var_name[i+4].Enabled := fcg.cb_check_position.Checked;
    fcg.se_position[i].Enabled := fcg.cb_check_position.Checked;
  end;
  FCGPositionTypeChange(fcg.cbx_position_type);
  fcg.cb_position_negation.Enabled := fcg.cb_check_position.Checked;
  fcg.cbx_position_type.Enabled := fcg.cb_check_position.Checked;
  fcg.btn_position_select.Enabled := fcg.cb_check_position.Checked;
  for i := 0 to High(fcg.cbx_criteria) do
    fcg.cbx_criteria[i].ItemIndex := fcg.filter_ptr.criteria_type[i] and 63;
  FCGCriteriaChange(fcg.cbx_criteria[0]);
  for i := 0 to High(fcg.cbx_criteria) do
  begin
    fcg.cbx_operation[i].ItemIndex := fcg.filter_ptr.criteria_type[i] shr 6;
    if (fcg.filter_ptr.pos_and_var_flags and (1 shl (i + 8))) <> 0 then
      fcg.edit_var_name[i + 8].Text := Mission.get_variable_name(fcg.filter_ptr.criteria_value[i], 1, IfThen(fcg.is_event, selected_event, -1))
    else if EventConfig.filter_criteria[object_type, fcg.cbx_criteria[i].ItemIndex].list_type = ltNone then
      fcg.se_value[i].Value := fcg.filter_ptr.criteria_value[i]
    else
      fcg.cbx_value[i].ItemIndex := fcg.filter_ptr.criteria_value[i];
  end;
  for i := 0 to High(fcg.cbx_and_or) do
  begin
    fcg.cbx_and_or[i].ItemIndex := (fcg.filter_ptr.criteria_and_or shr (i * 2)) and 3;
  end;
  loading := false;
end;

procedure TEventDialog.start_variable_selection(selecion_type: VariableSelectionType; index, default_var: integer; is_event: boolean);
var
  hook_type: integer;
  tmp_strings: TStringList;
  i: integer;
begin
  hook_type := -1;
  if is_event and (selected_event >= 0) then
    hook_type := Mission.event_indentation[selected_event].hook_type;
  if pending_update_variable_list or (variable_selection_last_hook_type <> hook_type) then
  begin
    tmp_strings := TStringList.Create;
    for i := 0 to MAX_VARIABLES - 1 do
      tmp_strings.Add(inttostr(i) + ': ' + Mission.get_variable_name(i, 0, IfThen(is_event, selected_event, -1)));
    lbSelectVariableList.Items := tmp_strings;
    tmp_strings.Destroy;
    pending_update_variable_list := false;
    variable_selection_last_hook_type := hook_type;
  end;
  SelectVariablePanel.Visible := true;
  variable_selection_type := selecion_type;
  variable_selection_index := index;
  if default_var <> -1 then
  begin
    lbSelectVariableList.ItemIndex := default_var;
    lbSelectVariableListClick(self);
  end;
  lbSelectVariableList.SetFocus;
  LowerPanel.Enabled := false;
  UpperPanel.Enabled := false;
end;

procedure TEventDialog.end_variable_selection(status: boolean);
var
  ccg: TCoordControlGroupPtr;
  x_or_y: integer;
  flag_index: integer;
  acg: TArgControlGroupPtr;
  fcg: TFilterControlGroupPtr;
  cond_expr: TCondExprPtr;
begin
  if not SelectVariablePanel.Visible then
    exit;
  SelectVariablePanel.Visible := false;
  LowerPanel.Enabled := true;
  UpperPanel.Enabled := true;
  if variable_name_changed then
  begin
    fill_grids;
    fill_event_ui;
    fill_condition_ui;
    variable_name_changed := false;
  end;
  if not status then
    exit;
  case variable_selection_type of
    vsCoord:
      begin
        ccg := Addr(ccgs[variable_selection_index div 2]);
        x_or_y := variable_selection_index mod 2;
        flag_index := ccg.coord_index * 2 + x_or_y;
        ccg.var_flag_ptr^ := ccg.var_flag_ptr^ or (1 shl flag_index);
        set_integer_value(ccg.data_ptr, IfThen(x_or_y = 0, ccg.offset_x, ccg.offset_y), 1, lbSelectVariableList.ItemIndex);
        fill_coord_control_group(ccg, ccg.coorddef);
      end;
    vsArg:
      begin
        acg := Addr(acgs[variable_selection_index]);
        acg.var_flag_ptr^ := acg.var_flag_ptr^ or (1 shl acg.struct_member);
        set_integer_struct_member(acg.data_ptr, acg.struct_def, acg.struct_member, lbSelectVariableList.ItemIndex);
        fill_arg_control_group(acg, acg.argdef);
      end;
    vsVarArg:
      begin
        acg := Addr(acgs[variable_selection_index]);
        set_integer_struct_member(acg.data_ptr, acg.struct_def, acg.struct_member, lbSelectVariableList.ItemIndex);
        fill_arg_control_group(acg, acg.argdef);
      end;
    vsEventMessageVar:
      begin
        tmp_event.data[13 + variable_selection_index] := lbSelectVariableList.ItemIndex;
        event_message_variable[variable_selection_index].Text := Mission.get_variable_name(lbSelectVariableList.ItemIndex, 1, selected_event);
      end;
    vsCondExprVar:
      begin
        cond_expr := Addr(tmp_event.data[1]);
        cond_expr.variable[variable_selection_index] := lbSelectVariableList.ItemIndex;
        cond_expr_variable[variable_selection_index].Text := Mission.get_variable_name(lbSelectVariableList.ItemIndex, 1, selected_event);
      end;
    vsCondExprValue:
      begin
        loading := true;
        cond_expr := Addr(tmp_event.data[1]);
        cond_expr.value[variable_selection_index] := lbSelectVariableList.ItemIndex;
        cond_expr.value_var_flags := cond_expr.value_var_flags or (1 shl variable_selection_index);
        cond_expr_value[variable_selection_index].Text := Mission.get_variable_name(lbSelectVariableList.ItemIndex, 1, selected_event);
        cond_expr_var_btn[variable_selection_index].Caption := 'C';
        loading := false;
      end;
    vsEventFilterSkip:
      begin
        tmp_event.event_flags := tmp_event.event_flags or 16;
        tmp_event.filter_skip := lbSelectVariableList.ItemIndex;
        fill_event_data_panel(edpFilter, true, fcgs[0].object_type);
      end;
    vsEventFilterLimit:
      begin
        tmp_event.event_flags := tmp_event.event_flags or 32;
        tmp_event.data[0] := lbSelectVariableList.ItemIndex;
        fill_event_data_panel(edpFilter, true, fcgs[0].object_type);
      end;
    vsEventObjectIndex:
      begin
        tmp_event.filter_skip := lbSelectVariableList.ItemIndex;
        fill_event_data_panel(edpFilter, true, fcgs[0].object_type);
      end;
    vsConditionFilterAmount:
    begin
        tmp_condition.arg1 := tmp_condition.arg1 or 2;
        tmp_condition.arg2 := lbSelectVariableList.ItemIndex;
        fill_condition_ui;
    end;
    vsFilter:
      begin
        fcg := Addr(fcgs[variable_selection_index div 16]);
        flag_index := variable_selection_index mod 16;
        fcg.filter_ptr.pos_and_var_flags := fcg.filter_ptr.pos_and_var_flags or (1 shl flag_index);
        if flag_index < 8 then
          fcg.filter_ptr.pos_values[flag_index - 4] := lbSelectVariableList.ItemIndex
        else
          fcg.filter_ptr.criteria_value[flag_index - 8] := lbSelectVariableList.ItemIndex;
        fill_filter_control_group(fcg, fcg.object_type);
      end;
  end;
end;

procedure TEventDialog.finish_point_selection(x, y: integer);
var
  ccg: TCoordControlGroupPtr;
  fcg: TFilterControlGroupPtr;
begin
  Show;
  if (x = -1) and (y = -1) then
    exit;
  if (selected_coord_index = -3) then
  begin
    seEventValueListXCoord1.Value := x;
    seEventValueListYCoord1.Value := y;
    btnAddValueClick(nil);
  end
  else if (selected_coord_index < 0) then
  begin
    fcg := Addr(fcgs[not selected_coord_index]);
    fcg.filter_ptr.pos_and_var_flags := fcg.filter_ptr.pos_and_var_flags and (not 48);
    fcg.filter_ptr.pos_values[0] := x;
    fcg.filter_ptr.pos_values[1] := y;
    fill_filter_control_group(fcg, fcg.object_type);
  end else
  begin
    ccg := Addr(ccgs[selected_coord_index]);
    set_integer_value(ccg.data_ptr, ccg.offset_x, 1, x);
    set_integer_value(ccg.data_ptr, ccg.offset_y, 1, y);
    ccg.var_flag_ptr^ := ccg.var_flag_ptr^ and (not (3 shl (ccg.coord_index * 2)));
    fill_coord_control_group(ccg, ccg.coorddef);
  end;
end;

procedure TEventDialog.finish_area_selection(min_x, max_x, min_y, max_y: integer);
var
  ccg: TCoordControlGroupPtr;
  fcg: TFilterControlGroupPtr;
begin
  Show;
  if (min_x = -1) and (min_y = -1) then
    exit;
  // Set up spin edits
  if (selected_coord_index = -3) then
  begin
    seEventValueListXCoord1.Value := min_x;
    seEventValueListXCoord2.Value := max_x;
    seEventValueListYCoord1.Value := min_y;
    seEventValueListYCoord2.Value := max_y;
    btnAddValueClick(nil);
  end
  else if (selected_coord_index < 0) then
  begin
    fcg := Addr(fcgs[not selected_coord_index]);
    fcg.filter_ptr.pos_and_var_flags := fcg.filter_ptr.pos_and_var_flags and (not 240);
    fcg.filter_ptr.pos_values[0] := min_x;
    fcg.filter_ptr.pos_values[1] := min_y;
    fcg.filter_ptr.pos_values[2] := max_x;
    fcg.filter_ptr.pos_values[3] := max_y;
    fill_filter_control_group(fcg, fcg.object_type);
  end else
  begin
    ccg := Addr(ccgs[selected_coord_index]);
    set_integer_value(ccg.data_ptr, ccg.offset_x, 1, min_x);
    set_integer_value(ccg.data_ptr, ccg.offset_y, 1, min_y);
    ccg.var_flag_ptr^ := ccg.var_flag_ptr^ and (not (3 shl (ccg.coord_index * 2)));
    fill_coord_control_group(ccg, ccg.coorddef);
    ccg := Addr(ccgs[selected_coord_index + 1]);
    set_integer_value(ccg.data_ptr, ccg.offset_x, 1, max_x);
    set_integer_value(ccg.data_ptr, ccg.offset_y, 1, max_y);
    ccg.var_flag_ptr^ := ccg.var_flag_ptr^ and (not (3 shl (ccg.coord_index * 2)));
    fill_coord_control_group(ccg, ccg.coorddef);
  end;
end;

procedure TEventDialog.finish_point_and_size_selection(x, y, width, height: integer);
var
  ccg: TCoordControlGroupPtr;
  tiles: integer;
  i, j: integer;
begin
  Show;
  if (x = -1) and (y = -1) then
    exit;
  tiles := width * height;
  // Check for size of tile block
  if (EventConfig.event_types[tmp_event.event_type].event_data = edTileBlock) and (tiles > 12) then
  begin
    Application.MessageBox(PChar(Format('You selected area which consists of %d tiles. Maximum allowed number of tiles is 12.', [tiles])), 'Cannot select area', MB_OK or MB_ICONERROR);
    exit;
  end;
  // Set up spin edits
  ccg := Addr(ccgs[selected_coord_index]);
  set_integer_value(ccg.data_ptr, ccg.offset_x, 1, x);
  set_integer_value(ccg.data_ptr, ccg.offset_y, 1, y);
  ccg.var_flag_ptr^ := ccg.var_flag_ptr^ and (not (3 shl (ccg.coord_index * 2)));
  fill_coord_control_group(ccg, ccg.coorddef);
  ccg := Addr(ccgs[selected_coord_index + 1]);
  set_integer_value(ccg.data_ptr, ccg.offset_x, 1, width);
  set_integer_value(ccg.data_ptr, ccg.offset_y, 1, height);
  ccg.var_flag_ptr^ := ccg.var_flag_ptr^ and (not (3 shl (ccg.coord_index * 2)));
  fill_coord_control_group(ccg, ccg.coorddef);
  // Copy tile block from map
  if EventConfig.event_types[tmp_event.event_type].event_data = edTileBlock then
  begin
    FillChar(tmp_event.data[0], Length(tmp_event.data), 0);
    for j := 0 to height - 1 do
      for i := 0 to width - 1 do
        set_integer_value(Addr(tmp_event.data[1]), (j * (width) + i) * 2, 2, Map.data[i + x, j + y].tile and $0FFF);
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
