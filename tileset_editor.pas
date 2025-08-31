unit tileset_editor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, Menus, StdCtrls, CheckLst, XPMan, Math,
  Buttons, IniFiles, StrUtils, _tileset, Spin, Grids;

const num_tileatr_values = 8;
const max_undo_steps = 4095;

type
  TUndoEntry = record
    index: integer;
    data: int64;
    is_first: boolean;
  end;

const atr_colors_game: array[0..31] of cardinal = (
  $000800, // Building/Unit owner side (bit1)
  $001000, // Building/Unit owner side (bit2)
  $002000, // Building/Unit owner side (bit3)
  $003F00, // Occupied by Unit
  $B00060, // Occupied by Building
  $6800B8, // Occupied by Infantry (middle)
  $6400B4, // Occupied by Infantry (top-right)
  $6200B2, // Occupied by Infantry (down-right)
  $6100B1, // Occupied by Infantry (down-left)
  $6000B0, // Occupied by Infantry (top-left)
  $500000, // Wall
  $800000, // Concrete
  $000050, // Non-buildable
  $000080, // Vehicles can pass
  $003000, // Infantry can pass
  $00C000, // Buildings can be placed, Rock craters
  $000060, // Sandworm can pass, Sand craters
  $000200, // Concrete owner side (bit 1)
  $000400, // Concrete owner side (bit 2)
  $000800, // Concrete owner side (bit 3)
  $52AEF7, // Spice amount (bit 1)
  $2179E7, // Spice amount (bit 2)
  $2179E7, // Spice amount (bit 3)
  $000000, // Unknown/Unused
  $000000, // Unknown/Unused
  $000000, // Unknown/Unused
  $000000, // Unknown/Unused
  $000000, // Unknown/Unused
  $000000, // Unknown/Unused
  $002020, // Rock (wheeled +10% speed)
  $C00000, // Dunes (wheeled -50%, other -20% sp.)
  $204000  // Rough Rock (all -50% speed)
  );

const atr_colors_editor: array[0..7] of cardinal = (
  $0000C0,
  $00C000,
  $C00000,
  $8000E0,
  $0080E0,
  $80E000,
  $E00080,
  $00A0A0
  );

const fill_area_group_colors: array[0..max_fill_area_rules-1] of cardinal = (
  $808080,
  $C00000,
  $C000C0,
  $0000C0,
  $C0C000,
  $00C0C0,
  $E00080,
  $0080E0,
  $80E000,
  $8000E0,
  $00E080,
  $E08000,
  $E06060,
  $60E060,
  $6060E0,
  $00C000
  );

type
   SetOperation = (opSet, opAdd, opRemove);

type
   FilterMode = (fmAll, fmExactAtr, fmHavingAllAtr, fmHavingAnyOfAtr, fmNotHavingAtr, fmByRule, fmNothing);

type
  TTilesetEditor = class(TForm)
    MainMenu: TMainMenu;
    StatusBar: TStatusBar;
    Savechanges1: TMenuItem;
    Opentileset1: TMenuItem;
    Reloadfiles1: TMenuItem;
    Edit1: TMenuItem;
    Undo1: TMenuItem;
    Redo1: TMenuItem;
    Saveandtest1: TMenuItem;
    Applychanges1: TMenuItem;
    TilesetImageOpenDialog: TOpenDialog;
    TilesetImageSaveDialog: TSaveDialog;
    PageControl: TPageControl;
    PageImage: TTabSheet;
    PageAttributes: TTabSheet;
    lbTileAtrValue: TLabel;
    lbTileAtrNotValue: TLabel;
    lbRule: TLabel;
    edRule: TEdit;
    cbMarkSelection: TCheckBox;
    cbDrawOwnerSide: TCheckBox;
    cbDrawEditorAttributes: TCheckBox;
    cbAnyOf: TCheckBox;
    btnTileAtrValueApply: TButton;
    btnClearAttributes: TButton;
    rgOperation: TRadioGroup;
    rgFilterMode: TRadioGroup;
    edTileAtrValue: TEdit;
    edTileAtrNotValue: TEdit;
    clbTileAtrListEditor: TCheckListBox;
    clbTileAtrList: TCheckListBox;
    pnTileAtrColor: TPanel;
    stSpeedModifier: TStaticText;
    stSideBitValues: TStaticText;
    TilesetScrollBar: TScrollBar;
    TilesetImage: TImage;
    cbShowGrid: TCheckBox;
    cbAlwaysOnTop: TCheckBox;
    PageHints: TTabSheet;
    lbTileHintText: TListBox;
    PageColors: TTabSheet;
    PageFillArea: TTabSheet;
    PagePresets: TTabSheet;
    cbHideUnmarkedTiles: TCheckBox;
    Newtileset1: TMenuItem;
    btnImportTilesetImage: TButton;
    btnExportTilesetImage: TButton;
    lblNumberOfTiles: TLabel;
    seNumberOfTiles: TSpinEdit;
    btnNumberOfTilesApply: TButton;
    btnImportTilesetPortion: TButton;
    btnExportTilesetPortion: TButton;
    TilesetPortionOpenDialog: TOpenDialog;
    TilesetPortionSaveDialog: TSaveDialog;
    sbCopySelectionTo: TSpeedButton;
    sbSwapSelectionWith: TSpeedButton;
    btnEraseSelection: TButton;
    lblPageImageMouseActions: TLabel;
    lblPageAttributesMouseActions: TLabel;
    lblPageHintsMouseActions: TLabel;
    btnTileHintTextClear: TButton;
    sgMinimapColorRules: TStringGrid;
    sgFillAreaRules: TStringGrid;
    PagePaint: TTabSheet;
    sgPaintTileGroups: TStringGrid;
    cbMarkSelectedItem: TCheckBox;
    sgBlockPresetGroups: TStringGrid;
    gbBasicTilesetData: TGroupBox;
    edTilesetFancyName: TEdit;
    lblTilesetFancyName: TLabel;
    lblAuthorName: TLabel;
    edAuthorName: TEdit;
    cbCustomMinimapColorsAllowed: TCheckBox;
    lblDefaultPaintGroup: TLabel;
    cbxDefaultPaintGroup: TComboBox;
    lblExtraAttributeName: TLabel;
    edExtraAttributeName: TEdit;
    btnMinimapColorRuleAdd: TButton;
    btnMinimapColorRuleRemove: TButton;
    btnMinimapColorRuleMoveDown: TButton;
    btnMinimapColorRuleMoveUp: TButton;
    lblMinimapColorRuleName: TLabel;
    edMinimapColorRuleName: TEdit;
    lblMinimapColorRuleColor: TLabel;
    edMinimapColorRuleColor: TEdit;
    lblMinimapColorRuleRule: TLabel;
    edMinimapColorRuleRule: TEdit;
    btnMinimapColorRuleApply: TButton;
    btnFillAreaRuleAdd: TButton;
    btnFillAreaRuleRemove: TButton;
    btnFillAreaRuleMoveDown: TButton;
    btnFillAreaRuleMoveUp: TButton;
    lblFillAreaRuleName: TLabel;
    edFillAreaRuleName: TEdit;
    lblFillAreaRuleRule: TLabel;
    edFillAreaRuleRule: TEdit;
    btnFillAreaRuleApply: TButton;
    lblPaintTileGroupButtonImage: TLabel;
    imgPaintTileGroupButtonImage: TImage;
    lblPaintTileGroupName: TLabel;
    edPaintTileGroupName: TEdit;
    lblPaintTileGroupRestrictionRule: TLabel;
    edPaintTileGroupRestrictionRule: TEdit;
    lblPaintTileGroupSmoothPresetGroup: TLabel;
    lblPaintTileGroupSmoothPresets: TLabel;
    cbxPaintTileGroupSmoothPresetGroup: TComboBox;
    edPaintTileGroupSmoothPresets: TEdit;
    lblPaintTileGroupRandomMapName: TLabel;
    edPaintTileGroupRandomMapName: TEdit;
    btnPaintTileGroupApply: TButton;
    Label1: TLabel;
    lblBlockPresetGroupName: TLabel;
    edBlockPresetGroupName: TEdit;
    lblBlockPresetGroupPaintGroup: TLabel;
    cbxBlockPresetGroupPaintGroup: TComboBox;
    btnBlockPresetGroupApply: TButton;
    imgBlockPresetKeys: TImage;
    btnBlockPresetAddPreset: TButton;
    pnBlockPreset: TPanel;
    imgBlockPreset: TImage;
    seBlockPresetWidth: TSpinEdit;
    seBlockPresetHeight: TSpinEdit;
    lblBlockPresetWidth: TLabel;
    lblBlockPresetHeight: TLabel;
    btnBlockPresetAdd: TButton;
    btnBlockPresetClose: TButton;
    lblBlockPresetHint: TLabel;
    lblTileimageModified: TLabel;
    gbTilesetRules: TGroupBox;
    cbRuleDoNotDrawRockCraters: TCheckBox;
    cbRuleDoNotDrawSandCraters: TCheckBox;
    // Form actions
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    // Main menu actions
    procedure Newtileset1Click(Sender: TObject);
    procedure Opentileset1Click(Sender: TObject);
    procedure Applychanges1Click(Sender: TObject);
    procedure Savechanges1Click(Sender: TObject);
    procedure Saveandtest1Click(Sender: TObject);
    procedure Reloadfiles1Click(Sender: TObject);
    procedure Undo1Click(Sender: TObject);
    procedure Redo1Click(Sender: TObject);
    // Page control actions
    procedure PageControlChange(Sender: TObject);
    // Top checkboxes actions
    procedure cbOptionClick(Sender: TObject);
    procedure cbAlwaysOnTopClick(Sender: TObject);
    // Basic tileset data actions
    procedure BasicTilesetDataChange(Sender: TObject);
    // Tileset image actions
    procedure TilesetImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TilesetImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure TilesetImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TilesetScrollBarChange(Sender: TObject);
    // Image page actions
    procedure btnImportTilesetImageClick(Sender: TObject);
    procedure btnExportTilesetImageClick(Sender: TObject);
    procedure btnImportTilesetPortionClick(Sender: TObject);
    procedure btnExportTilesetPortionClick(Sender: TObject);
    procedure sbCopySelectionToClick(Sender: TObject);
    procedure sbSwapSelectionWithClick(Sender: TObject);
    procedure btnEraseSelectionClick(Sender: TObject);
    procedure btnNumberOfTilesApplyClick(Sender: TObject);
    // Attributes page actions
    procedure clbTileAtrListClickCheck(Sender: TObject);
    procedure clbTileAtrListEditorClick(Sender: TObject);
    procedure edExtraAttributeNameChange(Sender: TObject);
    procedure btnTileAtrValueApplyClick(Sender: TObject);
    procedure btnClearAttributesClick(Sender: TObject);
    procedure rgFilterModeClick(Sender: TObject);
    procedure cbAnyOfClick(Sender: TObject);
    procedure edRuleChange(Sender: TObject);
    // Hints page actions
    procedure lbTileHintTextClick(Sender: TObject);
    procedure btnTileHintTextClearClick(Sender: TObject);
    // Colors page actions
    procedure sgMinimapColorRulesDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure sgMinimapColorRulesMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure sgMinimapColorRulesMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure sgMinimapColorRulesSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure btnMinimapColorRuleAddClick(Sender: TObject);
    procedure btnMinimapColorRuleRemoveClick(Sender: TObject);
    procedure btnMinimapColorRuleMoveDownClick(Sender: TObject);
    procedure btnMinimapColorRuleMoveUpClick(Sender: TObject);
    procedure btnMinimapColorRuleApplyClick(Sender: TObject);
    // Fill Area page actions
    procedure sgFillAreaRulesDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure sgFillAreaRulesMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure sgFillAreaRulesMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure sgFillAreaRulesSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure btnFillAreaRuleAddClick(Sender: TObject);
    procedure btnFillAreaRuleRemoveClick(Sender: TObject);
    procedure btnFillAreaRuleMoveDownClick(Sender: TObject);
    procedure btnFillAreaRuleMoveUpClick(Sender: TObject);
    procedure btnFillAreaRuleApplyClick(Sender: TObject);
    // Paint page actions
    procedure sgPaintTileGroupsDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
    procedure sgPaintTileGroupsMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure sgPaintTileGroupsMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure sgPaintTileGroupsSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure btnPaintTileGroupApplyClick(Sender: TObject);
    // Presets page actions
    procedure sgBlockPresetGroupsMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure sgBlockPresetGroupsMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure sgBlockPresetGroupsSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure btnBlockPresetGroupApplyClick(Sender: TObject);
    procedure imgBlockPresetKeysMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure BlockPresetImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure btnBlockPresetAddPresetClick(Sender: TObject);
    procedure imgBlockPresetMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure seBlockPresetSizeChange(Sender: TObject);
    procedure btnBlockPresetAddClick(Sender: TObject);
    procedure btnBlockPresetCloseClick(Sender: TObject);
  private
    // Configutarion variables
    block_preset_images: array of TImage;

    // Status variables
    loading: integer;
    active_tile: integer;
    tileset_top: integer;
    tileset_height: integer;
    rule: TTileAtrRule;
    block_preset_selected_key: integer;
    block_preset_width: integer;
    block_preset_height: integer;
    block_preset_selected_tile: integer;
    block_preset_tiles: array[0..15] of word;

    // Block preset coverage
    block_preset_coverage: array[0..max_tileset_tiles-1] of byte;

    // Undo variables
    undo_history: array[0..max_undo_steps] of TUndoEntry;
    undo_start: integer;
    undo_max: integer;
    undo_pos: integer;
    undo_block_start: boolean;

    // Mouse and control-related variables
    mouse_old_x: integer;
    mouse_old_y: integer;
    select_started: boolean;
    select_start_x: integer;
    select_start_y: integer;
    select_end_x: integer;
    select_end_y: integer;

    // Pending action flags
    pending_update_contents: boolean;
    pending_update_text_list: boolean;

  public
    // Dispatcher procedures
    procedure update_game_lists;
    procedure update_contents;
    procedure update_tileset_filenames;
    procedure update_grid_color;
    procedure update_text_list;
    procedure update_speed_modifiers;
  private
    procedure fill_paint_tile_group_combo_boxes;
    procedure fill_block_preset_group_combo_boxes;
    procedure fill_minimap_color_rules_grid;
    procedure fill_minimap_color_rule_ui;
    procedure fill_fill_area_rules_grid;
    procedure fill_fill_area_rule_ui;
    procedure fill_paint_tile_groups_grid;
    procedure fill_paint_tile_group_ui;
    procedure fill_block_preset_groups_grid;
    procedure fill_block_preset_group_ui;
    procedure draw_block_preset_keys;
    procedure draw_block_preset_images;
    procedure draw_block_preset;

    procedure render_tileset;
    procedure compute_block_preset_coverage;
    procedure do_undo;
    procedure do_redo;
    procedure reset_undo_history;
    procedure set_tile_attribute_list(value, not_value: int64);
    procedure set_tile_attribute_value(value, not_value: int64);
    procedure set_tile_attribute_rule(value, not_value: int64);
    function get_tile_attribute_color(value: int64): cardinal;
    procedure set_tile_attributes(tile_index: integer; single_op: boolean);
  end;

var
  TilesetEditor: TTilesetEditor;

implementation

uses main, set_dialog, _utils, _stringtable, _settings, _structures, _graphics, _dispatcher, _launcher, _gamelists;

{$R *.dfm}

procedure TTilesetEditor.FormCreate(Sender: TObject);
var
  i: integer;
  p: TPanel;
begin
  loading := 0;
  select_start_x := -1;
  select_start_y := -1;
  select_end_x := -1;
  select_end_y := -1;
  TilesetImage.Picture.Bitmap.Width := tileimage_width;
  // Create editor attributes color panels
  for i := 0 to 7 do
  begin
    p := TPanel.Create(self);
    p.Width := 13;
    p.Height := 13;
    p.Top := 34 + i * 17;
    p.Left := 892;
    p.BevelOuter := bvNone;
    p.Color := atr_colors_editor[i];
    p.ParentBackground := false;
    p.Parent := PageAttributes;
  end;
  // Initialize minimap color rules grid
  sgMinimapColorRules.ColWidths[0] := 20;
  sgMinimapColorRules.ColWidths[1] := 17;
  sgMinimapColorRules.ColWidths[2] := 96;
  sgMinimapColorRules.ColWidths[3] := 52;
  sgMinimapColorRules.ColWidths[4] := 200;
  sgMinimapColorRules.Cells[2,0] := 'Rule name';
  sgMinimapColorRules.Cells[3,0] := 'Color';
  sgMinimapColorRules.Cells[4,0] := 'Rule';
  // Initialize fill area rules grid
  sgFillAreaRules.ColWidths[0] := 20;
  sgFillAreaRules.ColWidths[1] := 17;
  sgFillAreaRules.ColWidths[2] := 96;
  sgFillAreaRules.ColWidths[3] := 269;
  sgFillAreaRules.Cells[2,0] := 'Rule name';
  sgFillAreaRules.Cells[3,0] := 'Rule';
  // Initialize paint tile groups grid
  sgPaintTileGroups.ColWidths[0] := 20;
  sgPaintTileGroups.ColWidths[1] := 17;
  sgPaintTileGroups.ColWidths[2] := 96;
  sgPaintTileGroups.ColWidths[3] := 32;
  sgPaintTileGroups.ColWidths[4] := 80;
  sgPaintTileGroups.ColWidths[5] := 155;
  sgPaintTileGroups.Cells[2,0] := 'Group name';
  sgPaintTileGroups.Cells[3,0] := 'Tiles';
  sgPaintTileGroups.Cells[4,0] := 'Smooth group';
  sgPaintTileGroups.Cells[5,0] := 'Smooth presets';
  for i := -4 to cnt_paint_tile_groups - 1 do
    sgPaintTileGroups.Cells[0,i+4+1] := Tileset.get_paint_tile_group_char(i);
  // Initialize block preset groups grid
  sgBlockPresetGroups.ColWidths[0] := 20;
  sgBlockPresetGroups.ColWidths[1] := 96;
  sgBlockPresetGroups.ColWidths[2] := 48;
  sgBlockPresetGroups.ColWidths[3] := 80;
  sgBlockPresetGroups.Cells[1,0] := 'Group name';
  sgBlockPresetGroups.Cells[2,0] := 'Presets';
  sgBlockPresetGroups.Cells[3,0] := 'Paint group';
  for i := 0 to cnt_block_preset_groups - 1 do
    sgBlockPresetGroups.Cells[0,i+1] := IntToStr(i);
  block_preset_selected_key := 0;
  block_preset_width := 1;
  block_preset_height := 1;
  block_preset_selected_tile := 0;
  for i := 0 to Length(block_preset_tiles) - 1 do
    block_preset_tiles[i] := 65535;
end;

procedure TTilesetEditor.FormShow(Sender: TObject);
begin
  if pending_update_contents then
    update_contents;
  if pending_update_text_list then
    update_text_list;
end;

procedure TTilesetEditor.FormResize(Sender: TObject);
var
  new_tileset_height: integer;
  tileset_max_height: integer;
begin
  lbTileHintText.Height := PageImage.ClientHeight - 4;
  new_tileset_height := (PageImage.ClientHeight - 4) div 32;
  tileset_max_height := Tileset.cnt_tiles div 20;
  if new_tileset_height > tileset_max_height then
    new_tileset_height := tileset_max_height;
  if new_tileset_height = tileset_height then
    exit;
  tileset_height := new_tileset_height;
  TilesetImage.Picture.Bitmap.Height := tileset_height * 32;
  TilesetImage.Height := tileset_height * 32;
  TilesetScrollBar.Height := tileset_height * 32;
  TilesetScrollBar.PageSize := new_tileset_height;
  TilesetScrollBar.Position := Min(TilesetScrollBar.Position, tileset_max_height - new_tileset_height);
  if tileset_height = tileset_max_height then
    TilesetScrollBar.Enabled := False
  else
    TilesetScrollBar.Enabled := True;
  render_tileset;
end;

procedure TTilesetEditor.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if key = 27 then
    Close;
  if key = 13 then
  begin
    if (PageControl.ActivePage = PageColors) then
      btnMinimapColorRuleApplyClick(Sender)
    else if (PageControl.ActivePage = PageFillArea) then
      btnFillAreaRuleApplyClick(Sender)
    else if (PageControl.ActivePage = PagePaint) then
      btnPaintTileGroupApplyClick(Sender)
    else if (PageControl.ActivePage = PagePresets) then
      btnBlockPresetGroupApplyClick(Sender);
  end;
  if ActiveControl is TEdit then
    exit;
  // Global shortcuts
  case key of
    ord('T'): cbMarkSelectedItem.Checked := not cbMarkSelectedItem.Checked;
    ord('H'): cbHideUnmarkedTiles.Checked := not cbHideUnmarkedTiles.Checked;
    ord('G'): cbShowGrid.Checked := not cbShowGrid.Checked;
  end;
  if (PageControl.ActivePage = PageImage) then
  begin
    // Image page shortcuts
    if (ActiveControl = seNumberOfTiles) and (Key = 13) then
      btnNumberOfTilesApplyClick(Sender);
    case key of
      ord('I'): btnImportTilesetPortionClick(Sender);
      ord('E'): btnExportTilesetPortionClick(Sender);
      8: btnEraseSelectionClick(Sender);
    end;
  end
  else if (PageControl.ActivePage = PageAttributes) then
  begin
    // Attributes page shortcuts
    if ((ActiveControl = edTileAtrValue) or (ActiveControl = edTileAtrNotValue)) and (Key = 13) then
      btnTileAtrValueApplyClick(Sender);
    case key of
      ord('M'): cbMarkSelection.Checked := not cbMarkSelection.Checked;
      ord('E'): cbDrawEditorAttributes.Checked := not cbDrawEditorAttributes.Checked;
      ord('S'): rgOperation.ItemIndex := 0;
      ord('A'): rgOperation.ItemIndex := 1;
      ord('R'): rgOperation.ItemIndex := 2;
      ord('C'): btnClearAttributesClick(nil);
    end;
  end;
end;

procedure TTilesetEditor.FormMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  TilesetScrollBar.Position := Min(TilesetScrollBar.Position + 2, TilesetScrollBar.Max - TilesetScrollBar.PageSize + 1);
  Handled := true;
end;

procedure TTilesetEditor.FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  TilesetScrollBar.Position := TilesetScrollBar.Position - 2;
  Handled := true;
end;

procedure TTilesetEditor.Newtileset1Click(Sender: TObject);
begin
  SetDialog.select_menu(6);
end;

procedure TTilesetEditor.Opentileset1Click(Sender: TObject);
begin
  SetDialog.select_menu(5);
end;

procedure TTilesetEditor.Applychanges1Click(Sender: TObject);
begin
  Dispatcher.register_event(evTilesetAttributesChange);
end;

procedure TTilesetEditor.Savechanges1Click(Sender: TObject);
begin
  if Tileset.tileset_name = '' then
    exit;
  Applychanges1Click(nil);
  Tileset.save_tileset;
  confirm_overwrite_original_file_last_answer := 0;
  lblTileimageModified.Visible := Tileset.tileimage_modified;
end;

procedure TTilesetEditor.Saveandtest1Click(Sender: TObject);
begin
  if Tileset.tileset_name = '' then
    exit;
  if not Launcher.check_map_can_be_tested then
    exit;
  Applychanges1Click(nil);
  Tileset.save_tileset;
  if confirm_overwrite_original_file_last_answer <> IDNO then
    Launcher.launch_current_mission;
  confirm_overwrite_original_file_last_answer := 0;
end;

procedure TTilesetEditor.Reloadfiles1Click(Sender: TObject);
begin
  Tileset.load_tileset(true);
end;

procedure TTilesetEditor.Undo1Click(Sender: TObject);
begin
  do_undo;
  render_tileset;
end;

procedure TTilesetEditor.Redo1Click(Sender: TObject);
begin
  do_redo;
  render_tileset;
end;

procedure TTilesetEditor.PageControlChange(Sender: TObject);
begin
  TilesetImage.Parent := PageControl.ActivePage;
  TilesetScrollBar.Parent := PageControl.ActivePage;
  cbHideUnmarkedTiles.Visible := PageControl.ActivePage <> PageImage;
  cbMarkSelectedItem.Visible := (PageControl.ActivePage <> PageImage) and (PageControl.ActivePage <> PageAttributes);
  sbCopySelectionTo.Down := False;
  sbSwapSelectionWith.Down := False;
  render_tileset;
end;

procedure TTilesetEditor.cbOptionClick(Sender: TObject);
begin
  if (Sender = cbMarkSelectedItem) then
    compute_block_preset_coverage;
  render_tileset;
end;

procedure TTilesetEditor.cbAlwaysOnTopClick(Sender: TObject);
begin
  if cbAlwaysOnTop.Checked then
    FormStyle := fsStayOnTop
  else
    FormStyle := fsNormal;
end;

procedure TTilesetEditor.BasicTilesetDataChange(Sender: TObject);
begin
  if loading > 0 then
    exit;
  Tileset.header.custom_minimap_colors_allowed := IfThen(cbCustomMinimapColorsAllowed.Checked, 1, 0);
  Tileset.header.default_paint_group := cbxDefaultPaintGroup.ItemIndex - 1;
  store_c_string(edTilesetFancyName.Text, Addr(Tileset.header.tileset_fancy_name), Length(Tileset.header.tileset_fancy_name));
  store_c_string(edAuthorName.Text, Addr(Tileset.header.author_name), Length(Tileset.header.author_name));
  Tileset.header.rule_do_not_draw_rock_craters := IfThen(cbRuleDoNotDrawRockCraters.Checked, 1, 0);
  Tileset.header.rule_do_not_draw_sand_craters := IfThen(cbRuleDoNotDrawSandCraters.Checked, 1, 0);
end;

procedure TTilesetEditor.TilesetImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pos_x, pos_y: integer;
  tile_index: integer;
  tile_value: int64;
  min_x, min_y, max_x, max_y, size_x, size_y: integer;
begin
  pos_x := X div 32;
  pos_y := Y div 32 + tileset_top;
  tile_index := pos_x + pos_y * 20;
  // Right button
  if Button = mbRight then
  begin
    if PageControl.ActivePage = PageAttributes then
    begin
      tile_value := Tileset.get_tile_attributes(tile_index, 0, false);
      set_tile_attribute_value(tile_value, 0);
      set_tile_attribute_list(tile_value, 0);
      set_tile_attribute_rule(tile_value, 0);
      active_tile := tile_index;
    end
    else if PageControl.ActivePage = PageHints then
      lbTileHintText.ItemIndex := Tileset.tile_hint_text[tile_index]
    else if PageControl.ActivePage = PagePaint then
    begin
      if (sgPaintTileGroups.Row >= 5) then
      begin
        Tileset.attributes_extra[tile_index] := Tileset.attributes_extra[tile_index] and (not (1 shl (sgPaintTileGroups.Row - 5 + 8)));
        Tileset.process_paint_tile_lists;
        fill_paint_tile_groups_grid;
      end;
    end;
  end
  // Left button
  else if Button = mbLeft then
  begin
    if (PageControl.ActivePage = PageImage) and (sbCopySelectionTo.Down or sbSwapSelectionWith.Down or (ssCtrl in Shift) or (ssShift in Shift)) then
    begin
      // Perform copy or swap selection operation
      min_x := min(select_start_x, select_end_x);
      max_x := max(select_start_x, select_end_x);
      min_y := min(select_start_y, select_end_y);
      max_y := max(select_start_y, select_end_y);
      size_x := max_x - min_x + 1;
      size_y := max_y - min_y + 1;
      if sbCopySelectionTo.Down or (ssCtrl in Shift) then
        Tileset.copy_tileimage_portion(min_x, min_y, pos_x, pos_y, size_x, size_y)
      else if sbSwapSelectionWith.Down or (ssShift in Shift) then
        Tileset.swap_tileimage_portion(min_x, min_y, pos_x, pos_y, size_x, size_y);
      sbCopySelectionTo.Down := false;
      sbSwapSelectionWith.Down := false;
    end
    else if (ssShift in Shift) or (PageControl.ActivePage = PageImage) then
    begin
      // Handle selection start
      select_started := true;
      select_start_x := pos_x;
      select_start_y := pos_y;
      select_end_x := pos_x;
      select_end_y := pos_y;
    end else
    begin
      // Handle mouse click
      if PageControl.ActivePage = PageAttributes then
        set_tile_attributes(tile_index, true)
      else if PageControl.ActivePage = PageHints then
        Tileset.tile_hint_text[tile_index] := lbTileHintText.ItemIndex
      else if PageControl.ActivePage = PagePaint then
      begin
        if (sgPaintTileGroups.Row >= 5) and (Tileset.paint_tile_lists[sgPaintTileGroups.Row - 5].cnt_tiles < max_paint_tiles) and ((Tileset.attributes_extra[tile_index] and $FF00) = 0) then
        begin
          Tileset.attributes_extra[tile_index] := Tileset.attributes_extra[tile_index] or (1 shl (sgPaintTileGroups.Row - 5 + 8));
          Tileset.process_paint_tile_lists;
          fill_paint_tile_groups_grid;
        end;
      end
      else if (PageControl.ActivePage = PagePresets) and pnBlockPreset.Visible then
      begin
        block_preset_tiles[block_preset_selected_tile] := tile_index;
        Inc(block_preset_selected_tile);
        if block_preset_selected_tile = (block_preset_width * block_preset_height) then
          block_preset_selected_tile := 0;
        draw_block_preset;
      end;
    end;
    TilesetImage.ShowHint := false;
  end
  // Middle button
  else if Button = mbMiddle then
  begin
    active_tile := -1;
    select_start_x := -1;
    select_start_y := -1;
    select_end_x := -1;
    select_end_y := -1;
    if PageControl.ActivePage = PagePaint then
    begin
      Tileset.paint_tile_groups[sgPaintTileGroups.Row - 5].tile_index := tile_index;
      fill_paint_tile_group_ui;
      if (sgPaintTileGroups.Row - 5) = -1 then
        render_tileset;
      Dispatcher.register_event(evTilesetPaintGroupsChange);
    end;
  end;
  render_tileset;
end;

procedure TTilesetEditor.TilesetImageMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
var
  pos_x, pos_y: integer;
  tile_index: integer;
  is_custom: boolean;
  tile_hint_text_id: integer;
  hint_str: string;
  show_hint: boolean;
  i: integer;
  tile_value: int64;
  and_value: int64;
  color: cardinal;
  rule_index: integer;
  tile_paint_group: integer;
begin
  pos_x := X div 32;
  pos_y := Y div 32 + tileset_top;
  if (mouse_old_x = pos_x) and (mouse_old_y = pos_y) then
    exit;
  mouse_old_x := pos_x;
  mouse_old_y := pos_y;
  tile_index := pos_y*20 + pos_x;
  // Show position on status bar
  StatusBar.Panels[0].Text := 'x : ' + inttostr(pos_x) + ' y : ' + inttostr(pos_y) + '  (' + inttostr(tile_index) + ')';
  // Handle tile editing while moving mouse with left button pressed
  if (not select_started) and (ssLeft in Shift) then
    TilesetImageMouseDown(Sender, mbLeft, Shift, X, Y);
  // Handle area selection
  if select_started and ((pos_x <> select_end_x) or (pos_y <> select_end_y)) then
  begin
    select_end_x := pos_x;
    select_end_y := pos_y;
    render_tileset;
  end;
  // Get hint text
  hint_str := '';
  show_hint := false;
  if PageControl.ActivePage = PageAttributes then
  begin
    tile_value := Tileset.get_tile_attributes(tile_index, 0, false);
    for i := 0 to 39 do
    begin
      and_value := 1;
      and_value := and_value shl i;
      if (tile_value and and_value) <> 0 then
      begin
        if i < 32 then
          hint_str := hint_str + clbTileAtrList.Items[i] + #13
        else
          hint_str := hint_str + '* ' + clbTileAtrListEditor.Items[i-32] + #13;
      end;
    end;
    hint_str := copy(hint_str, 1, Length(hint_str) - 1);
    show_hint := true;
  end
  else if PageControl.ActivePage = PageHints then
  begin
    tile_hint_text_id := Tileset.tile_hint_text[tile_index];
    if tile_hint_text_id <> -1 then
    begin
      hint_str := StringTable.get_text(tile_hint_text_id, false, is_custom);
      show_hint := true;
    end;
  end
  else if PageControl.ActivePage = PageColors then
  begin
    color := Tileset.get_tile_color(tile_index, 0, rule_index);
    hint_str := 'Rule ' + inttostr(rule_index) + ': $' + inttohex(color, 6);
    show_hint := true;
  end
  else if PageControl.ActivePage = PageFillArea then
  begin
    rule_index := Tileset.get_fill_area_type(tile_index, 0);
    hint_str := 'Rule ' + inttostr(rule_index) + ': ' + Tileset.fill_area_rules[rule_index].name;
    show_hint := true;
  end
  else if PageControl.ActivePage = PagePaint then
  begin
    tile_paint_group := Tileset.get_tile_paint_group(tile_index);
    if tile_paint_group <> -128 then
    begin
      hint_str := 'Group ' + Tileset.get_paint_tile_group_char(tile_paint_group) + ': ' + Tileset.paint_tile_groups[tile_paint_group].name;
      show_hint := true;
    end;
  end
  else if PageControl.ActivePage = PagePresets then
  begin
    hint_str := inttostr(block_preset_coverage[tile_index]) + ' occurences';
    tile_paint_group := Tileset.get_tile_paint_group(tile_index);
    if tile_paint_group <> -128 then
      hint_str := hint_str + #13'Paint group: ' + Tileset.paint_tile_groups[tile_paint_group].name;
    show_hint := true;
  end;
  // Show hint
  if show_hint then
  begin
    Application.CancelHint;
    TilesetImage.ShowHint := true;
    TilesetImage.Hint := hint_str;
  end else
    TilesetImage.ShowHint := false;
end;

procedure TTilesetEditor.TilesetImageMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  min_x, min_y, max_x, max_y: integer;
  select_width, select_height: integer;
  xx, yy: integer;
begin
  if select_started then
  begin
    select_started := false;
    undo_block_start := true;
    min_x := min(select_start_x, select_end_x);
    max_x := max(select_start_x, select_end_x);
    min_y := min(select_start_y, select_end_y);
    max_y := max(select_start_y, select_end_y);
    for yy := min_y to max_y do
      for xx:= min_x to max_x do
        begin
          if PageControl.ActivePage = PageAttributes then
            set_tile_attributes(xx + yy*20, false)
          else if PageControl.ActivePage = PageHints then
            Tileset.tile_hint_text[xx + yy*20] := lbTileHintText.ItemIndex
        end;
    if (PageControl.ActivePage = PagePresets) and pnBlockPreset.Visible then
    begin
      select_width := max_x - min_x + 1;
      select_height := max_y - min_y + 1;
      if (select_width <= 4) and (select_height <= 4) then
      begin
        block_preset_width := select_width;
        block_preset_height := select_height;
        for yy := 0 to select_height - 1 do
          for xx:= 0 to select_width - 1 do
            block_preset_tiles[yy * select_width + xx] := (yy + min_y) * 20 + (xx + min_x);
        draw_block_preset;
      end;
    end;
    render_tileset;
    TilesetImage.ShowHint := false;
    if PageControl.ActivePage <> PageImage then
    begin
      select_start_x := -1;
      select_start_y := -1;
      select_end_x := -1;
      select_end_y := -1;
    end;
  end;
end;

procedure TTilesetEditor.TilesetScrollBarChange(Sender: TObject);
var
  pos: TPoint;
begin
  tileset_top := Min(TilesetScrollBar.Position, (Tileset.cnt_tiles div 20) - TilesetScrollBar.PageSize);
  // Simulate MouseMove event so that coordinates are updated
  pos := TilesetEditor.ScreenToClient(Mouse.CursorPos);
  if PtInRect(TilesetImage.BoundsRect, pos) then
  begin
    pos := TilesetImage.ScreenToClient(Mouse.CursorPos);
    TilesetImageMouseMove(nil, [], pos.X, pos.Y);
  end;
  render_tileset;
end;

procedure TTilesetEditor.btnImportTilesetImageClick(Sender: TObject);
begin
  if TilesetImageOpenDialog.Execute then
    Tileset.import_tileimage_from_file(TilesetImageOpenDialog.FileName);
end;

procedure TTilesetEditor.btnExportTilesetImageClick(Sender: TObject);
begin
  if TilesetImageSaveDialog.Execute then
    Tileset.export_tileimage_to_file(TilesetImageSaveDialog.FileName);
end;

procedure TTilesetEditor.btnImportTilesetPortionClick(Sender: TObject);
var
  pos_x, pos_y: integer;
begin
  if (select_start_x = -1) or (select_start_y = -1) then
  begin
    Application.MessageBox('Select the portion first.', 'Import tileset portion', MB_OK or MB_ICONWARNING);
    exit;
  end;
  pos_x := min(select_start_x, select_end_x);
  pos_y := min(select_start_y, select_end_y);
  if TilesetPortionOpenDialog.Execute then
    Tileset.import_tileimage_portion_from_file(TilesetPortionOpenDialog.FileName, pos_x, pos_y);
end;

procedure TTilesetEditor.btnExportTilesetPortionClick(Sender: TObject);
var
  min_x, min_y, max_x, max_y, size_x, size_y: integer;
begin
  if (select_start_x = -1) or (select_start_y = -1) then
  begin
    Application.MessageBox('Select the portion first.', 'Export tileset portion', MB_OK or MB_ICONWARNING);
    exit;
  end;
  min_x := min(select_start_x, select_end_x);
  max_x := max(select_start_x, select_end_x);
  min_y := min(select_start_y, select_end_y);
  max_y := max(select_start_y, select_end_y);
  size_x := max_x - min_x + 1;
  size_y := max_y - min_y + 1;
  if TilesetPortionSaveDialog.Execute then
    Tileset.export_tileimage_portion_to_file(TilesetPortionSaveDialog.FileName, min_x, min_y, size_x, size_y);
end;

procedure TTilesetEditor.sbCopySelectionToClick(Sender: TObject);
begin
  if (select_start_x = -1) or (select_start_y = -1) then
  begin
    sbCopySelectionTo.Down := False;
    Application.MessageBox('Make a selection first.', 'Copy selection to', MB_OK or MB_ICONWARNING);
    exit;
  end;
end;

procedure TTilesetEditor.sbSwapSelectionWithClick(Sender: TObject);
begin
  if (select_start_x = -1) or (select_start_y = -1) then
  begin
    sbSwapSelectionWith.Down := False;
    Application.MessageBox('Make a selection first.', 'Swap selection with', MB_OK or MB_ICONWARNING);
    exit;
  end;
end;

procedure TTilesetEditor.btnEraseSelectionClick(Sender: TObject);
var
  min_x, min_y, max_x, max_y, size_x, size_y: integer;
begin
  if (select_start_x = -1) or (select_start_y = -1) then
  begin
    Application.MessageBox('Make a selection first.', 'Erase selection', MB_OK or MB_ICONWARNING);
    exit;
  end;
  min_x := min(select_start_x, select_end_x);
  max_x := max(select_start_x, select_end_x);
  min_y := min(select_start_y, select_end_y);
  max_y := max(select_start_y, select_end_y);
  size_x := max_x - min_x + 1;
  size_y := max_y - min_y + 1;
  Tileset.erase_tileimage_portion(min_x, min_y, size_x, size_y);
end;

procedure TTilesetEditor.btnNumberOfTilesApplyClick(Sender: TObject);
begin
  Tileset.resize_tileset(seNumberOfTiles.Value);
end;

procedure TTilesetEditor.clbTileAtrListClickCheck(Sender: TObject);
var
  i: integer;
  value, not_value: int64;
  v: int64;
begin
  value := 0;
  not_value := 0;
  for i := 0 to 31 do
  begin
    if clbTileAtrList.Checked[i] then
      value := value or (1 shl i)
    else if clbTileAtrList.State[i] = cbGrayed then
      not_value := not_value or (1 shl i);
  end;
  for i := 0 to 7 do
  begin
    v := 1;
    v := v shl (i+32);
    if clbTileAtrListEditor.Checked[i] then
      value := value or v
    else if clbTileAtrListEditor.State[i] = cbGrayed then
      not_value := not_value or v;
  end;
  set_tile_attribute_value(value, not_value);
  set_tile_attribute_rule(value, not_value);
  if sender <> nil then
    active_tile := -1;
  render_tileset;
end;

procedure TTilesetEditor.clbTileAtrListEditorClick(Sender: TObject);
begin
  inc(loading);
  edExtraAttributeName.Enabled := true;
  edExtraAttributeName.Text := Tileset.extra_attribute_names[clbTileAtrListEditor.ItemIndex];
  dec(loading);
end;

procedure TTilesetEditor.edExtraAttributeNameChange(Sender: TObject);
begin
  if loading > 0 then
    exit;
  clbTileAtrListEditor.Items[clbTileAtrListEditor.ItemIndex] := edExtraAttributeName.Text;
  store_c_string(edExtraAttributeName.Text, Addr(Tileset.extra_attribute_names[clbTileAtrListEditor.ItemIndex]), Length(Tileset.extra_attribute_names[clbTileAtrListEditor.ItemIndex]));
end;

procedure TTilesetEditor.btnTileAtrValueApplyClick(Sender: TObject);
var
  value, not_value: int64;
begin
  value := strtoint64('$'+edTileAtrValue.Text);
  not_value := strtoint64('$'+edTileAtrNotValue.Text);
  set_tile_attribute_list(value, not_value);
  set_tile_attribute_value(value, not_value);
  set_tile_attribute_rule(value, not_value);
  render_tileset;
end;

procedure TTilesetEditor.btnClearAttributesClick(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to 31 do
    clbTileAtrList.State[i] := cbUnchecked;
  for i := 0 to 7 do
    clbTileAtrListEditor.State[i] := cbUnchecked;
  clbTileAtrListClickCheck(nil);
end;

procedure TTilesetEditor.rgFilterModeClick(Sender: TObject);
var
  not_atr_active: boolean;
  i: integer;
begin
  not_atr_active := FilterMode(rgFilterMode.ItemIndex) = fmByRule;
  clbTileAtrList.AllowGrayed := not_atr_active;
  clbTileAtrListEditor.AllowGrayed := not_atr_active;
  lbTileAtrNotValue.Visible := not_atr_active;
  edTileAtrNotValue.Visible := not_atr_active;
  if not not_atr_active then
  begin
    for i := 0 to 31 do
      if clbTileAtrList.State[i] = cbGrayed then
        clbTileAtrList.State[i] := cbUnchecked;
    for i := 0 to 7 do
      if clbTileAtrListEditor.State[i] = cbGrayed then
        clbTileAtrListEditor.State[i] := cbUnchecked;
  end;
  clbTileAtrListClickCheck(nil);
  render_tileset;
end;

procedure TTilesetEditor.cbAnyOfClick(Sender: TObject);
var
  value, not_value: int64;
begin
  value := strtoint64('$'+edTileAtrValue.Text);
  not_value := strtoint64('$'+edTileAtrNotValue.Text);
  set_tile_attribute_rule(value, not_value);
  render_tileset;
end;

procedure TTilesetEditor.edRuleChange(Sender: TObject);
var
  rule_valid: boolean;
begin
  rule_valid := Tileset.string_to_rule(edRule.Text, Addr(rule));
  if rule_valid then
  begin
    edRule.Font.Color := clBlack;
    cbAnyOf.OnClick := nil;
    cbAnyOf.Checked := rule.attr < 0;
    cbAnyOf.OnClick := cbAnyOfClick;
    set_tile_attribute_list(abs(rule.attr) and $ffffffffff, rule.not_attr and $ffffffffff);
    set_tile_attribute_value(abs(rule.attr) and $ffffffffff, rule.not_attr and $ffffffffff);
    render_tileset;
  end else
    edRule.Font.Color := clRed;
end;

procedure TTilesetEditor.lbTileHintTextClick(Sender: TObject);
begin
  if cbMarkSelectedItem.Checked then
    render_tileset;
end;

procedure TTilesetEditor.btnTileHintTextClearClick(Sender: TObject);
begin
  lbTileHintText.ItemIndex := -1;
end;

procedure TTilesetEditor.sgMinimapColorRulesDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
var
  color: Cardinal;
begin
  if ACol <> 1 then
    exit;
  if (ARow > 0) and ((ARow - 1) < Tileset.minimap_color_rules_used) then
  begin
    color := Tileset.minimap_color_rules[ARow - 1].color;
    color := ((color and $ff) shl 16) or (color and $ff00) or ((color and $ff0000) shr 16);
    sgMinimapColorRules.Canvas.Brush.Color := color;
    sgMinimapColorRules.Canvas.FillRect(Rect);
  end;
end;

procedure TTilesetEditor.sgMinimapColorRulesMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  FormMouseWheelDown(Sender, Shift, MousePos, Handled);
  Handled := true;
end;

procedure TTilesetEditor.sgMinimapColorRulesMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  FormMouseWheelUp(Sender, Shift, MousePos, Handled);
  Handled := true;
end;

procedure TTilesetEditor.sgMinimapColorRulesSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
begin
  sgMinimapColorRules.Tag := ARow;
  fill_minimap_color_rule_ui;
  if cbMarkSelectedItem.Checked then
    render_tileset;
end;

procedure TTilesetEditor.btnMinimapColorRuleAddClick(Sender: TObject);
begin
  if Tileset.minimap_color_rules_used = max_minimap_color_rules then
    exit;
  FillChar(Tileset.minimap_color_rules[Tileset.minimap_color_rules_used], sizeof(Tileset.minimap_color_rules[Tileset.minimap_color_rules_used]), 0);
  Inc(Tileset.minimap_color_rules_used);
  fill_minimap_color_rules_grid;
  sgMinimapColorRules.Row := Tileset.minimap_color_rules_used;
  fill_minimap_color_rule_ui;
  render_tileset;
  Dispatcher.register_event(evTilesetMinimapColorsChange);
end;

procedure TTilesetEditor.btnMinimapColorRuleRemoveClick(Sender: TObject);
begin
  if Tileset.minimap_color_rules_used = 1 then
    exit;
  if sgMinimapColorRules.Row = Tileset.minimap_color_rules_used then
    sgMinimapColorRules.Row := sgMinimapColorRules.Row - 1;
  Dec(Tileset.minimap_color_rules_used);
  fill_minimap_color_rules_grid;
  fill_minimap_color_rule_ui;
  render_tileset;
  Dispatcher.register_event(evTilesetMinimapColorsChange);
end;

procedure TTilesetEditor.btnMinimapColorRuleMoveDownClick(Sender: TObject);
var
  tmp_rule: TMinimapColorRule;
begin
  if sgMinimapColorRules.Row = Tileset.minimap_color_rules_used then
    exit;
  tmp_rule := Tileset.minimap_color_rules[sgMinimapColorRules.Row];
  Tileset.minimap_color_rules[sgMinimapColorRules.Row] := Tileset.minimap_color_rules[sgMinimapColorRules.Row-1];
  Tileset.minimap_color_rules[sgMinimapColorRules.Row-1] := tmp_rule;
  fill_minimap_color_rules_grid;
  sgMinimapColorRules.Row := sgMinimapColorRules.Row + 1;
  render_tileset;
  Dispatcher.register_event(evTilesetMinimapColorsChange);
end;

procedure TTilesetEditor.btnMinimapColorRuleMoveUpClick(Sender: TObject);
var
  tmp_rule: TMinimapColorRule;
begin
  if sgMinimapColorRules.Row = 1 then
    exit;
  tmp_rule := Tileset.minimap_color_rules[sgMinimapColorRules.Row-2];
  Tileset.minimap_color_rules[sgMinimapColorRules.Row-2] := Tileset.minimap_color_rules[sgMinimapColorRules.Row-1];
  Tileset.minimap_color_rules[sgMinimapColorRules.Row-1] := tmp_rule;
  fill_minimap_color_rules_grid;
  sgMinimapColorRules.Row := sgMinimapColorRules.Row - 1;
  render_tileset;
  Dispatcher.register_event(evTilesetMinimapColorsChange);
end;

procedure TTilesetEditor.btnMinimapColorRuleApplyClick(Sender: TObject);
begin
  store_c_string(edMinimapColorRuleName.Text, Addr(Tileset.minimap_color_rules[sgMinimapColorRules.Row-1].name), Length(Tileset.minimap_color_rules[sgMinimapColorRules.Row-1].name));
  Tileset.string_to_rule(edMinimapColorRuleRule.Text, Addr(Tileset.minimap_color_rules[sgMinimapColorRules.Row-1].rule));
  Tileset.minimap_color_rules[sgMinimapColorRules.Row-1].color := StrToInt('$' + edMinimapColorRuleColor.Text);
  fill_minimap_color_rules_grid;
  render_tileset;
  Dispatcher.register_event(evTilesetMinimapColorsChange);
end;

procedure TTilesetEditor.sgFillAreaRulesDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
var
  color: Cardinal;
begin
  if ACol <> 1 then
    exit;
  if (ARow > 0) and ((ARow - 1) < Tileset.fill_area_rules_used) then
  begin
    color := fill_area_group_colors[ARow - 1];
    if (ARow - 1) = (Tileset.fill_area_rules_used - 1) then
      // Tiles from the last fill area group should be always green
      color := fill_area_group_colors[max_fill_area_rules - 1];
    sgFillAreaRules.Canvas.Brush.Color := color;
    sgFillAreaRules.Canvas.FillRect(Rect);
  end;
end;

procedure TTilesetEditor.sgFillAreaRulesMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  FormMouseWheelDown(Sender, Shift, MousePos, Handled);
  Handled := true;
end;

procedure TTilesetEditor.sgFillAreaRulesMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  FormMouseWheelUp(Sender, Shift, MousePos, Handled);
  Handled := true;
end;

procedure TTilesetEditor.sgFillAreaRulesSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
begin
  sgFillAreaRules.Tag := ARow;
  fill_fill_area_rule_ui;
  if cbMarkSelectedItem.Checked then
    render_tileset;
end;

procedure TTilesetEditor.btnFillAreaRuleAddClick(Sender: TObject);
begin
  if Tileset.fill_area_rules_used = max_fill_area_rules then
    exit;
  FillChar(Tileset.fill_area_rules[Tileset.fill_area_rules_used], sizeof(Tileset.fill_area_rules[Tileset.fill_area_rules_used]), 0);
  Inc(Tileset.fill_area_rules_used);
  fill_fill_area_rules_grid;
  sgFillAreaRules.Row := Tileset.fill_area_rules_used;
  fill_fill_area_rule_ui;
  render_tileset;
  Dispatcher.register_event(evTilesetFillAreasChange);
end;

procedure TTilesetEditor.btnFillAreaRuleRemoveClick(Sender: TObject);
begin
  if Tileset.fill_area_rules_used = 1 then
    exit;
  if sgFillAreaRules.Row = Tileset.fill_area_rules_used then
    sgFillAreaRules.Row := sgFillAreaRules.Row - 1;
  Dec(Tileset.fill_area_rules_used);
  fill_fill_area_rules_grid;
  fill_fill_area_rule_ui;
  render_tileset;
  Dispatcher.register_event(evTilesetFillAreasChange);
end;

procedure TTilesetEditor.btnFillAreaRuleMoveDownClick(Sender: TObject);
var
  tmp_rule: TFillAreaRule;
begin
  if sgFillAreaRules.Row = Tileset.fill_area_rules_used then
    exit;
  tmp_rule := Tileset.fill_area_rules[sgFillAreaRules.Row];
  Tileset.fill_area_rules[sgFillAreaRules.Row] := Tileset.fill_area_rules[sgFillAreaRules.Row-1];
  Tileset.fill_area_rules[sgFillAreaRules.Row-1] := tmp_rule;
  fill_fill_area_rules_grid;
  sgFillAreaRules.Row := sgFillAreaRules.Row + 1;
  render_tileset;
  Dispatcher.register_event(evTilesetFillAreasChange);
end;

procedure TTilesetEditor.btnFillAreaRuleMoveUpClick(Sender: TObject);
var
  tmp_rule: TFillAreaRule;
begin
  if sgFillAreaRules.Row = 1 then
    exit;
  tmp_rule := Tileset.fill_area_rules[sgFillAreaRules.Row-2];
  Tileset.fill_area_rules[sgFillAreaRules.Row-2] := Tileset.fill_area_rules[sgFillAreaRules.Row-1];
  Tileset.fill_area_rules[sgFillAreaRules.Row-1] := tmp_rule;
  fill_fill_area_rules_grid;
  sgFillAreaRules.Row := sgFillAreaRules.Row - 1;
  render_tileset;
  Dispatcher.register_event(evTilesetFillAreasChange);
end;

procedure TTilesetEditor.btnFillAreaRuleApplyClick(Sender: TObject);
begin
  store_c_string(edFillAreaRuleName.Text, Addr(Tileset.fill_area_rules[sgFillAreaRules.Row-1].name), Length(Tileset.fill_area_rules[sgFillAreaRules.Row-1].name));
  Tileset.string_to_rule(edFillAreaRuleRule.Text, Addr(Tileset.fill_area_rules[sgFillAreaRules.Row-1].rule));
  fill_fill_area_rules_grid;
  render_tileset;
  Dispatcher.register_event(evTilesetFillAreasChange);
end;

procedure TTilesetEditor.sgPaintTileGroupsDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
var
  color: Cardinal;
begin
  if (ACol <> 1) or (ARow < 4) then
    exit;
  color := fill_area_group_colors[ARow - 1];
  sgPaintTileGroups.Canvas.Brush.Color := color;
  sgPaintTileGroups.Canvas.FillRect(Rect);
end;

procedure TTilesetEditor.sgPaintTileGroupsMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  FormMouseWheelDown(Sender, Shift, MousePos, Handled);
  Handled := true;
end;

procedure TTilesetEditor.sgPaintTileGroupsMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  FormMouseWheelUp(Sender, Shift, MousePos, Handled);
  Handled := true;
end;

procedure TTilesetEditor.sgPaintTileGroupsSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
begin
  sgPaintTileGroups.Tag := ARow;
  fill_paint_tile_group_ui;
  if cbMarkSelectedItem.Checked then
    render_tileset;
end;

procedure TTilesetEditor.btnPaintTileGroupApplyClick(Sender: TObject);
var
  smooth_presets_length: integer;
begin
  smooth_presets_length := Length(edPaintTileGroupSmoothPresets.Text);
  if (sgPaintTileGroups.Row >= 5) and (cbxPaintTileGroupSmoothPresetGroup.ItemIndex > 0) and (smooth_presets_length <> 12) and (smooth_presets_length <> 14) and (smooth_presets_length <> 20) then
  begin
    Application.MessageBox('Auto-smooth presets must be either 12, 14 or 20 charachers.','Apply paint tile group', MB_OK or MB_ICONWARNING);
    exit;
  end;
  store_c_string(edPaintTileGroupName.Text, Addr(Tileset.paint_tile_groups[sgPaintTileGroups.Row-5].name), Length(Tileset.paint_tile_groups[sgPaintTileGroups.Row-5].name));
  Tileset.string_to_rule(edPaintTileGroupRestrictionRule.Text, Addr(Tileset.paint_tile_groups[sgPaintTileGroups.Row-5].restriction_rule));
  Tileset.paint_tile_groups[sgPaintTileGroups.Row-5].smooth_preset_group := cbxPaintTileGroupSmoothPresetGroup.ItemIndex - 1;
  if cbxPaintTileGroupSmoothPresetGroup.ItemIndex > 0 then
    store_c_string(edPaintTileGroupSmoothPresets.Text, Addr(Tileset.paint_tile_groups[sgPaintTileGroups.Row-5].smooth_presets), Length(Tileset.paint_tile_groups[sgPaintTileGroups.Row-5].smooth_presets));
  store_c_string(edPaintTileGroupRandomMapName.Text, Addr(Tileset.paint_tile_groups[sgPaintTileGroups.Row-5].random_map_name), Length(Tileset.paint_tile_groups[sgPaintTileGroups.Row-5].random_map_name));
  fill_paint_tile_groups_grid;
  fill_block_preset_groups_grid;
  fill_paint_tile_group_combo_boxes;
  Dispatcher.register_event(evTilesetPaintGroupsChange);
end;

procedure TTilesetEditor.sgBlockPresetGroupsMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  FormMouseWheelDown(Sender, Shift, MousePos, Handled);
  Handled := true;
end;

procedure TTilesetEditor.sgBlockPresetGroupsMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  FormMouseWheelUp(Sender, Shift, MousePos, Handled);
  Handled := true;
end;

procedure TTilesetEditor.sgBlockPresetGroupsSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
begin
  sgBlockPresetGroups.Tag := ARow;
  fill_block_preset_group_ui;
  if cbMarkSelectedItem.Checked then
  begin
    compute_block_preset_coverage;
    render_tileset;
  end;
end;

procedure TTilesetEditor.btnBlockPresetGroupApplyClick(Sender: TObject);
begin
  store_c_string(edBlockPresetGroupName.Text, Addr(Tileset.block_preset_groups[sgBlockPresetGroups.Row-1].name), Length(Tileset.block_preset_groups[sgBlockPresetGroups.Row-1].name));
  Tileset.block_preset_groups[sgBlockPresetGroups.Row-1].paint_group := cbxBlockPresetGroupPaintGroup.ItemIndex;
  fill_block_preset_groups_grid;
  fill_paint_tile_groups_grid;
  fill_block_preset_group_combo_boxes;
  Dispatcher.register_event(evTilesetBlockPresetsChange);
end;

procedure TTilesetEditor.imgBlockPresetKeysMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if Button <> mbLeft then
    exit;
  if (X >= (imgBlockPresetKeys.Width - 1)) or (Y >= imgBlockPresetKeys.Height - 1) then
    exit;
  block_preset_selected_key := (Y div 32) * 10 + (X div 32);
  draw_block_preset_keys;
  draw_block_preset_images;
end;

procedure TTilesetEditor.BlockPresetImageMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  preset_index: integer;
  i: integer;
begin
  preset_index := (Sender as TImage).Tag;
  if ssShift in Shift then
  begin
    block_preset_width := Tileset.block_presets[preset_index].width;
    block_preset_height := Tileset.block_presets[preset_index].height;
    for i := 0 to block_preset_width * block_preset_height - 1 do
      block_preset_tiles[i] := Tileset.block_preset_tiles[Tileset.block_preset_first_tile_indexes[preset_index] + i];
    btnBlockPresetAddPresetClick(Sender);
    draw_block_preset;
  end;
  if Button = mbRight then
  begin
    Tileset.delete_block_preset(preset_index);
    compute_block_preset_coverage;
    render_tileset;
    fill_block_preset_groups_grid;
    fill_block_preset_group_ui;
  end;
end;

procedure TTilesetEditor.btnBlockPresetAddPresetClick(Sender: TObject);
begin
  pnBlockPreset.Visible := true;
  btnBlockPresetAddPreset.Enabled := false;
end;

procedure TTilesetEditor.imgBlockPresetMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  tile_x, tile_y: integer;
begin
  tile_x := X div 32;
  tile_y := Y div 32;
  if Button = mbLeft then
  begin
    block_preset_selected_tile := tile_y * block_preset_width + tile_x;
    draw_block_preset;
  end
  else if Button = mbRight then
  begin
    block_preset_tiles[tile_y * block_preset_width + tile_x] := 65535;
    draw_block_preset;
  end;
end;

procedure TTilesetEditor.seBlockPresetSizeChange(Sender: TObject);
var
  new_width, new_height: integer;
  tmp_block_preset_tiles: array[0..15] of word;
  i, x, y: integer;
begin
  if loading > 0 then
    exit;
  new_width := seBlockPresetWidth.Value;
  new_height := seBlockPresetHeight.Value;
  // Update block preset tiles
  for y := 0 to new_height - 1 do
    for x := 0 to new_width - 1 do
      if (x >= block_preset_width) or (y >= block_preset_height) then
        tmp_block_preset_tiles[y * new_width + x] := 65535
      else
        tmp_block_preset_tiles[y * new_width + x] := block_preset_tiles[y * block_preset_width + x];
  for i := 0 to Length(tmp_block_preset_tiles) - 1 do
    block_preset_tiles[i] := tmp_block_preset_tiles[i];
  // Update selected tile
  x := block_preset_selected_tile mod block_preset_width;
  y := block_preset_selected_tile div block_preset_width;
  block_preset_selected_tile := Min(y, new_height - 1) * new_width + Min(x, new_width - 1);
  // Update preset size
  block_preset_width := new_width;
  block_preset_height := new_height;
  draw_block_preset;
end;

procedure TTilesetEditor.btnBlockPresetAddClick(Sender: TObject);
begin
  Tileset.add_block_preset(sgBlockPresetGroups.Row-1, block_preset_selected_key, block_preset_width, block_preset_height, block_preset_tiles);
  compute_block_preset_coverage;
  render_tileset;
  fill_block_preset_groups_grid;
  fill_block_preset_group_ui;
end;

procedure TTilesetEditor.btnBlockPresetCloseClick(Sender: TObject);
begin
  pnBlockPreset.Visible := false;
  btnBlockPresetAddPreset.Enabled := true;
end;

procedure TTilesetEditor.update_game_lists;
begin
  clbTileAtrList.Items := GameLists.get_list_ref('TileAtr');
end;

procedure TTilesetEditor.update_contents;
var
  i: integer;
begin
  if not Visible then
  begin
    pending_update_contents := true;
    exit;
  end;
  pending_update_contents := false;
  Inc(loading);
  update_tileset_filenames;
  TilesetScrollBar.Max := Tileset.cnt_tiles div 20 - 1;
  TilesetScrollBar.Position := Min(TilesetScrollBar.Position, Tileset.cnt_tiles div 20 - TilesetScrollBar.PageSize);
  // Fill combo boxes
  fill_paint_tile_group_combo_boxes;
  fill_block_preset_group_combo_boxes;
  // Fill basic tileset data
  edTilesetFancyName.Text := Tileset.header.tileset_fancy_name;
  edAuthorName.Text := Tileset.header.author_name;
  cbCustomMinimapColorsAllowed.Checked := Tileset.header.custom_minimap_colors_allowed <> 0;
  cbxDefaultPaintGroup.ItemIndex := Tileset.header.default_paint_group + 1;
  cbRuleDoNotDrawRockCraters.Checked := Tileset.header.rule_do_not_draw_rock_craters <> 0;
  cbRuleDoNotDrawSandCraters.Checked := Tileset.header.rule_do_not_draw_sand_craters <> 0;
  lblTileimageModified.Visible := Tileset.tileimage_modified;
  seNumberOfTiles.Value := Tileset.cnt_tiles;
  // Fill extra attribute names
  for i := 0 to 7 do
    clbTileAtrListEditor.Items[i] := Tileset.extra_attribute_names[i];
  // Fill minimap color rules grid
  fill_minimap_color_rules_grid;
  fill_minimap_color_rule_ui;
  // Fill fill area rules grid
  fill_fill_area_rules_grid;
  fill_fill_area_rule_ui;
  // Fill paint tile groups grid
  fill_paint_tile_groups_grid;
  fill_paint_tile_group_ui;
  // Fill block preset groups grid
  fill_block_preset_groups_grid;
  fill_block_preset_group_ui;
  // Final actions
  active_tile := -1;
  reset_undo_history;
  compute_block_preset_coverage;
  render_tileset;
  Dec(loading);
end;

procedure TTilesetEditor.update_tileset_filenames;
begin
  if Tileset.tileatr_name <> '' then
    StatusBar.Panels[1].Text := Format('%s / %s', [Tileset.tileset_name, Tileset.tileatr_name])
  else
    StatusBar.Panels[1].Text := Tileset.tileset_name;
  StatusBar.Panels[2].Text := Tileset.tileimage_filename;
  if Tileset.tls_filename <> '' then
  begin
    StatusBar.Panels[3].Text := Tileset.tls_filename;
    StatusBar.Panels[4].Text := '';
  end else
  begin
    StatusBar.Panels[3].Text := Tileset.tileatr_filename;
    StatusBar.Panels[4].Text := Tileset.ini_filename;
  end;
end;

procedure TTilesetEditor.update_grid_color;
begin
  render_tileset;
end;

procedure TTilesetEditor.update_text_list;
var
  string_list: TStringList;
  i: integer;
  is_custom: boolean;
  str: String;
begin
  if not Visible then
  begin
    pending_update_text_list := true;
    exit;
  end;
  pending_update_text_list := false;
  // Update tile hint selection list
  string_list := TStringList.Create;
  for i := 0 to StringTable.text_uib.Count-1 do
  begin
    str := inttostr(i) + ' - ' + StringTable.get_text(i, false, is_custom);
    if is_custom then
      str := '*' + str;
    string_list.Add(str);
  end;
  lbTileHintText.Items := string_list;
  string_list.Destroy;
end;

procedure TTilesetEditor.update_speed_modifiers;
var
  value: int64;
  speed_modifier: integer;
  str: string;
  i: integer;
begin
  value := StrToInt64('$' + edTileAtrValue.Text);
  speed_modifier := (value shr 29) and 7;
  str := 'Terrain type ' + inttostr(speed_modifier) + ': ';
  for i := 0 to Length(Structures.speed.SpeedNameStrings) - 1 do
    str := str + Format('%s = %.3f  ', [Structures.speed.SpeedNameStrings[i], Round(Structures.speed.values[speed_modifier, i] * 100)/100]);
  stSpeedModifier.Caption := str;
end;

procedure TTilesetEditor.fill_paint_tile_group_combo_boxes;
var
  tmp_strings: TStringList;
  i: integer;
  old_item_index: integer;
begin
  tmp_strings := TStringList.Create;
  for i := -1 to cnt_paint_tile_groups - 1 do
    tmp_strings.Add(Tileset.get_paint_tile_group_char(i) + ' - ' + Tileset.paint_tile_groups[i].name);
  old_item_index := cbxDefaultPaintGroup.ItemIndex;
  cbxDefaultPaintGroup.Items := tmp_strings;
  cbxDefaultPaintGroup.ItemIndex := old_item_index;
  tmp_strings.Delete(0);
  old_item_index := cbxBlockPresetGroupPaintGroup.ItemIndex;
  cbxBlockPresetGroupPaintGroup.Items := tmp_strings;
  cbxBlockPresetGroupPaintGroup.ItemIndex := old_item_index;
  tmp_strings.Destroy;
end;

procedure TTilesetEditor.fill_block_preset_group_combo_boxes;
var
  tmp_strings: TStringList;
  i: integer;
  old_item_index: integer;
begin
  tmp_strings := TStringList.Create;
  tmp_strings.Add('(none)');
  for i := 0 to cnt_block_preset_groups - 1 do
    tmp_strings.Add(IntToStr(i) + ' - ' + Tileset.block_preset_groups[i].name);
  old_item_index := cbxPaintTileGroupSmoothPresetGroup.ItemIndex;
  cbxPaintTileGroupSmoothPresetGroup.Items := tmp_strings;
  cbxPaintTileGroupSmoothPresetGroup.ItemIndex := old_item_index;
  tmp_strings.Destroy;
end;

procedure TTilesetEditor.fill_minimap_color_rules_grid;
var
  i: integer;
begin
  sgMinimapColorRules.RowCount := Tileset.minimap_color_rules_used + 1;
  for i := 0 to Tileset.minimap_color_rules_used - 1 do
  begin
    sgMinimapColorRules.Cells[0,i+1] := IntToStr(i);
    sgMinimapColorRules.Cells[2,i+1] := Tileset.minimap_color_rules[i].name;
    sgMinimapColorRules.Cells[3,i+1] := IntToHex(Tileset.minimap_color_rules[i].color, 6);
    sgMinimapColorRules.Cells[4,i+1] := Tileset.rule_to_string(Addr(Tileset.minimap_color_rules[i].rule));
  end;
  sgMinimapColorRules.Invalidate;
end;

procedure TTilesetEditor.fill_minimap_color_rule_ui;
begin
  edMinimapColorRuleName.Text := Tileset.minimap_color_rules[sgMinimapColorRules.Tag - 1].name;
  edMinimapColorRuleColor.Text := IntToHex(Tileset.minimap_color_rules[sgMinimapColorRules.Tag - 1].color, 6);
  edMinimapColorRuleRule.Text := Tileset.rule_to_string(Addr(Tileset.minimap_color_rules[sgMinimapColorRules.Tag - 1].rule));
end;

procedure TTilesetEditor.fill_fill_area_rules_grid;
var
  i: integer;
begin
  sgFillAreaRules.RowCount := Tileset.fill_area_rules_used + 1;
  for i := 0 to Tileset.fill_area_rules_used - 1 do
  begin
    sgFillAreaRules.Cells[0,i+1] := IntToStr(i);
    sgFillAreaRules.Cells[2,i+1] := Tileset.fill_area_rules[i].name;
    sgFillAreaRules.Cells[3,i+1] := Tileset.rule_to_string(Addr(Tileset.fill_area_rules[i].rule));
  end;
  sgFillAreaRules.Invalidate;
end;

procedure TTilesetEditor.fill_fill_area_rule_ui;
begin
  edFillAreaRuleName.Text := Tileset.fill_area_rules[sgFillAreaRules.Tag - 1].name;
  edFillAreaRuleRule.Text := Tileset.rule_to_string(Addr(Tileset.fill_area_rules[sgFillAreaRules.Tag - 1].rule));
end;

procedure TTilesetEditor.fill_paint_tile_groups_grid;
var
  i: integer;
begin
  for i := -4 to cnt_paint_tile_groups - 1 do
  begin
    sgPaintTileGroups.Cells[2,i+1+4] := Tileset.paint_tile_groups[i].name;
    if i >= 0 then
    begin
      sgPaintTileGroups.Cells[3,i+1+4] := IntToStr(Tileset.paint_tile_lists[i].cnt_tiles);
      if Tileset.paint_tile_groups[i].smooth_preset_group >= 0 then
        sgPaintTileGroups.Cells[4,i+1+4] := Tileset.block_preset_groups[Tileset.paint_tile_groups[i].smooth_preset_group].name
      else
        sgPaintTileGroups.Cells[4,i+1+4] := '-';
    end else
    begin
      sgPaintTileGroups.Cells[3,i+1+4] := '-';
      sgPaintTileGroups.Cells[4,i+1+4] := '-';
    end;
    sgPaintTileGroups.Cells[5,i+1+4] := Tileset.paint_tile_groups[i].smooth_presets;
  end;
end;

procedure TTilesetEditor.fill_paint_tile_group_ui;
var
  tile_x, tile_y: integer;
  show_controls: boolean;
begin
  tile_x := Tileset.paint_tile_groups[sgPaintTileGroups.Tag - 5].tile_index mod 20;
  tile_y := Tileset.paint_tile_groups[sgPaintTileGroups.Tag - 5].tile_index div 20;
  imgPaintTileGroupButtonImage.Canvas.CopyRect(Rect(0,0,32,32), Tileset.tileimage.Canvas, Rect(tile_x*32, tile_y*32, tile_x*32+32, tile_y*32+32));
  imgPaintTileGroupButtonImage.Hint := 'Tile index: ' + IntToStr(Tileset.paint_tile_groups[sgPaintTileGroups.Tag - 5].tile_index);
  edPaintTileGroupName.Text := Tileset.paint_tile_groups[sgPaintTileGroups.Tag - 5].name;
  edPaintTileGroupRestrictionRule.Text := Tileset.rule_to_string(Addr(Tileset.paint_tile_groups[sgPaintTileGroups.Tag - 5].restriction_rule));
  cbxPaintTileGroupSmoothPresetGroup.ItemIndex := Tileset.paint_tile_groups[sgPaintTileGroups.Tag - 5].smooth_preset_group + 1;
  edPaintTileGroupSmoothPresets.Text := Tileset.paint_tile_groups[sgPaintTileGroups.Tag - 5].smooth_presets;
  edPaintTileGroupRandomMapName.Text := Tileset.paint_tile_groups[sgPaintTileGroups.Tag - 5].random_map_name;
  show_controls := sgPaintTileGroups.Tag >= 5;
  lblPaintTileGroupSmoothPresetGroup.Visible := show_controls;
  cbxPaintTileGroupSmoothPresetGroup.Visible := show_controls;
  lblPaintTileGroupSmoothPresets.Visible := show_controls;
  edPaintTileGroupSmoothPresets.Visible := show_controls;
  lblPaintTileGroupRandomMapName.Visible := show_controls;
  edPaintTileGroupRandomMapName.Visible := show_controls;
end;

procedure TTilesetEditor.fill_block_preset_groups_grid;
var
  i, j: integer;
  block_presets: integer;
begin
  for i := 0 to cnt_block_preset_groups - 1 do
  begin
    sgBlockPresetGroups.Cells[1,i+1] := Tileset.block_preset_groups[i].name;
    block_presets := 0;
    for j := 0 to cnt_block_preset_keys - 1 do
      block_presets := block_presets + Tileset.block_preset_key_variants[i,j];
    sgBlockPresetGroups.Cells[2,i+1] := IntToStr(block_presets);
    sgBlockPresetGroups.Cells[3,i+1] := Tileset.paint_tile_groups[Tileset.block_preset_groups[i].paint_group].name;
  end;
end;

procedure TTilesetEditor.fill_block_preset_group_ui;
begin
  edBlockPresetGroupName.Text := Tileset.block_preset_groups[sgBlockPresetGroups.Tag - 1].name;
  cbxBlockPresetGroupPaintGroup.ItemIndex := Tileset.block_preset_groups[sgBlockPresetGroups.Tag - 1].paint_group;
  draw_block_preset_keys;
  draw_block_preset_images;
  draw_block_preset;
end;

procedure TTilesetEditor.draw_block_preset_keys;
var
  i,j: integer;
  key_index: integer;
  num_variants: integer;
  text: string;
  text_width: integer;
begin
  imgBlockPresetKeys.Canvas.Pen.Color := clBlack;
  for i := 0 to 4 do
  begin
    imgBlockPresetKeys.Canvas.MoveTo(0, i*32);
    imgBlockPresetKeys.Canvas.LineTo(imgBlockPresetKeys.Width, i*32);
  end;
  for i := 0 to 10 do
  begin
    imgBlockPresetKeys.Canvas.MoveTo(i*32, 0);
    imgBlockPresetKeys.Canvas.LineTo(i*32, imgBlockPresetKeys.Height);
  end;
  for i := 0 to 3 do
    for j := 0 to 9 do
    begin
      key_index := i * block_preset_cols + j;
      num_variants := Tileset.block_preset_key_variants[sgBlockPresetGroups.Tag - 1, key_index];
      if key_index = block_preset_selected_key then
        imgBlockPresetKeys.Canvas.Brush.Color := $FFFF00
      else
        imgBlockPresetKeys.Canvas.Brush.Color := $FFFFFF - $101000 * Min(num_variants, 15);
      imgBlockPresetKeys.Canvas.FillRect(Rect(j*32+1, i*32+1, j*32+32, i*32+32));
      text := block_preset_keys[key_index];
      text_width := imgBlockPresetKeys.Canvas.TextWidth(text);
      imgBlockPresetKeys.Canvas.TextOut(j * 32 + (32 - text_width) div 2 + 1, i * 32 + 2, text);
      text := IntToStr(num_variants);
      text_width := imgBlockPresetKeys.Canvas.TextWidth(text);
      imgBlockPresetKeys.Canvas.TextOut(j * 32 + (32 - text_width) div 2 + 1, i * 32 + 17, text);
    end;
end;

procedure TTilesetEditor.draw_block_preset_images;
var
  i, x, y: integer;
  num_variants: integer;
  old_length: integer;
  preset_index: integer;
  preset: PBlockPreset;
  pos_x, pos_y, max_height: integer;
  tile: word;
  tile_x, tile_y: integer;
begin
  num_variants := Tileset.block_preset_key_variants[sgBlockPresetGroups.Tag - 1, block_preset_selected_key];
  if num_variants > Length(block_preset_images) then
  begin
    old_length := Length(block_preset_images);
    SetLength(block_preset_images, num_variants);
    for i := old_length to num_variants - 1 do
    begin
      block_preset_images[i] := TImage.Create(self);
      block_preset_images[i].Parent := PagePresets;
      block_preset_images[i].Canvas.Brush.Color := clWhite;
      block_preset_images[i].ShowHint := true;
      block_preset_images[i].OnMouseDown := BlockPresetImageMouseDown;
    end;
  end;
  pos_x := 680;
  pos_y := 368;
  max_height := 0;
  for i := 0 to num_variants - 1 do
  begin
    preset_index := Tileset.block_preset_key_variant_first_preset_indexes[sgBlockPresetGroups.Tag - 1, block_preset_selected_key] + i;
    preset := Addr(Tileset.block_presets[preset_index]);
    block_preset_images[i].Visible := true;
    block_preset_images[i].Tag := preset_index;
    block_preset_images[i].Hint := 'Preset ' + IntToStr(preset_index) + #13'Shift + click = copy preset'#13'Right click = delete preset';
    block_preset_images[i].Width := preset.width * 32;
    block_preset_images[i].Picture.Bitmap.Width := preset.width * 32;
    block_preset_images[i].Height := preset.height * 32;
    block_preset_images[i].Picture.Bitmap.Height := preset.height * 32;
    if (pos_x + preset.width * 32 + 16) > 1088 then
    begin
      Inc(pos_y, max_height + 16);
      pos_x := 680;
      max_height := 0;
    end;
    block_preset_images[i].Top := pos_y;
    block_preset_images[i].Left := pos_x;
    Inc(pos_x, preset.width * 32 + 16);
    max_height := Max(max_height, preset.height * 32);
    for y := 0 to preset.height - 1 do
      for x := 0 to preset.width - 1 do
      begin
        tile := Tileset.block_preset_tiles[Tileset.block_preset_first_tile_indexes[preset_index] + y * preset.width + x];
        if tile = 65535 then
        begin
          block_preset_images[i].Canvas.FillRect(Rect(x * 32, y * 32, x * 32 + 32, y * 32 + 32));
          continue;
        end;
        tile_x := tile mod 20;
        tile_y := tile div 20;
        block_preset_images[i].Canvas.CopyRect(Rect(x * 32, y * 32, x * 32 + 32, y * 32 + 32), Tileset.tileimage.Canvas, Rect(tile_x * 32, tile_y * 32, tile_x * 32 + 32, tile_y * 32 + 32));
      end;
  end;
  for i := num_variants to Length(block_preset_images) - 1 do
    block_preset_images[i].Visible := false;
end;

procedure TTilesetEditor.draw_block_preset;
var
  x, y: integer;
  tile_index: integer;
  tile: word;
  tile_x, tile_y: integer;
begin
  inc(loading);
  seBlockPresetWidth.Value := block_preset_width;
  seBlockPresetHeight.Value := block_preset_height;
  imgBlockPreset.Canvas.Brush.Color := clWhite;
  imgBlockPreset.Canvas.Pen.Color := clGray;
  imgBlockPreset.Width := block_preset_width * 32;
  imgBlockPreset.Height := block_preset_height * 32;
  for y := 0 to block_preset_height - 1 do
    for x := 0 to block_preset_width - 1 do
    begin
      tile_index := y * block_preset_width + x;
      tile := block_preset_tiles[tile_index];
      if tile = 65535 then
      begin
        imgBlockPreset.Canvas.Brush.Style := bsSolid;
        imgBlockPreset.Canvas.FillRect(Rect(x*32, y*32, x*32+32, y*32+32))
      end else
      begin
        tile_x := tile mod 20;
        tile_y := tile div 20;
        imgBlockPreset.Canvas.CopyRect(Rect(x*32, y*32, x*32+32, y*32+32), tileset.tileimage.Canvas, Rect(tile_x * 32, tile_y * 32, tile_x * 32 + 32, tile_y * 32 + 32));
      end;
      if tile_index = block_preset_selected_tile then
      begin
        imgBlockPreset.Canvas.Brush.Style := bsClear;
        imgBlockPreset.Canvas.Rectangle(x*32, y*32, x*32+32, y*32+32);
      end;
    end;
  Dec(loading);
end;

procedure TTilesetEditor.render_tileset;
var
  x, y: integer;
  tile_index: integer;
  tile_value: int64;
  filter_mode: FilterMode;
  mark_tile: boolean;
  fill_area_type: integer;
  selected_value: int64;
  color, color_editor: cardinal;
  num_editor_attributes: integer;
  i, j: integer;
  tile_text: String;
  min_x, min_y, max_x, max_y: integer;
  t1, t2: Int64;
  rule_index: integer;
  tile_paint_group: integer;
begin
  if Tileset.tileimage = nil then
    exit;
  // Start measuring rendering time
  if Settings.Debug_ShowRenderTime then
    QueryPerformanceCounter(t1);
  // Draw tileset
  TilesetImage.Canvas.CopyRect(Rect(0,0,tileimage_width,tileset_height*32), Tileset.tileimage.Canvas, Rect(0,tileset_top*32,tileimage_width,tileset_top*32+tileset_height*32));
  // Draw grid
  if cbShowGrid.Checked then
  begin
    TilesetImage.Canvas.Pen.Color:= Settings.GridColor;
    TilesetImage.Canvas.Pen.Width := 1;
    for x:= 0 to 20-1 do
    begin
      TilesetImage.Canvas.MoveTo(x*32,0);
      TilesetImage.Canvas.LineTo(x*32,tileset_height*32);
    end;
    for y:= 0 to tileset_height-1 do
    begin
      TilesetImage.Canvas.MoveTo(0,y*32);
      TilesetImage.Canvas.LineTo(tileimage_width,y*32);
    end;
  end;
  // Draw markers
  if PageControl.ActivePage <> PageImage then
  begin
    TilesetImage.Canvas.Font.Color := Settings.GridColor;
    TilesetImage.Canvas.Brush.Style := bsClear;
    TilesetImage.Canvas.Pen.Width := 1;
    for y := 0 to tileset_height - 1 do
      for x := 0 to 19 do
      begin
        tile_index := x + (y + tileset_top) * 20;
        filter_mode := FilterMode(rgFilterMode.ItemIndex);
        tile_value := Tileset.get_tile_attributes(tile_index, 0, (PageControl.ActivePage <> PageAttributes) or (filter_mode = fmByRule));
        color := 0;
        mark_tile := false;
        tile_text := '';
        if PageControl.ActivePage = PageAttributes then
        begin
          // Determine whether tile should be marked according to current filter mode
          selected_value := strtoint64('$'+edTileAtrValue.Text);
          case filter_mode of
            fmAll:            mark_tile := true;
            fmExactAtr:       mark_tile := tile_value = selected_value;
            fmHavingAllAtr:   mark_tile := (tile_value and selected_value) = selected_value;
            fmHavingAnyOfAtr: mark_tile := (tile_value and selected_value) <> 0;
            fmNotHavingAtr:   mark_tile := (tile_value and selected_value) = 0;
            fmByRule:         mark_tile := Tileset.evaluate_rule(tile_value, Addr(rule));
            fmNothing:        mark_tile := false;
          end;
          // Get color
          if mark_tile then
            color := get_tile_attribute_color(tile_value);
        end
        else if PageControl.ActivePage = PageHints then
        begin
          mark_tile := Tileset.tile_hint_text[tile_index] <> -1;
          if cbMarkSelectedItem.Checked then
            mark_tile := mark_tile and (Tileset.tile_hint_text[tile_index] = lbTileHintText.ItemIndex);
          if mark_tile then
          begin
            color := Settings.GridColor;
            tile_text := inttostr(Tileset.tile_hint_text[tile_index]);
          end;
        end
        else if PageControl.ActivePage = PageColors then
        begin
          mark_tile := true;
          if cbMarkSelectedItem.Checked then
            mark_tile := Tileset.evaluate_rule(tile_value, Addr(Tileset.minimap_color_rules[sgMinimapColorRules.Tag - 1].rule));
          if mark_tile then
          begin
            color := Tileset.get_tile_color(tile_index, 0, rule_index);
            color := ((color and $FF0000) shr 16) or (color and $00FF00) or ((color and $0000FF) shl 16);
          end;
        end
        else if PageControl.ActivePage = PageFillArea then
        begin
          mark_tile := true;
          if cbMarkSelectedItem.Checked then
            mark_tile := Tileset.evaluate_rule(tile_value, Addr(Tileset.fill_area_rules[sgFillAreaRules.Tag - 1].rule));
          if mark_tile then
          begin
            fill_area_type := Tileset.get_fill_area_type(tile_index, 0);
            if fill_area_type = (Tileset.fill_area_rules_used - 1) then
              // Tiles ftom the last fill area group should be always green
              fill_area_type := max_fill_area_rules - 1;
            color := fill_area_group_colors[fill_area_type];
          end;
        end
        else if PageControl.ActivePage = PagePaint then
        begin
          tile_paint_group := Tileset.get_tile_paint_group(tile_index);
          mark_tile := tile_paint_group <> -128;
          if cbMarkSelectedItem.Checked then
            mark_tile := tile_paint_group = sgPaintTileGroups.Tag - 5;
          if mark_tile then
            color := fill_area_group_colors[tile_paint_group + 4];
        end
        else if PageControl.ActivePage = PagePresets then
        begin
          tile_paint_group := Tileset.get_tile_paint_group(tile_index);
          if (block_preset_coverage[tile_index] > 0) or ((tile_paint_group <> -128) and (not cbMarkSelectedItem.Checked)) then
          begin
            mark_tile := true;
            color := $000000;
            if (tile_paint_group <> -128) then
            begin
              color := $D00000;
              tile_text := Tileset.get_paint_tile_group_char(tile_paint_group);
            end;
            if block_preset_coverage[tile_index] = 1 then
              color := color or $00A000
            else if block_preset_coverage[tile_index] > 1 then
              color := color or $0000D0;
          end;
        end;
        // Mark tile with color
        if mark_tile then
        begin
          TilesetImage.Canvas.Pen.Color := color;
          TilesetImage.Canvas.Rectangle(x*32+1, y*32+1, x*32+31, y*32+31);
          TilesetImage.Canvas.Rectangle(x*32+2, y*32+2, x*32+30, y*32+30);
          // Draw editor attribute color markers
          if (PageControl.ActivePage = PageAttributes) and cbDrawEditorAttributes.Checked then
          begin
            num_editor_attributes := 0;
            for i := 0 to 7 do
              if ((tile_value shr 32) and (1 shl i)) <> 0 then
                inc(num_editor_attributes);
            j := 0;
            for i := 0 to 7 do
            begin
              if ((tile_value shr 32) and (1 shl i)) = 0 then
                continue;
              TilesetImage.Canvas.Pen.Color := atr_colors_editor[i];
              TilesetImage.Canvas.Rectangle(x*32 + 16 - num_editor_attributes*2 + j*4, y*32+14, x*32 + 20 - num_editor_attributes*2 + j*4, y*32+18);
              TilesetImage.Canvas.Rectangle(x*32 + 17 - num_editor_attributes*2 + j*4, y*32+15, x*32 + 19 - num_editor_attributes*2 + j*4, y*32+17);
              inc(j);
            end;
          end;
          // Draw building/unit owner side marker
          if (PageControl.ActivePage = PageAttributes) and cbDrawOwnerSide.Checked and ((tile_value and $3F8) <> 0) then
          begin
            color := StructGraphics.house_colors_inv[tile_value and 7];
            TilesetImage.Canvas.Pen.Color := color;
            TilesetImage.Canvas.Brush.Color := color;
            TilesetImage.Canvas.Brush.Style := bsSolid;
            TilesetImage.Canvas.Ellipse(x*32 + 11, y*32 + 3, x*32 + 21, y*32 + 14);
            TilesetImage.Canvas.Brush.Style := bsClear;
          end;
          // Draw concrete owner side marker
          if (PageControl.ActivePage = PageAttributes) and cbDrawOwnerSide.Checked and ((tile_value and $800) <> 0) then
          begin
            color := StructGraphics.house_colors_inv[(tile_value shr 17) and 7];
            TilesetImage.Canvas.Pen.Color := color;
            TilesetImage.Canvas.Brush.Color := color;
            TilesetImage.Canvas.Brush.Style := bsSolid;
            TilesetImage.Canvas.Ellipse(x*32 + 11, y*32 + 18, x*32 + 21, y*32 + 29);
            TilesetImage.Canvas.Brush.Style := bsClear;
          end;
          // Draw text (i.e. number)
          if tile_text <> '' then
            TilesetImage.Canvas.TextOut(x*32 + (32 - TilesetImage.Canvas.TextWidth(tile_text)) div 2, y*32+10, tile_text);
        end
        else if cbHideUnmarkedTiles.Checked then
        begin
          TilesetImage.Canvas.Pen.Color := Settings.GridColor;
          TilesetImage.Canvas.Brush.Color := Settings.GridColor;
          TilesetImage.Canvas.Brush.Style := bsSolid;
          TilesetImage.Canvas.Rectangle(x*32, y*32, x*32 + 32, y*32 + 32);
          TilesetImage.Canvas.Brush.Style := bsClear;
        end;
        // Highlight selected tile
        if (PageControl.ActivePage = PageAttributes) and (active_tile = tile_index) and cbMarkSelection.Checked then
        begin
          TilesetImage.Canvas.Brush.Style := bsClear;
          TilesetImage.Canvas.Pen.Width := 2;
          TilesetImage.Canvas.Pen.Color := $FF0000;
          TilesetImage.Canvas.Rectangle(x*32+1, y*32+1, x*32+32, y*32+32);
          TilesetImage.Canvas.Pen.Width := 1;
        end;
      end;
  end;
  // Mark selection
  if select_started or (PageControl.ActivePage = PageImage) then
  begin
    TilesetImage.Canvas.Brush.Style := bsClear;
    TilesetImage.Canvas.Pen.Width := 2;
    TilesetImage.Canvas.Pen.Color := $FF;
    min_x := min(select_start_x, select_end_x) * 32+1;
    max_x := max(select_start_x, select_end_x) * 32+32;
    min_y := (min(select_start_y, select_end_y) - tileset_top) * 32+1;
    max_y := (max(select_start_y, select_end_y) - tileset_top) * 32+32;
    TilesetImage.Canvas.Rectangle(min_x, min_y, max_x, max_y);
  end;
  // Measure rendering time
  if Settings.Debug_ShowRenderTime then
  begin
    QueryPerformanceCounter(t2);
    Caption := FloatToStr((t2-t1) / performance_frequency);
  end;
end;

procedure TTilesetEditor.compute_block_preset_coverage;
var
  i, j, k, l: integer;
  preset_index: integer;
  tile_index: integer;
begin
  FillChar(block_preset_coverage, sizeof(block_preset_coverage), 0);
  preset_index := 1;
  tile_index := 0;
  for i := 0 to cnt_block_preset_groups - 1 do
    for j := 0 to cnt_block_preset_keys - 1 do
      for k := 0 to Tileset.block_preset_key_variants[i,j] - 1 do
      begin
        for l := 0 to Tileset.block_presets[preset_index].width * Tileset.block_presets[preset_index].height - 1 do
        begin
          if (not cbMarkSelectedItem.Checked) or (i = sgBlockPresetGroups.Tag - 1) then
            Inc(block_preset_coverage[Tileset.block_preset_tiles[tile_index]]);
          Inc(tile_index);
        end;
        Inc(preset_index);
      end;
end;

procedure TTilesetEditor.do_undo;
var
  tmp_data: int64;
begin
  if undo_pos = undo_start then
    exit;
  repeat
    undo_pos := (undo_pos - 1) and max_undo_steps;
    tmp_data := Tileset.get_tile_attributes(undo_history[undo_pos].index, 0, false);
    Tileset.set_tile_attributes(undo_history[undo_pos].index, undo_history[undo_pos].data);
    undo_history[undo_pos].data := tmp_data;
  until undo_history[undo_pos].is_first or (undo_pos = undo_start);
  if undo_pos = undo_start then
    Undo1.Enabled := false;
  Redo1.Enabled := true;
end;

procedure TTilesetEditor.do_redo;
var
  tmp_data: int64;
begin
  if undo_pos = undo_max then
    exit;
  repeat
    tmp_data := Tileset.get_tile_attributes(undo_history[undo_pos].index, 0, false);
    Tileset.set_tile_attributes(undo_history[undo_pos].index, undo_history[undo_pos].data);
    undo_history[undo_pos].data := tmp_data;
    undo_pos := (undo_pos + 1) and max_undo_steps;
  until undo_history[undo_pos].is_first or (undo_pos = undo_max);
  if undo_pos = undo_max then
    Redo1.Enabled := false;
  Undo1.Enabled := true;
end;

procedure TTilesetEditor.reset_undo_history;
begin
  Undo1.Enabled := false;
  Redo1.Enabled := false;
  undo_start := 0;
  undo_max := 0;
  undo_pos := 0;
end;

procedure TTilesetEditor.set_tile_attribute_list(value, not_value: int64);
var
  i: integer;
  v: int64;
begin
  for i := 0 to 31 do
  begin
    if (value and (1 shl i)) <> 0 then
      clbTileAtrList.State[i] := cbChecked
    else if (not_value and (1 shl i)) <> 0 then
      clbTileAtrList.State[i] := cbGrayed
    else
      clbTileAtrList.State[i] := cbUnchecked;
  end;
  for i := 0 to 7 do
  begin
    v := 1;
    v := v shl (i+32);
    if (value and v) <> 0 then
      clbTileAtrListEditor.State[i] := cbChecked
    else if (not_value and v) <> 0 then
      clbTileAtrListEditor.State[i] := cbGrayed
    else
      clbTileAtrListEditor.State[i] := cbUnchecked;
  end;
end;

procedure TTilesetEditor.set_tile_attribute_value(value, not_value: int64);
var
  color: cardinal;
  building_unit_owner, concrete_owner, spice_amount, unknown_owner: integer;
begin
  edTileAtrValue.Text := IntToHex(value, 10);
  edTileAtrNotValue.Text := IntToHex(not_value, 10);
  color := get_tile_attribute_color(value);
  pnTileAtrColor.Color := color;
  // Set side bit values label
  building_unit_owner := value and 7;
  concrete_owner := (value shr 17) and 7;
  spice_amount := (value shr 20) and 7;
  unknown_owner := (value shr 25) and 7;
  stSideBitValues.Caption := Format('Owner: %d  Conc: %d  Spice: %d  SecOwner: %d', [building_unit_owner, concrete_owner, spice_amount, unknown_owner]);
  // Set speed modifier label
  update_speed_modifiers;
end;

procedure TTilesetEditor.set_tile_attribute_rule(value, not_value: int64);
var
  mode: FilterMode;
  rule_str: string;
begin
  mode := FilterMode(rgFilterMode.ItemIndex);
  rule_str := '';
  case mode of
    fmAll:            rule_str := '';
    fmExactAtr:       rule_str := '';
    fmHavingAllAtr:   rule_str := '$' + IntToHex(value, 1);
    fmHavingAnyOfAtr: rule_str := '-$' + IntToHex(value, 1);
    fmNotHavingAtr:   rule_str := '$0;$' + IntToHex(value, 1);
    fmByRule:         rule_str := IfThen(cbAnyOf.Checked, '-', '') + '$' + IntToHex(value, 1) + IfThen(not_value <> 0, ';$' + IntToHex(not_value, 1), '');
    fmNothing:        rule_str := '';
  end;
  edRule.OnChange := nil;
  edRule.Text := rule_str;
  edRule.Font.Color := clBlack;
  edRule.ReadOnly := mode <> fmByRule;
  edRule.OnChange := edRuleChange;
  if mode = fmByRule then
    Tileset.string_to_rule(edRule.Text, Addr(rule));
end;

function TTilesetEditor.get_tile_attribute_color(value: int64): cardinal;
var
  i: integer;
begin
  result := $0;
  for i := 0 to 31 do
    if (value and (1 shl i)) <> 0 then
      result := result or atr_colors_game[i];
end;

procedure TTilesetEditor.set_tile_attributes(tile_index: integer; single_op: boolean);
var
  selected_value: int64;
  current_value: int64;
  target_value: int64;
  operation: SetOperation;
begin
  selected_value := strtoint64('$'+edTileAtrValue.Text);
  current_value := Tileset.get_tile_attributes(tile_index, 0, false);
  target_value := 0;
  operation := SetOperation(rgOperation.ItemIndex);
  // Get the target tileatr value according to the operation
  case operation of
    opSet:    target_value := selected_value;
    opAdd:    target_value := current_value or selected_value;
    opRemove: target_value := current_value and (not selected_value);
    end;
  // Save old tileatr value into undo history
  undo_history[undo_pos].index := tile_index;
  undo_history[undo_pos].data := current_value;
  undo_history[undo_pos].is_first := single_op or undo_block_start;
  undo_block_start := false;
  undo_pos := (undo_pos + 1) and max_undo_steps;
  if undo_start = undo_pos then
    undo_start := (undo_start + 1) and max_undo_steps;
  undo_max := undo_pos;
  Undo1.Enabled := true;
  Redo1.Enabled := false;
  // Save new tileatr value
  Tileset.set_tile_attributes(tile_index, target_value);
end;

end.
