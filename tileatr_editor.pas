unit tileatr_editor;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, Menus, StdCtrls, CheckLst, XPMan, Math,
  Buttons, IniFiles, StrUtils, _tileset;

const tileset_max_height = 40;
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
   ViewMode = (vmDrawTilesetAttributes, vmDrawMinimapColors, vmDrawFillAreaGroups, vmCheckBlockPresetCoverage, vmEditTileHintText);

type
  TTileAtrEditor = class(TForm)
    MainMenu: TMainMenu;
    StatusBar: TStatusBar;
    TilesetImage: TImage;
    TilesetScrollBar: TScrollBar;
    TileAtrValue: TEdit;
    lbTileAtrValue: TLabel;
    File1: TMenuItem;
    OpenTileset1: TMenuItem;
    OpenTileAtr1: TMenuItem;
    OpenBoth1: TMenuItem;
    N1: TMenuItem;
    SaveTileAtr1: TMenuItem;
    Help1: TMenuItem;
    MouseActions1: TMenuItem;
    TileAtrList: TCheckListBox;
    btnTileAtrValueApply: TButton;
    Quickopen1: TMenuItem;
    rgFilterMode: TRadioGroup;
    rgOperation: TRadioGroup;
    cbMultipleSelectMode: TCheckBox;
    ReloadTileatr1: TMenuItem;
    N3: TMenuItem;
    SaveTileAtras1: TMenuItem;
    SaveTileAtrDialog: TSaveDialog;
    rgViewMode: TRadioGroup;
    btnConvertEditorAttributes: TButton;
    N5: TMenuItem;
    Exit1: TMenuItem;
    cbShowGrid: TCheckBox;
    cbMarkSelection: TCheckBox;
    Edit1: TMenuItem;
    Undo1: TMenuItem;
    Redo1: TMenuItem;
    btnClearAttributes: TButton;
    KeyShortcuts1: TMenuItem;
    TileAtrListEditor: TCheckListBox;
    Saveandtest1: TMenuItem;
    TileAtrColor: TPanel;
    TileAtrNotValue: TEdit;
    lbTileAtrNotValue: TLabel;
    cbDrawEditorAttributes: TCheckBox;
    TileAtrColorEditor: TPanel;
    cbAlwaysOnTop: TCheckBox;
    lbTileHintText: TListBox;
    edRule: TEdit;
    lbRule: TLabel;
    cbAnyOf: TCheckBox;
    // Form actions
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    // Menu actions
    procedure OpenTileset1Click(Sender: TObject);
    procedure OpenTileAtr1Click(Sender: TObject);
    procedure OpenBoth1Click(Sender: TObject);
    procedure ReloadTileatr1Click(Sender: TObject);
    procedure SaveTileAtr1Click(Sender: TObject);
    procedure Saveandtest1Click(Sender: TObject);
    procedure SaveTileAtras1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure QuickOpenClick(Sender: TObject);
    procedure Undo1Click(Sender: TObject);
    procedure Redo1Click(Sender: TObject);
    procedure KeyShortcuts1Click(Sender: TObject);
    procedure MouseActions1Click(Sender: TObject);
    // Controls actions
    procedure TilesetImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TilesetImageMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure TilesetImageMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure TilesetScrollBarChange(Sender: TObject);
    procedure TileAtrListClickCheck(Sender: TObject);
    procedure btnTileAtrValueApplyClick(Sender: TObject);
    procedure btnClearAttributesClick(Sender: TObject);
    procedure rgFilterModeClick(Sender: TObject);
    procedure cbAnyOfClick(Sender: TObject);
    procedure cbOptionClick(Sender: TObject);
    procedure btnConvertEditorAttributesClick(Sender: TObject);
    procedure cbAlwaysOnTopClick(Sender: TObject);
    procedure rgViewModeClick(Sender: TObject);
    procedure edRuleChange(Sender: TObject);

  private
    { Private declarations }

    // Configutarion variables
    menuitems: array of TMenuItem;

    // Status variables
    active_tile: integer;
    tileset_top: integer;
    tileset_height: integer;
    rule: TTileAtrRule;

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

    procedure init_tilesets;
  public
    procedure init_tile_hint_text_list;
    procedure render_tileset;
  private
    procedure do_undo;
    procedure do_redo;
    procedure reset_undo_history;
    procedure set_tile_attribute_list(value, not_value: int64);
    procedure set_tile_attribute_value(value, not_value: int64);
    procedure set_tile_attribute_rule(value, not_value: int64);
    procedure get_tile_attribute_color(value: int64; var color, color_editor: cardinal);
    procedure set_tile_attributes(tile_index: integer; single_op: boolean);
  public
    procedure tileset_changed;
  private
    function confirm_overwrite_original_file(filename: string): boolean;
  end;

var
  TileAtrEditor: TTileAtrEditor;

implementation

uses main, _stringtable, _settings;

{$R *.dfm}

procedure TTileAtrEditor.FormCreate(Sender: TObject);
begin
  TilesetImage.Picture.Bitmap.Width := 640;
  init_tilesets;
  init_tile_hint_text_list;
end;

procedure TTileAtrEditor.FormResize(Sender: TObject);
var
  new_tileset_height: integer;
begin
  new_tileset_height := (ClientHeight - 32) div 32;
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

procedure TTileAtrEditor.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (ActiveControl = TileAtrValue) or (ActiveControl = TileAtrNotValue) or (ActiveControl = edRule) then
    exit;
  case key of
    ord('G'): cbShowGrid.Checked := not cbShowGrid.Checked;
    ord('L'): cbMarkSelection.Checked := not cbMarkSelection.Checked;
    ord('S'): rgOperation.ItemIndex := 0;
    ord('A'): rgOperation.ItemIndex := 1;
    ord('R'): rgOperation.ItemIndex := 2;
    ord('M'): cbMultipleSelectMode.Checked := not cbMultipleSelectMode.Checked;
    ord('C'): btnClearAttributesClick(nil);
    ord('E'): cbDrawEditorAttributes.Checked := not cbDrawEditorAttributes.Checked;
    27: Close;
  end;
end;

procedure TTileAtrEditor.FormMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  TilesetScrollBar.Position := TilesetScrollBar.Position + 2;
  Handled := true;
end;

procedure TTileAtrEditor.FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  TilesetScrollBar.Position := TilesetScrollBar.Position - 2;
  Handled := true;
end;

procedure TTileAtrEditor.OpenTileset1Click(Sender: TObject);
begin
  MainWindow.Loadtileset1Click(Sender);
end;

procedure TTileAtrEditor.OpenTileAtr1Click(Sender: TObject);
begin
  MainWindow.Loadtilesetattributes1Click(Sender);
end;

procedure TTileAtrEditor.OpenBoth1Click(Sender: TObject);
begin
  MainWindow.Loadtileset1Click(Sender);
  MainWindow.Loadtilesetattributes1Click(Sender);
end;

procedure TTileAtrEditor.ReloadTileatr1Click(Sender: TObject);
begin
  MainWindow.Reloadtileset1Click(nil);
end;

procedure TTileAtrEditor.SaveTileAtr1Click(Sender: TObject);
begin
  if (Tileset.tileatr_filename <> '') and confirm_overwrite_original_file(Tileset.tileatr_filename) then
  begin
    Tileset.save_attributes;
    MainWindow.render_map;
    MainWindow.render_minimap;
  end;
end;

procedure TTileAtrEditor.Saveandtest1Click(Sender: TObject);
begin
  if (Tileset.tileatr_filename <> '') and confirm_overwrite_original_file(Tileset.tileatr_filename) then
  begin
    Tileset.save_attributes;
    MainWindow.render_map;
    MainWindow.render_minimap;
    MainWindow.Quicklaunch1Click(Sender);
  end;
end;

procedure TTileAtrEditor.SaveTileAtras1Click(Sender: TObject);
begin
  if Tileset.tileatr_filename <> '' then
  begin
    if SaveTileAtrDialog.Execute and confirm_overwrite_original_file(SaveTileAtrDialog.FileName) then
    begin
      Tileset.save_attributes_to_file(SaveTileAtrDialog.FileName);
      MainWindow.tileset_changed;
      MainWindow.render_map;
      MainWindow.render_minimap;
    end;
  end;
end;

procedure TTileAtrEditor.Exit1Click(Sender: TObject);
begin
  Close;
end;

procedure TTileAtrEditor.QuickOpenClick(Sender: TObject);
begin
  MainWindow.change_tileset((Sender as TMenuItem).Tag);
end;

procedure TTileAtrEditor.Undo1Click(Sender: TObject);
begin
  do_undo;
  render_tileset;
end;

procedure TTileAtrEditor.Redo1Click(Sender: TObject);
begin
  do_redo;
  render_tileset;
end;

procedure TTileAtrEditor.KeyShortcuts1Click(Sender: TObject);
begin
  ShowMessage('Key shortcuts:'#13#13'G = Show Grid'#13'L = Mark Selection'#13 +
              'S = Set attributes'#13'A = Add selected attributes'#13'R = Remove selected attributes'#13'C = Clear selected attributes'#13'M = Multi-select mode'#13'E = Draw editor attributes');
end;

procedure TTileAtrEditor.MouseActions1Click(Sender: TObject);
begin
  ShowMessage('Mouse actions:'#13#13'Left click = Set tileset attributes'#13'Right click = Get tileset attributes'#13'Middle click = Unmark selected tile'#13'Use "Multi-select mode" and'#13'drag over all tiles you want to modify.');
end;

procedure TTileAtrEditor.TilesetImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  pos_x, pos_y: integer;
  tile_index: integer;
  tile_value: int64;
  view_mode: ViewMode;
  attributes_mode: boolean;
begin
  pos_x := X div 32;
  pos_y := Y div 32 + tileset_top;
  tile_index := pos_x + pos_y * 20;
  view_mode := ViewMode(rgViewMode.ItemIndex);
  attributes_mode := (view_mode = vmDrawTilesetAttributes) or (view_mode = vmDrawMinimapColors) or (view_mode = vmDrawFillAreaGroups);
  if Button = mbRight then
  begin
    if attributes_mode then
    begin
      tile_value := Tileset.get_tile_attributes(tile_index, 0, false);
      set_tile_attribute_value(tile_value, 0);
      set_tile_attribute_list(tile_value, 0);
      set_tile_attribute_rule(tile_value, 0);
    end else
    if view_mode = vmEditTileHintText then
      lbTileHintText.ItemIndex := Tileset.tile_hint_text[tile_index];
    active_tile := tile_index;
  end
  else if Button = mbLeft then
  begin
    if not cbMultipleSelectMode.Checked then
    begin
      if attributes_mode then
        set_tile_attributes(tile_index, true)
      else
        Tileset.tile_hint_text[tile_index] := lbTileHintText.ItemIndex;
    end else
    begin
      select_started := true;
      select_start_x := pos_x;
      select_start_y := pos_y;
      select_end_x := pos_x;
      select_end_y := pos_y;
    end;
    TilesetImage.ShowHint := false;
  end
  else if Button = mbMiddle then
  begin
    active_tile := -1;
  end;
  render_tileset;
end;

procedure TTileAtrEditor.TilesetImageMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  pos_x, pos_y: integer;
  is_custom: boolean;
  tile_hint_text_id: integer;
begin
  pos_x := X div 32;
  pos_y := Y div 32 + tileset_top;
  if (mouse_old_x = pos_x) and (mouse_old_y = pos_y) then
    exit;
  mouse_old_x := pos_x;
  mouse_old_y := pos_y;
  StatusBar.Panels[0].Text := 'x : ' + inttostr(pos_x) + ' y : ' + inttostr(pos_y) + '  (' + inttostr(pos_y*20 + pos_x) + ')';
  if (not select_started) and (ssLeft in Shift) then
    TilesetImageMouseDown(Sender, mbLeft, Shift, X, Y);
  if select_started and ((pos_x <> select_end_x) or (pos_y <> select_end_y)) then
  begin
    select_end_x := pos_x;
    select_end_y := pos_y;
    render_tileset;
  end;
  if ViewMode(rgViewMode.ItemIndex) = vmEditTileHintText then
  begin
    tile_hint_text_id := Tileset.tile_hint_text[pos_x + pos_y*20];
    if tile_hint_text_id <> -1 then
    begin
      Application.CancelHint;
      TilesetImage.ShowHint := true;
      TilesetImage.Hint := StringTable.get_text(tile_hint_text_id, false, is_custom);
    end else
    begin
      TilesetImage.ShowHint := false;
    end;
  end;
end;

procedure TTileAtrEditor.TilesetImageMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  min_x, min_y, max_x, max_y: integer;
  xx, yy: integer;
  view_mode: ViewMode;
  attributes_mode: boolean;
begin
  if select_started then
  begin
    select_started := false;
    undo_block_start := true;
    min_x := min(select_start_x, select_end_x);
    max_x := max(select_start_x, select_end_x);
    min_y := min(select_start_y, select_end_y);
    max_y := max(select_start_y, select_end_y);
    view_mode := ViewMode(rgViewMode.ItemIndex);
    attributes_mode := (view_mode = vmDrawTilesetAttributes) or (view_mode = vmDrawMinimapColors) or (view_mode = vmDrawFillAreaGroups);
    for yy := min_y to max_y do
      for xx:= min_x to max_x do
        begin
          if attributes_mode then
            set_tile_attributes(xx + yy*20, false)
          else if view_mode = vmEditTileHintText then
            Tileset.tile_hint_text[xx + yy*20] := lbTileHintText.ItemIndex;
        end;
    render_tileset;
    TilesetImage.ShowHint := false;
  end;
end;

procedure TTileAtrEditor.TilesetScrollBarChange(Sender: TObject);
begin
  tileset_top := Min(TilesetScrollBar.Position, tileset_max_height - TilesetScrollBar.PageSize);
  render_tileset;
end;

procedure TTileAtrEditor.TileAtrListClickCheck(Sender: TObject);
var
  i: integer;
  value, not_value: int64;
  v: int64;
begin
  value := 0;
  not_value := 0;
  for i := 0 to 31 do
  begin
    if TileAtrList.Checked[i] then
      value := value or (1 shl i)
    else if TileAtrList.State[i] = cbGrayed then
      not_value := not_value or (1 shl i);
  end;
  for i := 0 to 7 do
  begin
    v := 1;
    v := v shl (i+32);
    if TileAtrListEditor.Checked[i] then
      value := value or v
    else if TileAtrListEditor.State[i] = cbGrayed then
      not_value := not_value or v;
  end;
  set_tile_attribute_value(value, not_value);
  set_tile_attribute_rule(value, not_value);
  if sender <> nil then
    active_tile := -1;
  render_tileset;
end;

procedure TTileAtrEditor.btnTileAtrValueApplyClick(Sender: TObject);
var
  value, not_value: int64;
begin
  value := strtoint64('$'+TileAtrValue.Text);
  not_value := strtoint64('$'+TileAtrNotValue.Text);
  set_tile_attribute_list(value, not_value);
  set_tile_attribute_value(value, not_value);
  set_tile_attribute_rule(value, not_value);
  render_tileset;
end;

procedure TTileAtrEditor.btnClearAttributesClick(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to 31 do
    TileAtrList.State[i] := cbUnchecked;
  for i := 0 to 7 do
    TileAtrListEditor.State[i] := cbUnchecked;
  TileAtrListClickCheck(nil);
  lbTileHintText.ItemIndex := -1;
end;

procedure TTileAtrEditor.rgFilterModeClick(Sender: TObject);
var
  not_atr_active: boolean;
  i: integer;
begin
  not_atr_active := FilterMode(rgFilterMode.ItemIndex) = fmByRule;
  TileAtrList.AllowGrayed := not_atr_active;
  TileAtrListEditor.AllowGrayed := not_atr_active;
  lbTileAtrNotValue.Visible := not_atr_active;
  TileAtrNotValue.Visible := not_atr_active;
  if not not_atr_active then
  begin
    for i := 0 to 31 do
      if TileAtrList.State[i] = cbGrayed then
        TileAtrList.State[i] := cbUnchecked;
    for i := 0 to 7 do
      if TileAtrListEditor.State[i] = cbGrayed then
        TileAtrListEditor.State[i] := cbUnchecked;
  end;
  TileAtrListClickCheck(nil);
  render_tileset;
end;

procedure TTileAtrEditor.cbAnyOfClick(Sender: TObject);
var
  value, not_value: int64;
begin
  value := strtoint64('$'+TileAtrValue.Text);
  not_value := strtoint64('$'+TileAtrNotValue.Text);
  set_tile_attribute_rule(value, not_value);
  render_tileset;
end;

procedure TTileAtrEditor.cbOptionClick(Sender: TObject);
begin
  render_tileset;
end;

procedure TTileAtrEditor.btnConvertEditorAttributesClick(Sender: TObject);
begin
  Tileset.convert_editor_attributes;
  render_tileset;
end;

procedure TTileAtrEditor.cbAlwaysOnTopClick(Sender: TObject);
begin
  if cbAlwaysOnTop.Checked then
    FormStyle := fsStayOnTop
  else
    FormStyle := fsNormal;
end;

procedure TTileAtrEditor.rgViewModeClick(Sender: TObject);
var
  view_mode: ViewMode;
  attributes_mode: boolean;
begin
  view_mode := ViewMode(rgViewMode.ItemIndex);
  attributes_mode := (view_mode = vmDrawTilesetAttributes) or (view_mode = vmDrawMinimapColors) or (view_mode = vmDrawFillAreaGroups);
  TileAtrList.Enabled := attributes_mode;
  TileAtrListEditor.Enabled := attributes_mode;
  rgOperation.Enabled := attributes_mode;
  rgFilterMode.Enabled := attributes_mode;
  TileAtrValue.Enabled := attributes_mode;
  TileAtrNotValue.Enabled := attributes_mode;
  edRule.Enabled := attributes_mode;
  cbAnyOf.Enabled := attributes_mode;
  cbDrawEditorAttributes.Enabled := attributes_mode;
  btnConvertEditorAttributes.Enabled := attributes_mode;
  TileAtrList.Visible := view_mode <> vmEditTileHintText;
  lbTileHintText.Visible := view_mode = vmEditTileHintText;
  render_tileset;
end;

procedure TTileAtrEditor.edRuleChange(Sender: TObject);
var
  rule_valid: boolean;
begin
  rule_valid := Tileset.load_rule(edRule.Text, Addr(rule));
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

procedure TTileAtrEditor.init_tilesets;
var
  i: integer;
begin
  // Load list of tilesets
  SetLength(menuitems, Tileset.cnt_tilesets);
  for i := 0 to Tileset.cnt_tilesets -1 do
  begin
    menuitems[i] := TMenuItem.Create(Quickopen1);
    menuitems[i].Caption := Tileset.tileset_list.Names[i];
    menuitems[i].RadioItem := true;
    menuitems[i].GroupIndex := 1;
    menuitems[i].Tag := i;
    menuitems[i].OnClick := QuickOpenClick;
    Quickopen1.Add(menuitems[i]);
  end;
end;

procedure TTileAtrEditor.init_tile_hint_text_list;
var
  string_list: TStringList;
  i: integer;
  is_custom: boolean;
  str: String;
begin
  string_list := TStringList.Create;
  for i := 0 to StringTable.get_table_size-1 do
  begin
    str := inttostr(i) + ' - ' + StringTable.get_text(i, false, is_custom);
    if is_custom then
      str := '*' + str;
    string_list.Add(str);
  end;
  lbTileHintText.Items := string_list;
end;

procedure TTileAtrEditor.render_tileset;
var
  top_pixels: integer;
  x, y: integer;
  tile_index: integer;
  tile_value: int64;
  filter_mode: FilterMode;
  view_mode: ViewMode;
  mark_tile: boolean;
  fill_area_type: integer;
  selected_value: int64;
  color, color_editor: cardinal;
  tile_text: String;
  min_x, min_y, max_x, max_y: integer;
begin
  top_pixels := tileset_top * 32;
  selected_value := strtoint64('$'+TileAtrValue.Text);
  if Tileset.tileatr_filename <> '' then
  begin
    TilesetImage.Canvas.Font.Color := Settings.GridColor;
    // Draw tileset
    TilesetImage.Canvas.CopyRect(Rect(0,0,640,tileset_height*32), Tileset.tileimage.Canvas, Rect(0,top_pixels,640,top_pixels+tileset_height*32));
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
        TilesetImage.Canvas.LineTo(640,y*32);
      end;
    end;
    // Draw attributes
    filter_mode := FilterMode(rgFilterMode.ItemIndex);
    view_mode := ViewMode(rgViewMode.ItemIndex);
    TilesetImage.Canvas.Brush.Style := bsClear;
    TilesetImage.Canvas.Pen.Width := 1;
    for y := 0 to tileset_height - 1 do
      for x := 0 to 19 do
      begin
        tile_index := x + (y + tileset_top) * 20;
        tile_value := Tileset.get_tile_attributes(tile_index, 0, filter_mode = fmByRule);
        // Determine whether tile should be marked according to current filter mode
        mark_tile := false;
        color_editor := 0;
        tile_text := '';
        case filter_mode of
          fmAll:            mark_tile := true;
          fmExactAtr:       mark_tile := tile_value = selected_value;
          fmHavingAllAtr:   mark_tile := (tile_value and selected_value) = selected_value;
          fmHavingAnyOfAtr: mark_tile := (tile_value and selected_value) <> 0;
          fmNotHavingAtr:   mark_tile := (tile_value and selected_value) = 0;
          fmByRule:         mark_tile := Tileset.evaluate_rule(tile_value, Addr(rule));
          fmNothing:        mark_tile := false;
          end;
        // Determine color according to current view mode
        if view_mode = vmDrawTilesetAttributes then
        begin
          if mark_tile then
            get_tile_attribute_color(tile_value, color, color_editor);
        end else
        if view_mode = vmDrawMinimapColors then
        begin
          if mark_tile then
            color := Tileset.get_tile_color(tile_index, 0);
        end else
        if view_mode = vmDrawFillAreaGroups then
        begin
          if mark_tile then
          begin
            fill_area_type := Tileset.get_fill_area_type(tile_index, 0);
            if fill_area_type = (Tileset.fill_area_rules_used - 1) then
              // Tiles ftom the last fill area group should be always green
              fill_area_type := max_fill_area_rules - 1;
            color := fill_area_group_colors[fill_area_type];
          end;
        end else
        if view_mode = vmCheckBlockPresetCoverage then
        begin
          if (Tileset.block_preset_coverage[tile_index] > 0) or (Tileset.tile_paint_group[tile_index] <> -128) then
          begin
            mark_tile := true;
            color := $000000;
            if Tileset.tile_paint_group[tile_index] <> -128 then
            begin
              color := $D00000;
              tile_text := Tileset.get_paint_tile_group_char(Tileset.tile_paint_group[tile_index]);
            end;
            if Tileset.block_preset_coverage[tile_index] = 1 then
              color := color or $00A000
            else if Tileset.block_preset_coverage[tile_index] > 1 then
              color := color or $0000D0;
          end else
            mark_tile := false;
        end else
        if view_mode = vmEditTileHintText then
        begin
          if Tileset.tile_hint_text[tile_index] <> -1 then
          begin
            mark_tile := true;
            color := Settings.GridColor;
            tile_text := inttostr(Tileset.tile_hint_text[tile_index]);
          end else
            mark_tile := false;
        end;
        // Mark tile with color
        if mark_tile then
        begin
          TilesetImage.Canvas.Pen.Color := color;
          TilesetImage.Canvas.Rectangle(x*32+1, y*32+1, x*32+31, y*32+31);
          TilesetImage.Canvas.Rectangle(x*32+2, y*32+2, x*32+30, y*32+30);
          // Draw editor attribute color
          if cbDrawEditorAttributes.Checked and (color_editor <> 0) then
          begin
            TilesetImage.Canvas.Pen.Color := color_editor;
            TilesetImage.Canvas.Rectangle(x*32+14, y*32+14, x*32+18, y*32+18);
            TilesetImage.Canvas.Rectangle(x*32+15, y*32+15, x*32+17, y*32+17);
          end;
          //Draw text (i.e. number)
          if tile_text <> '' then
            TilesetImage.Canvas.TextOut(x*32 + (32 - TilesetImage.Canvas.TextWidth(tile_text)) div 2, y*32+10, tile_text);
        end;
        // Highlight selected tile
        if (active_tile = tile_index) and cbMarkSelection.Checked then
        begin
          TilesetImage.Canvas.Brush.Style := bsClear;
          TilesetImage.Canvas.Pen.Width := 2;
          TilesetImage.Canvas.Pen.Color := $FF0000;
          TilesetImage.Canvas.Rectangle(x*32+1, y*32+1, x*32+32, y*32+32);
          TilesetImage.Canvas.Pen.Width := 1;
        end;
      end;
    // Mark selection
    if select_started then
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
  end;
end;

procedure TTileAtrEditor.do_undo;
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

procedure TTileAtrEditor.do_redo;
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

procedure TTileAtrEditor.reset_undo_history;
begin
  Undo1.Enabled := false;
  Redo1.Enabled := false;
  undo_start := 0;
  undo_max := 0;
  undo_pos := 0;
end;

procedure TTileAtrEditor.set_tile_attribute_list(value, not_value: int64);
var
  i: integer;
  v: int64;
begin
  for i := 0 to 31 do
  begin
    if (value and (1 shl i)) <> 0 then
      TileAtrList.State[i] := cbChecked
    else if (not_value and (1 shl i)) <> 0 then
      TileAtrList.State[i] := cbGrayed
    else
      TileAtrList.State[i] := cbUnchecked;
  end;
  for i := 0 to 7 do
  begin
    v := 1;
    v := v shl (i+32);
    if (value and v) <> 0 then
      TileAtrListEditor.State[i] := cbChecked
    else if (not_value and v) <> 0 then
      TileAtrListEditor.State[i] := cbGrayed
    else
      TileAtrListEditor.State[i] := cbUnchecked;
  end;
end;

procedure TTileAtrEditor.set_tile_attribute_value(value, not_value: int64);
var
  color, color_editor: cardinal;
begin
  TileAtrValue.Text := IntToHex(value, 10);
  TileAtrNotValue.Text := IntToHex(not_value, 10);
  get_tile_attribute_color(value, color, color_editor);
  TileAtrColor.Color := color;
  if color_editor = 0 then
    TileAtrColorEditor.Color := color
  else
    TileAtrColorEditor.Color := color_editor;
end;

procedure TTileAtrEditor.set_tile_attribute_rule(value, not_value: int64);
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
    Tileset.load_rule(edRule.Text, Addr(rule));
end;

procedure TTileAtrEditor.get_tile_attribute_color(value: int64; var color, color_editor: cardinal);
var
  i: integer;
  v: int64;
begin
  color := $0;
  color_editor := $0;
  for i := 0 to 31 do
    if (value and (1 shl i)) <> 0 then
      color := color or atr_colors_game[i];
  for i := 0 to 7 do
  begin
    v := 1;
    v := v shl (i+32);
    if (value and v) <> 0 then
      color_editor := color_editor or atr_colors_editor[i];
  end;
end;

procedure TTileAtrEditor.set_tile_attributes(tile_index: integer; single_op: boolean);
var
  selected_value: int64;
  current_value: int64;
  target_value: int64;
  operation: SetOperation;
begin
  selected_value := strtoint64('$'+TileAtrValue.Text);
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

procedure TTileAtrEditor.tileset_changed;
var
  i: integer;
begin
  StatusBar.Panels[2].Text := Tileset.tileimage_filename;
  StatusBar.Panels[3].Text := Tileset.tileatr_filename;
  StatusBar.Panels[4].Text := Tileset.config_filename;
  if Tileset.current_tileset <> -1 then
  begin
    menuitems[Tileset.current_tileset].Checked := true;
    StatusBar.Panels[1].Text := Tileset.tileset_name;
  end else
  begin
    StatusBar.Panels[1].Text := 'Custom files';
    for i := 0 to Length(menuitems) -1 do
      menuitems[i].Checked := false;
  end;
  TileAtrListEditor.Enabled := Tileset.config_filename <> '';
  if not TileAtrListEditor.Enabled then
  begin
    for i := 0 to 7 do
      TileAtrListEditor.State[i] := cbUnchecked;
    TileAtrListClickCheck(nil);
  end;
  btnConvertEditorAttributes.Enabled := Tileset.config_filename <> '';
  active_tile := -1;
  reset_undo_history;
  render_tileset;
end;

function TTileAtrEditor.confirm_overwrite_original_file(filename: string): boolean;
var
  f: string;
  modified_date: TDateTime;
  year, month, day: word;
  dialog_result: integer;
begin
  result := true;
  if AnsiCompareText(ExtractFilePath(filename), Settings.GamePath + '\DATA\BIN\') <> 0 then
    exit;
  f := UpperCase(ExtractFileName(filename));
  if (f <> 'TILEATR1.BIN') and (f <> 'TILEATR2.BIN') and (f <> 'TILEATR3.BIN') and (f <> 'TILEATR4.BIN') and (f <> 'TILEATR5.BIN') and (f <> 'TILEATR6.BIN') and (f <> 'TILEATR7.BIN') then
    exit;
  modified_date := FileDateToDateTime(FileAge(filename));
  DecodeDate(modified_date, year, month, day);
  if year <> 1998 then
    exit;
  // At this point we know the file is original game's file - show confirmation dialog
  dialog_result := Application.MessageBox(
    PChar(
    'You are about to modify an original Dune 2000 game file, which can break the game!'#13+
    'Make sure you have a backup of file '''+filename+''' before overwriting it!'#13#13+
    'Do you really want to save over the file?'),
    'Save Tile Attributes Warning', MB_YESNO or MB_ICONWARNING);
  result := dialog_result = IDYES;
end;

end.
