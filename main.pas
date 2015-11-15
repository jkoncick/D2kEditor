unit main;

interface

uses
  // System libraries
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, Menus, StdCtrls, XPMan, Math, Spin, Buttons,
  ShellApi, IniFiles, Clipbrd,
  // Dialogs
  set_dialog, tileset_dialog, block_preset_dialog, test_map_dialog, event_dialog, mission_dialog, map_stats_dialog,
  // Units
  _renderer, _map, _mission, _tileset, _structures, _stringtable, _settings;

const brush_size_presets: array[0..7,1..2] of word = ((1,1),(2,2),(3,3),(4,4),(2,1),(1,2),(3,2),(2,3));

type
  SelectedMode = (mStructures, mStructuresPaint, mTerrain, mPaintMode, mBlockMode, mSelectMode);

type
  TBlockClipboard = record
    block_width: word;
    block_height: word;
    block_data: TMapData;
  end;

type
  TMainWindow = class(TForm)
    MapCanvas: TImage;
    MapScrollH: TScrollBar;
    MapScrollV: TScrollBar;
    AppMenu: TMainMenu;
    File1: TMenuItem;
    StatusBar: TStatusBar;
    EditorMenu: TPanel;
    Openmap1: TMenuItem;
    MapOpenDialog: TOpenDialog;
    Savemap1: TMenuItem;
    ileset1: TMenuItem;
    Loadtileset1: TMenuItem;
    TilesetOpenDialog: TOpenDialog;
    MapSaveDialog: TSaveDialog;
    Selecttileset1: TMenuItem;
    MiniMap: TImage;
    Settings1: TMenuItem;
    ShowGrid1: TMenuItem;
    XPManifest1: TXPManifest;
    Reopenmap1: TMenuItem;
    N1: TMenuItem;
    Savemapas1: TMenuItem;
    N2: TMenuItem;
    Exit1: TMenuItem;
    Selectnext1: TMenuItem;
    N3: TMenuItem;
    Map1: TMenuItem;
    Setmapsize1: TMenuItem;
    Shiftmap1: TMenuItem;
    Changestructureowner1: TMenuItem;
    Fastrendering1: TMenuItem;
    EditorPages: TPageControl;
    PageStructures: TTabSheet;
    PageTerrain: TTabSheet;
    LbStructureValue: TLabel;
    SpecialValue: TEdit;
    LbMiscObjList: TLabel;
    MiscObjList: TListBox;
    LbPlayerSelect: TLabel;
    PlayerSelect: TComboBox;
    LbBuildingList: TLabel;
    BuildingList: TListBox;
    RbBlockMode: TRadioButton;
    RbPaintMode: TRadioButton;
    LbBrushSize: TLabel;
    OpenTileset: TButton;
    BlockImage: TImage;
    Newmap1: TMenuItem;
    N4: TMenuItem;
    Help1: TMenuItem;
    KeyShortcuts1: TMenuItem;
    About1: TMenuItem;
    Mouseactions1: TMenuItem;
    N5: TMenuItem;
    MiniMapFrame: TBevel;
    Marktiles1: TMenuItem;
    Useallocationindexes1: TMenuItem;
    N6: TMenuItem;
    N7: TMenuItem;
    Showeventmarkers1: TMenuItem;
    Savemapimage1: TMenuItem;
    N8: TMenuItem;
    CursorImage: TImage;
    RbSelectMode: TRadioButton;
    MapImageSaveDialog: TSaveDialog;
    CbSelectStructures: TCheckBox;
    Edit1: TMenuItem;
    Undo1: TMenuItem;
    Redo1: TMenuItem;
    Showunknownspecials1: TMenuItem;
    Launchgame1: TMenuItem;
    Quicklaunch1: TMenuItem;
    Launchwithsettings1: TMenuItem;
    TileatrOpenDialog: TOpenDialog;
    Loadtilesetattributes1: TMenuItem;
    N10: TMenuItem;
    Copy1: TMenuItem;
    Paste1: TMenuItem;
    Mission1: TMenuItem;
    EventsandConditions1: TMenuItem;
    Missionsettings1: TMenuItem;
    N9: TMenuItem;
    Assignmisfile1: TMenuItem;
    Markdefenceareas1: TMenuItem;
    BlockFrame: TBevel;
    Bevel1: TBevel;
    Bevel2: TBevel;
    cbBrushSize: TComboBox;
    sbThinSpice: TSpeedButton;
    sbThickSpice: TSpeedButton;
    LbPaintTileGroupName: TLabel;
    Markbuildabletiles1: TMenuItem;
    UnitList: TListBox;
    LbUnitList: TLabel;
    Drawconcrete1: TMenuItem;
    N11: TMenuItem;
    Showmapstatistics1: TMenuItem;
    // Main form events
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormResize(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
    procedure CMDialogKey(var AMessage: TCMDialogKey); message CM_DIALOGKEY;
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    procedure FormMouseWheelDown(Sender: TObject; Shift: TShiftState;
      MousePos: TPoint; var Handled: Boolean);
    // Main menu events
    procedure Newmap1Click(Sender: TObject);
    procedure Openmap1Click(Sender: TObject);
    procedure Reopenmap1Click(Sender: TObject);
    procedure Savemap1Click(Sender: TObject);
    procedure Savemapas1Click(Sender: TObject);
    procedure Savemapimage1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Undo1Click(Sender: TObject);
    procedure Redo1Click(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure Paste1Click(Sender: TObject);
    procedure SelectTileset(Sender: TObject);
    procedure Selectnext1Click(Sender: TObject);
    procedure Loadtileset1Click(Sender: TObject);
    procedure Loadtilesetattributes1Click(Sender: TObject);
    procedure SettingChange(Sender: TObject);
    procedure Useallocationindexes1Click(Sender: TObject);
    procedure Setmapsize1Click(Sender: TObject);
    procedure Shiftmap1Click(Sender: TObject);
    procedure Changestructureowner1Click(Sender: TObject);
    procedure Showmapstatistics1Click(Sender: TObject);
    procedure EventsandConditions1Click(Sender: TObject);
    procedure Missionsettings1Click(Sender: TObject);
    procedure Assignmisfile1Click(Sender: TObject);
    procedure Quicklaunch1Click(Sender: TObject);
    procedure Launchwithsettings1Click(Sender: TObject);
    procedure KeyShortcuts1Click(Sender: TObject);
    procedure Mouseactions1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    // Main form components events
    procedure MapScrollHChange(Sender: TObject);
    procedure MapScrollVChange(Sender: TObject);
    procedure MapScrollHKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure MapScrollVKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure MapCanvasMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure MapCanvasMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure CursorImageMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure CursorImageMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MapCanvasDblClick(Sender: TObject);
    procedure MapCanvasMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    // Editor menu components events
    procedure MiniMapMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MiniMapMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    // Structure/terrain editor
    procedure EditorPagesChange(Sender: TObject);
    procedure BuildingListClick(Sender: TObject);
    procedure UnitListClick(Sender: TObject);
    procedure PlayerSelectChange(Sender: TObject);
    procedure MiscObjListClick(Sender: TObject);
    procedure OpenTilesetClick(Sender: TObject);
    procedure BlockImageClick(Sender: TObject);
    procedure RbTerrainModeClick(Sender: TObject);
    procedure PaintTileSelectClick(Sender: TObject);
    procedure BlockPresetGroupSelectClick(Sender: TObject);

  public

    // Map canvas variables
    map_canvas_width: word;
    map_canvas_height: word;
    map_canvas_left: word;
    map_canvas_top: word;

    // Mouse related variables
    mouse_old_x: word;
    mouse_old_y: word;
    mouse_already_clicked: boolean;
    mouse_last_button: TMouseButton;

    // Minimap variables
    mmap_border_x: word;
    mmap_border_y: word;
    minimap_buffer: TBitmap;

    // Terrain editing variables
    paint_tile_group: integer;
    paint_tile_select: array[-2..cnt_paint_tile_groups-1] of TSpeedButton;
    block_preset_group: integer;
    block_preset_select: array[0..cnt_block_preset_groups-1] of TSpeedButton;
    block_preset_dialog_opened: boolean;

    // Tile block variables
    block_width: word;
    block_height: word;
    block_data: TMapData;
    block_select_started: boolean;
    block_select_start_x: word;
    block_select_start_y: word;
    block_select_end_x: word;
    block_select_end_y: word;

    // Event marker moving variables
    moving_disable: boolean;
    moving_event: integer;
    moving_condition: integer;

    // Clipboard variables
    clipboard_format: cardinal;

    // Rendering procedures
    procedure resize_map_canvas;
    procedure render_map;
    procedure render_minimap;
    procedure render_minimap_position_marker;
    procedure render_selection_marker;
    procedure render_tileset;

    // Map loading & saving procedures
    procedure load_map(filename: String);
    procedure save_map(filename: String);
    procedure unload_mission;
    function check_map_errors: boolean;    
    procedure set_window_titles(map_name: String);

    // Map testing procedures
    function check_map_can_be_tested: boolean;
    procedure launch_game;

    // Miscellaneous helper procedures
    procedure set_special_value;
    function mode(m: SelectedMode): boolean;
    procedure apply_key_preset(key: word);
    procedure show_power_and_statistics;
    procedure start_event_position_selection(x, y: integer);
    procedure finish_event_position_selection(x, y: integer);
    procedure draw_paint_tile_select_glyph(target: integer; tile_index: integer; source_canvas: TCanvas);

    // Procedures related to selecting/placing block
    procedure select_block_from_tileset(b_width, b_height, b_left, b_top: word; custom_block_index: integer);
    procedure copy_block_from_map(b_width, b_height, b_left, b_top: word; structures: boolean);

    // Procedures related to cursor image
    procedure resize_cursor_image;
    procedure set_cursor_image_visibility;
    procedure draw_cursor_image;

    // Procedures called from other dialog
    procedure set_map_size(new_width, new_height: integer);
    procedure shift_map(direction, num_tiles: integer);
    procedure change_structure_owner(player_from, player_to: integer; swap: boolean);
    procedure new_map(new_width, new_height: integer);
  end;

var
  current_dir: String;
  MainWindow: TMainWindow;

implementation

{$R *.dfm}

procedure TMainWindow.FormCreate(Sender: TObject);
var
  i: integer;
  btn: TSpeedButton;
begin
  // Miscellaneous initializations
  randomize;
  current_dir := ExtractFilePath(Application.ExeName);
  Application.HintPause := 100;
  Application.HintHidePause:= 100000;
  DragAcceptFiles(Handle, True);
  clipboard_format := RegisterClipboardFormat('D2kEditorBlock');
  moving_event := -1;
  moving_condition := -1;
  top := 60;
  // Initialize terrain editing controls
  for i := 0 to Length(brush_size_presets) - 1 do
    cbBrushSize.Items.Add(inttostr(brush_size_presets[i,1]) + ' x ' + inttostr(brush_size_presets[i,2]));
  cbBrushSize.ItemIndex := 0;
  sbThinSpice.Glyph.Width := 28;
  sbThinSpice.Glyph.Height := 28;
  sbThickSpice.Glyph.Width := 28;
  sbThickSpice.Glyph.Height := 28;
  paint_tile_select[-1] := sbThinSpice;
  paint_tile_select[-2] := sbThickSpice;
  for i := 0 to cnt_paint_tile_groups-1 do
  begin
    btn := TSpeedButton.Create(self);
    btn.Tag := i;
    btn.GroupIndex := 1;
    btn.Top := 70;
    btn.Left := 2 + i * 38;
    btn.Width := 38;
    btn.Height := 38;
    btn.Glyph.Width := 28;
    btn.Glyph.Height := 28;
    btn.ShowHint := True;
    btn.AllowAllUp := True;
    btn.OnClick := PaintTileSelectClick;
    btn.Parent := PageTerrain;
    paint_tile_select[i] := btn;
  end;
  for i := 0 to cnt_block_preset_groups-1 do
  begin
    btn := TSpeedButton.Create(self);
    btn.Tag := i;
    btn.GroupIndex := 2;
    btn.Top := 320 + 20 * (i mod (cnt_block_preset_groups div 2));
    btn.Left := 2 + 76 * (i div (cnt_block_preset_groups div 2));
    btn.Width := 76;
    btn.Height := 20;
    btn.OnClick := BlockPresetGroupSelectClick;
    btn.OnDblClick := BlockImageClick;
    btn.Parent := PageTerrain;
    block_preset_select[i] := btn;
  end;
  block_preset_select[0].Down := True;
  // Load settings
  Settings.load_precreate_editor_settings;
  // Initialize structures
  Structures.init;
  // Initialize mission
  Mission.init;
  // Load string table
  StringTable.load_from_file(Settings.TextUIBPath);
  // Load and initialize graphics
  Renderer.init;
  minimap_buffer := TBitmap.Create;
  minimap_buffer.Width := 128;
  minimap_buffer.Height := 128;
  draw_cursor_image;
  // Initialize tilesets
  Tileset.init;
  // Initialize Structures
  for i := 0 to Structures.cnt_misc_objects - 1 do
    MiscObjList.Items.Add(Structures.misc_object_info[i].name);
  for i := 0 to Structures.cnt_map_players - 1 do
    PlayerSelect.Items.Add(inttostr(i) + ' - '+ Structures.map_player_info[i].name);
  PlayerSelect.ItemIndex := 0;
  for i := 0 to Structures.cnt_structures - 1 do
    if i < Structures.first_unit_index then
      BuildingList.Items.Add(Structures.structure_info[i].name)
    else
      UnitList.Items.Add(Structures.structure_info[i].name);
end;

procedure TMainWindow.FormDestroy(Sender: TObject);
begin
  DragAcceptFiles(Handle, False);
end;

procedure TMainWindow.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  if Map.loaded and Settings.AlwaysAskOnQuit then
  begin
    if Application.MessageBox('Do you really want to quit?','D2kEditor', MB_YESNO or MB_ICONQUESTION) = IDNO then
    begin
      Action := caNone;
      exit;
    end;
  end;
  Settings.save_editor_settings;
end;

procedure TMainWindow.FormResize(Sender: TObject);
var
  tmp_height: integer;
begin
  resize_map_canvas;
  EditorMenu.Left := ClientWidth - 168;
  EditorMenu.Height := ClientHeight - StatusBar.Height;
  tmp_height := EditorMenu.Height - 376;
  BuildingList.Height := tmp_height div 2;
  UnitList.Height := tmp_height div 2;
  LbUnitList.Top := BuildingList.Top + BuildingList.Height + 3;
  UnitList.Top := LbUnitList.Top + 16;
  EditorPages.Height := EditorMenu.Height - 146;
  StatusBar.Panels[3].Width := ClientWidth - 550;
  if Map.loaded then
  begin
    render_minimap_position_marker;
    render_map;
  end;
end;

procedure TMainWindow.FormDeactivate(Sender: TObject);
begin
  Undo1.ShortCut := 0;
  Redo1.ShortCut := 0;
  Copy1.ShortCut := 0;
  Paste1.ShortCut := 0;
end;

procedure TMainWindow.FormActivate(Sender: TObject);
begin
  Undo1.ShortCut := 16474;
  Redo1.ShortCut := 16473;
  Copy1.ShortCut := 16451;
  Paste1.ShortCut := 16470;
end;

procedure TMainWindow.WMDropFiles(var Msg: TWMDropFiles);
var
  filename: string;
  length: integer;
begin
  length := DragQueryFile(Msg.Drop, 0, nil, 0);
  setlength(filename, length);
  DragQueryFile(Msg.Drop, 0, PChar(filename), length + 1);
  load_map(filename);
  DragFinish(Msg.Drop);
end;

procedure TMainWindow.CMDialogKey(var AMessage: TCMDialogKey);
begin
  if AMessage.CharCode = VK_TAB then
  begin
    EditorPages.TabIndex := (EditorPages.TabIndex + 1) and 1;
    EditorPagesChange(nil);
    AMessage.Result := 1;
  end else
    inherited;
end;

procedure TMainWindow.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  index: integer;
begin
  case key of
    // Esc: cancel event position selection
    27:
    begin
      if not EditorPages.Visible then
        finish_event_position_selection(-1,-1);
    end;
    // Space: open tileset window
    32:
    begin
      EditorPages.TabIndex := 1;
      if block_preset_dialog_opened then
        BlockPresetDialog.Show
      else
        TilesetDialog.Show;
      key := 0;
      exit;
    end;
    // Paint sand/rock/dunes
    192: begin
      cbBrushSize.ItemIndex := 0;
      paint_tile_select[Tileset.block_preset_groups[block_preset_group].paint_group].Down := true;
      PaintTileSelectClick(paint_tile_select[Tileset.block_preset_groups[block_preset_group].paint_group]);
    end;
  end;
  if mode(mBlockMode) then
  case key of
    // Move cursor image
    98:  {Num2} begin CursorImage.Top := CursorImage.Top + 32; resize_cursor_image; end;
    100: {Num4} begin CursorImage.Left := CursorImage.Left - 32; resize_cursor_image; end;
    102: {Num6} begin CursorImage.Left := CursorImage.Left + 32; resize_cursor_image; end;
    104: {Num8} begin CursorImage.Top := CursorImage.Top - 32; resize_cursor_image; end;
    end
  else if mode(mStructures) and (ActiveControl <> SpecialValue) and (key >= 96) and (key <= 96 + Structures.cnt_map_players) then
  begin
    // Select player
    PlayerSelect.ItemIndex := key - 96;
    PlayerSelectChange(nil);
  end;
  // F1-F4 - Select block preset group
  if (key >= 112) and (key <= 115) then
  begin
    index := key - 112;
    if block_preset_group = index then
      index := index + 4;
    if not block_preset_select[index].Enabled then
      exit;
    block_preset_select[index].Down := true;
    BlockPresetGroupSelectClick(block_preset_select[index]);
  end;
  // Shift+key
  if ssShift in Shift then
  begin
    // Brush size preset selection
    if (key >= ord('1')) and (key <= ord('8')) then
      cbBrushSize.ItemIndex := key - ord('1');
    case key of
    // Structures editing mode selection
    ord('E'): begin MiscObjList.ItemIndex := 3; MiscObjListClick(nil); EditorPages.TabIndex := 0; EditorPages.SetFocus; end;
    // Terrain editing mode selection
    ord('Q'): begin EditorPages.TabIndex := 1; sbThinSpice.Down := true; PaintTileSelectClick(sbThinSpice); end;
    ord('W'): begin EditorPages.TabIndex := 1; sbThickSpice.Down := true; PaintTileSelectClick(sbThickSpice);  end;
    ord('S'): begin EditorPages.TabIndex := 1; paint_tile_select[0].Down := true; PaintTileSelectClick(paint_tile_select[0]); end;
    ord('R'): begin EditorPages.TabIndex := 1; paint_tile_select[1].Down := true; PaintTileSelectClick(paint_tile_select[1]); end;
    ord('D'): begin EditorPages.TabIndex := 1; paint_tile_select[2].Down := true; PaintTileSelectClick(paint_tile_select[2]); end;
    ord('C'): begin EditorPages.TabIndex := 1; RbSelectMode.Checked := true; end;
    ord('T'): CbSelectStructures.Checked := not CbSelectStructures.Checked;
    ord('B'): begin EditorPages.TabIndex := 1; RbBlockMode.Checked := true; end;
    end;
  end else
  if mode(mTerrain) then
  begin
    // Block key presets
    if ((key >= ord('0')) and (key <= ord('9'))) or ((key >= ord('A')) and (key <= ord('Z'))) or (key = 186) or (key = 188) or (key = 190) or (key = 191) then
      apply_key_preset(key);
  end;
end;

procedure TMainWindow.FormMouseWheelUp(Sender: TObject; Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  if (MousePos.X - left) < (mapCanvas.Width + 30) then
  begin
    if (MousePos.Y - top) < (mapCanvas.Height - 30)
    then
      MapScrollV.Position := MapScrollV.Position - 2
    else
      MapScrollH.Position := MapScrollH.Position - 2;
    Handled := true;
  end;
end;

procedure TMainWindow.FormMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if (MousePos.X - left) < (mapCanvas.Width + 30) then
  begin
    if (MousePos.Y - top) < (mapCanvas.Height - 30)
    then
      MapScrollV.Position := MapScrollV.Position + 2
    else
      MapScrollH.Position := MapScrollH.Position + 2;
    Handled := true;
  end;
end;

procedure TMainWindow.Newmap1Click(Sender: TObject);
begin
  SetDialog.select_menu(4);
end;

procedure TMainWindow.Openmap1Click(Sender: TObject);
begin
  if MapOpenDialog.Execute then
  begin
    load_map(MapOpenDialog.FileName);
  end;
end;


procedure TMainWindow.Reopenmap1Click(Sender: TObject);
begin
  if Map.loaded and (Map.filename <> '') then
    load_map(Map.filename);
end;

procedure TMainWindow.Savemap1Click(Sender: TObject);
begin
  if not Map.loaded then
    exit;
  if Map.filename = '' then
    Savemapas1Click(Sender)
  else begin
    if Settings.CheckMapErrorsOnSave then
      check_map_errors;
    save_map(Map.filename);
  end;
end;

procedure TMainWindow.Savemapas1Click(Sender: TObject);
begin
  if not Map.loaded then
    exit;
  if Settings.CheckMapErrorsOnSave then
    check_map_errors;
  if MapSaveDialog.Execute then
  begin
    if Map.filename <> MapSaveDialog.FileName then
    begin
      Map.filename := MapSaveDialog.FileName;
      if Mission.mis_assigned then
        Mission.mis_filename := Mission.get_mis_filename(Map.filename);
      StatusBar.Panels[3].Text := MapSaveDialog.FileName;
      Settings.get_file_paths_from_map_filename;
      set_window_titles(ChangeFileExt(ExtractFileName(Map.filename),''));
    end;
    save_map(MapSaveDialog.FileName);
  end;
end;

procedure TMainWindow.Savemapimage1Click(Sender: TObject);
var
  tmp_bitmap: TBitmap;
begin
  if not Map.loaded then
    exit;
  if MapImageSaveDialog.Execute then
  begin
    tmp_bitmap := TBitmap.Create;
    tmp_bitmap.Width := Map.width * 32;
    tmp_bitmap.Height := Map.height * 32;
    Renderer.render_map_contents(tmp_bitmap.Canvas, 0, 0, Map.width, Map.height, Addr(Map.data), Map.width, Map.height,
      ShowGrid1.Checked, Drawconcrete1.Checked, Marktiles1.Checked, Markbuildabletiles1.Checked, Showunknownspecials1.Checked,
      Useallocationindexes1.Checked, Showeventmarkers1.Checked, Markdefenceareas1.Checked, false);
    tmp_bitmap.SaveToFile(MapImageSaveDialog.FileName);
    tmp_bitmap.Destroy;
  end;
end;

procedure TMainWindow.Exit1Click(Sender: TObject);
begin
  application.Terminate;
end;

procedure TMainWindow.Undo1Click(Sender: TObject);
begin
  Map.do_undo;
  render_minimap;
  render_map;
  mouse_already_clicked := false;
end;

procedure TMainWindow.Redo1Click(Sender: TObject);
begin
  Map.do_redo;
  render_minimap;
  render_map;
  mouse_already_clicked := false;
end;

procedure TMainWindow.Copy1Click(Sender: TObject);
var
  handle: THandle;
  pointer: ^TBlockClipboard;
begin
  if not Map.loaded or not mode(mBlockMode) then
    exit;
  OpenClipboard(Application.Handle);
  EmptyClipboard;

  handle := GlobalAlloc(GMEM_DDESHARE or GMEM_MOVEABLE, SizeOf(TBlockClipboard));
  pointer := GlobalLock(handle);

  pointer.block_width := block_width;
  pointer.block_height := block_height;
  Move(block_data, pointer.block_data, sizeof(block_data));

  GlobalUnLock(handle);
  SetClipboardData(clipboard_format, handle);
  CloseClipboard;
end;

procedure TMainWindow.Paste1Click(Sender: TObject);
var
  handle: THandle;
  pointer: ^TBlockClipboard;
begin
  if not Map.loaded or not Clipboard.HasFormat(clipboard_format) then
    exit;
  OpenClipboard(Application.Handle);
  handle := GetClipboardData(clipboard_format);
  pointer := GlobalLock(handle);

  block_width := pointer.block_width;
  block_height := pointer.block_height;
  Move(pointer.block_data, block_data, sizeof(block_data));

  RbBlockMode.Checked := true;
  EditorPages.TabIndex := 1;
  draw_cursor_image;

  GlobalUnLock(handle);
  CloseClipboard;
end;

procedure TMainWindow.SelectTileset(Sender: TObject);
begin
  Tileset.change_tileset((sender as TMenuItem).Tag);
  // Re-render everything
  render_minimap;
  render_map;
end;

procedure TMainWindow.Selectnext1Click(Sender: TObject);
begin
  Tileset.next_tileset;
  // Re-render everything
  render_minimap;
  render_map;
end;

procedure TMainWindow.Loadtileset1Click(Sender: TObject);
begin
  if TilesetOpenDialog.Execute then
  begin
    Tileset.use_custom_image(TilesetOpenDialog.FileName);
    // Re-render everything
    render_map;
  end;
end;

procedure TMainWindow.Loadtilesetattributes1Click(Sender: TObject);
begin
  if TileatrOpenDialog.Execute then
  begin
    Tileset.load_attributes(TileatrOpenDialog.FileName);
    // Re-render everything
    render_minimap;
    render_map;
  end;
end;

procedure TMainWindow.SettingChange(Sender: TObject);
begin
  render_map;
end;

procedure TMainWindow.Useallocationindexes1Click(Sender: TObject);
begin
  Useallocationindexes1.Checked := not Useallocationindexes1.Checked;
  render_minimap;
  render_map;
end;

procedure TMainWindow.Setmapsize1Click(Sender: TObject);
begin
  SetDialog.SetMapSize_Width.Value := Map.width;
  SetDialog.SetMapSize_Height.Value := Map.height;
  SetDialog.select_menu(1);
end;

procedure TMainWindow.Shiftmap1Click(Sender: TObject);
begin
  SetDialog.select_menu(2);
end;

procedure TMainWindow.Changestructureowner1Click(Sender: TObject);
begin
  SetDialog.select_menu(3);
end;

procedure TMainWindow.Showmapstatistics1Click(Sender: TObject);
begin
  MapStatsDialog.update_stats;
  MapStatsDialog.Show;
end;

procedure TMainWindow.EventsandConditions1Click(Sender: TObject);
begin
  if not Mission.mis_assigned then
  begin
    Application.MessageBox('No mission file is assigned to this map.', 'Error', MB_ICONERROR);
    exit;
  end;
  EventDialog.Show;
end;

procedure TMainWindow.Missionsettings1Click(Sender: TObject);
begin
  if not Mission.mis_assigned then
  begin
    Application.MessageBox('No mission file is assigned to this map.', 'Error', MB_ICONERROR);
    exit;
  end;
  MissionDialog.Show;
end;

procedure TMainWindow.Assignmisfile1Click(Sender: TObject);
begin
  if not Map.loaded then
    exit;
  if not Mission.mis_assigned then
  begin
    Mission.mis_assigned := true;
    Mission.mis_filename := Mission.get_mis_filename(Map.filename);
    if FileExists(Mission.mis_filename) then
      Mission.load_mis_file(Mission.mis_filename);
    StatusBar.Panels[4].Text := 'MIS';
    Assignmisfile1.Caption := 'Unassign .mis file';
    MissionDialog.fill_data;
    EventDialog.update_contents;
  end else
    unload_mission;
  render_map;
end;

procedure TMainWindow.Quicklaunch1Click(Sender: TObject);
begin
  if not check_map_can_be_tested then
    exit;
  launch_game;
end;

procedure TMainWindow.Launchwithsettings1Click(Sender: TObject);
begin
  if not check_map_can_be_tested then
    exit;
  TestMapDialog.invoke;
end;

procedure TMainWindow.KeyShortcuts1Click(Sender: TObject);
begin
  ShowMessage('Key Shortcuts:'#13#13'Space = Open tileset window'#13'Tab = Switch Structures / Terrain'#13'Shift + 1 - 8 = Block size preset'#13+
              'Shift + Q = Thin spice'#13'Shift + W = Thick spice'#13'Shift + E = Spice bloom'#13'Shift + S = Paint sand'#13'Shift + R = Paint rock'#13'Shift + D = Paint dunes'#13'Shift + B = Tile block'#13'Shift + C = Select and copy mode'#13'Shift + T = Select structures'#13+
              'F1 - F4 = Block key-preset group'#13'Num 2,4,6,8 = Move block on map'#13'Num 0 - Num 6 = Select player');
end;

procedure TMainWindow.Mouseactions1Click(Sender: TObject);
begin
  ShowMessage('Mouse actions'#13#13+
              'In Structures mode:'#13'Left = Place structure'#13'Right = Remove structure'#13'Middle = Copy structure'#13#13+
              'Event marker in Struct. mode:'#13'Double click = Go to event'#13'Left+drag = Move event'#13#13+
              'In Terrain mode:'#13'Left = Paint / Place block'#13'Double click = Fill area'#13'Shift+click = Smooth edge'#13'Middle = Copy block'#13'Right+drag = Scroll map');
end;

procedure TMainWindow.About1Click(Sender: TObject);
begin
  ShowMessage('Dune 2000 Map and Mission Editor'#13#13'Part of D2K+ Editing tools'#13#13+
              'Made by Klofkac'#13'Version 1.1'#13'Date: 2015-10-16'#13#13+
              'http://github.com/jkoncick/D2kEditor'#13#13+
              'Special thanks to:'#13'mvi - for making the original Mission editor'#13'FunkyFr3sh - for patching Dune 2000'#13'FED2k community - for their support');
end;

procedure TMainWindow.MapScrollHChange(Sender: TObject);
begin
  map_canvas_left := MapScrollH.Position;
  render_map;
  render_minimap_position_marker;
end;

procedure TMainWindow.MapScrollVChange(Sender: TObject);
begin
  map_canvas_top := MapScrollV.Position;
  render_map;
  render_minimap_position_marker;
end;

procedure TMainWindow.MapScrollHKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  // Keyboard control of map shifting
  if key = 38 then
  begin
    key := 0;
    MapScrollV.Position := MapScrollV.Position-1;
  end;
  if key = 40 then
  begin
    key := 0;
    MapScrollV.Position := MapScrollV.Position+1;
  end;
end;

procedure TMainWindow.MapScrollVKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  // Keyboard control of map shifting
  if key = 37 then
  begin
    MapScrollH.Position := MapScrollH.Position-1;
    key := 0;
  end;
  if key = 39 then
  begin
    MapScrollH.Position := MapScrollH.Position+1;
    key := 0;
  end;
end;

procedure TMainWindow.MapCanvasMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
canvas_x, canvas_y: integer;
map_x, map_y: integer;
eventnum: integer;
numunits: integer;
i: integer;
tmp_hint: string;
begin
  // Get tile coordinates
  canvas_x := X div 32;
  canvas_y := Y div 32;
  map_x := canvas_x + map_canvas_left;
  map_y := canvas_y + map_canvas_top;
  if map_x < 0 then
    map_x := 0;
  if map_x >= Map.width then
    map_x := Map.width - 1;
  if map_y < 0 then
    map_y := 0;
  if map_y >= Map.height then
    map_y := Map.height - 1;
  // Write coordinates on status bar
  StatusBar.Panels[0].Text := 'x: '+inttostr(map_x)+' y: '+inttostr(map_y);
  // Show cursor image if needed
  set_cursor_image_visibility;
  // If mouse is still inside same tile, exit (for optimization)
  if (mouse_old_x = map_x) and (mouse_old_y = map_y) then
    exit;
  mouse_already_clicked := false;
  // If mouse moved over Reinforcement or Spawn event marker, show "hint" with list of units
  if mode(mStructures) and Showeventmarkers1.Checked and ((Mission.event_markers[map_x,map_y].emtype = emReinforcement) or (Mission.event_markers[map_x,map_y].emtype = emUnitSpawn)) then
  begin
    Application.CancelHint;
    eventnum := Mission.event_markers[map_x,map_y].index;
    numunits := Mission.mis_data.events[eventnum].num_units;
    tmp_hint := inttostr(numunits) + ' units:';
    for i := 0 to (numunits -1) do
      tmp_hint := tmp_hint + chr(13) + Mission.unit_names[Mission.mis_data.events[eventnum].units[i]];
    MapCanvas.Hint := tmp_hint;
    MapCanvas.ShowHint := true
  end else
    MapCanvas.ShowHint := false;
  // Scroll map while holding right button
  if mode(mTerrain) and (ssRight in shift) then
  begin
    MapScrollH.Position := map_canvas_left + (mouse_old_x - map_x);
    MapScrollV.Position := map_canvas_top + (mouse_old_y - map_y);
  end else
  begin
    mouse_old_x := map_x;
    mouse_old_y := map_y;
  end;
  // Move event marker
  if (moving_event <> -1) then
  begin
    Mission.mis_data.events[moving_event].map_pos_x := map_x;
    Mission.mis_data.events[moving_event].map_pos_y := map_y;
    Mission.process_event_markers;
    render_map;
    exit;
  end;
  if (moving_condition <> -1) then
  begin
    Mission.mis_data.conditions[moving_condition].map_pos_x := map_x;
    Mission.mis_data.conditions[moving_condition].map_pos_y := map_y;
    Mission.process_event_markers;
    render_map;
    exit;
  end;
  // Move cursor image and resize if exceeding map canvas border
  CursorImage.Left := canvas_x * 32 + MapCanvas.Left;
  CursorImage.Top := canvas_y * 32 + MapCanvas.Top;
  resize_cursor_image;
  // Move end of block selection
  if block_select_started then
  begin
    block_select_end_x := map_x;
    block_select_end_y := map_y;
    render_map;
    render_selection_marker;
  end else
  // If left button is held, paint sand/rock/dunes/spice during mouse move
  if (ssLeft in shift) and (mode(mPaintMode) or mode(mStructuresPaint)) then
    MapCanvasMouseDown(sender,mbLeft,Shift,x,y);
end;

procedure TMainWindow.MapCanvasMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  map_x, map_y: integer;
  index, player: word;
  is_misc: boolean;
  event_marker: ^TEventMarker;
  cursor_left: integer;
  cursor_top: integer;
begin
  map_x := x div 32 + map_canvas_left;
  map_y := y div 32 + map_canvas_top;
  if mouse_already_clicked and (mouse_last_button = Button) then
    exit;
  mouse_already_clicked := true;
  mouse_last_button := Button;
  // Event position selection mode
  if (not EditorPages.Visible) and (Button = mbLeft) then
  begin
    finish_event_position_selection(map_x, map_y);
    exit;
  end;
  // Moving event markers
  if (Mission.event_markers[map_x, map_y].emtype <> emNone) and Showeventmarkers1.Checked and not moving_disable and
    mode(mStructures) and (Button = mbLeft) then
  begin
    event_marker := Addr(Mission.event_markers[map_x, map_y]);
    if event_marker.emtype = emTileRevealed then
      moving_condition := event_marker.index
    else
      moving_event := event_marker.index;
    exit;
  end;
  moving_disable := true;
  // Finally placing/painting structures/terrain
  if mode(mStructures) then
    begin
    // Editing structures
    if Button = mbLeft then
      // Put structure on map
      Map.set_special_value(map_x, map_y, strtoint(SpecialValue.Text))
    else if Button = mbRight then
      // Delete structure from map
      Map.set_special_value(map_x, map_y, 0)
    else if Button = mbMiddle then
    begin
      // Get structure parameters on position and set them in menu
      SpecialValue.text := inttostr(Map.data[map_x, map_y].special);
      if Structures.special_value_to_params(Map.data[map_x, map_y].special, player, index, is_misc) then
      begin
        if is_misc then
        begin
          MiscObjList.ItemIndex := index;
          BuildingList.ItemIndex := -1;
          UnitList.ItemIndex := -1;
        end else
        begin
          MiscObjList.ItemIndex := -1;
          if index < Structures.first_unit_index then
          begin
            BuildingList.ItemIndex := index;
            UnitList.ItemIndex := -1;
          end else
          begin
            BuildingList.ItemIndex := -1;
            UnitList.ItemIndex := index - Structures.first_unit_index;
          end;
          PlayerSelect.ItemIndex := player;
          show_power_and_statistics;
        end;
      end else
      begin
        MiscObjList.ItemIndex := -1;
        BuildingList.ItemIndex := -1;
        UnitList.ItemIndex := -1;
      end;
      exit;
    end;
  end else
  begin
    // Editing terrain
    if button = mbLeft then
    begin
      if mode(mSelectMode) then
      begin
        // Start selection
        block_select_started := true;
        block_select_start_x := map_x;
        block_select_start_y := map_y;
        block_select_end_x := map_x;
        block_select_end_y := map_y;
        render_selection_marker;
        exit;
      end
      else if mode(mBlockMode) then
      begin
        // Draw selected block
        cursor_left := (CursorImage.Left - MapCanvas.Left) div 32 + map_canvas_left;
        cursor_top := (CursorImage.Top - MapCanvas.Top) div 32 + map_canvas_top;
        Map.put_block(cursor_left, cursor_top, block_width, block_height, Addr(block_data));
      end
      else if (ssShift in Shift) and mode(mPaintMode) and (Tileset.paint_tile_groups[paint_tile_group].smooth_group > -1) then
      begin
        // Smooth rock/dunes edge
        Map.smooth_edges(map_x, map_y, paint_tile_group);
      end
      else if mode(mPaintMode) then
      begin
        // Paint
        Map.paint_rect(map_x, map_y, brush_size_presets[cbBrushSize.ItemIndex,1], brush_size_presets[cbBrushSize.ItemIndex,2], paint_tile_group);
      end;
    end
    else if button = mbMiddle then
    begin
      // Copy selected block
      copy_block_from_map(brush_size_presets[cbBrushSize.ItemIndex,1], brush_size_presets[cbBrushSize.ItemIndex,2], map_x, map_y, false);
      exit;
    end;
  end;
  if not mode(mPaintMode) then
  begin
    Map.calculate_power_and_statistics;
  end;
  render_minimap;
  render_map;
end;

procedure TMainWindow.MapCanvasDblClick(Sender: TObject);
var
  event_marker: ^TEventMarker;
begin
  // Double-click on event marker
  if mode(mStructures) and Showeventmarkers1.Checked and (Mission.event_markers[mouse_old_x, mouse_old_y].emtype <> emNone) then
  begin
    event_marker := Addr(Mission.event_markers[mouse_old_x, mouse_old_y]);
    EventDialog.Show;
    if event_marker.emtype = emTileRevealed then
    begin
      EventDialog.ConditionGrid.Row := event_marker.index + 1;
      EventDialog.ConditionGrid.SetFocus;
    end else
    begin
      EventDialog.EventGrid.Row := event_marker.index + 1;
      EventDialog.EventGrid.SetFocus;
    end;
  end else
  // Double click for filling area
  if mode(mPaintMode) then
  begin
    Map.fill_area_start(mouse_old_x, mouse_old_y, paint_tile_group);
    render_minimap;
    render_map;
  end;
end;

procedure TMainWindow.MapCanvasMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  min_x, max_x, min_y, max_y: word;
begin
  moving_disable := false;
  if (moving_event <> -1) or (moving_condition <> -1) then
  begin
    moving_event := -1;
    moving_condition := -1;
    EventDialog.update_contents;
  end;
  if block_select_started and (Button = mbLeft) then
  begin
    block_select_started := false;
    min_x := Min(block_select_start_x, block_select_end_x);
    max_x := Max(block_select_start_x, block_select_end_x);
    min_y := Min(block_select_start_y, block_select_end_y);
    max_y := Max(block_select_start_y, block_select_end_y);
    copy_block_from_map(max_x - min_x + 1, max_y - min_y + 1, min_x, min_y, true);
    // Erase copied area
    if ssShift in Shift then
    begin
      Map.paint_rect(min_x, min_y, max_x-min_x, max_y-min_y, 0);
      render_minimap;
    end;
    render_map;
  end;
  if mode(mPaintMode) then
  begin
    Map.calculate_power_and_statistics;
  end;
end;

procedure TMainWindow.CursorImageMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  MapCanvasMouseMove(Sender, Shift, X + CursorImage.Left - MapCanvas.Left, Y + CursorImage.Top - MapCanvas.Top);
end;

procedure TMainWindow.CursorImageMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  MapCanvasMousedown(Sender, Button, Shift, X + CursorImage.Left - MapCanvas.Left, Y + CursorImage.Top - MapCanvas.Top);
end;

procedure TMainWindow.FormMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  StatusBar.Panels[0].Text := '';
  CursorImage.Visible := false;
end;

procedure TMainWindow.MiniMapMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not Map.loaded then
    exit;
  if (x < mmap_border_x) or (y < mmap_border_y) or (x > 128 - mmap_border_x) or (y > 128 - mmap_border_y) then
    exit;
  MapScrollH.Position := x - mmap_border_x - (map_canvas_width div 2);
  MapScrollV.Position := y - mmap_border_y - (map_canvas_height div 2);
end;

procedure TMainWindow.MiniMapMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
  if ssLeft in Shift then
    MiniMapMouseDown(Sender, mbLeft, Shift, X, Y);
end;

procedure TMainWindow.EditorPagesChange(Sender: TObject);
begin
  if EditorPages.ActivePageIndex = 0 then
  begin
    if MiscObjList.ItemIndex <> -1 then
      MiscObjList.SetFocus;
    if BuildingList.ItemIndex <> -1 then
      BuildingList.SetFocus;
    if UnitList.ItemIndex <> -1 then
      UnitList.SetFocus;
  end;
end;

procedure TMainWindow.BuildingListClick(Sender: TObject);
begin
  MiscObjList.ItemIndex := -1;
  UnitList.ItemIndex := -1;
  set_special_value;
end;

procedure TMainWindow.UnitListClick(Sender: TObject);
begin
  MiscObjList.ItemIndex := -1;
  BuildingList.ItemIndex := -1;
  set_special_value;
end;

procedure TMainWindow.PlayerSelectChange(Sender: TObject);
begin
  set_special_value;
  show_power_and_statistics;
end;

procedure TMainWindow.MiscObjListClick(Sender: TObject);
begin
  BuildingList.ItemIndex := -1;
  UnitList.ItemIndex := -1;
  set_special_value;
end;

procedure TMainWindow.OpenTilesetClick(Sender: TObject);
begin
  TilesetDialog.Show;
end;

procedure TMainWindow.BlockImageClick(Sender: TObject);
begin
  BlockPresetDialog.Show;
end;

procedure TMainWindow.RbTerrainModeClick(Sender: TObject);
begin
  if sender <> nil then
    (Sender as TRadioButton).SetFocus;
  if not RbPaintMode.Checked then
  begin
    paint_tile_select[paint_tile_group].Down := false;
    LbPaintTileGroupName.Caption := '';
  end;
  set_cursor_image_visibility;
end;

procedure TMainWindow.PaintTileSelectClick(Sender: TObject);
var
  index: integer;
begin
  index := (Sender as TSpeedButton).Tag;
  paint_tile_group := index;
  LbPaintTileGroupName.Caption := (Sender as TSpeedButton).Hint;
  RbPaintMode.Checked := true;
end;

procedure TMainWindow.BlockPresetGroupSelectClick(Sender: TObject);
begin
  block_preset_group := (Sender as TSpeedButton).Tag;
  if BlockPresetDialog.Visible then
    BlockPresetDialog.Show;
  BlockPresetDialog.init_presets;
end;

procedure TMainWindow.resize_map_canvas;
begin
  map_canvas_width := (ClientWidth - 200) div 32;
  if map_canvas_width > Map.width then
    map_canvas_width := Map.width;
  map_canvas_height := (ClientHeight - 50) div 32;
  if map_canvas_height > Map.height then
    map_canvas_height := Map.height;
  MapCanvas.Picture.Bitmap.Width := map_canvas_width * 32;
  MapCanvas.Picture.Bitmap.Height := map_canvas_height * 32;
  MapCanvas.Width := map_canvas_width * 32;
  MapCanvas.Height := map_canvas_height * 32;
  MapScrollH.Top := map_canvas_height * 32 + 8;
  MapScrollH.Width := map_canvas_width * 32;
  MapScrollH.Visible := MapScrollH.Width > 0;
  MapScrollH.Max := Map.width - map_canvas_width;
  if Map.width = map_canvas_width then
    MapScrollH.Enabled := False
  else
    MapScrollH.Enabled := True;
  MapScrollV.Left := map_canvas_width * 32 + 8;
  MapScrollV.Height := map_canvas_height * 32;
  MapScrollV.Visible := MapScrollV.Height > 0;
  MapScrollV.Max := Map.height - map_canvas_height;
  if Map.height = map_canvas_height then
    MapScrollV.Enabled := False
  else
    MapScrollV.Enabled := True;
  mmap_border_x := (128 - Map.width) div 2;
  mmap_border_y := (128 - Map.height) div 2;
end;

procedure TMainWindow.render_map;
begin
  if not Map.loaded then
    exit;
  Renderer.render_map_contents(MapCanvas.Canvas, map_canvas_left, map_canvas_top, map_canvas_width, map_canvas_height, Addr(Map.data), Map.width, Map.height,
    ShowGrid1.Checked, Drawconcrete1.Checked, Marktiles1.Checked, Markbuildabletiles1.Checked, Showunknownspecials1.Checked,
    Useallocationindexes1.Checked, Showeventmarkers1.Checked, Markdefenceareas1.Checked,
    Fastrendering1.Checked);
end;

procedure TMainWindow.render_minimap;
begin
  if not Map.loaded then
    exit;
  Renderer.render_minimap_contents(minimap_buffer.Canvas, Addr(Map.data), Map.width, Map.height, Useallocationindexes1.Checked);
  render_minimap_position_marker;
end;

procedure TMainWindow.render_minimap_position_marker;
begin
  MiniMap.Canvas.CopyRect(rect(0,0,128,128),minimap_buffer.Canvas,rect(0,0,128,128));
  MiniMap.Canvas.Pen.Color:= $00FF00;
  MiniMap.Canvas.Brush.Style := bsClear;
  MiniMap.Canvas.Rectangle(mmap_border_x + map_canvas_left,mmap_border_y + map_canvas_top,mmap_border_x + map_canvas_left + map_canvas_width,mmap_border_y + map_canvas_top + map_canvas_height);
  MiniMap.Canvas.Brush.Style := bsSolid;
end;

procedure TMainWindow.render_selection_marker;
var
  min_x, min_y, max_x, max_y: integer;
begin
  // Draw border around selected block on map
  if block_select_started then
  begin
    min_x := (min(block_select_start_x, block_select_end_x) - map_canvas_left) * 32;
    max_x := (max(block_select_start_x, block_select_end_x) - map_canvas_left) * 32 + 33;
    min_y := (min(block_select_start_y, block_select_end_y) - map_canvas_top) * 32;
    max_y := (max(block_select_start_y, block_select_end_y) - map_canvas_top) * 32 + 33;
    MapCanvas.Canvas.Brush.Style := bsClear;
    MapCanvas.Canvas.Pen.Color := clRed;
    MapCanvas.Canvas.Rectangle(min_x, min_y, max_x, max_y);
    MapCanvas.Canvas.Brush.Style := bsSolid;
  end;
end;

procedure TMainWindow.render_tileset;
begin
  draw_cursor_image;
  if (TilesetDialog <> nil) and TilesetDialog.Visible then
    TilesetDialog.DrawTileset(nil);
  if (BlockPresetDialog <> nil) then
    BlockPresetDialog.init_presets;
end;

procedure TMainWindow.load_map(filename: String);
var
  tmp_mis_filename: String;
begin
  if not FileExists(filename) then
    exit;
  if UpperCase(Copy(filename, Length(filename)-2, 3)) <> 'MAP' then
  begin
    Application.MessageBox('Invalid file type', 'Load map error', MB_ICONERROR);
    exit;
  end;
  // Load map file
  Map.load_map_file(filename);
  // Set status bar
  StatusBar.Panels[3].Text := filename;
  StatusBar.Panels[2].Text := inttostr(Map.width)+' x '+inttostr(Map.height);
  // Initialize settings
  Settings.get_file_paths_from_map_filename;
  Settings.get_map_test_settings;
  // Load mis file
  tmp_mis_filename := Mission.get_mis_filename(Map.filename);
  if FileExists(tmp_mis_filename) then
  begin
    Mission.mis_assigned := true;
    StatusBar.Panels[4].Text := 'MIS';
    Mission.load_mis_file(tmp_mis_filename);
    Assignmisfile1.Caption := 'Unassign .mis file';
    // Update Mission settings and Events and Conditions dialogs.
    // Map's ini file will be loaded along with Mission dialog.
    MissionDialog.fill_data;
    EventDialog.update_contents;
  end else
    unload_mission;
  set_window_titles(ChangeFileExt(ExtractFileName(Map.filename), ''));
  if MapStatsDialog.Visible then
    MapStatsDialog.update_stats;
  // Rendering
  resize_map_canvas;
  render_minimap;
  render_map;
end;

procedure TMainWindow.save_map(filename: String);
begin
  Map.save_map_file(filename);
  if Mission.mis_assigned then
  begin
    Mission.save_mis_file(Mission.get_mis_filename(filename));
    MissionDialog.save_ini_fields(filename);
  end;
end;

procedure TMainWindow.unload_mission;
begin
  Mission.mis_assigned := false;
  Mission.mis_filename := '';
  StatusBar.Panels[4].Text := '';
  Mission.set_default_mis_values;
  Assignmisfile1.Caption := 'Assign .mis file';
  MissionDialog.Close;
  EventDialog.Close;
end;

function TMainWindow.check_map_errors: boolean;
var
  errmsg: String;
begin
  errmsg := Map.check_errors;
  result := true;
  if errmsg <> '' then
  begin
    Application.MessageBox(PChar(errmsg), 'Map error', MB_ICONWARNING);
    result := false;
  end;
end;

procedure TMainWindow.set_window_titles(map_name: String);
begin
  Caption := 'Dune 2000 Map and Mission Editor';
  MissionDialog.Caption := 'Mission settings';
  EventDialog.Caption := 'Events and Conditions';
  if map_name <> '' then
  begin
    Caption := Caption + ' - ' + map_name;
    MissionDialog.Caption := MissionDialog.Caption + ' - ' + map_name;
    EventDialog.Caption := EventDialog.Caption + ' - ' + map_name;
  end;
end;

function TMainWindow.check_map_can_be_tested: boolean;
begin
  if not Map.loaded then
  begin
    Application.MessageBox('No map to test.', 'Cannot test map', MB_ICONERROR);
    result := false;
  end else
  if not Mission.mis_assigned then
  begin
    Application.MessageBox('No mission file is assigned to this map.', 'Cannot test map', MB_ICONERROR);
    result := false;
  end else
  if not FileExists(Settings.GameExecutable) then
  begin
    Application.MessageBox(PChar('Cannot find game executable (' + Settings.GameExecutable + ')'), 'Cannot test map', MB_ICONERROR);
    result := false;
  end else
  if Settings.CheckMapErrorsOnTest then
    result := check_map_errors
  else
    result := true;
end;

procedure TMainWindow.launch_game;
begin
  Settings.save_map_test_settings;
  save_map(Settings.GamePath + 'Missions\TESTMAP.MAP');
  if not MissionDialog.cbUseINI.Checked then
    DeleteFile(Settings.GamePath + 'Missions\TESTMAP.INI');
  ShellExecuteA(0, 'open', PChar(Settings.GameExecutable), PChar('-SPAWN ' + Settings.TestMapParameters), PChar(Settings.GamePath), SW_SHOWNORMAL);
end;

procedure TMainWindow.set_special_value;
var
  value: word;
begin
  if MiscObjList.ItemIndex > -1 then
    value := Structures.misc_object_info[MiscObjList.ItemIndex].value
  else if BuildingList.ItemIndex > -1 then
    value := Structures.structure_info[BuildingList.ItemIndex].values[PlayerSelect.ItemIndex]
  else
    value := Structures.structure_info[UnitList.ItemIndex + Structures.first_unit_index].values[PlayerSelect.ItemIndex];
  SpecialValue.Text := inttostr(value);
  mouse_already_clicked := false;
end;

function TMainWindow.mode(m: SelectedMode): boolean;
begin
  result := false;
  case m of
    mStructures:      result := EditorPages.TabIndex = 0;
    mStructuresPaint: result := (EditorPages.TabIndex = 0) and ((strtoint(SpecialValue.Text) <= 2) or (BuildingList.ItemIndex = 0));
    mTerrain:         result := EditorPages.TabIndex = 1;
    mPaintMode:       result := (EditorPages.TabIndex = 1) and RbPaintMode.Checked;
    mBlockMode:       result := (EditorPages.TabIndex = 1) and RbBlockMode.Checked;
    mSelectMode:      result := (EditorPages.TabIndex = 1) and RbSelectMode.Checked;
  end;
end;

procedure TMainWindow.apply_key_preset(key: word);
var
  preset: TBlockPreset;
begin
  preset := Tileset.get_block_preset(block_preset_group, key, bpNext);
  select_block_from_tileset(preset.width, preset.height, preset.pos_x, preset.pos_y, preset.custom_block_index);
end;

procedure TMainWindow.show_power_and_statistics;
var
  i: integer;
begin
  i := PlayerSelect.ItemIndex;
  StatusBar.Panels[5].Text := 'W: '+inttostr(Map.stats.objects[1])+'  S: '+inttostr(Map.stats.objects[2])+'  B: '+inttostr(Map.stats.objects[3]);
  StatusBar.Panels[6].Text := 'Power: '+inttostr(Map.stats.players[i].power_percent)+'%   ('+inttostr(Map.stats.players[i].power_output)+'/'+inttostr(Map.stats.players[i].power_need)+')';
end;

procedure TMainWindow.start_event_position_selection(x, y: integer);
begin
  EditorPages.TabIndex := 0;
  if (x <> 0) or (y <> 0) then
  begin
    MapScrollH.Position := x - (map_canvas_width div 2);
    MapScrollV.Position := y - (map_canvas_height div 2);
    exit;
  end;
  EditorPages.Visible := false;
  File1.Enabled := false;
  Edit1.Enabled := false;
  ileset1.Enabled := false;
  Map1.Enabled := false;
  Mission1.Enabled := false;
  Launchgame1.Enabled := false;
  MapCanvas.Cursor := crHandPoint;
end;

procedure TMainWindow.finish_event_position_selection(x, y: integer);
begin
  EditorPages.Visible := true;
  File1.Enabled := true;
  Edit1.Enabled := true;
  ileset1.Enabled := true;
  Map1.Enabled := true;
  Mission1.Enabled := true;
  Launchgame1.Enabled := true;
  MapCanvas.Cursor := crDefault;
  EventDialog.Show;
  if (x = -1) and (y = -1) then
    exit;
  EventDialog.finish_event_position_selection(x, y);
end;

procedure TMainWindow.draw_paint_tile_select_glyph(target, tile_index: integer; source_canvas: TCanvas);
var
  tile_x, tile_y: integer;
begin
  tile_x := tile_index mod 20;
  tile_y := tile_index div 20;
  paint_tile_select[target].Glyph.Canvas.CopyRect(Rect(0,0,28,28), source_canvas, Rect(tile_x*32+2, tile_y*32+2, tile_x*32+30, tile_y*32+30));
  paint_tile_select[target].Glyph.Canvas.Pixels[0,27] := $0;
end;

procedure TMainWindow.select_block_from_tileset(b_width, b_height, b_left, b_top: word; custom_block_index: integer);
var
  x, y: integer;
begin
  TilesetDialog.block_width := b_width;
  TilesetDialog.block_height := b_height;
  TilesetDialog.block_left := b_left;
  TilesetDialog.block_top := b_top;
  if custom_block_index = -1 then
  begin
    // Normal block
    block_width := b_width;
    block_height := b_height;
    for x:= 0 to block_width - 1 do
      for y := 0 to block_height - 1 do
      begin
        block_data[x,y].tile := (b_top + y) * 20 + b_left + x;
        block_data[x,y].special := 0;
      end;
  end else
  begin
    // Custom block
    block_width := Tileset.custom_blocks[custom_block_index].width;
    block_height := Tileset.custom_blocks[custom_block_index].height;
    for x:= 0 to block_width - 1 do
      for y := 0 to block_height - 1 do
      begin
        block_data[x,y].tile := Tileset.custom_blocks[custom_block_index].tiles[x,y];
        block_data[x,y].special := 0;
      end;
  end;
  RbBlockMode.Checked := true;
  draw_cursor_image;
end;

procedure TMainWindow.copy_block_from_map(b_width, b_height, b_left, b_top: word; structures: boolean);
begin
  block_width := b_width;
  block_height := b_height;
  Map.copy_block(b_left, b_top, b_width, b_height, Addr(block_data), CbSelectStructures.Checked and structures);
  RbBlockMode.Checked := True;
  draw_cursor_image;
end;

procedure TMainWindow.resize_cursor_image;
var
  cursor_image_left: integer;
  cursor_image_top: integer;
begin
  cursor_image_left := (CursorImage.Left - MapCanvas.Left) div 32;
  cursor_image_top := (CursorImage.Top - MapCanvas.Top) div 32;
  if (cursor_image_left + block_width) > map_canvas_width then
    CursorImage.Width := (map_canvas_width - cursor_image_left) * 32
  else
    CursorImage.Width := block_width * 32 + 1;
  if (cursor_image_top + block_height) > map_canvas_height then
    CursorImage.Height := (map_canvas_height - cursor_image_top) * 32
  else
    CursorImage.Height := block_height * 32 + 1;
end;

procedure TMainWindow.set_cursor_image_visibility;
begin
  if mode(mBlockMode) and (Mouse.CursorPos.X - Left < EditorMenu.Left) then
  begin
    CursorImage.Visible := true;
  end else
    CursorImage.Visible := false;
end;

procedure TMainWindow.draw_cursor_image;
var
  x, y: integer;
  tile_x, tile_y: word;
  border_x, border_y: integer;
begin
  border_x := (128 - block_width * 32) div 2;
  border_y := (128 - block_height * 32) div 2;
  BlockImage.Canvas.Brush.Color := clBtnFace;
  BlockImage.Canvas.Pen.Color := clBtnFace;
  BlockImage.Canvas.Rectangle(0,0,128,128);
  CursorImage.Width := block_width * 32 + 1;
  CursorImage.Height := block_height * 32 + 1;
  CursorImage.Picture.Bitmap.Width := block_width * 32 + 1;
  CursorImage.Picture.Bitmap.Height := block_height * 32 + 1;
  for x:= 0 to block_width-1 do
    for y := 0 to block_height-1 do
    begin
      tile_x := block_data[x,y].tile mod 20;
      tile_y := block_data[x,y].tile div 20;
      BlockImage.Canvas.CopyRect(rect(x*32+border_x, y*32+border_y, x*32+32+border_x, y*32+32+border_y), Tileset.tileimage.Canvas,rect(tile_x*32, tile_y*32, tile_x*32+32, tile_y*32+32));
    end;
  Renderer.render_map_contents(CursorImage.Canvas, 0, 0, block_width, block_height, Addr(block_data), block_width, block_height,
    false, Drawconcrete1.Checked, false, false, Showunknownspecials1.Checked,
    Useallocationindexes1.Checked, false, false, false);
  CursorImage.Canvas.Pen.Color := clBlue;
  CursorImage.Canvas.Brush.Style := bsClear;
  CursorImage.Canvas.Rectangle(0, 0, block_width * 32 + 1, block_height * 32 + 1);
  resize_cursor_image;
end;

procedure TMainWindow.set_map_size(new_width, new_height: integer);
begin
  if (Map.width = new_width) and (Map.height = new_height) then
    exit;
  Map.set_map_size(new_width, new_height);
  StatusBar.Panels[2].Text := inttostr(Map.width)+' x '+inttostr(Map.height);
  resize_map_canvas;
  render_minimap;
  render_map;
end;

procedure TMainWindow.shift_map(direction, num_tiles: integer);
begin
  Map.shift_map(direction, num_tiles);
  EventDialog.fill_grids;
  render_minimap;
  render_map;
end;

procedure TMainWindow.change_structure_owner(player_from,
  player_to: integer; swap: boolean);
begin
  Map.change_structure_owner(player_from, player_to, swap);
  render_minimap;
  render_map;
end;

procedure TMainWindow.new_map(new_width, new_height: integer);
begin
  Map.new_map(new_width, new_height);
  StatusBar.Panels[2].Text := inttostr(Map.width)+' x '+inttostr(Map.height);
  StatusBar.Panels[3].Text := 'Map not saved';
  set_window_titles('Untitled');
  // Reset mission
  if Settings.AssignMisFileToNewMap then
  begin
    Mission.mis_assigned := false;
    Mission.set_default_mis_values;
    Assignmisfile1Click(nil);
  end else
    unload_mission;
  // Get test map settings
  Settings.get_map_test_settings;
  // Finish it
  if MapStatsDialog.Visible then
    MapStatsDialog.update_stats;
  resize_map_canvas;
  render_minimap;
  render_map;
end;

end.
