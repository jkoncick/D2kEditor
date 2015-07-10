unit main;

interface

uses
  // System libraries
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, Menus, StdCtrls, XPMan, Math, Spin, Buttons,
  ShellApi, IniFiles, Clipbrd,
  // Dialogs
  set_dialog, tileset_dialog, test_map_dialog, event_dialog, mission_dialog,
  // Units
  _map, _mission, _tileset, _stringtable, _settings;

const max_undo_steps = 32767;

const block_size_presets: array[1..8,1..2] of word = ((1,1),(2,2),(3,3),(4,4),(2,1),(1,2),(3,2),(2,3));

type
  SelectedMode = (mStructures, mTerrain, mAnyPaint, mSand, mRock, mDunes, mTileBlock, mSelectMode);

type
  TUndoEntry = record
    x, y: word;
    data: TMapTile;
    is_first: boolean;
  end;

type
  TBlockClipboard = record
    block_width: word;
    block_height: word;
    block_data: array[0..127,0..127] of TMapTile;
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
    MiniMapTmp: TImage;
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
    LbStructureList: TLabel;
    StructureList: TListBox;
    RbTileBlock: TRadioButton;
    RbSand: TRadioButton;
    RbRock: TRadioButton;
    RbDunes: TRadioButton;
    LbBlockSize: TLabel;
    BlockWidth: TSpinEdit;
    BlockHeight: TSpinEdit;
    LbX: TLabel;
    Block11: TButton;
    Block22: TButton;
    Block33: TButton;
    Block44: TButton;
    OpenTileset: TButton;
    BlockImage: TImage;
    Block21: TButton;
    Block12: TButton;
    Block32: TButton;
    Block23: TButton;
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
    BlockPresetGroupSelect: TRadioGroup;
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
    procedure ShowGrid1Click(Sender: TObject);
    procedure Marktiles1Click(Sender: TObject);
    procedure Showunknownspecials1Click(Sender: TObject);
    procedure Useallocationindexes1Click(Sender: TObject);
    procedure Showeventmarkers1Click(Sender: TObject);
    procedure Markdefenceareas1Click(Sender: TObject);
    procedure Setmapsize1Click(Sender: TObject);
    procedure Shiftmap1Click(Sender: TObject);
    procedure Changestructureowner1Click(Sender: TObject);
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
    // Structure editor
    procedure StructureListClick(Sender: TObject);
    procedure PlayerSelectChange(Sender: TObject);
    procedure MiscObjListClick(Sender: TObject);
    // Terrain editor
    procedure SetBlockSize(Sender: TObject);
    procedure OpenTilesetClick(Sender: TObject);
    procedure SetCursorImageVisibility(Sender: TObject);

  public

    // Graphic data
    graphics_structures: TPicture;
    graphics_structures_mask: TPicture;
    graphics_misc_objects: TPicture;

    // Map canvas variables
    map_canvas_width: word;
    map_canvas_height: word;
    map_canvas_left: word;
    map_canvas_top: word;
    map_canvas_old_left: word;
    map_canvas_old_top: word;

    // Mouse related variables
    mouse_old_x: word;
    mouse_old_y: word;
    mouse_already_clicked: boolean;
    mouse_last_button: TMouseButton;

    // Minimap variables
    mmap_border_x: word;
    mmap_border_y: word;

    // Tile block variables
    block_width: word;
    block_height: word;
    block_data: array[0..127,0..127] of TMapTile;
    block_preset_order: integer;
    block_select_started: boolean;
    block_select_start_x: word;
    block_select_start_y: word;
    block_select_end_x: word;
    block_select_end_y: word;

    // Event marker moving variables
    moving_disable: boolean;
    moving_event: integer;
    moving_condition: integer;

    // Undo variables
    undo_history: array[0..max_undo_steps] of TUndoEntry;
    undo_start: integer;
    undo_max: integer;
    undo_pos: integer;
    undo_block_start: boolean;

    // Clipboard variables
    clipboard_format: cardinal;

    // Rendering procedures
    procedure resize_map_canvas;
    procedure render_map;
    procedure render_minimap;
    procedure render_minimap_position_marker;

    // Map loading & saving procedures
    procedure load_map(filename: String);
    procedure save_map(filename: String);
    procedure unload_mission;
    procedure set_window_titles(map_name: String);

    // Map testing procedures
    function check_map_can_be_tested: boolean;
    procedure launch_game;

    // Miscellaneous helper procedures
    procedure set_special_value;
    function mode(m: SelectedMode): boolean;
    procedure show_power_and_statistics;
    procedure start_event_position_selection(x, y: integer);
    procedure finish_event_position_selection(x, y: integer);

    // Procedures related to drawing tiles/terrain and cursor image
    procedure select_block_from_tileset(b_width, b_height, b_left, b_top: word);
    procedure copy_block_from_map(b_width, b_height, b_left, b_top: word; structures: boolean);
    procedure put_block_on_map;
    procedure fill_area(x,y: word; area_type: FillAreaType);
    procedure put_sand_rock_dunes(x,y: word);
    procedure set_tile_value(x,y: word; tile: word; special: word; single_op: boolean);
    procedure do_undo;
    procedure do_redo;
    procedure reset_undo_history;
    procedure resize_cursor_image;
    procedure draw_cursor_image;
    procedure apply_key_preset(num: integer; count_cliff: integer; count_border: integer);

    // Procedures related to auto-smoothing edges
    function is_rock_dunes(x,y: integer; exact: boolean): boolean;
    procedure put_edge_block(var xpos, ypos: integer; moveoff_x, moveoff_y, blockoff_x, blockoff_y, block_preset, variants_rock: integer);
    procedure smooth_edges(x, y: integer);

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
  // Load settings
  Settings.load_precreate_editor_settings;
  // Initialize mission
  Mission.init;
  // Initialize tilesets
  Tileset.init;
  // Load string table
  StringTable.load_from_file(Settings.TextUIBPath);
  // Load and initialize graphics
  graphics_structures := TPicture.Create;
  graphics_structures_mask := TPicture.Create;
  graphics_misc_objects := TPicture.Create;
  graphics_structures_mask.bitmap.pixelformat := pf1bit;
  graphics_structures.LoadFromFile(current_dir + '/graphics/structures.bmp');
  graphics_structures_mask.LoadFromFile(current_dir + '/graphics/structures_mask.bmp');
  graphics_misc_objects.LoadFromFile(current_dir + '/graphics/misc_objects.bmp');
  draw_cursor_image;
  // Load map given as first parameter
  if ParamCount > 0 then
    load_map(ParamStr(1));
end;

procedure TMainWindow.FormDestroy(Sender: TObject);
begin
  DragAcceptFiles(Handle, False);
end;

procedure TMainWindow.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Settings.save_editor_settings;
end;

procedure TMainWindow.FormResize(Sender: TObject);
begin
  resize_map_canvas;
  EditorMenu.Left := ClientWidth - 168;
  EditorMenu.Height := Height - 72;
  StructureList.Height := EditorMenu.Height - 354;
  EditorPages.Height := Height - 214;
  StatusBar.Panels[3].Width := ClientWidth - 550;
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
    AMessage.Result := 1;
  end else
    inherited;
end;

procedure TMainWindow.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
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
      OpenTilesetClick(nil);
      key := 0;
      exit;
    end;
    // Block preset group selection
    112: {F1} BlockPresetGroupSelect.ItemIndex := 0;
    113: {F2} BlockPresetGroupSelect.ItemIndex := 1;
    114: {F3} BlockPresetGroupSelect.ItemIndex := 2;
    115: {F4} BlockPresetGroupSelect.ItemIndex := 3;
    // Move cursor image
    98:  {Num2} begin CursorImage.Top := CursorImage.Top + 32; resize_cursor_image; end;
    100: {Num4} begin CursorImage.Left := CursorImage.Left - 32; resize_cursor_image; end;
    102: {Num6} begin CursorImage.Left := CursorImage.Left + 32; resize_cursor_image; end;
    104: {Num8} begin CursorImage.Top := CursorImage.Top - 32; resize_cursor_image; end;
    // Paint sand/rock/dunes
    192: begin
      SetBlockSize(Block11);
      if (BlockPresetGroupSelect.ItemIndex = 0) or (BlockPresetGroupSelect.ItemIndex = 2) then
        RbRock.Checked := true
      else if BlockPresetGroupSelect.ItemIndex = 1 then
        RbSand.Checked := true
      else if BlockPresetGroupSelect.ItemIndex = 3 then
        RbDunes.Checked := true;
    end;
  end;
  // Shift+key
  if ssShift in Shift then
    case key of
    // Block size preset selection
    ord('1'): SetBlockSize(Block11);
    ord('2'): SetBlockSize(Block22);
    ord('3'): SetBlockSize(Block33);
    ord('4'): SetBlockSize(Block44);
    ord('5'): SetBlockSize(Block21);
    ord('6'): SetBlockSize(Block12);
    ord('7'): SetBlockSize(Block32);
    ord('8'): SetBlockSize(Block23);
    // Terrain editing mode selection
    ord('B'): RbTileBlock.Checked := true;
    ord('D'): RbDunes.Checked := true;
    ord('R'): RbRock.Checked := true;
    ord('S'): RbSand.Checked := true;
    ord('C'): RbSelectMode.Checked := true;
    ord('T'): CbSelectStructures.Checked := not CbSelectStructures.Checked;
  end else
    case key of
    ord('1'): apply_key_preset(1, 1, 1); // Up
    ord('2'): apply_key_preset(2, 1, 1);
    ord('3'): apply_key_preset(3, 4, 5);
    ord('4'): apply_key_preset(8, 1, 1);
    ord('5'): apply_key_preset(9, 1, 2);
    ord('Q'): apply_key_preset(11, 1, 1); // Left
    ord('E'): apply_key_preset(12, 4, 5);
    ord('A'): apply_key_preset(17, 1, 1);
    ord('T'): apply_key_preset(18, 1, 1); // Right
    ord('D'): apply_key_preset(19, 4, 4);
    ord('G'): apply_key_preset(23, 1, 1);
    ord('Z'): apply_key_preset(24, 1, 1); // Down
    ord('X'): apply_key_preset(25, 1, 1);
    ord('C'): apply_key_preset(26, 4, 3);
    ord('V'): apply_key_preset(30, 1, 1);
    ord('B'): apply_key_preset(31, 1, 1);
    ord('W'): apply_key_preset(32, 1, 1); // Inner curves
    ord('S'): apply_key_preset(33, 1, 1);
    ord('R'): apply_key_preset(34, 1, 1);
    ord('F'): apply_key_preset(35, 1, 1);
    ord('6'): apply_key_preset(36, 1, 1); // Up
    ord('7'): apply_key_preset(37, 1, 1);
    ord('8'): apply_key_preset(38, 1, 1);
    ord('9'): apply_key_preset(39, 1, 1);
    ord('Y'): apply_key_preset(40, 1, 1); // Left
    ord('U'): apply_key_preset(41, 1, 1);
    ord('J'): apply_key_preset(42, 1, 1);
    ord('H'): apply_key_preset(43, 1, 1);
    ord('O'): apply_key_preset(44, 1, 1); // Right
    ord('I'): apply_key_preset(45, 1, 1);
    ord('K'): apply_key_preset(46, 1, 1);
    ord('L'): apply_key_preset(47, 1, 1);
    ord('N'): apply_key_preset(48, 1, 1); // Down
    ord('M'): apply_key_preset(49, 1, 1);
    188:      apply_key_preset(50, 1, 1);
    190:      apply_key_preset(51, 1, 1);
    ord('0'): apply_key_preset(52, 1, 1); // Others
    ord('P'): apply_key_preset(53, 1, 1);
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
  if map_loaded and (map_filename <> '') then
    load_map(map_filename);
end;

procedure TMainWindow.Savemap1Click(Sender: TObject);
begin
  if not map_loaded then
    exit;
  if map_filename = '' then
    Savemapas1Click(Sender)
  else begin
    check_map_errors;
    save_map(map_filename);
  end;
end;

procedure TMainWindow.Savemapas1Click(Sender: TObject);
begin
  if not map_loaded then
    exit;
  check_map_errors;
  if MapSaveDialog.Execute then
  begin
    if map_filename <> MapSaveDialog.FileName then
    begin
      map_filename := MapSaveDialog.FileName;
      if Mission.mis_assigned then
        Mission.mis_filename := Mission.get_mis_filename(map_filename);
      StatusBar.Panels[3].Text := MapSaveDialog.FileName;
      Settings.get_file_paths_from_map_filename;
      set_window_titles(ChangeFileExt(ExtractFileName(map_filename),''));
    end;
    save_map(MapSaveDialog.FileName);
  end;
end;

procedure TMainWindow.Savemapimage1Click(Sender: TObject);
var
  fast_rendering: boolean;
begin
  if not map_loaded then
    exit;
  if MapImageSaveDialog.Execute then
  begin
    map_canvas_left := 0;
    map_canvas_top := 0;
    map_canvas_width := map_width;
    map_canvas_height := map_height;
    MapCanvas.Picture.Bitmap.Width := map_canvas_width * 32;
    MapCanvas.Picture.Bitmap.Height := map_canvas_height * 32;
    fast_rendering := Fastrendering1.Checked;
    Fastrendering1.Checked := false;
    render_map;
    MapCanvas.Picture.Bitmap.SaveToFile(MapImageSaveDialog.FileName);
    map_canvas_left := MapScrollH.Position;
    map_canvas_top := MapScrollV.Position;
    resize_map_canvas;
    Fastrendering1.Checked := fast_rendering;
  end;
end;

procedure TMainWindow.Exit1Click(Sender: TObject);
begin
  application.Terminate;
end;

procedure TMainWindow.Undo1Click(Sender: TObject);
begin
  do_undo;
  mouse_already_clicked := false;
end;

procedure TMainWindow.Redo1Click(Sender: TObject);
begin
  do_redo;
  mouse_already_clicked := false;
end;

procedure TMainWindow.Copy1Click(Sender: TObject);
var
  handle: THandle;
  pointer: ^TBlockClipboard;
begin
  if not map_loaded or not mode(mTileBlock) then
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
  if not map_loaded or not Clipboard.HasFormat(clipboard_format) then
    exit;
  OpenClipboard(Application.Handle);
  handle := GetClipboardData(clipboard_format);
  pointer := GlobalLock(handle);

  block_width := pointer.block_width;
  block_height := pointer.block_height;
  Move(pointer.block_data, block_data, sizeof(block_data));

  RbTileBlock.Checked := true;
  EditorPages.TabIndex := 1;
  draw_cursor_image;

  GlobalUnLock(handle);
  CloseClipboard;
end;

procedure TMainWindow.SelectTileset(Sender: TObject);
begin
  Tileset.change_tileset((sender as TMenuItem).Tag)
end;

procedure TMainWindow.Selectnext1Click(Sender: TObject);
begin
  Tileset.next_tileset;
end;

procedure TMainWindow.Loadtileset1Click(Sender: TObject);
begin
  if TilesetOpenDialog.Execute then
  begin
    Tileset.load_custom_image(TilesetOpenDialog.FileName);
  end;
end;

procedure TMainWindow.Loadtilesetattributes1Click(Sender: TObject);
begin
  if TileatrOpenDialog.Execute then
  begin
    Tileset.load_tileatr(TileatrOpenDialog.FileName);
    render_minimap;
    render_map;
  end;
end;

procedure TMainWindow.ShowGrid1Click(Sender: TObject);
begin
  ShowGrid1.Checked := not ShowGrid1.Checked;
  render_map;
end;

procedure TMainWindow.Marktiles1Click(Sender: TObject);
begin
  Marktiles1.Checked := not Marktiles1.Checked;
  render_map;
end;

procedure TMainWindow.Showunknownspecials1Click(Sender: TObject);
begin
  Showunknownspecials1.Checked := not Showunknownspecials1.Checked;
  render_map;
end;

procedure TMainWindow.Useallocationindexes1Click(Sender: TObject);
begin
  Useallocationindexes1.Checked := not Useallocationindexes1.Checked;
  render_minimap;
  render_map;
end;

procedure TMainWindow.Showeventmarkers1Click(Sender: TObject);
begin
  Showeventmarkers1.Checked := not Showeventmarkers1.Checked;
  render_map;
end;

procedure TMainWindow.Markdefenceareas1Click(Sender: TObject);
begin
  Markdefenceareas1.Checked := not Markdefenceareas1.Checked;
  render_map;
end;

procedure TMainWindow.Setmapsize1Click(Sender: TObject);
begin
  SetDialog.SetMapSize_Width.Value := map_width;
  SetDialog.SetMapSize_Height.Value := map_height;
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
  if not map_loaded then
    exit;
  if not Mission.mis_assigned then
  begin
    Mission.mis_assigned := true;
    Mission.mis_filename := Mission.get_mis_filename(map_filename);
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
  ShowMessage('Key Shortcuts:'#13#13'Space = Open tileset window'#13'Esc = Close tileset window'#13'Tab = Switch Structures / Terrain'#13'Shift + 1 - 8 = Block size preset'#13+
              'Shift + S = Paint sand'#13'Shift + R = Paint rock'#13'Shift + D = Paint dunes'#13'Shift + B = Tile block'#13'Shift + C = Select and copy mode'#13'Shift + T = Select structures'#13'Ctrl + Z = Undo'#13'Ctrl + Y = Redo'#13'F1 - F4 = Block key-preset group'#13'Num 2,4,6,8 = Move block on map');
end;

procedure TMainWindow.Mouseactions1Click(Sender: TObject);
begin
  ShowMessage('Mouse actions'#13#13'When editing structures:'#13'Left = Place structure'#13'Right = Remove structure'#13'Middle = Copy structure'#13#13+'When editing terrain:'#13'Left = Draw / Place block'#13'Double click = Fill area'#13'Shift+click = Smooth edge'#13'Middle = Copy block'#13'Right = Drag and scroll map');
end;

procedure TMainWindow.About1Click(Sender: TObject);
begin
  ShowMessage('Dune 2000 Campaign Map Editor'#13#13'Part of D2K+ Editing tools'#13#13'Made by Klofkac'#13'Version 0.6'#13'Date: 2015-05-30'#13#13'http://github.com/jkoncick/D2kEditor');
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
  if map_x >= map_width then
    map_x := map_width - 1;
  if map_y < 0 then
    map_y := 0;
  if map_y >= map_height then
    map_y := map_height - 1;
  // Write coordinates on status bar
  StatusBar.Panels[0].Text := 'x: '+inttostr(map_x)+' y: '+inttostr(map_y);
  // Show cursor image if needed
  SetCursorImageVisibility(Sender);
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
      tmp_hint := tmp_hint + chr(13) + unit_names[Mission.mis_data.events[eventnum].units[i]];
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
  end else
  // If left button is held, paint sand/rock/dunes/spice during mouse move
  if (ssLeft in shift) and mode(mAnyPaint) then
    MapCanvasMouseDown(sender,mbLeft,Shift,x,y);
end;

procedure TMainWindow.MapCanvasMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  xx, yy: integer;
  map_x, map_y: integer;
  index, player: word;
  is_misc: boolean;
  event_marker: ^TEventMarker;
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
  // Check for spice-to-sand restriction
  if ((MiscObjList.ItemIndex = 1) or (MiscObjList.ItemIndex = 2)) and Settings.RestrictSpiceToSand and (Tileset.get_fill_area_type(map_data[map_x, map_y].tile, 0) <> faSand) and
    mode(mStructures) and (Button = mbLeft) then
    exit;
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
      set_tile_value(map_x, map_y, map_data[map_x, map_y].tile, strtoint(SpecialValue.Text), true)
    else if Button = mbRight then
      // Delete structure from map
      set_tile_value(map_x, map_y, map_data[map_x, map_y].tile, 0, true)
    else if Button = mbMiddle then
    begin
      // Get structure parameters on position and set them in menu
      SpecialValue.text := inttostr(map_data[map_x, map_y].special);
      if special_value_to_params(map_data[map_x, map_y].special, player, index, is_misc) then
      begin
        if is_misc then
        begin
          MiscObjList.ItemIndex := index;
          StructureList.ItemIndex := -1;
        end else
        begin
          MiscObjList.ItemIndex := -1;
          StructureList.ItemIndex := index;
          PlayerSelect.ItemIndex := player;
          show_power_and_statistics;
        end;
      end else
      begin
        MiscObjList.ItemIndex := -1;
        StructureList.ItemIndex := -1;
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
      end
      else if mode(mTileBlock) then
      begin
        // Draw selected block
        put_block_on_map;
      end
      else if (ssShift in Shift) and (mode(mRock) or mode(mDunes)) then
      begin
        // Smooth rock/dunes edge
        smooth_edges(map_x, map_y);
      end
      else if mode(mAnyPaint) then
      begin
        // Paint Sand/Rock/Dunes
        undo_block_start := true;
        for xx := map_x to map_x + BlockWidth.Value - 1 do
          for yy := map_y to map_y + BlockHeight.Value - 1 do
          begin
            if (xx >= map_width) or (xx < 0) or (yy >= map_height) or (yy < 0) then
              continue;
            put_sand_rock_dunes(xx, yy);
          end;
      end;
    end
    else if button = mbMiddle then
    begin
      // Copy selected block
      copy_block_from_map(BlockWidth.Value, BlockHeight.Value, map_x, map_y, false);
      exit;
    end;
  end;
  if not mode(mAnyPaint) then
  begin
    render_minimap;
    calculate_power_and_statistics;
  end;
  render_map;
end;

procedure TMainWindow.MapCanvasDblClick(Sender: TObject);
var
  tmp_pos: integer;
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
  // Double click for filling
  if mode(mAnyPaint) then
  begin
    // Undo the action which was made by first click
    tmp_pos := undo_pos;
    repeat
      tmp_pos := (tmp_pos - 1) and max_undo_steps
    until undo_history[tmp_pos].is_first;
    if (undo_history[tmp_pos].x = mouse_old_x) and (undo_history[tmp_pos].y = mouse_old_y) then
      do_undo;
    // Fill area
    undo_block_start := true;
    fill_area(mouse_old_x, mouse_old_y, Tileset.get_fill_area_type(map_data[mouse_old_x, mouse_old_y].tile, map_data[mouse_old_x, mouse_old_y].special));
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
    render_map;
  end;
  if mode(mAnyPaint) then
  begin
    render_minimap;
    calculate_power_and_statistics;
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
  if not map_loaded then
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

procedure TMainWindow.StructureListClick(Sender: TObject);
begin
  MiscObjList.ItemIndex := -1;
  set_special_value;
end;

procedure TMainWindow.PlayerSelectChange(Sender: TObject);
begin
  set_special_value;
  show_power_and_statistics;
end;

procedure TMainWindow.MiscObjListClick(Sender: TObject);
begin
  StructureList.ItemIndex := -1;
  set_special_value;
end;

procedure TMainWindow.SetBlockSize(Sender: TObject);
begin
  BlockWidth.Value := block_size_presets[(Sender as TButton).Tag,1];
  BlockHeight.Value := block_size_presets[(Sender as TButton).Tag,2];
end;

procedure TMainWindow.OpenTilesetClick(Sender: TObject);
begin
  TilesetDialog.Show;
end;

procedure TMainWindow.SetCursorImageVisibility(Sender: TObject);
begin
  if mode(mTileBlock) and (Mouse.CursorPos.X - Left < EditorMenu.Left) then
  begin
    CursorImage.Visible := true;
    if (not TilesetDialog.Visible) and EditorPages.Visible and (not EventDialog.Visible) then
      RbTileBlock.SetFocus;
  end else
    CursorImage.Visible := false;
end;

procedure TMainWindow.resize_map_canvas;
begin
  map_canvas_width := (ClientWidth - 200) div 32;
  if map_canvas_width > map_width then
    map_canvas_width := map_width;
  map_canvas_height := (ClientHeight - 50) div 32;
  if map_canvas_height > map_height then
    map_canvas_height := map_height;
  MapCanvas.Picture.Bitmap.Width := map_canvas_width * 32;
  MapCanvas.Picture.Bitmap.Height := map_canvas_height * 32;
  MapCanvas.Width := map_canvas_width * 32;
  MapCanvas.Height := map_canvas_height * 32;
  MapScrollH.Top := map_canvas_height * 32 + 8;
  MapScrollH.Width := map_canvas_width * 32;
  MapScrollH.Max := map_width - map_canvas_width;
  if map_width = map_canvas_width then
    MapScrollH.Enabled := False
  else
    MapScrollH.Enabled := True;
  MapScrollV.Left := map_canvas_width * 32 + 8;
  MapScrollV.Height := map_canvas_height * 32;
  MapScrollV.Max := map_height - map_canvas_height;
  if map_height = map_canvas_height then
    MapScrollV.Enabled := False
  else
    MapScrollV.Enabled := True;
  if map_loaded then
  begin
    render_map;
    render_minimap_position_marker;
  end;
end;


procedure TMainWindow.render_map;
var
  min_x, min_y, max_x, max_y: integer;
  max_y_plus: word;
  shift_count: word;
  x, y: integer;
  actual_x, actual_y: integer;
  tile: word;
  value: word;
  player, index: word;
  is_misc: boolean;
  wall_bitmap: word;
  dest_rect: TRect;
  src_rect: TRect;
  tile_attr: TileType;
  event_marker: ^TEventMarker;
begin
  if not map_loaded then
    exit;
  min_x := 0;
  min_y := 0;
  max_x := map_canvas_width - 1;
  max_y := map_canvas_height - 1;
  max_y_plus := 1;
  // Scrolling optimization
  if Fastrendering1.Checked then
  begin
    // Horizontal scroll
    if (map_canvas_left <> map_canvas_old_left) and (abs(map_canvas_left - map_canvas_old_left) < map_canvas_width)  then
    begin
      shift_count := abs(map_canvas_left - map_canvas_old_left);
      if map_canvas_left < map_canvas_old_left then
      begin
        // Scrolling left
        max_x := shift_count - 1;
        dest_rect := rect(shift_count*32,0,map_canvas_width*32,map_canvas_height*32);
        src_rect := rect(0,0,map_canvas_width*32-shift_count*32,map_canvas_height*32);
      end else
      begin
        // Scrolling right
        min_x := max_x - shift_count + 1;
        src_rect := rect(shift_count*32,0,map_canvas_width*32,map_canvas_height*32);
        dest_rect := rect(0,0,map_canvas_width*32-shift_count*32,map_canvas_height*32);
      end;
      // Shifting part of map canvas
      MapCanvas.Canvas.CopyRect(dest_rect,MapCanvas.Canvas,src_rect);
    end;
    // Vertical scroll
    if (map_canvas_top <> map_canvas_old_top) and (abs(map_canvas_top - map_canvas_old_top) < map_canvas_height)  then
    begin
      shift_count := abs(map_canvas_top - map_canvas_old_top);
      if map_canvas_top < map_canvas_old_top then
      begin
        // Scrolling up
        max_y := shift_count - 1;
        max_y_plus := 4;
        dest_rect := rect(0,shift_count*32,map_canvas_width*32,map_canvas_height*32);
        src_rect := rect(0,0,map_canvas_width*32,map_canvas_height*32-shift_count*32);
      end else
      begin
        // Scrolling down
        min_y := max_y - shift_count + 1;
        src_rect := rect(0,shift_count*32,map_canvas_width*32,map_canvas_height*32);
        dest_rect := rect(0,0,map_canvas_width*32,map_canvas_height*32-shift_count*32);
      end;
      // Shifting part of map canvas
      MapCanvas.Canvas.CopyRect(dest_rect,MapCanvas.Canvas,src_rect);
    end;
  end;
  map_canvas_old_left := map_canvas_left;
  map_canvas_old_top := map_canvas_top;
  // Draw terrain
  for y:= min_y to max_y do
  begin
    for x:= min_x to max_x do
    begin
      tile := map_data[x + map_canvas_left, y + map_canvas_top].tile;
      MapCanvas.Canvas.CopyRect(rect(x*32,y*32,x*32+32,y*32+32),Tileset.tileimage.Canvas,rect((tile mod 20)*32,(tile div 20 * 32),(tile mod 20)*32+32,(tile div 20 * 32+32)));
      // Draw tile attribute markers
      if Marktiles1.Checked then
      begin
        tile_attr := Tileset.get_tile_type(tile);
        if (tile_attr = ttImpassable) or (tile_attr = ttInfantryOnly) then
        begin
          if (tile_attr = ttImpassable) then
            MapCanvas.Canvas.Pen.Color := clRed
          else if (tile_attr = ttInfantryOnly) then
            MapCanvas.Canvas.Pen.Color := $4080FF;
          MapCanvas.Canvas.Pen.Width := 2;
          MapCanvas.Canvas.MoveTo(x*32, y*32);
          MapCanvas.Canvas.LineTo(x*32+31, y*32+31);
          MapCanvas.Canvas.MoveTo(x*32+31, y*32);
          MapCanvas.Canvas.LineTo(x*32, y*32+31);
        end;
      end;
    end;
  end;
  MapCanvas.Canvas.Pen.Width := 1;
  MapCanvas.Canvas.Brush.Style := bsClear;
  // Draw structures
  for y:= min_y -3 to max_y + max_y_plus do
  begin
    for x:= min_x -2 to max_x do
    begin
      actual_x := x + map_canvas_left;
      actual_y := y + map_canvas_top;
      // If tile is out of map
      if (actual_x < 0) or (actual_x >= map_width) or (actual_y < 0) or (actual_y >= map_height) then
        continue;
      value := map_data[actual_x, actual_y].special;
      // Getting structure parameters
      if special_value_to_params(value,player,index,is_misc) then
      begin
        // Structure is not empty
        if is_misc then
        begin
          // Value is misc
          src_rect := rect((index-1)*32,0,(index-1)*32+32,32);
          dest_rect := rect(x*32,y*32,x*32+32,y*32+32);
          MapCanvas.Canvas.CopyMode:=cmSrcCopy;
          MapCanvas.Canvas.CopyRect(dest_rect,graphics_misc_objects.Bitmap.Canvas,src_rect);
        end else
        begin
          // Value is structure
          dest_rect := rect(x*32,y*32,x*32+structure_params[index].size_x*32,y*32+structure_params[index].size_y*32);
          // Translate player number according to allocation index
          if Useallocationindexes1.Checked then
            player := Mission.mis_data.allocation_index[player];
          if player >= cnt_map_players then
            player := 0;
          if index = 0 then
          begin
            // Structure is wall
            wall_bitmap := 0;
            // Checking left of wall
            if (actual_x - 1) >= 0 then
            begin
              value := map_data[actual_x - 1, actual_y].special;
              if special_value_to_params(value,player,index,is_misc) and structure_params[index].lnwall then
                wall_bitmap := wall_bitmap + 1
            end;
            // Checking up of wall
            if (actual_y - 1) >= 0 then
            begin
              value := map_data[actual_x, actual_y - 1].special;
              if special_value_to_params(value,player,index,is_misc) and structure_params[index].lnwall then
                wall_bitmap := wall_bitmap + 2
            end;
            // Checking right of wall
            if (actual_x + 1) < map_width then
            begin
              value := map_data[actual_x + 1, actual_y].special;
              if special_value_to_params(value,player,index,is_misc) and structure_params[index].lnwall then
                wall_bitmap := wall_bitmap + 4
            end;
            // Checking down of wall
            if (actual_y + 1) < map_height then
            begin
              value := map_data[actual_x, actual_y + 1].special;
              if special_value_to_params(value,player,index,is_misc) and structure_params[index].lnwall then
                wall_bitmap := wall_bitmap + 8
            end;
            // Wall source rect
            src_rect := rect(0,wall_bitmap*32,32,wall_bitmap*32+32);
            index := 0;
          end else
            // Structure is not wall
            src_rect := rect(structure_params[index].offs_x*32,32+player*128+structure_params[index].offs_y*32,structure_params[index].offs_x*32+structure_params[index].size_x*32,32+player*128+structure_params[index].offs_y*32+structure_params[index].size_y*32);
          // Buildings / units overflows
          // Overflow up (buildings)
          if structure_params[index].overfl = 1 then
          begin
            src_rect.Top := src_rect.Top - 16;
            dest_rect.Top := dest_rect.Top - 16;
          end else
          // Overflow down (infantry)
          if structure_params[index].overfl = 2 then
          begin
            src_rect.Bottom := src_rect.Bottom - 16;
            dest_rect.Bottom := dest_rect.Bottom - 16;
          end else
          // Overflow to all sides (harvester, MCV)
          if structure_params[index].overfl = 3 then
          begin
            src_rect.Top := src_rect.Top - 4;
            dest_rect.Top := dest_rect.Top - 4;
            src_rect.Bottom := src_rect.Bottom + 4;
            dest_rect.Bottom := dest_rect.Bottom + 4;
            src_rect.Left := src_rect.Left - 4;
            dest_rect.Left := dest_rect.Left - 4;
            src_rect.Right := src_rect.Right + 4;
            dest_rect.Right := dest_rect.Right + 4;
          end;
          // Drawing only overflow up when under map canvas
          if (y > max_y) then
          begin
            if structure_params[index].overfl <> 1 then
              continue;
            src_rect.Bottom := 32+player*128+structure_params[index].offs_y*32;
            dest_rect.Bottom := y*32;
          end;
          // Drawing structure
          MapCanvas.Canvas.CopyMode := cmSrcAnd;
          MapCanvas.Canvas.CopyRect(dest_rect,graphics_structures_mask.Bitmap.Canvas,src_rect);
          MapCanvas.Canvas.CopyMode := cmSrcPaint;
          MapCanvas.Canvas.CopyRect(dest_rect,graphics_structures.Bitmap.Canvas,src_rect);
        end;
      end
      else if (value <> 0) and Showunknownspecials1.Checked then
        MapCanvas.Canvas.TextOut(x * 32 + 2, y * 32 + 2, inttostr(value));
    end;
  end;
  // Draw event markers
  if Showeventmarkers1.Checked then
  begin
    for y:= 0 to map_canvas_height - 1 do
      for x:= 0 to map_canvas_width - 1 do
      begin
        event_marker := addr(Mission.event_markers[x + map_canvas_left, y + map_canvas_top]);
        if event_marker.emtype = emNone then
          continue;
        if event_marker_type_info[ord(event_marker.emtype)].player_related then
        begin
          player := event_marker.side;
          if player >= cnt_mis_players then
            player := 0;
          MapCanvas.Canvas.Pen.Color := mmap_player_colors[player];
          MapCanvas.Canvas.Brush.Color := mmap_player_colors[player];
        end else
        begin
          MapCanvas.Canvas.Pen.Color := clGray;
          MapCanvas.Canvas.Brush.Color := clGray;
        end;
        MapCanvas.Canvas.Rectangle(x*32, y*32, x*32+32, y*32+32);
        MapCanvas.Canvas.Pen.Color := clBlack;
        MapCanvas.Canvas.TextOut(x * 32 + 12, y * 32 + 3, event_marker_type_info[ord(event_marker.emtype)].letter);
        MapCanvas.Canvas.TextOut(x * 32 + 12, y * 32 + 17, inttostr(event_marker.index));
        if event_marker.moved then
          MapCanvas.Canvas.TextOut(x * 32 + 2, y * 32 + 10, '<');
      end;
  end;
  // Draw defence area markers
  if Markdefenceareas1.Checked then
  begin
    MapCanvas.Canvas.Brush.Style := bsClear;
    MapCanvas.Canvas.pen.Width := 2;
    for x := 0 to cnt_mis_players - 1 do
      for y := 0 to Mission.mis_data.ai_segments[x,7505] - 1 do
      begin
        MapCanvas.Canvas.Pen.Color := mmap_player_colors[x];
        MapCanvas.Canvas.Rectangle(
          (Mission.mis_data.ai_segments[x,7508+y*20] - map_canvas_left) * 32,
          (Mission.mis_data.ai_segments[x,7510+y*20] - map_canvas_top) * 32,
          (Mission.mis_data.ai_segments[x,7509+y*20] - map_canvas_left) * 32 + 32,
          (Mission.mis_data.ai_segments[x,7511+y*20] - map_canvas_top) * 32 + 32);
        MapCanvas.Canvas.TextOut(
          (Mission.mis_data.ai_segments[x,7508+y*20] - map_canvas_left) * 32 + 3,
          (Mission.mis_data.ai_segments[x,7510+y*20] - map_canvas_top) * 32 + 3,
          'Area' + inttostr(y+1));
      end;
  end;
  // Draw grid
  MapCanvas.Canvas.CopyMode:=cmSrcCopy;
  MapCanvas.Canvas.Pen.Color := clBlack;
  MapCanvas.Canvas.Pen.Width := 1;
  if ShowGrid1.Checked then
  begin
    for x:= 0 to map_canvas_width do
    begin
      MapCanvas.Canvas.MoveTo(x*32,0);
      MapCanvas.Canvas.LineTo(x*32,map_canvas_height*32);
    end;
    for y:= 0 to map_canvas_height do
    begin
      MapCanvas.Canvas.MoveTo(0,y*32);
      MapCanvas.Canvas.LineTo(map_canvas_width*32,y*32);
    end;
  end;
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

procedure TMainWindow.render_minimap;
var
  x, y: integer;
  value: integer;
  player, index: word;
  is_misc: boolean;
  tile_attr: TileType;
begin
  if not map_loaded then
    exit;
  MiniMapTmp.Canvas.Brush.Color := ClBtnFace;
  MiniMapTmp.Canvas.Pen.Color := ClBtnFace;
  MiniMapTmp.Canvas.Rectangle(0,0,128,128);
  mmap_border_x := (128 - map_width) div 2;
  mmap_border_y := (128 - map_height) div 2;
  // Rendering terrain
  for y:= 0 to map_height - 1 do
    for x:= 0 to map_width - 1 do
    begin
      tile_attr := Tileset.get_tile_type(map_data[x,y].tile);
      MiniMapTmp.Canvas.Pixels[x+mmap_border_x,y+mmap_border_y] := mmap_tile_type_colors[ord(tile_attr)];
    end;
  // Rendering structures
  for y:= 0 to map_height - 1 do
    for x:= 0 to map_width - 1 do
    begin
      value := map_data[x,y].special;
      if not special_value_to_params(value,player,index,is_misc) then
        continue
      else if is_misc then
      begin
        MiniMapTmp.Canvas.Pixels[x+mmap_border_x,y+mmap_border_y] := mmap_misc_objects_colors[index];
      end else
      begin
        // Translate player number according to allocation index
        if Useallocationindexes1.Checked then
          player := Mission.mis_data.allocation_index[player];
        if player >= cnt_mis_players then
          player := 0;
        // Render structure on map
        MiniMapTmp.Canvas.Pen.Color := mmap_player_colors[player];
        MiniMapTmp.Canvas.Brush.Color := mmap_player_colors[player];
        MiniMapTmp.Canvas.Pixels[x+mmap_border_x,y+mmap_border_y] := mmap_player_colors[player];
        MiniMapTmp.Canvas.Rectangle(x+mmap_border_x,y+mmap_border_y,x+mmap_border_x+structure_params[index].size_x,y+mmap_border_y+structure_params[index].size_y);
      end;
    end;
  render_minimap_position_marker;
end;

procedure TMainWindow.render_minimap_position_marker;
begin
  MiniMap.Canvas.CopyRect(rect(0,0,128,128),MiniMapTmp.Canvas,rect(0,0,128,128));
  MiniMap.Canvas.Pen.Color:= $00FF00;
  MiniMap.Canvas.Brush.Style := bsClear;
  MiniMap.Canvas.Rectangle(mmap_border_x + map_canvas_left,mmap_border_y + map_canvas_top,mmap_border_x + map_canvas_left + map_canvas_width,mmap_border_y + map_canvas_top + map_canvas_height);
  MiniMap.Canvas.Brush.Style := bsSolid;
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
  reset_undo_history;
  // Load map file
  load_map_file(filename);
  // Set status bar
  StatusBar.Panels[3].Text := filename;
  StatusBar.Panels[2].Text := inttostr(map_width)+' x '+inttostr(map_height);
  // Initialize settings
  Settings.get_file_paths_from_map_filename;
  Settings.get_map_test_settings;
  // Load mis file
  tmp_mis_filename := Mission.get_mis_filename(map_filename);
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
  set_window_titles(ChangeFileExt(ExtractFileName(map_filename), ''));
  // Rendering
  resize_map_canvas;
  render_minimap;
  render_map;
  calculate_power_and_statistics;
end;

procedure TMainWindow.save_map(filename: String);
begin
  save_map_file(filename);
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
  if not map_loaded then
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
    result := check_map_errors;
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
    value := misc_obj_values[MiscObjList.ItemIndex]
  else
    value := structure_params[StructureList.ItemIndex].values[PlayerSelect.ItemIndex];
  SpecialValue.Text := inttostr(value);
end;

function TMainWindow.mode(m: SelectedMode): boolean;
begin
  result := false;
  case m of
    mStructures:      result := EditorPages.TabIndex = 0;
    mTerrain:         result := EditorPages.TabIndex = 1;
    mAnyPaint:        result := ((EditorPages.TabIndex = 0) and (strtoint(SpecialValue.Text) <= 2)) or ((EditorPages.TabIndex = 1) and (RbSand.Checked or RbRock.Checked or RbDunes.Checked));
    mSand:            result := (EditorPages.TabIndex = 1) and RbSand.Checked;
    mRock:            result := (EditorPages.TabIndex = 1) and RbRock.Checked;
    mDunes:           result := (EditorPages.TabIndex = 1) and RbDunes.Checked;
    mTileBlock:       result := (EditorPages.TabIndex = 1) and RbTileBlock.Checked;
    mSelectMode:      result := (EditorPages.TabIndex = 1) and RbSelectMode.Checked;
  end;
end;

procedure TMainWindow.show_power_and_statistics;
var
  i: integer;
begin
  i := PlayerSelect.ItemIndex;
  StatusBar.Panels[5].Text := 'W: '+inttostr(mstat_num_worm_spawners)+'  S: '+inttostr(mstat_num_player_starts)+'  B: '+inttostr(mstat_num_spice_blooms);
  StatusBar.Panels[6].Text := 'Power: '+inttostr(mstat_player[i].power_percent)+'%   ('+inttostr(mstat_player[i].power_output)+'/'+inttostr(mstat_player[i].power_need)+')';
end;

procedure TMainWindow.start_event_position_selection(x, y: integer);
begin
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

procedure TMainWindow.select_block_from_tileset(b_width, b_height, b_left,
  b_top: word);
var
  x, y: integer;
begin
  TilesetDialog.block_width := b_width;
  TilesetDialog.block_height := b_height;
  TilesetDialog.block_left := b_left;
  TilesetDialog.block_top := b_top;
  block_width := b_width;
  block_height := b_height;
  for x:= 0 to b_width - 1 do
    for y := 0 to b_height - 1 do
    begin
      block_data[x,y].tile := (b_top + y) * 20 + b_left + x;
      block_data[x,y].special := 0;
    end;
  RbTileBlock.Checked := true;
  draw_cursor_image;
end;

procedure TMainWindow.copy_block_from_map(b_width, b_height, b_left,
  b_top: word; structures: boolean);
var
  x, y: integer;
  value: TMapTile;
begin
  block_width := b_width;
  block_height := b_height;
  for x := 0 to b_width - 1 do
    for y := 0 to b_height - 1 do
    begin
      if (b_left + x < map_width) and (b_top + y < map_height) then
      begin
        value := map_data[b_left + x, b_top + y];
        if (not CbSelectStructures.Checked) or (not structures) then
          value.special := 0;
      end else
      begin
        value.tile := 0;
        value.special := 0;
      end;
      block_data[x,y] := value;
    end;
  RbTileBlock.Checked := True;
  draw_cursor_image;
end;

procedure TMainWindow.put_block_on_map;
var
  x, y: integer;
  cursor_left: integer;
  cursor_top: integer;
begin
  cursor_left := (CursorImage.Left - MapCanvas.Left) div 32 + map_canvas_left;
  cursor_top := (CursorImage.Top - MapCanvas.Top) div 32 + map_canvas_top;
  undo_block_start := true;
  for x := 0 to block_width - 1 do
    for y := 0 to block_height - 1 do
      if (cursor_left + x < map_width) and (cursor_left + x >= 0) and (cursor_top + y < map_height) and (cursor_top + y >= 0) then
        set_tile_value(cursor_left + x, cursor_top + y, block_data[x,y].tile, block_data[x,y].special, false);
end;

procedure TMainWindow.fill_area(x, y: word; area_type: FillAreaType);
begin
  if Tileset.get_fill_area_type(map_data[x,y].tile, map_data[x,y].special) <> area_type then
    exit;
  if mode(mStructures) then
    set_tile_value(x, y, map_data[x,y].tile, strtoint(SpecialValue.text), false)
  else
    put_sand_rock_dunes(x, y);
  if Tileset.get_fill_area_type(map_data[x,y].tile, map_data[x,y].special) = area_type then
    exit;
  if x > 0 then
    fill_area(x-1, y, area_type);
  if x < (map_width - 1) then
    fill_area(x+1, y, area_type);
  if y > 0 then
    fill_area(x, y-1, area_type);
  if y < (map_height - 1) then
    fill_area(x, y+1, area_type);
end;

procedure TMainWindow.put_sand_rock_dunes(x, y: word);
begin
  if mode(mSand) then
    set_tile_value(x, y, tiles_sand[random(10)], 0, false)
  else if mode(mRock) then
    set_tile_value(x, y, tiles_rock[random(15)], 0, false)
  else if mode(mDunes) then
    set_tile_value(x, y, tiles_dunes[random(8)], 0, false);
end;

procedure TMainWindow.set_tile_value(x, y, tile, special: word; single_op: boolean);
begin
  undo_history[undo_pos].x := x;
  undo_history[undo_pos].y := y;
  undo_history[undo_pos].data := map_data[x,y];
  undo_history[undo_pos].is_first := single_op or undo_block_start;
  undo_block_start := false;
  undo_pos := (undo_pos + 1) and max_undo_steps;
  if undo_start = undo_pos then
    undo_start := (undo_start + 1) and max_undo_steps;
  undo_max := undo_pos;
  Undo1.Enabled := true;
  Redo1.Enabled := false;
  map_data[x,y].tile := tile;
  map_data[x,y].special := special;
end;

procedure TMainWindow.do_undo;
var
  tmp_data: TMapTile;
begin
  if undo_pos = undo_start then
    exit;
  repeat
    undo_pos := (undo_pos - 1) and max_undo_steps;
    tmp_data := map_data[undo_history[undo_pos].x, undo_history[undo_pos].y];
    map_data[undo_history[undo_pos].x, undo_history[undo_pos].y] := undo_history[undo_pos].data;
    undo_history[undo_pos].data := tmp_data;
  until undo_history[undo_pos].is_first or (undo_pos = undo_start);
  if undo_pos = undo_start then
    Undo1.Enabled := false;
  Redo1.Enabled := true;
  render_minimap;
  render_map;
end;

procedure TMainWindow.do_redo;
var
  tmp_data: TMapTile;
begin
  if undo_pos = undo_max then
    exit;
  repeat
    tmp_data := map_data[undo_history[undo_pos].x, undo_history[undo_pos].y];
    map_data[undo_history[undo_pos].x, undo_history[undo_pos].y] := undo_history[undo_pos].data;
    undo_history[undo_pos].data := tmp_data;
    undo_pos := (undo_pos + 1) and max_undo_steps;
  until undo_history[undo_pos].is_first or (undo_pos = undo_max);
  if undo_pos = undo_max then
    Redo1.Enabled := false;
  Undo1.Enabled := true;
  render_minimap;
  render_map;
end;

procedure TMainWindow.reset_undo_history;
begin
  Undo1.Enabled := false;
  Redo1.Enabled := false;
  undo_start := 0;
  undo_max := 0;
  undo_pos := 0;
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

procedure TMainWindow.draw_cursor_image;
var
  x, y: integer;
  tile_x, tile_y: word;
  border_x, border_y: integer;
begin
  border_x := (128 - block_width * 32) div 2;
  border_y := (128 - block_height * 32) div 2;
  BlockImage.Canvas.Brush.Color := clBtnFace;
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
      CursorImage.Canvas.CopyRect(rect(x*32,y*32, x*32+32, y*32+32), Tileset.tileimage.Canvas,rect(tile_x*32, tile_y*32, tile_x*32+32, tile_y*32+32));
    end;
  CursorImage.Canvas.Pen.Color := clBlue;
  CursorImage.Canvas.Brush.Style := bsClear;
  CursorImage.Canvas.Rectangle(0, 0, block_width * 32 + 1, block_height * 32 + 1);
  resize_cursor_image;
end;

procedure TMainWindow.apply_key_preset(num: integer; count_cliff: integer; count_border: integer);
var
  grp: integer;
  count: integer;
begin
  grp := BlockPresetGroupSelect.ItemIndex;
  if (grp = 0) or (grp = 1) then
    count := count_cliff
  else if grp = 2 then
    count := count_border
  else
    count := 1;
  if count > 1 then
  begin
    inc(block_preset_order);
    if block_preset_order >= count then
      block_preset_order := 0;
    num := num + block_preset_order;
  end;
  select_block_from_tileset(
    block_key_presets[num, grp, 0],
    block_key_presets[num, grp, 1],
    block_key_presets[num, grp, 2],
    block_key_presets[num, grp, 3]);
end;

function TMainWindow.is_rock_dunes(x, y: integer; exact: boolean): boolean;
var
  atr: cardinal;
begin
  if x < 0 then x := 0;
  if y < 0 then y := 0;
  if x >= map_width then x := map_width - 1;
  if y >= map_width then y := map_height - 1;
  atr := Tileset.attributes[map_data[x,y].tile];
  result := false;
  if mode(mRock) and exact then // Tile is exactly clear rock
    result := (atr and 2) = 2;
  if mode(mRock) and not exact then // Tile is part of rock area
    result := (atr and 16) = 16;
  if mode(mDunes) and exact then // Tile is exactly clear dunes
    result := (atr and 4) = 4;
  if mode(mDunes) and not exact then // Tile is part of dunes area
    result := (atr and 8) = 8;
end;

procedure TMainWindow.put_edge_block(var xpos, ypos: integer; moveoff_x,
  moveoff_y, blockoff_x, blockoff_y, block_preset, variants_rock: integer);
var
  preset_type: integer;
  x, y: integer;
begin
  // Reuse already defined block-key-presets for this purpose
  preset_type := 3;
  if mode(mRock) then
  begin
    preset_type := 2;
    block_preset := block_preset + random(variants_rock);
  end;
  // Place edge block (it can be either 1x1 or 2x2, so we use loops)
  for y := 0 to block_key_presets[block_preset, preset_type, 1] - 1 do
    for x := 0 to 0 + block_key_presets[block_preset, preset_type, 0] - 1 do
    begin
      // We cannot place the edge block immediately physically into map,
      // because it would interfere with checks for tiles around following tile.
      // Instead, we exploit undo feature for this purpose: we store all changes
      // into history and in the end we apply the changes (like doing redo)
      undo_history[undo_max].x := x + xpos + blockoff_x;
      undo_history[undo_max].y := y + ypos + blockoff_y;
      undo_history[undo_max].data.tile := (block_key_presets[block_preset, preset_type, 3] + y) * 20 + block_key_presets[block_preset, preset_type, 2] + x;
      undo_history[undo_max].data.special := 0;
      undo_history[undo_max].is_first := false;
      undo_max := (undo_max + 1) and max_undo_steps;
    end;
  // Finally move to next position (anticlockwise direction)
  xpos := xpos + moveoff_x;
  ypos := ypos + moveoff_y;
end;

procedure TMainWindow.smooth_edges(x, y: integer);
var
  start_x, start_y: integer;
  sum: integer;
  steps: integer;
begin
  start_x := x;
  start_y := y;
  undo_max := undo_pos;
  steps := 0;
  // Start smoothing edge from starting point (where user shift-clicked)
  while is_rock_dunes(x, y, true) do
  begin
    // Check for all 8 tiles around current tile to determine the direction of edge
    sum := 0;
    if is_rock_dunes(x,   y-1, false) then sum := sum + 1;   // 16 1 32
    if is_rock_dunes(x-1, y  , false) then sum := sum + 2;   //  2 X 4
    if is_rock_dunes(x+1, y  , false) then sum := sum + 4;   // 64 8 128
    if is_rock_dunes(x  , y+1, false) then sum := sum + 8;
    if is_rock_dunes(x-1, y-1, false) then sum := sum + 16;
    if is_rock_dunes(x+1, y-1, false) then sum := sum + 32;
    if is_rock_dunes(x-1, y+1, false) then sum := sum + 64;
    if is_rock_dunes(x+1, y+1, false) then sum := sum + 128;
    // Transform current tile into edge tile and move to following tile
    case (sum and 15) of
       7: begin // down
          if (sum and 128 > 0) and not is_rock_dunes(x+1,y+2, false) then
            put_edge_block(x,y,2,1,0,0,25,1)
          else
            put_edge_block(x,y,1,0,0,0,26,3);
        end;
      11: begin // right
          if (sum and 32 > 0) and not is_rock_dunes(x+2,y-1, false) then
            put_edge_block(x,y,1,-2,0,-1,23,1)
          else
            put_edge_block(x,y,0,-1,0,0,19,4);
        end;
      14: begin // up
          if (sum and 16 > 0) and not is_rock_dunes(x-1,y-2, false) then
            put_edge_block(x,y,-2,-1,-1,-1,8,1)
          else
            put_edge_block(x,y,-1,0,0,0,3,5);
        end;
      13: begin // left
          if (sum and 64 > 0) and not is_rock_dunes(x-2,y+1, false) then
            put_edge_block(x,y,-1,2,-1,0,11,1)
          else
            put_edge_block(x,y,0,1,0,0,12,5);
        end;
       3: begin // down-right corner
          if (sum and 32 > 0) and is_rock_dunes(x+2,y-1, false) then
            put_edge_block(x,y,2,-1,0,-1,30,1)
          else
            put_edge_block(x,y,0,-1,0,0,31,1);
        end;
      10: begin // up-right corner
          if (sum and 16 > 0) and is_rock_dunes(x-1,y-2, false) then
            put_edge_block(x,y,-1,-2,-1,-1,18,1)
          else
            put_edge_block(x,y,-1,0,0,0,9,2);
        end;
      12: begin // up-left corner
          if (sum and 64 > 0) and is_rock_dunes(x-2,y+1, false) then
            put_edge_block(x,y,-2,1,-1,0,2,1)
          else
            put_edge_block(x,y,0,1,0,0,1,1);
        end;
       5: begin // down-left corner
          if (sum and 128 > 0) and is_rock_dunes(x+1,y+2, false) then
            put_edge_block(x,y,1,2,0,0,17,1)
          else
            put_edge_block(x,y,1,0,0,0,24,1);
        end;
      15: begin // inner curves
        case sum of
          239: put_edge_block(x,y,-1,0,0,0,35,1); // down-right curve
          191: put_edge_block(x,y,0,1,0,0,34,1);  // up-right curve
          127: put_edge_block(x,y,1,0,0,0,32,1);  // up-left curve
          223: put_edge_block(x,y,0,-1,0,0,33,1); // down-left curve
          else break; // Invalid combination - end
        end;
        end;
      else break; // Invalid combination - end
    end;
    // End if we got back into starting point
    if (x = start_x) and (y = start_y) then
      break;
    // End if we got outside the map
    if (x < 0) or (y < 0) or (x >= map_width) or (y >= map_height) then
      break;
    // Sometimes the algorithm may end up in infinite loop. This is to prevent it.
    inc(steps);
    if steps > 1000 then
      break;
  end;
  undo_history[undo_pos].is_first := true;
  // Finally put smoothed edges on map - apply all changes we stored into undo history
  do_redo;
end;

procedure TMainWindow.set_map_size(new_width, new_height: integer);
var
  i, j: integer;
begin
  if (map_width = new_width) and (map_height = new_height) then
    exit;
  for i := 0 to new_height - 1 do
    for j := 0 to new_width - 1 do
      if (i >= map_height) or (j >= map_width) then
      begin
        map_data[j,i].tile := tiles_sand[random(10)];
        map_data[j,i].special := 0;
      end;
  map_width := new_width;
  map_height := new_height;
  StatusBar.Panels[2].Text := inttostr(map_width)+' x '+inttostr(map_height);
  reset_undo_history;
  calculate_power_and_statistics;
  resize_map_canvas;
  render_minimap;
  render_map;
end;

procedure TMainWindow.shift_map(direction, num_tiles: integer);
var
  x, y: integer;
  src_x, src_y: integer;
begin
  case direction of
    1:  begin // Left
          for y := 0 to map_height - 1 do
            for x := 0 to map_width - 1 do
            begin
              src_x := x + num_tiles;
              if (src_x < map_width) then
                map_data[x,y] := map_data[src_x,y]
              else
              begin
                map_data[x,y].tile := tiles_sand[random(10)];
                map_data[x,y].special := 0;
              end;
            end;
        end;
    2:  begin // Up
          for y := 0 to map_height - 1 do
            for x := 0 to map_width - 1 do
            begin
              src_y := y + num_tiles;
              if (src_y < map_height) then
                map_data[x,y] := map_data[x,src_y]
              else
              begin
                map_data[x,y].tile := tiles_sand[random(10)];
                map_data[x,y].special := 0;
              end;
            end;
        end;
    3:  begin // Right
          for y := map_height - 1 downto 0 do
            for x := map_width - 1 downto 0 do
            begin
              src_x := x - num_tiles;
              if (src_x >= 0) then
                map_data[x,y] := map_data[src_x,y]
              else
              begin
                map_data[x,y].tile := tiles_sand[random(10)];
                map_data[x,y].special := 0;
              end;
            end;
        end;
    4:  begin
          for y := map_height - 1 downto 0 do
            for x := map_width - 1 downto 0 do
            begin
              src_y := y - num_tiles;
              if (src_y >= 0) then
                map_data[x,y] := map_data[x,src_y]
              else
              begin
                map_data[x,y].tile := tiles_sand[random(10)];
                map_data[x,y].special := 0;
              end;
            end;
        end;
  end;
  reset_undo_history;
  calculate_power_and_statistics;
  render_minimap;
  render_map;
end;

procedure TMainWindow.change_structure_owner(player_from,
  player_to: integer; swap: boolean);
var
  x,y: integer;
  player, index: word;
  is_misc: boolean;
begin
  undo_block_start := true;
  for y:= 0 to map_height - 1 do
    for x := 0 to map_width - 1 do
    begin
      if special_value_to_params(map_data[x,y].special, player, index, is_misc) and (not is_misc) then
      begin
        // Change from -> to
        if player = player_from then
        begin
          if player_to = cnt_map_players then
            set_tile_value(x, y, map_data[x,y].tile, 0, false)
          else
            set_tile_value(x, y, map_data[x,y].tile, structure_params[index].values[player_to], false);
        end;
        // Swap is checked (change to -> from)
        if (player = player_to) and swap then
          set_tile_value(x, y, map_data[x,y].tile, structure_params[index].values[player_from], false);
      end;
    end;
  calculate_power_and_statistics;
  render_minimap;
  render_map;
end;

procedure TMainWindow.new_map(new_width, new_height: integer);
var
  x, y: integer;
begin
  // Reset map data
  for x := 0 to 127 do
    for y := 0 to 127 do
    begin
      map_data[x,y].tile := 0;
      map_data[x,y].special := 0;
    end;
  reset_undo_history;
  // Initialize map
  map_width := new_width;
  map_height := new_height;
  for x := 0 to map_width - 1 do
    for y := 0 to map_height - 1 do
    begin
      map_data[x,y].tile := tiles_sand[random(10)];
      map_data[x,y].special := 0;
    end;
  StatusBar.Panels[2].Text := inttostr(map_width)+' x '+inttostr(map_height);
  StatusBar.Panels[3].Text := 'Map not saved';
  map_filename := '';
  map_loaded := true;
  // Clear mission
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
  set_window_titles('');
  calculate_power_and_statistics;
  resize_map_canvas;
  render_minimap;
  render_map;
end;

end.
