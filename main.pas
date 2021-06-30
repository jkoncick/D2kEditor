unit main;

interface

uses
  // System libraries
  Windows, Messages, SysUtils, StrUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, Menus, StdCtrls, XPMan, Math, Spin, Buttons,
  ShellApi, IniFiles, Clipbrd,
  // Units
  _utils, _dispatcher, _renderer, _map, _mission, _missionini, _tileset, _structures, _stringtable, _settings, _randomgen, _launcher;

const brush_size_presets: array[0..7,1..2] of word = ((1,1),(2,2),(3,3),(4,4),(2,1),(1,2),(3,2),(2,3));

type
  SelectedMode = (mStructures, mStructuresPaint, mTerrain, mPaintMode, mBlockMode, mSelectMode);

type
  EventPositionSelectionMode = (epmNone, epmEventCoordinates, epmDefenceArea);

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
    Changetileset1: TMenuItem;
    MiniMap: TImage;
    Settings1: TMenuItem;
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
    Useallocationindexes1: TMenuItem;
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
    LbPaintTileGroupName: TLabel;
    UnitList: TListBox;
    LbUnitList: TLabel;
    N11: TMenuItem;
    Showmapstatistics1: TMenuItem;
    btnFindSelectedObject: TButton;
    sbShowGrid: TSpeedButton;
    sbMarkImpassableTiles: TSpeedButton;
    sbMarkBuildableTiles: TSpeedButton;
    sbMarkOwnerSide: TSpeedButton;
    Recentfiles1: TMenuItem;
    N6: TMenuItem;
    GridColorDialog: TColorDialog;
    N7: TMenuItem;
    Gridcolor1: TMenuItem;
    Alwaysaskonquit1: TMenuItem;
    Hidepresetwindow1: TMenuItem;
    More1: TMenuItem;
    TileAttributeseditor1: TMenuItem;
    Restrictpainting1: TMenuItem;
    Saveminimapimage1: TMenuItem;
    Reloadtileset1: TMenuItem;
    cbSelectAreaType: TComboBox;
    lbSelectAreaType: TLabel;
    FindDune2000Dialog: TOpenDialog;
    Showunknownspecials1: TMenuItem;
    Missionlauncher1: TMenuItem;
    N13: TMenuItem;
    RemapTilesOpenDialog: TOpenDialog;
    Remaptiles1: TMenuItem;
    Userandompaintmap1: TMenuItem;
    LbStructureName: TLabel;
    Structures1: TMenuItem;
    Structureseditor1: TMenuItem;
    Translatestructurenames1: TMenuItem;
    N15: TMenuItem;
    Debugwindow1: TMenuItem;
    N12: TMenuItem;
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
    procedure OpenRecentFile(Sender: TObject);
    procedure Savemap1Click(Sender: TObject);
    procedure Savemapas1Click(Sender: TObject);
    procedure Savemapimage1Click(Sender: TObject);
    procedure Saveminimapimage1Click(Sender: TObject);
    procedure Missionlauncher1Click(Sender: TObject);
    procedure Exit1Click(Sender: TObject);
    procedure Undo1Click(Sender: TObject);
    procedure Redo1Click(Sender: TObject);
    procedure Copy1Click(Sender: TObject);
    procedure Paste1Click(Sender: TObject);
    procedure Changetileset1Click(Sender: TObject);
    procedure Selectnext1Click(Sender: TObject);
    procedure Reloadtileset1Click(Sender: TObject);
    procedure Loadtileset1Click(Sender: TObject);
    procedure Loadtilesetattributes1Click(Sender: TObject);
    procedure TileAttributeseditor1Click(Sender: TObject);
    procedure Structureseditor1Click(Sender: TObject);
    procedure SettingChange(Sender: TObject);
    procedure More1Click(Sender: TObject);
    procedure Setmapsize1Click(Sender: TObject);
    procedure Shiftmap1Click(Sender: TObject);
    procedure Changestructureowner1Click(Sender: TObject);
    procedure Remaptiles1Click(Sender: TObject);
    procedure Showmapstatistics1Click(Sender: TObject);
    procedure EventsandConditions1Click(Sender: TObject);
    procedure Missionsettings1Click(Sender: TObject);
    procedure Assignmisfile1Click(Sender: TObject);
    procedure Quicklaunch1Click(Sender: TObject);
    procedure Launchwithsettings1Click(Sender: TObject);
    procedure OpenHelpDoc(Sender: TObject);
    procedure KeyShortcuts1Click(Sender: TObject);
    procedure Mouseactions1Click(Sender: TObject);
    procedure Debugwindow1Click(Sender: TObject);
    procedure About1Click(Sender: TObject);
    // Main form components events
    procedure MapScrollChange(Sender: TObject);
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
    procedure ImageMouseLeave(Sender: TObject);
    // Editor menu components events
    procedure MiniMapMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure MiniMapMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    // Structure/terrain editor
    procedure EditorPagesChange(Sender: TObject);
    procedure SpecialValueChange(Sender: TObject);
    procedure btnFindSelectedObjectClick(Sender: TObject);
    procedure BuildingListClick(Sender: TObject);
    procedure UnitListClick(Sender: TObject);
    procedure PlayerSelectChange(Sender: TObject);
    procedure MiscObjListClick(Sender: TObject);
    procedure OpenTilesetClick(Sender: TObject);
    procedure BlockImageClick(Sender: TObject);
    procedure RbTerrainModeClick(Sender: TObject);
    procedure PaintTileSelectClick(Sender: TObject);
    procedure PaintTileSelectDblClick(Sender: TObject);
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
    paint_tile_select: array[-4..cnt_paint_tile_groups-1] of TSpeedButton;
    paint_tile_select_active: TSpeedButton;
    block_preset_group: integer;
    block_preset_select: array[0..cnt_block_preset_groups-1] of TSpeedButton;
    block_preset_dialog_opened: boolean;
    cur_preset_index: integer;

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

    // Event position selection variables
    event_position_selection_mode: EventPositionSelectionMode;

    // Clipboard variables
    clipboard_format: cardinal;

    // Dynamic menu items
    recent_files_menuitems: array[1..cnt_recent_files] of TMenuItem;
    help_doc_filenames: TStringList;

    // Others
    special_value_changing: boolean;
    editing_marker_disabled: boolean;

    // Dispatcher procedures
    procedure update_structures_list(building_list, unit_list: TStringList);
    procedure update_player_list(player_list: TStringList);
    procedure update_misc_object_list;
    procedure update_tileset;
    procedure update_structure_controls;
    procedure update_map_dimensions;
    procedure update_map_name;
    procedure update_mission_load_status;
    procedure update_map_stats;

    // Rendering procedures
    procedure resize_map_canvas;
    procedure render_map;
    procedure render_minimap;
    procedure render_minimap_position_marker;
    procedure render_editing_marker;
    procedure resize_cursor_image;
    procedure render_cursor_image;

    // Miscellaneous helper procedures
    procedure refresh_recent_files_menu;
    procedure set_special_value;
    function mode(m: SelectedMode): boolean;
    function mouse_over_map_canvas: boolean;
    procedure apply_key_preset(key: word);
    procedure start_event_position_selection(x, y: integer; mode: EventPositionSelectionMode);
    procedure finish_event_position_selection(min_x, max_x, min_y, max_y: integer);

    // Procedures related to selecting/placing block
    procedure select_block_from_tileset(b_width, b_height, b_left, b_top: word);
    procedure select_block_preset(preset_index: integer);
    procedure copy_block_from_map(b_width, b_height, b_left, b_top: word; structures, erase: boolean; area_type: integer);
  end;

var
  MainWindow: TMainWindow;

implementation

uses
  // Dialogs
  set_dialog, tileset_dialog, block_preset_dialog, test_map_dialog, event_dialog, mission_dialog, map_stats_dialog, mission_launcher, tileatr_editor, structures_editor, debug_window;

{$R *.dfm}

procedure TMainWindow.FormCreate(Sender: TObject);
var
  i: integer;
  btn: TSpeedButton;
  tmp_strings: TStringList;
  SR: TSearchRec;
  menuitem: TMenuItem;
begin
  // Load GUI setings
  Settings.load_window_position(self);
  CbSelectStructures.State := TCheckBoxState(Settings.load_control_property_int(CbSelectStructures, 'State', Ord(CbSelectStructures.State)));
  // Miscellaneous initializations
  DragAcceptFiles(Handle, True);
  clipboard_format := RegisterClipboardFormat('D2kEditorBlock');
  moving_event := -1;
  moving_condition := -1;
  event_position_selection_mode := epmNone;
  top := 40;
  CursorImage.Picture.Bitmap.TransparentColor := clBtnFace;
  // Initialize terrain editing controls
  tmp_strings := TStringList.Create;
  for i := 0 to Length(brush_size_presets) - 1 do
    tmp_strings.Add(inttostr(brush_size_presets[i,1]) + ' x ' + inttostr(brush_size_presets[i,2]));
  cbBrushSize.Items := tmp_strings;
  cbBrushSize.ItemIndex := 0;
  tmp_strings.Destroy;
  for i := -4 to cnt_paint_tile_groups-1 do
  begin
    btn := TSpeedButton.Create(self);
    btn.Tag := i;
    btn.GroupIndex := 1;
    btn.Top := 48 + ((i+4) div 4) * 38;
    btn.Left := 2 + ((i+4) mod 4) * 38;
    btn.Width := 38;
    btn.Height := 38;
    btn.Glyph.Width := 28;
    btn.Glyph.Height := 28;
    btn.ShowHint := True;
    btn.OnClick := PaintTileSelectClick;
    btn.OnDblClick := PaintTileSelectDblClick;
    btn.Parent := PageTerrain;
    paint_tile_select[i] := btn;
  end;
  for i := 0 to cnt_block_preset_groups-1 do
  begin
    btn := TSpeedButton.Create(self);
    btn.Tag := i;
    btn.GroupIndex := 2;
    btn.Top := 396 + 20 * (i mod (cnt_block_preset_groups div 2));
    btn.Left := 2 + 76 * (i div (cnt_block_preset_groups div 2));
    btn.Width := 76;
    btn.Height := 20;
    btn.OnClick := BlockPresetGroupSelectClick;
    btn.OnDblClick := BlockImageClick;
    btn.Parent := PageTerrain;
    block_preset_select[i] := btn;
  end;
  block_preset_select[0].Down := True;
  // First time run intro message
  if (Settings.GamePath = '') then
  begin
    Show;
    if Application.MessageBox(
      'This program requires original Dune 2000 graphics files to work.'#13+
      'It needs to know where Dune 2000 is located on your computer and will load graphics from that location.'#13+
      'Press YES button if you want to navigate to your Dune 2000 game location and configure it now.'#13+
      'Press NO button if you want to configure it later. Program will fail with errors and will not be usable!',
      'First time run', MB_YESNO or MB_ICONINFORMATION) = IDYES then
    begin
      if FindDune2000Dialog.Execute then
      begin
        Settings.determine_game_paths_from_path(FindDune2000Dialog.FileName);
        if Settings.GamePath <> '' then
          Application.MessageBox('Game path configured.', 'First time run', MB_OK or MB_ICONINFORMATION);
      end;
    end;
  end;
  // Initialize minimap buffer
  minimap_buffer := TBitmap.Create;
  minimap_buffer.Width := MiniMap.Width;
  minimap_buffer.Height := MiniMap.Height;
  minimap_buffer.PixelFormat := pf32bit;
  // Initialize Random Generator
  //--RandomGen.init(Memo1.Lines);
  // Initialize recent files menu items
  for i := 1 to cnt_recent_files do
  begin
    recent_files_menuitems[i] := TMenuItem.Create(MainWindow.Recentfiles1);
    recent_files_menuitems[i].Caption := '';
    recent_files_menuitems[i].Tag := i;
    recent_files_menuitems[i].Visible := false;
    recent_files_menuitems[i].OnClick := OpenRecentFile;
    Recentfiles1.Add(recent_files_menuitems[i]);
  end;
  refresh_recent_files_menu;
  // Initialize settings menu items
  Useallocationindexes1.Checked := Settings.UseAllocationIndexes;
  Showeventmarkers1.Checked := Settings.ShowEventMarkers;
  Markdefenceareas1.Checked := Settings.MarkDefenceAreas;
  Showunknownspecials1.Checked := Settings.ShowUnknownSpecials;
  GridColorDialog.Color := Settings.GridColor;
  Alwaysaskonquit1.Checked := Settings.AlwaysAskOnQuit;
  Hidepresetwindow1.Checked := Settings.HidePresetWindow;
  Restrictpainting1.Checked := Settings.RestrictPainting;
  Userandompaintmap1.Checked := Settings.UseRandomPaintMap;
  Translatestructurenames1.Checked := Settings.TranslateStructureNames;
  // Initialize documents in help menu
  help_doc_filenames := TStringList.Create;
  if FindFirst(current_dir + 'doc\*', 0, SR) = 0 then
  begin
    repeat
      menuitem := TMenuItem.Create(Help1);
      menuitem.Caption := ChangeFileExt(SR.Name, '');
      menuitem.Tag := help_doc_filenames.Count;
      menuitem.OnClick := OpenHelpDoc;
      Help1.Insert(0, menuitem);
      help_doc_filenames.Add(SR.Name);
    until FindNext(SR) <> 0;
    FindClose(SR);
  end;
end;

procedure TMainWindow.FormDestroy(Sender: TObject);
begin
  DragAcceptFiles(Handle, False);
end;

procedure TMainWindow.FormClose(Sender: TObject; var Action: TCloseAction);
var
  dialog_result: integer;
begin
  if Launcher.check_game_is_running then
  begin
    action := caNone;
    exit;
  end;
  if Settings.AlwaysAskOnQuit and Map.check_map_modified then
  begin
    dialog_result := Application.MessageBox('Do you want to save your map changes before exiting the program?','Save changes?', MB_YESNOCANCEL or MB_ICONQUESTION);
    if dialog_result = IDCANCEL then
    begin
      Action := caNone;
      exit;
    end else
    if dialog_result = IDYES then
    begin
      Savemap1Click(nil);
    end;
  end;
  Settings.save_editor_settings;
  MainWindow.OnResize := nil;
  TileAtrEditor.OnResize := nil;
end;

procedure TMainWindow.FormResize(Sender: TObject);
var
  tmp_height: integer;
begin
  resize_map_canvas;
  EditorMenu.Left := ClientWidth - 168;
  EditorMenu.Height := ClientHeight - StatusBar.Height;
  tmp_height := EditorMenu.Height - 404;
  BuildingList.Height := tmp_height div 2;
  UnitList.Height := tmp_height div 2;
  LbUnitList.Top := BuildingList.Top + BuildingList.Height + 3;
  UnitList.Top := LbUnitList.Top + 16;
  EditorPages.Height := EditorMenu.Height - 168;
  StatusBar.Panels[3].Width := ClientWidth - 590;
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
  Map.load_map(filename);
  DragFinish(Msg.Drop);
end;

procedure TMainWindow.CMDialogKey(var AMessage: TCMDialogKey);
begin
  // Arrow keys
  if (AMessage.CharCode >= 37) and (AMessage.CharCode <= 40) then
  begin
    FormKeyDown(nil, AMessage.CharCode, []);
    AMessage.Result := 1;
  end else
  // TAB
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
  mouse_already_clicked := false;
  case key of
    {--106:
      begin RandomGen.reset; render_minimap; render_map;  end;
    109: //KP-
      begin RandomGen.manual_undo(ssShift in Shift); render_map; end;
    107: //KP+
      begin RandomGen.manual_step(0); render_map; end;
    111: //KP/
      Memo1.Lines.Clear;}
    // Esc: cancel event position selection
    27:
    begin
      if not EditorPages.Visible then
        finish_event_position_selection(-1,-1,-1,-1);
    end;
    // Space: open tileset/preset window
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
    // The key under Esc:
    192: begin
      if mode(mStructures) then
      begin
        MiscObjList.ItemIndex := 0;
        MiscObjListClick(nil);
        MiscObjList.SetFocus;
      end else
      begin
        if Tileset.block_preset_groups[block_preset_group].paint_group <> -5 then
          paint_tile_group := Tileset.block_preset_groups[block_preset_group].paint_group;
        paint_tile_select[paint_tile_group].Down := true;
        PaintTileSelectClick(paint_tile_select[paint_tile_group]);
      end;
    end;
  end;
  if (key >= 37) and (key <= 40) then
  begin
    // Arrow keys
    if not ((ActiveControl = MiscObjList) or (ActiveControl = BuildingList) or (ActiveControl = UnitList)) then
    begin
      // Scroll map
      if ssShift in Shift then
        case key of
          37: MapScrollH.Position := 0;
          38: MapScrollV.Position := 0;
          39: MapScrollH.Position := Map.width;
          40: MapScrollV.Position := Map.height;
          end
      else
        case key of
          37: MapScrollH.Position := MapScrollH.Position-1;
          38: MapScrollV.Position := MapScrollV.Position-1;
          39: MapScrollH.Position := MapScrollH.Position+1;
          40: MapScrollV.Position := MapScrollV.Position+1;
          end;
      key := 0;
    end else
    if (key = 37) or (key = 39) then
    begin
      // Switch between building list, unit list and misc. object list
      MiscObjList.ItemIndex := -1;
      BuildingList.ItemIndex := -1;
      UnitList.ItemIndex := -1;
      if ((key = 39) and (ActiveControl = MiscObjList)) or ((key = 37) and (ActiveControl = UnitList)) then
      begin
        BuildingList.ItemIndex := 0;
        BuildingList.SetFocus;
      end else
      if key = 37 then
      begin
        MiscObjList.ItemIndex := 0;
        MiscObjList.SetFocus
      end else
      begin
        UnitList.ItemIndex := 0;
        UnitList.SetFocus;
      end;
      key := 0;
      set_special_value;
    end;
  end;
  if mode(mBlockMode) then
  case key of
    // Move cursor image
    98:  {Num2} begin CursorImage.Top := CursorImage.Top + 32; resize_cursor_image; end;
    100: {Num4} begin CursorImage.Left := CursorImage.Left - 32; resize_cursor_image; end;
    102: {Num6} begin CursorImage.Left := CursorImage.Left + 32; resize_cursor_image; end;
    104: {Num8} begin CursorImage.Top := CursorImage.Top - 32; resize_cursor_image; end;
    101: {Num5} begin MapCanvasMouseDown(nil, mbLeft, [], CursorImage.Top,CursorImage.Left) end;
    end
  else if mode(mStructures) and (ActiveControl <> SpecialValue) and (key >= 96) and (key < 96 + cnt_players) then
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
    begin
      cbBrushSize.ItemIndex := key - ord('1');
      render_editing_marker;
    end;
    case key of
    // Structures editing mode selection
    ord('E'): begin MiscObjList.ItemIndex := 3; MiscObjListClick(nil); EditorPages.TabIndex := 0; EditorPages.SetFocus; end;
    // Terrain editing mode selection
    ord('Q'): begin EditorPages.TabIndex := 1; paint_tile_select[-4].Down := true; PaintTileSelectClick(paint_tile_select[-4]); end;
    ord('W'): begin EditorPages.TabIndex := 1; paint_tile_select[-3].Down := true; PaintTileSelectClick(paint_tile_select[-3]); end;
    ord('S'): begin EditorPages.TabIndex := 1; paint_tile_select[0].Down := true; PaintTileSelectClick(paint_tile_select[0]); end;
    ord('R'): begin EditorPages.TabIndex := 1; paint_tile_select[1].Down := true; PaintTileSelectClick(paint_tile_select[1]); end;
    ord('D'): begin EditorPages.TabIndex := 1; paint_tile_select[2].Down := true; PaintTileSelectClick(paint_tile_select[2]); end;
    ord('C'): begin EditorPages.TabIndex := 1; RbSelectMode.Checked := true; end;
    ord('T'): CbSelectStructures.State := TCheckBoxState((Ord(CbSelectStructures.State) + 1) mod 3);
    ord('B'): begin EditorPages.TabIndex := 1; RbBlockMode.Checked := true; end;
    end;
  end else
  // Ctrl+key
  if ssCtrl in Shift then
  begin
    case key of
    ord('F'): btnFindSelectedObjectClick(nil);
    ord('G'): begin sbShowGrid.Down := not sbShowGrid.Down; SettingChange(sbShowGrid); end;
    ord('M'): begin sbMarkImpassableTiles.Down := not sbMarkImpassableTiles.Down; SettingChange(sbMarkImpassableTiles); end;
    ord('B'): begin sbMarkBuildableTiles.Down := not sbMarkBuildableTiles.Down; SettingChange(sbMarkBuildableTiles); end;
    ord('W'): begin sbMarkOwnerSide.Down := not sbMarkOwnerSide.Down; SettingChange(sbMarkOwnerSide); end;
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
    Handled := true;
    // If Ctrl is held and in paint mode, change brush size
    if (ssShift in Shift) and mode(mTerrain) then
    begin
      cbBrushSize.ItemIndex := Min(cbBrushSize.ItemIndex + 1, 3);
      render_editing_marker;
      exit;
    end;
    // Scroll map
    if (MousePos.Y - top - 50) < (mapCanvas.Height - (mapCanvas.Height div 8))
    then
      MapScrollV.Position := MapScrollV.Position - 2
    else
      MapScrollH.Position := MapScrollH.Position - 2;
  end;
end;

procedure TMainWindow.FormMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  if (MousePos.X - left) < (mapCanvas.Width + 30) then
  begin
    Handled := true;
    // If Ctrl is held and in paint mode, change brush size
    if (ssShift in Shift) and mode(mTerrain) then
    begin
      cbBrushSize.ItemIndex := Max(cbBrushSize.ItemIndex - 1, 0);
      render_editing_marker;
      exit;
    end;
    // Scroll map
    if (MousePos.Y - top - 50) < (mapCanvas.Height - (mapCanvas.Height div 8))
    then
      MapScrollV.Position := MapScrollV.Position + 2
    else
      MapScrollH.Position := MapScrollH.Position + 2;
  end;
end;

procedure TMainWindow.Newmap1Click(Sender: TObject);
begin
  if SetDialog.select_menu(5) = mrOk then
    SetDialog.select_menu(4);
end;

procedure TMainWindow.Openmap1Click(Sender: TObject);
begin
  if MapOpenDialog.Execute then
    Map.load_map(MapOpenDialog.FileName);
end;

procedure TMainWindow.Reopenmap1Click(Sender: TObject);
begin
  if Map.loaded and (Map.filename <> '') then
    Map.load_map(Map.filename)
  else if (not Map.loaded) and (Settings.RecentFiles[1] <> '') then
    Map.load_map(Settings.RecentFiles[1]);
end;

procedure TMainWindow.OpenRecentFile(Sender: TObject);
begin
  Map.load_map(Settings.RecentFiles[(Sender as TMenuItem).Tag]);
end;

procedure TMainWindow.Savemap1Click(Sender: TObject);
begin
  if not Map.loaded then
    exit;
  if Map.filename = '' then
    Savemapas1Click(Sender)
  else begin
    if Settings.CheckMapErrorsOnSave then
      Map.check_errors;
    Map.save_map(Map.filename, false);
  end;
end;

procedure TMainWindow.Savemapas1Click(Sender: TObject);
begin
  if not Map.loaded then
    exit;
  if Settings.CheckMapErrorsOnSave then
    Map.check_errors;
  if MapSaveDialog.Execute then
    Map.save_map(MapSaveDialog.FileName, false);
end;

procedure TMainWindow.Savemapimage1Click(Sender: TObject);
var
  tmp_bitmap: TBitmap;
begin
  if not Map.loaded then
    exit;
  MapImageSaveDialog.Title := 'Save map image';
  if MapImageSaveDialog.Execute then
  begin
    tmp_bitmap := TBitmap.Create;
    tmp_bitmap.Width := Map.width * 32;
    tmp_bitmap.Height := Map.height * 32;
    Renderer.render_map_contents(tmp_bitmap.Canvas, 0, 0, Map.width, Map.height, Addr(Map.data), Map.width, Map.height,
      sbShowGrid.Down, sbMarkImpassableTiles.Down, sbMarkBuildableTiles.Down, sbMarkOwnerSide.Down,
      Useallocationindexes1.Checked, Showeventmarkers1.Checked, Markdefenceareas1.Checked, Showunknownspecials1.Checked,
      false);
    tmp_bitmap.SaveToFile(MapImageSaveDialog.FileName);
    tmp_bitmap.Destroy;
  end;
end;

procedure TMainWindow.Saveminimapimage1Click(Sender: TObject);
begin
  if not Map.loaded then
    exit;
  MapImageSaveDialog.Title := 'Save minimap image';
  if MapImageSaveDialog.Execute then
  begin
    minimap_buffer.SaveToFile(MapImageSaveDialog.FileName);
  end;
end;

procedure TMainWindow.Missionlauncher1Click(Sender: TObject);
begin
  MissionLauncher.Show;
end;

procedure TMainWindow.Exit1Click(Sender: TObject);
begin
  close;
end;

procedure TMainWindow.Undo1Click(Sender: TObject);
begin
  Map.do_undo;
  mouse_already_clicked := false;
end;

procedure TMainWindow.Redo1Click(Sender: TObject);
begin
  Map.do_redo;
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
  render_cursor_image;

  GlobalUnLock(handle);
  CloseClipboard;
end;

procedure TMainWindow.Changetileset1Click(Sender: TObject);
begin
  SetDialog.select_menu(5);
end;

procedure TMainWindow.Selectnext1Click(Sender: TObject);
begin
  Tileset.change_tileset_next;
end;

procedure TMainWindow.Reloadtileset1Click(Sender: TObject);
begin
  Tileset.load_tileset(true);
end;

procedure TMainWindow.Loadtileset1Click(Sender: TObject);
begin
  if TilesetOpenDialog.Execute then
    Tileset.load_tileimage_from_file(TilesetOpenDialog.FileName);
end;

procedure TMainWindow.Loadtilesetattributes1Click(Sender: TObject);
begin
  if TileatrOpenDialog.Execute then
    Tileset.load_tileatr_from_file(TileatrOpenDialog.FileName);
end;

procedure TMainWindow.TileAttributeseditor1Click(Sender: TObject);
begin
  TileAtrEditor.Show;
end;

procedure TMainWindow.Structureseditor1Click(Sender: TObject);
begin
  StructuresEditor.Show;
end;

procedure TMainWindow.SettingChange(Sender: TObject);
begin
  case (Sender as TComponent).Tag of
  1: begin Settings.UseAllocationIndexes := (Sender as TMenuItem).Checked; render_map; render_minimap; render_cursor_image; end;
  2: begin Settings.ShowEventMarkers := (Sender as TMenuItem).Checked; render_map; end;
  3: begin Settings.MarkDefenceAreas := (Sender as TMenuItem).Checked; render_map; end;
  4: begin Settings.ShowUnknownSpecials := (Sender as TMenuItem).Checked; render_map; render_cursor_image; end;
  11: begin Settings.AlwaysAskOnQuit := (Sender as TMenuItem).Checked end;
  12: begin Settings.HidePresetWindow := (Sender as TMenuItem).Checked end;
  13: begin Settings.RestrictPainting := (Sender as TMenuItem).Checked end;
  14: begin Settings.UseRandomPaintMap := (Sender as TMenuItem).Checked end;
  15: begin Settings.TranslateStructureNames := (Sender as TMenuItem).Checked; Dispatcher.register_event(evSCTranslateStructureNames); end;
  20:
    begin
      if GridColorDialog.Execute then
      begin
        Settings.GridColor := GridColorDialog.Color;
        if sbShowGrid.Down then
          render_map;
        TileAtrEditor.update_grid_color;
        TilesetDialog.update_grid_color;
      end;
    end;
  24:
    begin
      render_map;
      render_cursor_image;
    end;
  else render_map;
  end;
end;

procedure TMainWindow.More1Click(Sender: TObject);
begin
  ShowMessage('To change more advanced settings and preferences,'#13'edit the file D2kEditor.ini directly in a text editor.'#13#13+
              'The file is saved each time you close the program.'#13'Close the program before you do any changes.');
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

procedure TMainWindow.Remaptiles1Click(Sender: TObject);
begin
  if not Map.loaded then
    exit;
  if RemapTilesOpenDialog.Execute then
    if not Map.remap_tiles(RemapTilesOpenDialog.FileName) then
      Application.MessageBox('The ini file must contain at least one of [Remap_Tiles] and [Remap_Specials] sections'#13'with key-value pairs in the form'#13'from_index=to_index'#13'where key and value is a tile/special index.', 'Invalid remap tiles ini file', MB_OK or MB_ICONERROR);
end;

procedure TMainWindow.Showmapstatistics1Click(Sender: TObject);
begin
  MapStatsDialog.Show;
end;

procedure TMainWindow.EventsandConditions1Click(Sender: TObject);
begin
  EventDialog.Show;
end;

procedure TMainWindow.Missionsettings1Click(Sender: TObject);
begin
  MissionDialog.Show;
end;

procedure TMainWindow.Assignmisfile1Click(Sender: TObject);
var
  msg: string;
begin
  if not Map.loaded then
    exit;
  if Mission.mis_assigned then
  begin
    msg := 'Warning: This action will erase all Mission data.'#13;
    if Mission.mis_filename <> '' then
    begin
      msg := msg + 'When you save this map, the mission file ' + ExtractFileName(Mission.mis_filename);
      if MissionIni.mission_ini_filename <> '' then
        msg := msg + ' and ini file ' + ExtractFileName(MissionIni.mission_ini_filename);
      msg := msg + ' will be deleted and data will be lost!'#13
    end;
    msg := msg + 'Do you want to continue?'#13;
    if Application.MessageBox(PChar(msg), 'Unassign mission file', MB_YESNO or MB_ICONWARNING) = IDYES then
      Mission.unload_mission;
  end else
    Mission.assign_mission;
end;

procedure TMainWindow.Quicklaunch1Click(Sender: TObject);
begin
  if not Launcher.check_map_can_be_tested then
    exit;
  Launcher.launch_current_mission;
end;

procedure TMainWindow.Launchwithsettings1Click(Sender: TObject);
begin
  if not Launcher.check_map_can_be_tested then
    exit;
  if TestMapDialog.invoke = mrOk then
    Launcher.launch_current_mission;
end;

procedure TMainWindow.OpenHelpDoc(Sender: TObject);
begin
  ShellExecuteA(0, 'open', PChar(current_dir + 'doc\' + help_doc_filenames[(Sender as TMenuItem).Tag]), '', PChar(current_dir), SW_SHOWNORMAL);
end;

procedure TMainWindow.KeyShortcuts1Click(Sender: TObject);
begin
  ShowMessage('Key Shortcuts:'#13#13+
              'Arrow keys = Scroll map'#13+
              'Space = Open tileset/preset window'#13+
              'Tab = Switch Structures / Terrain mode'#13+
              'Shift + Q = Thin spice'#13+
              'Shift + W = Thick spice'#13+
              'Shift + E = Spice bloom'#13+
              'Shift + S = Paint sand'#13+
              'Shift + R = Paint rock'#13+
              'Shift + D = Paint dunes'#13+
              'Shift + B = Block mode'#13+
              'Shift + C = Select mode'#13#13+
              'In Terrain mode:'#13+
              '0-9, A-Z = Block preset'#13+
              '` (key under Esc) = Switch to paint mode'#13+
              'F1 - F4 = Change block preset group'#13+
              'Shift + 1 - 8 = Change brush size'#13+
              'Shift + T = Toggle select structures'#13+
              'Num 2,4,6,8 = Move block on map'#13+
              'Num 5 = Place block'#13#13+
              'In Structures mode:'#13+
              'Num 0 - Num 7 = Select player');
end;

procedure TMainWindow.Mouseactions1Click(Sender: TObject);
begin
  ShowMessage('Mouse actions'#13#13+
              'In Structures mode:'#13+
              'Left = Place structure'#13+
              'Right = Remove structure'#13+
              'Middle = Copy structure'#13#13+
              'Event marker in Struct. mode:'#13+
              'Double click = Go to event'#13+
              'Left+move = Move event'#13#13+
              'In Terrain mode:'#13+
              'Left = Paint / Place block'#13+
              'Double click = Fill area'#13+
              'Shift+click = Auto-smooth edge'#13+
              'Middle = Copy block'#13+
              'Right+move = Scroll map'#13+
              'Hold Ctrl = Switch to select mode'#13+
              'Shift+wheel = Change brush size'#13+
              'Shift+select = Erase selection'#13#13+
              'On Block preset window:'#13+
              'Left = Select block'#13+
              'Right = Next variant'#13+
              'Middle = Show / hide keys');
end;

procedure TMainWindow.Debugwindow1Click(Sender: TObject);
begin
  DebugWindow.Show;
end;

procedure TMainWindow.About1Click(Sender: TObject);
begin
  ShowMessage('Dune 2000 Map and Mission Editor'#13#13+
              'Part of D2K+ Editing tools'#13#13+
              'Made by Klofkac (kozten@seznam.cz)'#13+
              'Version 2.0 rc1'#13+
              'Date: 2021-04-20'#13#13+
              'http://github.com/jkoncick/D2kEditor'#13#13+
              'Special thanks to:'#13+
              'mvi - for making the original Mission editor'#13+
              'FunkyFr3sh - for patching Dune 2000'#13+
              'FED2k community - for their support');
end;

procedure TMainWindow.MapScrollChange(Sender: TObject);
var
  pos: TPoint;
begin
  map_canvas_left := MapScrollH.Position;
  map_canvas_top := MapScrollV.Position;
  render_map;
  render_minimap_position_marker;
  // Simulate MouseMove event so that editing marker and coordinates are updated
  if mouse_over_map_canvas then
  begin
    pos := MapCanvas.ScreenToClient(Mouse.CursorPos);
    MapCanvasMouseMove(nil, [], pos.X, pos.Y);
  end
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
  // If mouse is still inside same tile, exit (for optimization)
  if (mouse_old_x = map_x) and (mouse_old_y = map_y) then
    exit;
  mouse_already_clicked := false;
  // Write coordinates on status bar
  StatusBar.Panels[0].Text := 'x: '+inttostr(map_x)+' y: '+inttostr(map_y);
  // If mouse moved over Reinforcement or Spawn event marker, show "hint" with list of units
  if mode(mStructures) and Showeventmarkers1.Checked and ((Mission.event_markers[map_x,map_y].emtype = emReinforcement) or (Mission.event_markers[map_x,map_y].emtype = emUnitSpawn)) then
  begin
    Application.CancelHint;
    eventnum := Mission.event_markers[map_x,map_y].index;
    numunits := Mission.mis_data.events[eventnum].num_units;
    tmp_hint := inttostr(numunits) + ' units:';
    for i := 0 to (numunits -1) do
      tmp_hint := tmp_hint + chr(13) + Structures.get_unit_name_str(Mission.mis_data.events[eventnum].units[i]);
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
    Renderer.invalidate_map_tile(0, Mission.mis_data.events[moving_event].map_pos_y);
    Renderer.invalidate_map_tile(Map.width-1, map_y);
    Mission.mis_data.events[moving_event].map_pos_x := map_x;
    Mission.mis_data.events[moving_event].map_pos_y := map_y;
    Dispatcher.register_event(evMisEventPositionChange);
    exit;
  end;
  if (moving_condition <> -1) then
  begin
    Renderer.invalidate_map_tile(0, Mission.mis_data.conditions[moving_condition].map_pos_y);
    Renderer.invalidate_map_tile(Map.width-1, map_y);
    Mission.mis_data.conditions[moving_condition].map_pos_x := map_x;
    Mission.mis_data.conditions[moving_condition].map_pos_y := map_y;
    Dispatcher.register_event(evMisEventPositionChange);
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
  end;
  // Redraw editing marker
  render_editing_marker;
  // If left button is held, paint sand/rock/dunes/spice during mouse move
  if (ssLeft in shift) and (mode(mPaintMode) or mode(mStructuresPaint)) then
    MapCanvasMouseDown(sender,mbLeft,Shift,x,y);
  // If right button is held in structures mode, erase structures
  if (ssRight in shift) and mode(mStructures) then
    MapCanvasMouseDown(sender,mbRight,Shift,x,y);
end;

procedure TMainWindow.MapCanvasMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  map_x, map_y: integer;
  special: word;
  tiledata_entry: TTileDataEntryPtr;
  event_marker: ^TEventMarker;
  cursor_left: integer;
  cursor_top: integer;
begin
  map_x := x div 32 + map_canvas_left;
  map_y := y div 32 + map_canvas_top;
  // Disable multiple clicks in the same tile
  if mouse_already_clicked and (mouse_last_button = Button) then
    exit;
  mouse_already_clicked := true;
  mouse_last_button := Button;
  // Get tile attributes for this tile (DEBUG only)
  if (Button = mbRight) and (ssShift in Shift) then
  begin
    Caption := inttostr(Map.data[map_x, map_y].tile) + ' ' + inttohex(Tileset.get_tile_attributes(Map.data[map_x, map_y].tile, Map.data[map_x, map_y].special, true), 16);
    exit;
  end;
  // Event position selection mode
  if (event_position_selection_mode = epmEventCoordinates) and (Button = mbLeft) then
  begin
    finish_event_position_selection(map_x, map_x, map_y, map_y);
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
  // Switch to Select mode if Ctrl is held
  if (Button = mbLeft) and (ssCtrl in Shift) then
  begin
    EditorPages.TabIndex := 1;
    RbSelectMode.Checked := true;
  end;
  // Finally placing/painting structures/terrain
  if mode(mStructures) then
    begin
    // Editing structures
    if Button = mbLeft then
    begin
      // Put structure on map
      special := strtoint(SpecialValue.Text);
      if Map.check_structure_can_be_placed(map_x, map_y, special) then
      begin
        Map.set_special_value(map_x, map_y, special);
        // After placing building do not draw building marker
        tiledata_entry := Structures.get_tiledata_entry(special);
        if tiledata_entry.stype = ST_BUILDING then
          editing_marker_disabled := true;
      end;
    end else
    if Button = mbRight then
    begin
      // Delete structure from map
      Map.set_special_value(map_x, map_y, 0);
    end else
    if Button = mbMiddle then
    begin
      // Get structure parameters on position and set them in menu
      SpecialValue.text := inttostr(Map.data[map_x, map_y].special);
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
        render_editing_marker;
        exit;
      end
      else if mode(mBlockMode) then
      begin
        // Draw selected block
        cursor_left := (CursorImage.Left - MapCanvas.Left) div 32 + map_canvas_left;
        cursor_top := (CursorImage.Top - MapCanvas.Top) div 32 + map_canvas_top;
        if (cursor_left <> map_x) or (cursor_top <> map_y) then
          // Enable additional clicks if cursor image was moved from mouse cursor position
          mouse_already_clicked := false;
        Map.put_block(cursor_left, cursor_top, block_width, block_height, Addr(block_data));
        //--RandomGen.place_seed_block(cur_preset_index, cursor_left, cursor_top, ssShift in Shift);
      end
      else if (ssShift in Shift) and mode(mPaintMode) and (Tileset.paint_tile_groups[paint_tile_group].smooth_preset_group > -1) then
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
      copy_block_from_map(brush_size_presets[cbBrushSize.ItemIndex,1], brush_size_presets[cbBrushSize.ItemIndex,2], map_x, map_y, false, false, -1);
      exit;
    end;
  end;
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
    Map.fill_area_start(mouse_old_x, mouse_old_y, paint_tile_group);
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
    if event_position_selection_mode = epmDefenceArea then
    begin
      finish_event_position_selection(min_x, max_x, min_y, max_y);
      exit;
    end;
    copy_block_from_map(max_x - min_x + 1, max_y - min_y + 1, min_x, min_y, true, ssShift in Shift, cbSelectAreaType.ItemIndex - 1);
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

procedure TMainWindow.ImageMouseLeave(Sender: TObject);
begin
  if (Sender <> MapCanvas) and (Sender <> CursorImage) then
    exit;
  if mouse_over_map_canvas then
    exit;
  StatusBar.Panels[0].Text := '';
  // Reset mouse position to a value outside of map range
  mouse_old_x := max_map_width;
  mouse_old_y := max_map_height;
  // Remove editing markers
  render_editing_marker;
end;

procedure TMainWindow.MiniMapMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not Map.loaded then
    exit;
  if (x < mmap_border_x) or (y < mmap_border_y) or (x > MiniMap.Width - mmap_border_x) or (y > MiniMap.Height - mmap_border_y) then
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
  render_editing_marker;
end;

procedure TMainWindow.SpecialValueChange(Sender: TObject);
begin
  update_structure_controls;
end;

procedure TMainWindow.btnFindSelectedObjectClick(Sender: TObject);
var
  pos_x, pos_y: integer;
begin
  if Map.search_special(strtoint(SpecialValue.Text), pos_x, pos_y) then
  begin
    MapScrollH.Position := pos_x - (map_canvas_width div 2);
    MapScrollV.Position := pos_y - (map_canvas_height div 2);
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
  update_map_stats;
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
    paint_tile_select[paint_tile_group].AllowAllUp := true;
    paint_tile_select[paint_tile_group].Down := false;
    LbPaintTileGroupName.Caption := '';
  end;
  if RbPaintMode.Checked and (paint_tile_select_active <> nil) then
  begin
    paint_tile_select_active.Down := true;
    LbPaintTileGroupName.Caption := paint_tile_select_active.Hint;
  end;
  render_editing_marker;
end;

procedure TMainWindow.PaintTileSelectClick(Sender: TObject);
begin
  paint_tile_select_active := Sender as TSpeedButton;
  paint_tile_group := paint_tile_select_active.Tag;
  paint_tile_select[paint_tile_group].AllowAllUp := false;
  LbPaintTileGroupName.Caption := paint_tile_select_active.Hint;
  RbPaintMode.Checked := true;
end;

procedure TMainWindow.PaintTileSelectDblClick(Sender: TObject);
begin
  // Auto-select preset group
  if Tileset.paint_tile_groups[paint_tile_group].smooth_preset_group <> -1 then
  begin
    block_preset_select[Tileset.paint_tile_groups[paint_tile_group].smooth_preset_group].Down := true;
    BlockPresetGroupSelectClick(block_preset_select[Tileset.paint_tile_groups[paint_tile_group].smooth_preset_group]);
  end;
end;

procedure TMainWindow.BlockPresetGroupSelectClick(Sender: TObject);
begin
  block_preset_group := (Sender as TSpeedButton).Tag;
  if BlockPresetDialog.Visible then
    BlockPresetDialog.Show;
  BlockPresetDialog.init_presets;
end;

procedure TMainWindow.update_structures_list(building_list, unit_list: TStringList);
var
  prev_index: integer;
begin
  prev_index := BuildingList.ItemIndex;
  BuildingList.Items := building_list;
  BuildingList.ItemIndex := prev_index;
  prev_index := UnitList.ItemIndex;
  UnitList.Items := unit_list;
  UnitList.ItemIndex := prev_index;
end;

procedure TMainWindow.update_player_list(player_list: TStringList);
var
  prev_index: integer;
begin
  prev_index := PlayerSelect.ItemIndex;
  PlayerSelect.Items := player_list;
  PlayerSelect.ItemIndex := Max(prev_index, 0);
end;

procedure TMainWindow.update_misc_object_list;
var
  i: integer;
  tmp_strings: TStringList;
begin
  tmp_strings := TStringList.Create;
  for i := 0 to Structures.cnt_misc_objects - 1 do
    tmp_strings.Add(Structures.misc_object_info[i].name);
  MiscObjList.Items := tmp_strings;
  tmp_strings.Destroy;
end;

procedure TMainWindow.update_tileset;
var
  area_types: TStringList;
  i: integer;
  tile_x, tile_y: integer;
begin
  // Show tileset name
  if Tileset.tileset_index <> -1 then
    StatusBar.Panels[1].Text := Tileset.tileset_name
  else
    StatusBar.Panels[1].Text := '*' + Tileset.tileset_name;
  // Draw glyphs on paint tile group buttons in terrain editing GUI
  for i := -4 to cnt_paint_tile_groups-1 do
  begin
    if Tileset.paint_tile_groups[i].name <> '' then
    begin
      tile_x := Tileset.paint_tile_groups[i].tile_index mod 20;
      tile_y := Tileset.paint_tile_groups[i].tile_index div 20;
      paint_tile_select[i].Glyph.Canvas.CopyRect(Rect(0,0,28,28), Tileset.tileimage.Canvas, Rect(tile_x*32+2, tile_y*32+2, tile_x*32+30, tile_y*32+30));
      paint_tile_select[i].Glyph.Canvas.Pixels[0,27] := $1;
    end else
    begin
      paint_tile_select[i].Glyph.Canvas.Brush.Color := clBlack;
      paint_tile_select[i].Glyph.Canvas.Brush.Style := bsSolid;
      paint_tile_select[i].Glyph.Canvas.Rectangle(0, 0, paint_tile_select[i].Glyph.Width, paint_tile_select[i].Glyph.Height);
    end;
    paint_tile_select[i].Enabled := Tileset.paint_tile_groups[i].name <> '';
    paint_tile_select[i].Hint := Tileset.paint_tile_groups[i].name;
  end;
  // Set text on block preset group buttons
  for i := 0 to cnt_block_preset_groups-1 do
  begin
    block_preset_select[i].Enabled := Tileset.block_preset_groups[i].name <> '';
    block_preset_select[i].Caption := Tileset.block_preset_groups[i].name;
  end;
  // Fill Area type combo box
  area_types := TStringList.Create;
  area_types.Add('(Everything)');
  for i := 0 to Tileset.fill_area_rules_used - 1 do
    area_types.Add(Tileset.fill_area_rules[i].name);
  cbSelectAreaType.Items := area_types;
  cbSelectAreaType.ItemIndex := 0;
  area_types.Destroy;
  // Update paint tile group label
  if (paint_tile_select_active <> nil) and RbPaintMode.Checked then
    LbPaintTileGroupName.Caption := paint_tile_select_active.Hint;
end;

procedure TMainWindow.update_structure_controls;
var
  i: integer;
  special, player: integer;
  tiledata_entry: TTileDataEntryPtr;
begin
  special := StrToIntDef(SpecialValue.Text, 0);
  tiledata_entry := Structures.get_tiledata_entry(special);
  if tiledata_entry.stype = ST_MISC_OBJECT then
  begin
    if not special_value_changing then
    begin
      MiscObjList.ItemIndex := tiledata_entry.index;
      BuildingList.ItemIndex := -1;
      UnitList.ItemIndex := -1;
    end;
    LbStructureName.Caption := Structures.misc_object_info[tiledata_entry.index].name;
  end else
  if tiledata_entry.stype = ST_BUILDING then
  begin
    if not special_value_changing then
    begin
      MiscObjList.ItemIndex := -1;
      for i := 0 to Structures.building_group_mapping_count - 1 do
        if Structures.building_group_mapping[i] = Integer(tiledata_entry.index) then
        begin
          BuildingList.ItemIndex := i;
          break;
        end;
      UnitList.ItemIndex := -1;
      PlayerSelect.ItemIndex := tiledata_entry.player;
    end;
    player := Mission.get_player_alloc_index(tiledata_entry.player);
    if (tiledata_entry.index < MAX_BUILDING_TYPES) and (Structures.building_side_versions[tiledata_entry.index, player] <> -1) then
      LbStructureName.Caption := Structures.get_building_name_str(Structures.building_side_versions[tiledata_entry.index, player])
    else
      LbStructureName.Caption := 'INVALID';
    update_map_stats;
  end else
  if tiledata_entry.stype = ST_UNIT then
  begin
    if not special_value_changing then
    begin
      MiscObjList.ItemIndex := -1;
      BuildingList.ItemIndex := -1;
      UnitList.ItemIndex := tiledata_entry.index;
      PlayerSelect.ItemIndex := tiledata_entry.player;
    end;
    player := Mission.get_player_alloc_index(tiledata_entry.player);
    if (tiledata_entry.index < MAX_UNIT_TYPES) and (Structures.unit_side_versions[tiledata_entry.index, player] <> -1) then
      LbStructureName.Caption := Structures.get_unit_name_str(Structures.unit_side_versions[tiledata_entry.index, player])
    else
      LbStructureName.Caption := 'INVALID';
    update_map_stats;
  end else
  begin
    if not special_value_changing then
    begin
      MiscObjList.ItemIndex := -1;
      BuildingList.ItemIndex := -1;
      UnitList.ItemIndex := -1;
    end;
    LbStructureName.Caption := '';
  end;
  render_editing_marker;
end;

procedure TMainWindow.update_map_dimensions;
begin
  StatusBar.Panels[2].Text := inttostr(Map.width)+' x '+inttostr(Map.height);
  resize_map_canvas;
end;

procedure TMainWindow.update_map_name;
begin
  if Map.filename = '' then
  begin
    StatusBar.Panels[3].Text := 'Map not saved';
    EditorPages.ActivePage := PageTerrain;
  end else
  begin
    StatusBar.Panels[3].Text := Map.filename;
    refresh_recent_files_menu;
  end;
end;

procedure TMainWindow.update_mission_load_status;
begin
  Missionsettings1.Enabled := Mission.mis_assigned;
  EventsandConditions1.Enabled := Mission.mis_assigned;
  Assignmisfile1.Caption := IfThen(Mission.mis_assigned, 'Unassign .mis file', 'Assign .mis file');
  StatusBar.Panels[4].Text := IfThen(Mission.mis_assigned, 'MIS', '');
end;

procedure TMainWindow.update_map_stats;
var
  i: integer;
begin
  i := PlayerSelect.ItemIndex;
  StatusBar.Panels[5].Text := Format('W: %d  S: %d  B: %d  C: %d', [Map.stats.misc_objects[MOT_WORM_SPAWNER], Map.stats.misc_objects[MOT_PLAYER_START], Map.stats.misc_objects[MOT_SPICE_BLOOM], Map.stats.misc_objects[MOT_CRATE]]);
  StatusBar.Panels[6].Text := Format('Power: %d%%   (%d/%d)', [Map.stats.players[i].power_percent, Map.stats.players[i].power_output, Map.stats.players[i].power_need]);
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
  mmap_border_x := (max_map_width - Map.width) div 2;
  mmap_border_y := (max_map_height - Map.height) div 2;
end;

procedure TMainWindow.render_map;
var
  t1, t2: Int64;
begin
  if not Map.loaded then
    exit;
  if Settings.Debug_ShowRenderTime then
    QueryPerformanceCounter(t1);
  Renderer.render_map_contents(MapCanvas.Canvas, map_canvas_left, map_canvas_top, map_canvas_width, map_canvas_height, Addr(Map.data), Map.width, Map.height,
    sbShowGrid.Down, sbMarkImpassableTiles.Down, sbMarkBuildableTiles.Down, sbMarkOwnerSide.Down,
    Useallocationindexes1.Checked, Showeventmarkers1.Checked, Markdefenceareas1.Checked, Showunknownspecials1.Checked,
    true);
  if Settings.Debug_ShowRenderTime then
  begin
    QueryPerformanceCounter(t2);
    Caption := FloatToStr((t2-t1) / performance_frequency);
  end;
  if not editing_marker_disabled then
    render_editing_marker;
  editing_marker_disabled := false;
end;

procedure TMainWindow.render_minimap;
begin
  if not Map.loaded then
    exit;
  Renderer.render_minimap_contents(minimap_buffer, Addr(Map.data), Map.width, Map.height, Useallocationindexes1.Checked);
  render_minimap_position_marker;
end;

procedure TMainWindow.render_minimap_position_marker;
begin
  MiniMap.Canvas.CopyRect(rect(0,0,MiniMap.Width,MiniMap.Height),minimap_buffer.Canvas,rect(0,0,MiniMap.Width,MiniMap.Height));
  MiniMap.Canvas.Pen.Color:= $00FF00;
  MiniMap.Canvas.Brush.Style := bsClear;
  MiniMap.Canvas.Rectangle(mmap_border_x + map_canvas_left,mmap_border_y + map_canvas_top,mmap_border_x + map_canvas_left + map_canvas_width,mmap_border_y + map_canvas_top + map_canvas_height);
  MiniMap.Canvas.Brush.Style := bsSolid;
end;

procedure TMainWindow.render_editing_marker;
var
  tiledata_entry: TTileDataEntryPtr;
  building_template: TBuildingTemplatePtr;
  marker_type: EditingMarkerType;
  min_x, min_y, max_x, max_y: integer;
begin
  if not mouse_over_map_canvas then
  begin
    Renderer.remove_editing_marker(MapCanvas.Canvas);
    CursorImage.Visible := false;
    exit;
  end;
  tiledata_entry := Structures.get_tiledata_entry(StrToIntDef(SpecialValue.Text, 0));
  if mode(mStructures) and (tiledata_entry.stype = ST_BUILDING) then
  begin
    // Draw building placement marker
    building_template := Structures.get_building_template(tiledata_entry.index, Mission.get_player_alloc_index(tiledata_entry.player));
    if building_template = nil then
      exit;
    marker_type := emBuilding;
    if (building_template.Flags and BF_NO_CONCRETE) <> 0 then
      marker_type := emBuildingNoConcrete;
    Renderer.draw_editing_marker(MapCanvas.Canvas, map_canvas_left, map_canvas_top, map_canvas_width, map_canvas_height,
      Addr(Map.data), mouse_old_x, mouse_old_y, MAX_BUILDING_SIZE, MAX_BUILDING_SIZE, marker_type, building_template.TilesOccupiedAll);
  end else
  if mode(mStructures) and ((tiledata_entry.stype = ST_UNIT) or (tiledata_entry.stype = ST_MISC_OBJECT)) then
  begin
    // Draw unit / misc object marker
    Renderer.draw_editing_marker(MapCanvas.Canvas, map_canvas_left, map_canvas_top, map_canvas_width, map_canvas_height,
      Addr(Map.data), mouse_old_x, mouse_old_y, 1, 1, emSingleObject, 0);
  end else
  if block_select_started then
  begin
    // Draw border around selected block on map
    min_x := min(block_select_start_x, block_select_end_x);
    max_x := max(block_select_start_x, block_select_end_x);
    min_y := min(block_select_start_y, block_select_end_y);
    max_y := max(block_select_start_y, block_select_end_y);
    Renderer.draw_editing_marker(MapCanvas.Canvas, map_canvas_left, map_canvas_top, map_canvas_width, map_canvas_height,
      Addr(Map.data), min_x, min_y, max_x-min_x+1, max_y-min_y+1, emSelectionArea, 0);
  end else
  if mode(mPaintMode) then
  begin
    // Draw paint brush marker
    Renderer.draw_editing_marker(MapCanvas.Canvas, map_canvas_left, map_canvas_top, map_canvas_width, map_canvas_height,
      Addr(Map.data), mouse_old_x, mouse_old_y, brush_size_presets[cbBrushSize.ItemIndex,1], brush_size_presets[cbBrushSize.ItemIndex,2], emPaintArea, 0);
  end else
  begin
    Renderer.remove_editing_marker(MapCanvas.Canvas);
  end;
  // Set cursor image visibility
  CursorImage.Visible := mode(mBlockMode) and (block_width > 0) and (block_height > 0);
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

procedure TMainWindow.render_cursor_image;
var
  x, y: integer;
  tile_x, tile_y: word;
  border_x, border_y: integer;
  str: String;
  any_blank_tiles: boolean;
begin
  border_x := (BlockImage.Width - block_width * 32) div 2;
  border_y := (BlockImage.Height - block_height * 32) div 2;
  BlockImage.Canvas.Brush.Color := clBtnFace;
  BlockImage.Canvas.Pen.Color := clBtnFace;
  BlockImage.Canvas.Rectangle(0,0,BlockImage.Width,BlockImage.Height);
  CursorImage.Width := block_width * 32 + 1;
  CursorImage.Height := block_height * 32 + 1;
  CursorImage.Picture.Bitmap.Width := block_width * 32 + 1;
  CursorImage.Picture.Bitmap.Height := block_height * 32 + 1;
  // Render block image
  if (block_width = 0) or (block_width > 8) or (block_height = 0) or (block_height > 8) then
  begin
    // If block size is zero or too big, render dummy text there
    str := 'Click here to';
    BlockImage.Canvas.TextOut((BlockImage.Width - BlockImage.Canvas.TextWidth(str)) div 2, 52, str);
    str := 'select a block';
    BlockImage.Canvas.TextOut((BlockImage.Width - BlockImage.Canvas.TextWidth(str)) div 2, 66, str);
  end else
  begin
    for x:= 0 to block_width-1 do
      for y := 0 to block_height-1 do
      begin
        tile_x := block_data[x,y].tile mod 20;
        tile_y := block_data[x,y].tile div 20;
        BlockImage.Canvas.CopyRect(rect(x*32+border_x, y*32+border_y, x*32+32+border_x, y*32+32+border_y), Tileset.tileimage.Canvas,rect(tile_x*32, tile_y*32, tile_x*32+32, tile_y*32+32));
      end;
  end;
  // Check if block has any blank tile so that it should be transparent
  any_blank_tiles := false;
  for x:= 0 to block_width-1 do
    for y := 0 to block_height-1 do
      if block_data[x,y].tile = 65535 then
        any_blank_tiles := true;
  CursorImage.Transparent := any_blank_tiles;
  // Render cursor image
  Renderer.render_map_contents(CursorImage.Canvas, 0, 0, block_width, block_height, Addr(block_data), block_width, block_height,
    false, false, false, sbMarkOwnerSide.Down,
    Useallocationindexes1.Checked, false, false, Showunknownspecials1.Checked,
    false);
  CursorImage.Canvas.Pen.Color := clBlue;
  CursorImage.Canvas.Brush.Style := bsClear;
  CursorImage.Canvas.Rectangle(0, 0, block_width * 32 + 1, block_height * 32 + 1);
  resize_cursor_image;
  render_editing_marker;
end;

procedure TMainWindow.refresh_recent_files_menu;
var
  i: integer;
begin
  for i := 1 to cnt_recent_files do
  begin
    if Settings.RecentFiles[i] <> '' then
    begin
      recent_files_menuitems[i].Caption := Settings.RecentFiles[i];
      recent_files_menuitems[i].Visible := true;
    end;
  end;
end;

procedure TMainWindow.set_special_value;
var
  tiledata_entry: TTileDataEntryPtr;
  value: word;
  i: integer;
begin
  if MiscObjList.ItemIndex > -1 then
    value := Structures.misc_object_info[MiscObjList.ItemIndex].value
  else if BuildingList.ItemIndex > -1 then
  begin
    // Find building special value
    value := 0;
    for i := 0 to CNT_TILEDATA_ENTRIES - 1 do
    begin
      tiledata_entry := Structures.get_tiledata_entry(i);
      if (tiledata_entry.stype = ST_BUILDING) and (tiledata_entry.player = PlayerSelect.ItemIndex) and (Integer(tiledata_entry.index) = Structures.building_group_mapping[BuildingList.ItemIndex]) then
      begin
        value := i;
        break;
      end;
    end;
  end else
  begin
    // Find unit special value
    value := 0;
    for i := 0 to CNT_TILEDATA_ENTRIES - 1 do
    begin
      tiledata_entry := Structures.get_tiledata_entry(i);
      if (tiledata_entry.stype = ST_UNIT) and (tiledata_entry.player = PlayerSelect.ItemIndex) and (tiledata_entry.index = UnitList.ItemIndex) then
      begin
        value := i;
        break;
      end;
    end;
  end;
  special_value_changing := true;
  SpecialValue.Text := inttostr(value);
  special_value_changing := false;
  mouse_already_clicked := false;
end;

function TMainWindow.mode(m: SelectedMode): boolean;
var
  tiledata_entry: TTileDataEntryPtr;
  building_template: TBuildingTemplatePtr;
begin
  result := false;
  case m of
    mStructures:      result := (EditorPages.TabIndex = 0) and (event_position_selection_mode = epmNone);
    mStructuresPaint:
      begin
        if (EditorPages.TabIndex <> 0) or (event_position_selection_mode <> epmNone) then
          exit;
        // Walls can be painted while holding mouse
        tiledata_entry := Structures.get_tiledata_entry(StrToIntDef(SpecialValue.Text, 0));
        if tiledata_entry.stype = ST_BUILDING then
        begin
          building_template := Structures.get_building_template(tiledata_entry.index, Mission.get_player_alloc_index(tiledata_entry.player));
          // Check if building behavior is wall
          result := (building_template <> nil) and (building_template.SpecialBehavior = 14);
        end;
      end;
    mTerrain:         result := EditorPages.TabIndex = 1;
    mPaintMode:       result := (EditorPages.TabIndex = 1) and RbPaintMode.Checked;
    mBlockMode:       result := (EditorPages.TabIndex = 1) and RbBlockMode.Checked;
    mSelectMode:      result := ((EditorPages.TabIndex = 1) and RbSelectMode.Checked) or (event_position_selection_mode = epmDefenceArea);
  end;
end;

function TMainWindow.mouse_over_map_canvas: boolean;
var
  pos: TPoint;
  ForegroundWindow: HWND;
begin
  pos := MainWindow.ScreenToClient(Mouse.CursorPos);
  ForegroundWindow := GetForegroundWindow;
  result := PtInRect(MapCanvas.BoundsRect, pos);
  result := result and not (BlockPresetDialog.Visible and PtInRect(BlockPresetDialog.BoundsRect, Mouse.CursorPos));
  result := result and not ((ForegroundWindow = TilesetDialog.Handle) and PtInRect(TilesetDialog.BoundsRect, Mouse.CursorPos));
  result := result and not (MapStatsDialog.Visible and PtInRect(MapStatsDialog.BoundsRect, Mouse.CursorPos));
  result := result and not ((ForegroundWindow = MissionDialog.Handle) and PtInRect(MissionDialog.BoundsRect, Mouse.CursorPos));
  result := result and not ((ForegroundWindow = EventDialog.Handle) and PtInRect(EventDialog.BoundsRect, Mouse.CursorPos));
  result := result and not ((ForegroundWindow = MissionLauncher.Handle) and PtInRect(MissionLauncher.BoundsRect, Mouse.CursorPos));
  result := result and not ((ForegroundWindow = TileAtrEditor.Handle) and PtInRect(TileAtrEditor.BoundsRect, Mouse.CursorPos));
  result := result and not ((ForegroundWindow = StructuresEditor.Handle) and PtInRect(StructuresEditor.BoundsRect, Mouse.CursorPos));
  result := result and not (DebugWindow.Visible and PtInRect(DebugWindow.BoundsRect, Mouse.CursorPos));
  result := result or block_select_started;
end;

procedure TMainWindow.apply_key_preset(key: word);
begin
  select_block_preset(Tileset.get_block_preset(block_preset_group, key, bpNext));
end;

procedure TMainWindow.start_event_position_selection(x, y: integer; mode: EventPositionSelectionMode);
begin
  EditorPages.TabIndex := 0;
  if (x <> 0) or (y <> 0) then
  begin
    MapScrollH.Position := x - (map_canvas_width div 2);
    MapScrollV.Position := y - (map_canvas_height div 2);
    if mode = epmEventCoordinates then
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
  event_position_selection_mode := mode;
end;

procedure TMainWindow.finish_event_position_selection(min_x, max_x, min_y, max_y: integer);
var
  mode: EventPositionSelectionMode;
begin
  mode := event_position_selection_mode;
  EditorPages.Visible := true;
  File1.Enabled := true;
  Edit1.Enabled := true;
  ileset1.Enabled := true;
  Map1.Enabled := true;
  Mission1.Enabled := true;
  Launchgame1.Enabled := true;
  MapCanvas.Cursor := crDefault;
  event_position_selection_mode := epmNone;
  if mode = epmEventCoordinates then
    EventDialog.finish_event_position_selection(min_x, min_y)
  else
    MissionDialog.finish_defence_area_position_selection(min_x, max_x, min_y, max_y);
end;

procedure TMainWindow.select_block_from_tileset(b_width, b_height, b_left, b_top: word);
var
  x, y: integer;
begin
  block_width := b_width;
  block_height := b_height;
  for x:= 0 to block_width - 1 do
    for y := 0 to block_height - 1 do
    begin
      block_data[x,y].tile := (b_top + y) * 20 + b_left + x;
      block_data[x,y].special := 0;
    end;
  mouse_already_clicked := false;
  render_cursor_image;
end;

procedure TMainWindow.select_block_preset(preset_index: integer);
var
  preset: PBlockPreset;
  x, y: integer;
begin
  cur_preset_index := preset_index;
  preset := @Tileset.block_presets[preset_index];
  block_width := preset.width;
  block_height := preset.height;
  for x := 0 to block_width - 1 do
    for y := 0 to block_height - 1 do
    begin
      block_data[x,y].tile := Tileset.block_preset_tiles[preset.block_preset_tile_index + x + y * preset.width];
      block_data[x,y].special := IfThen(block_data[x,y].tile = 65535, 65535, 0);
    end;
  RbBlockMode.Checked := true;
  mouse_already_clicked := false;
  render_cursor_image;
end;

procedure TMainWindow.copy_block_from_map(b_width, b_height, b_left, b_top: word; structures, erase: boolean; area_type: integer);
begin
  block_width := b_width;
  block_height := b_height;
  Map.copy_block(b_left, b_top, b_width, b_height, Addr(block_data), CbSelectStructures.State <> cbGrayed, (CbSelectStructures.State <> cbUnchecked) and structures, area_type, erase);
  render_cursor_image;
  RbBlockMode.Checked := True;
end;

end.
