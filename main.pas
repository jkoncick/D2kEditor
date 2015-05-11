unit main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, ComCtrls, Menus, StdCtrls, XPMan, set_dialog, tileset_dialog, Math,
  Spin, Buttons;

const cnt_tilesets = 7;
const cnt_players = 7;
const cnt_mis_players = 8;
const size_tileatr = 800;
const max_undo_steps = 32767;

type
   TileType = (ttNormal, ttImpassable, ttInfantryOnly, ttSlowdown, ttBuildable);

type
  TStructureParams = record
    offs_x: word; // X-offset in image
    offs_y: word; // Y-offset in image
    size_x: word; // Structure width
    size_y: word; // Structure height
    overfl: word; // Sprite overflow (1 = up (for buildings), 2 = infantry, 3 = wide (harvester, MCV))
    lnwall: boolean;  // Structure links with wall
    power: SmallInt; // Power the structure gives/needs
    values: array[0..cnt_players-1] of word; // Map special values
  end;

const structure_params: array[0..31] of TStructureParams =
  (
    (offs_x:  0; offs_y: 0; size_x: 1; size_y: 1; overfl:  0; lnwall:  true; power:   0; values:(  4,204,404,580,620,660,700)), // Wall
    (offs_x:  1; offs_y: 0; size_x: 2; size_y: 3; overfl:  1; lnwall: false; power:-200; values:(  5,205,405,581,621,661,701)), // Wind trap
    (offs_x:  3; offs_y: 0; size_x: 3; size_y: 3; overfl:  1; lnwall: false; power: -20; values:(  8,208,408,582,622,662,702)), // Construction Yard
    (offs_x:  6; offs_y: 0; size_x: 2; size_y: 3; overfl:  1; lnwall: false; power:  30; values:( 11,211,411,583,623,663,703)), // Barracks
    (offs_x:  8; offs_y: 0; size_x: 3; size_y: 3; overfl:  1; lnwall: false; power:  75; values:( 14,214,414,584,624,664,704)), // Refinery
    (offs_x: 11; offs_y: 0; size_x: 3; size_y: 3; overfl:  1; lnwall: false; power: 125; values:( 17,217,417,585,625,665,705)), // Outpost
    (offs_x: 14; offs_y: 0; size_x: 3; size_y: 3; overfl:  1; lnwall: false; power: 125; values:( 63,263,463,587,627,667,707)), // Light Factory
    (offs_x: 17; offs_y: 0; size_x: 1; size_y: 1; overfl:  0; lnwall: false; power:  15; values:( 69,269,469,589,629,668,708)), // Silo
    (offs_x: 18; offs_y: 0; size_x: 3; size_y: 4; overfl:  0; lnwall: false; power: 150; values:( 72,272,472,590,630,669,709)), // Heavy Factory
    (offs_x: 21; offs_y: 0; size_x: 3; size_y: 3; overfl:  0; lnwall: false; power:  50; values:( 75,275,475,591,631,670,710)), // Repair Pad
    (offs_x: 24; offs_y: 0; size_x: 1; size_y: 1; overfl:  1; lnwall:  true; power:  50; values:( 78,278,478,592,632,671,711)), // Gun Turret
    (offs_x: 25; offs_y: 0; size_x: 3; size_y: 4; overfl:  0; lnwall: false; power:  75; values:(120,320,520,593,633,672,712)), // High Tech Factory
    (offs_x: 28; offs_y: 0; size_x: 1; size_y: 1; overfl:  1; lnwall:  true; power:  60; values:(123,323,523,594,634,673,713)), // Rocket Turret
    (offs_x: 29; offs_y: 0; size_x: 3; size_y: 4; overfl:  0; lnwall: false; power: 175; values:(126,326,526,595,635,674,714)), // IX Research Centre
    (offs_x: 32; offs_y: 0; size_x: 3; size_y: 3; overfl:  0; lnwall: false; power: 150; values:(129,329,529,596,636,675,715)), // Starport
    (offs_x: 35; offs_y: 0; size_x: 3; size_y: 3; overfl:  1; lnwall: false; power: 200; values:(132,332,532,588,628,676,716)), // Palace
    (offs_x: 38; offs_y: 0; size_x: 2; size_y: 2; overfl:  0; lnwall: false; power:  50; values:(  0,  0,  0,597,637,  0,  0)), // Sietch
    (offs_x: 40; offs_y: 0; size_x: 3; size_y: 3; overfl:  1; lnwall: false; power: 100; values:(  0,218,418,  0,  0,666,  0)), // Modified Outpost
    (offs_x: 43; offs_y: 0; size_x: 1; size_y: 1; overfl:  2; lnwall: false; power:   0; values:(180,360,560,598,638,677,717)), // Light Infantry
    (offs_x: 44; offs_y: 0; size_x: 1; size_y: 1; overfl:  2; lnwall: false; power:   0; values:(181,361,561,599,639,678,718)), // Trooper
    (offs_x: 45; offs_y: 0; size_x: 1; size_y: 1; overfl:  2; lnwall: false; power:   0; values:(182,362,562,600,640,679,719)), // St. Fremen / Saboteur
    (offs_x: 46; offs_y: 0; size_x: 1; size_y: 1; overfl:  2; lnwall: false; power:   0; values:(183,363,563,601,641,  0,  0)), // Sardakaur / Fremen
    (offs_x: 47; offs_y: 0; size_x: 1; size_y: 1; overfl:  2; lnwall: false; power:   0; values:(184,364,564,602,642,680,720)), // Engineer
    (offs_x: 44; offs_y: 1; size_x: 1; size_y: 1; overfl:  3; lnwall: false; power:   0; values:(185,365,565,603,643,681,721)), // Harvester
    (offs_x: 46; offs_y: 1; size_x: 1; size_y: 1; overfl:  3; lnwall: false; power:   0; values:(186,366,566,604,644,682,722)), // MCV
    (offs_x: 43; offs_y: 3; size_x: 1; size_y: 1; overfl:  0; lnwall: false; power:   0; values:(187,367,567,605,645,683,723)), // Trike / Raider
    (offs_x: 44; offs_y: 3; size_x: 1; size_y: 1; overfl:  0; lnwall: false; power:   0; values:(188,368,568,606,646,684,724)), // Quad
    (offs_x: 45; offs_y: 3; size_x: 1; size_y: 1; overfl:  0; lnwall: false; power:   0; values:(189,369,569,607,647,685,725)), // Combat Tank
    (offs_x: 46; offs_y: 3; size_x: 1; size_y: 1; overfl:  0; lnwall: false; power:   0; values:(190,370,570,608,648,686,726)), // Missile Tank
    (offs_x: 47; offs_y: 3; size_x: 1; size_y: 1; overfl:  1; lnwall: false; power:   0; values:(191,371,571,609,649,687,727)), // Siege Tank
    (offs_x: 17; offs_y: 1; size_x: 1; size_y: 1; overfl:  0; lnwall: false; power:   0; values:(192,372,572,610,650,688,728)), // Carryall
    (offs_x: 47; offs_y: 1; size_x: 1; size_y: 1; overfl:  1; lnwall: false; power:   0; values:(194,374,574,  0,652,  0,  0))  // House Special Tank
  );

const unit_names: array[0..29] of string = ('Light infantry', 'Trooper', 'Engineer', 'Thumper infantry',
  'Sardaukar', 'Trike', 'Raider', 'Quad', 'Harvester', 'Combat tank (A)', 'Combat tank (H)', 'Combat tank (O)',
  'MCV', 'Missile tank', 'Deviator', 'Siege tank', 'Sonic tank', 'Devastator', 'Carryall', 'Carryall (A)',
  'Ornithropter', 'Stealth Fremen', 'Fremen', 'Saboteur', 'Death Hand Missile', 'Glitched infantry', 'Frigate',
  'Grenadier', 'Stealth Raider', 'MP Sardaukar');

const misc_obj_values: array[0..9] of word = (0,1,2,20,23,45,41,42,43,44);

const mmap_misc_objects_colors: array[1..9] of TColor = ($52AEF7,$2179E7,$FF00FF,$FFFF00,$0000FF,$0000B0,$0000C0,$0000D0,$0000E0);
const mmap_tile_colors: array[0..4] of TColor = ($8CDFEF,$29285A,$375582,$ACDFEF,$58A4E4);
const mmap_player_colors: array[0..cnt_players-1] of TColor = ($84614A,$3231C6,$63824A,$6B0063,$747274,$00106B,$08728C);

const tilesets: array[1..cnt_tilesets] of String = ('BLOXBASE','BLOXBAT','BLOXBGBS','BLOXICE','BLOXTREE','BLOXWAST','BLOXXMAS');
const tileatr_filenames: array[1..cnt_tilesets] of String = ('tileatr2.bin','tileatr6.bin','tileatr3.bin','tileatr5.bin','tileatr1.bin','tileatr4.bin','tileatr7.bin');

const tiles_sand: array[0..9] of word = (48,49,50,51,52,68,69,70,71,72);
const tiles_rock: array[0..14] of word = (552,553,554,555,556,572,573,574,575,576,592,593,594,595,596);
const tiles_dunes: array[0..7] of word = (63,64,65,66,83,84,103,104);

const block_size_presets: array[1..8,1..2] of word = ((1,1),(2,2),(3,3),(4,4),(2,1),(1,2),(3,2),(2,3));

const block_key_presets: array[1..52,0..3,0..3] of word = (
  // Up
  ((2,2,16,18), (2,2, 5, 4), (1,1, 8,14), (1,1, 0, 3)),
  ((2,3,16,22), (2,3, 6,10), (2,2,18,12), (2,2,16, 0)),
  ((2,2, 7,26), (2,2, 7, 4), (1,1,11,14), (1,1, 1, 3)),
  ((2,2, 9,26), (2,2, 0, 8), (1,1,12,14), (1,1, 1, 3)),
  ((2,2, 0,28), (2,2, 2, 8), (1,1,13,14), (1,1, 1, 3)),
  ((1,2,17,33), (1,2,15,33), (1,1,14,14), (1,1, 1, 3)),
  ((0,0, 0, 0), (0,0, 0, 0), (1,1,15,14), (1,1, 1, 3)),
  ((2,3,14,22), (2,3, 4,10), (2,2,16,12), (2,2,18, 0)),
  ((2,2,18,18), (2,2, 9, 4), (1,1,16,14), (1,1, 2, 3)),
  // Left
  ((3,2, 0,26), (3,2, 8,12), (2,2, 6,13), (2,2,10, 0)),
  ((2,2, 4,22), (2,2,18, 4), (1,1,14,16), (1,1, 0, 4)),
  ((2,2, 6,22), (2,2,18, 6), (1,1,15,16), (1,1, 0, 4)),
  ((2,2, 8,22), (2,2, 6, 8), (1,1,16,16), (1,1, 0, 4)),
  ((2,1,18,34), (2,1,12,34), (1,1,17,16), (1,1, 0, 4)),
  ((0,0, 0, 0), (0,0, 0, 0), (1,1,14,15), (1,1, 0, 4)),
  ((3,2, 0,24), (3,2, 8,10), (2,2, 4,13), (2,2, 4, 1)),
  // Right
  ((3,2,11,23), (3,2,11,12), (2,2, 0,13), (2,2, 6, 1)),
  ((2,2, 5,24), (2,2,11, 4), (1,1,11,16), (1,1, 2, 4)),
  ((2,2, 7,24), (2,2,13, 4), (1,1,12,16), (1,1, 2, 4)),
  ((2,2, 9,24), (2,2, 8, 8), (1,1,13,16), (1,1, 2, 4)),
  ((2,1,18,33), (2,1, 9,34), (1,1,15,15), (1,1, 2, 4)),
  ((3,2,11,25), (3,2,11,10), (2,2, 2,13), (2,2, 2, 1)),
  // Down
  ((2,2,16,20), (2,2, 0, 6), (1,1, 9,16), (1,1, 0, 5)),
  ((2,3, 2,20), (2,3, 0,10), (2,2,18, 8), (2,2,16, 2)),
  ((2,2, 4,20), (2,2, 2, 6), (1,1, 6,16), (1,1, 1, 5)),
  ((2,2, 6,20), (2,2,14, 6), (1,1, 7,16), (1,1, 1, 5)),
  ((2,2, 8,20), (2,2,16, 6), (1,1,17,15), (1,1, 1, 5)),
  ((1,2,16,33), (1,2,14,33), (0,0, 0, 0), (1,1, 1, 5)),
  ((2,3,10,20), (2,3, 2,10), (2,2,18,10), (2,2,18, 2)),
  ((2,2,18,20), (2,2, 4, 6), (1,1,18,15), (1,1, 2, 5)),
  // Inner curves
  ((2,2, 4,28), (2,2,12, 6), (1,1, 3,15), (1,1, 0, 1)),
  ((2,2,10,28), (2,2, 8, 6), (1,1, 3,16), (1,1, 0, 2)),
  ((2,2, 6,28), (2,2,10, 6), (1,1, 4,15), (1,1, 1, 1)),
  ((2,2, 8,28), (2,2, 6, 6), (1,1, 4,16), (1,1, 1, 2)),
  // Up
  ((2,2, 5,26), (2,2,14,10), (0,0, 0, 0), (0,0, 0, 0)),
  ((2,2,18,24), (2,2,18,22), (0,0, 0, 0), (0,0, 0, 0)),
  ((2,2,18,22), (2,2,18,24), (0,0, 0, 0), (0,0, 0, 0)),
  ((2,2, 2,28), (2,2,16,10), (0,0, 0, 0), (0,0, 0, 0)),
  // Left
  ((2,2, 0,22), (2,2,14, 8), (0,0, 0, 0), (0,0, 0, 0)),
  ((2,2,12, 0), (2,2,14, 0), (0,0, 0, 0), (0,0, 0, 0)),
  ((2,2,14, 0), (2,2,12, 0), (0,0, 0, 0), (0,0, 0, 0)),
  ((2,2, 3,24), (2,2,16, 8), (0,0, 0, 0), (0,0, 0, 0)),
  // Right
  ((2,1,12,22), (2,1,18,14), (0,0, 0, 0), (0,0, 0, 0)),
  ((2,2,14, 2), (2,2,15, 4), (0,0, 0, 0), (0,0, 0, 0)),
  ((2,2,15, 4), (2,2,14, 2), (0,0, 0, 0), (0,0, 0, 0)),
  ((2,2, 3,26), (2,2,14,12), (0,0, 0, 0), (0,0, 0, 0)),
  // Down
  ((2,2, 0,20), (2,2,10, 8), (0,0, 0, 0), (0,0, 0, 0)),
  ((2,2,14,25), (2,2,16,25), (0,0, 0, 0), (0,0, 0, 0)),
  ((2,2,16,25), (2,2,14,25), (0,0, 0, 0), (0,0, 0, 0)),
  ((2,2,12,20), (2,2,12, 8), (0,0, 0, 0), (0,0, 0, 0)),
  // Others
  ((3,3, 0,30), (2,2, 3,30), (0,0, 0, 0), (0,0, 0, 0)),
  ((1,2,13, 2), (2,2, 5,30), (0,0, 0, 0), (0,0, 0, 0))
  );

type
  TMapTile = record
    tile: word;
    special: word;
  end;

type
  TUndoEntry = record
    x, y: word;
    data: TMapTile;
    is_first: boolean;
  end;

type
   EventMarkerType = (emNone, emReinforcement, emHarvester, emSpawn, emTileTrigger, emRevealMap);

type
  TEventMarker = record
    emtype: EventMarkerType;
    player: word;
    index: word;
  end;

type
  TPlayerStats = record
    power_percent: word;
    power_output: word;
    power_need: word;
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
    BLOXBASE1: TMenuItem;
    BLOXBAT1: TMenuItem;
    BLOXBGBS1: TMenuItem;
    BLOXICE1: TMenuItem;
    BLOXTREE1: TMenuItem;
    BLOXWAST1: TMenuItem;
    BLOXXMAS1: TMenuItem;
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
    Detectfrommis1: TMenuItem;
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
    StructureValue: TEdit;
    LbMiscObjList: TLabel;
    MiscObjList: TListBox;
    LbPlayerSelect: TLabel;
    PlayerSelect: TComboBox;
    LbStructureList: TLabel;
    StructureList: TListBox;
    RbCustomBlock: TRadioButton;
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
    // Main form events
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
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
    procedure SelectTileset(Sender: TObject);
    procedure Selectnext1Click(Sender: TObject);
    procedure Loadtileset1Click(Sender: TObject);
    procedure ShowGrid1Click(Sender: TObject);
    procedure Marktiles1Click(Sender: TObject);
    procedure Useallocationindexes1Click(Sender: TObject);
    procedure Showeventmarkers1Click(Sender: TObject);
    procedure Setmapsize1Click(Sender: TObject);
    procedure Shiftmap1Click(Sender: TObject);
    procedure Changestructureowner1Click(Sender: TObject);
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

    current_dir: String;
    tileset_menuitems: array[1..cnt_tilesets] of TMenuItem;

    // Graphic data
    graphics_tileset: TPicture;
    graphics_structures: TPicture;
    graphics_structures_mask: TPicture;
    graphics_misc_objects: TPicture;

    // Tileset data
    tileset_attributes: array[0..size_tileatr-1] of integer;

    // Map variables
    map_data: array[0..127, 0..127] of TMapTile;
    map_loaded: boolean;
    map_filename: String;
    map_width: word;
    map_height: word;
    map_tileset: word;

    // Mis file variables
    mis_map_markers: array[0..127, 0..127] of TEventMarker;
    mis_events: array[0..63,0..71] of byte;
    mis_alloc_index: array[0..cnt_mis_players] of byte;

    // Statistics variables
    mstat_player: array[0..cnt_players-1] of TPlayerStats;
    mstat_num_worm_spawners: word;
    mstat_num_player_starts: word;
    mstat_num_spice_blooms: word;

    // Map canvas variables
    map_canvas_width: word;
    map_canvas_height: word;
    map_canvas_left: word;
    map_canvas_top: word;
    map_canvas_old_left: word;
    map_canvas_old_top: word;
    mouse_old_x: word;
    mouse_old_y: word;

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
    block_placed: boolean;

    // Undo variables
    undo_history: array[0..max_undo_steps] of TUndoEntry;
    undo_start: integer;
    undo_max: integer;
    undo_pos: integer;
    undo_block_start: boolean;

    // Procedures and functions
    procedure resize_map_canvas;
    procedure render_map;
    procedure render_minimap;
    procedure load_map(filename: String);
    procedure save_map(filename: String);
    procedure load_misfile(filename: String);
    procedure load_tileatr(tileset: integer);
    procedure check_map_errors;
    function get_tile_type(value: integer): TileType;
    procedure structure_params_to_value;
    function structure_value_to_params(value: word; var player: word; var index: word; var is_misc: boolean): boolean;
    procedure change_tileset(index: integer);
    procedure calculate_power_and_statistics;
    procedure show_power;

    // Procedures related to drawing tiles/terrain and cursor image
    procedure select_block_from_tileset(b_width, b_height, b_left, b_top: word);
    procedure copy_block_from_map(b_width, b_height, b_left, b_top: word; structures: boolean);
    procedure put_block_on_map;
    procedure fill_area(x,y: word);
    procedure put_sand_rock_dunes(x,y: word);
    procedure set_tile_value(x,y: word; tile: word; special: word; single_op: boolean);
    procedure do_undo;
    procedure do_redo;
    procedure resize_cursor_image;
    procedure draw_cursor_image;
    procedure apply_key_preset(num: integer; count_cliff: integer; count_border: integer);

    // Procedures called from other dialog
    procedure set_map_size(new_width, new_height: integer);
    procedure shift_map(direction, num_tiles: integer);
    procedure change_structure_owner(player_from, player_to: integer; swap: boolean);
    procedure new_map(new_width, new_height: integer);
  end;

var
  MainWindow: TMainWindow;

implementation

{$R *.dfm}

procedure TMainWindow.FormCreate(Sender: TObject);
begin
  randomize;
  tileset_menuitems[1] := BLOXBASE1;
  tileset_menuitems[2] := BLOXBAT1;
  tileset_menuitems[3] := BLOXBGBS1;
  tileset_menuitems[4] := BLOXICE1;
  tileset_menuitems[5] := BLOXTREE1;
  tileset_menuitems[6] := BLOXWAST1;
  tileset_menuitems[7] := BLOXXMAS1;
  current_dir := ExtractFilePath(Application.ExeName);
  graphics_tileset := TPicture.Create;
  change_tileset(3);
  graphics_structures := TPicture.Create;
  graphics_structures_mask := TPicture.Create;
  graphics_misc_objects := TPicture.Create;
  graphics_structures_mask.bitmap.pixelformat := pf1bit;
  graphics_structures.LoadFromFile(current_dir + '/graphics/structures.bmp');
  graphics_structures_mask.LoadFromFile(current_dir + '/graphics/structures_mask.bmp');
  graphics_misc_objects.LoadFromFile(current_dir + '/graphics/misc_objects.bmp');
  map_loaded := false;
  top := 60;
  Application.HintHidePause:=100000;
  draw_cursor_image;
  // Load map given as first parameter
  if ParamCount > 0 then
    load_map(ParamStr(1));
end;

procedure TMainWindow.FormResize(Sender: TObject);
begin
  resize_map_canvas;
  EditorMenu.Left := ClientWidth - 168;
  EditorMenu.Height := Height - 72;
  StructureList.Height := EditorMenu.Height - 354;
  EditorPages.Height := Height - 214;
  StatusBar.Panels[3].Width := ClientWidth - 520;
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
    ord('B'): RbCustomBlock.Checked := true;
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
    ord('5'): apply_key_preset(9, 1, 1);
    ord('Q'): apply_key_preset(10, 1, 1); // Left
    ord('E'): apply_key_preset(11, 4, 5);
    ord('A'): apply_key_preset(16, 1, 1);
    ord('T'): apply_key_preset(17, 1, 1); // Down
    ord('D'): apply_key_preset(18, 4, 4);
    ord('G'): apply_key_preset(22, 1, 1);
    ord('Z'): apply_key_preset(23, 1, 1); // Right
    ord('X'): apply_key_preset(24, 1, 1);
    ord('C'): apply_key_preset(25, 4, 3);
    ord('V'): apply_key_preset(29, 1, 1);
    ord('B'): apply_key_preset(30, 1, 1);
    ord('W'): apply_key_preset(31, 1, 1); // Inner curves
    ord('S'): apply_key_preset(32, 1, 1);
    ord('R'): apply_key_preset(33, 1, 1);
    ord('F'): apply_key_preset(34, 1, 1);
    ord('6'): apply_key_preset(35, 1, 1); // Up
    ord('7'): apply_key_preset(36, 1, 1);
    ord('8'): apply_key_preset(37, 1, 1);
    ord('9'): apply_key_preset(38, 1, 1);
    ord('Y'): apply_key_preset(39, 1, 1); // Left
    ord('U'): apply_key_preset(40, 1, 1);
    ord('J'): apply_key_preset(41, 1, 1);
    ord('H'): apply_key_preset(42, 1, 1);
    ord('O'): apply_key_preset(43, 1, 1); // Right
    ord('I'): apply_key_preset(44, 1, 1);
    ord('K'): apply_key_preset(45, 1, 1);
    ord('L'): apply_key_preset(46, 1, 1);
    ord('N'): apply_key_preset(47, 1, 1); // Down
    ord('M'): apply_key_preset(48, 1, 1);
    188:      apply_key_preset(49, 1, 1);
    190:      apply_key_preset(50, 1, 1);
    ord('0'): apply_key_preset(51, 1, 1); // Others
    ord('P'): apply_key_preset(52, 1, 1);
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
    map_filename := MapSaveDialog.FileName;
    StatusBar.Panels[3].Text := MapSaveDialog.FileName;
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
  block_placed := false;
end;

procedure TMainWindow.Redo1Click(Sender: TObject);
begin
  do_redo;
  block_placed := false;
end;

procedure TMainWindow.SelectTileset(Sender: TObject);
begin
  change_tileset((sender as TMenuItem).Tag)
end;

procedure TMainWindow.Selectnext1Click(Sender: TObject);
begin
  map_tileset := map_tileset + 1;
  if map_tileset > cnt_tilesets then
    map_tileset := 1;
  change_tileset(map_tileset);  
end;

procedure TMainWindow.Loadtileset1Click(Sender: TObject);
begin
  if TilesetOpenDialog.Execute then
  begin
    graphics_tileset.LoadFromFile(TilesetOpenDialog.FileName);
    tileset_menuitems[map_tileset].Checked := False;
    map_tileset := 0;
    load_tileatr(3);
    StatusBar.Panels[1].Text := 'Tileset File';
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

procedure TMainWindow.KeyShortcuts1Click(Sender: TObject);
begin
  ShowMessage('Key Shortcuts:'#13#13'Space = Open tileset window'#13'Esc = Close tileset window'#13'Tab = Switch Structures / Terrain'#13'Shift + 1 - 8 = Block size preset'#13+
              'Shift + S = Paint sand'#13'Shift + R = Paint rock'#13'Shift + D = Paint dunes'#13'Shift + B = Tile block'#13'Shift + C = Select and copy mode'#13'Shift + T = Select structures'#13'Ctrl + Z = Undo'#13'Ctrl + Y = Redo'#13'F1 - F4 = Block key-preset group'#13'Num 2,4,6,8 = Move block on map');
end;

procedure TMainWindow.Mouseactions1Click(Sender: TObject);
begin
  ShowMessage('Mouse actions'#13#13'When editing structures:'#13'Left = Place structure'#13'Right = Remove structure'#13'Middle = Copy structure'#13#13'When editing terrain:'#13'Left = Draw / Place block'#13'Middle = Copy block'#13'Right = Drag and scroll map');
end;

procedure TMainWindow.About1Click(Sender: TObject);
begin
  ShowMessage('Dune 2000 Campaign Map Editor'#13#13'Part of D2K+ Editing tools'#13#13'Made by Klofkac'#13'Version 0.5'#13'Date: 2015-05-02'#13#13'http://github.com/jkoncick/D2kEditor');
end;

procedure TMainWindow.MapScrollHChange(Sender: TObject);
begin
  map_canvas_left := MapScrollH.Position;
  render_map;
end;

procedure TMainWindow.MapScrollVChange(Sender: TObject);
begin
  map_canvas_top := MapScrollV.Position;
  render_map;
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
  if (mouse_old_x = map_x) and (mouse_old_y = map_y) then
    exit;
  block_placed := false;
  if (EditorPages.TabIndex = 0) and ((mis_map_markers[map_x,map_y].emtype = emReinforcement) or (mis_map_markers[map_x,map_y].emtype = emSpawn)) then
  begin
    eventnum := mis_map_markers[map_x,map_y].index;
    numunits := mis_events[eventnum, 14];
    tmp_hint := inttostr(numunits) + ' units:';
    for i := 0 to (numunits -1) do
      tmp_hint := tmp_hint + chr(13) + unit_names[mis_events[eventnum, 47+i]];
    MapCanvas.Hint := tmp_hint;
    MapCanvas.ShowHint := true
  end else
    MapCanvas.ShowHint := false;
  // Scroll map while holding right button
  if (ssRight in shift) and (EditorPages.TabIndex = 1) then
  begin
    MapScrollH.Position := map_canvas_left + (mouse_old_x - map_x);
    MapScrollV.Position := map_canvas_top + (mouse_old_y - map_y);
  end else
  begin
    mouse_old_x := map_x;
    mouse_old_y := map_y;
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
  if (ssLeft in shift) and (((EditorPages.TabIndex = 1) and (not RbCustomBlock.Checked)) or ((EditorPages.TabIndex = 0) and (strtoint(StructureValue.Text) <= 2))) then
    MapCanvasMouseDown(sender,mbLeft,Shift,x,y);
end;

procedure TMainWindow.MapCanvasMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  xx, yy: integer;
  map_x, map_y: integer;
  index, player: word;
  is_misc: boolean;
begin
  map_x := x div 32 + map_canvas_left;
  map_y := y div 32 + map_canvas_top;
  if block_placed then
    exit;
  block_placed := true;
  if EditorPages.TabIndex = 0 then
    begin
    // Editing structures
    if Button = mbLeft then
      // Put structure on map
      set_tile_value(map_x, map_y, map_data[map_x, map_y].tile, strtoint(StructureValue.Text), true)
    else if Button = mbRight then
      // Delete structure from map
      set_tile_value(map_x, map_y, map_data[map_x, map_y].tile, 0, true)
    else if Button = mbMiddle then
    begin
      // Get structure parameters on position and set them in menu
      StructureValue.text := inttostr(map_data[map_x, map_y].special);
      if structure_value_to_params(map_data[map_x, map_y].special, player, index, is_misc) then
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
          show_power;
        end;
      end else
      begin
        MiscObjList.ItemIndex := -1;
        StructureList.ItemIndex := -1;
        exit;
      end;
      exit;
    end;
    calculate_power_and_statistics;
  end else
  begin
    // Editing terrain
    if button = mbLeft then
    begin
      if RbSelectMode.Checked then
      begin
        // Start selection
        block_select_started := true;
        block_select_start_x := map_x;
        block_select_start_y := map_y;
        block_select_end_x := map_x;
        block_select_end_y := map_y;
      end else
      if RbCustomBlock.Checked then
      begin
        // Draw selected block
        put_block_on_map;
      end else
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
  render_minimap;
  render_map;
end;

procedure TMainWindow.MapCanvasDblClick(Sender: TObject);
var
  tmp_pos: integer;
begin
  if ((EditorPages.TabIndex = 1) and (RbSand.Checked or RbRock.Checked or RbDunes.Checked)) or ((EditorPages.TabIndex = 0) and (StructureValue.Text = '2')) then
  begin
    tmp_pos := undo_pos;
    repeat
      tmp_pos := (tmp_pos - 1) and max_undo_steps
    until undo_history[tmp_pos].is_first;
    if (undo_history[tmp_pos].x = mouse_old_x) and (undo_history[tmp_pos].y = mouse_old_y) then
      do_undo;
    undo_block_start := true;
    fill_area(mouse_old_x, mouse_old_y);
    render_minimap;
    render_map;
  end;
end;

procedure TMainWindow.MapCanvasMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  min_x, max_x, min_y, max_y: word;
begin
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
  if EditorPages.TabIndex = 1 then
    calculate_power_and_statistics;
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
  structure_params_to_value;
end;

procedure TMainWindow.PlayerSelectChange(Sender: TObject);
begin
  structure_params_to_value;
  show_power;
end;

procedure TMainWindow.MiscObjListClick(Sender: TObject);
begin
  StructureList.ItemIndex := -1;
  structure_params_to_value;
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
  if RbCustomBlock.Checked and (EditorPages.TabIndex = 1) and (Mouse.CursorPos.X - Left < EditorMenu.Left) then
  begin
    CursorImage.Visible := true;
    if not TilesetDialog.Visible then
      RbCustomBlock.SetFocus;
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
  render_map;
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
      MapCanvas.Canvas.CopyRect(rect(x*32,y*32,x*32+32,y*32+32),graphics_tileset.Bitmap.Canvas,rect((tile mod 20)*32,(tile div 20 * 32),(tile mod 20)*32+32,(tile div 20 * 32+32)));
      // Draw tile attribute markers
      if Marktiles1.Checked then
      begin
        tile_attr := get_tile_type(tileset_attributes[tile]);
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
      if structure_value_to_params(value,player,index,is_misc) then
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
            player := mis_alloc_index[player];
          if player >= cnt_players then
            player := 0;
          if index = 0 then
          begin
            // Structure is wall
            wall_bitmap := 0;
            // Checking left of wall
            if (actual_x - 1) >= 0 then
            begin
              value := map_data[actual_x - 1, actual_y].special;
              if structure_value_to_params(value,player,index,is_misc) and structure_params[index].lnwall then
                wall_bitmap := wall_bitmap + 1
            end;
            // Checking up of wall
            if (actual_y - 1) >= 0 then
            begin
              value := map_data[actual_x, actual_y - 1].special;
              if structure_value_to_params(value,player,index,is_misc) and structure_params[index].lnwall then
                wall_bitmap := wall_bitmap + 2
            end;
            // Checking right of wall
            if (actual_x + 1) < map_width then
            begin
              value := map_data[actual_x + 1, actual_y].special;
              if structure_value_to_params(value,player,index,is_misc) and structure_params[index].lnwall then
                wall_bitmap := wall_bitmap + 4
            end;
            // Checking down of wall
            if (actual_y + 1) < map_height then
            begin
              value := map_data[actual_x, actual_y + 1].special;
              if structure_value_to_params(value,player,index,is_misc) and structure_params[index].lnwall then
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
      end;
    end;
  end;
  // Draw event markers
  if Showeventmarkers1.Checked then
  begin
    for y:= 0 to map_canvas_height - 1 do
    begin
      for x:= 0 to map_canvas_width - 1 do
      begin
        event_marker := addr(mis_map_markers[x + map_canvas_left, y + map_canvas_top]);
        if (event_marker.emtype = emReinforcement) or (event_marker.emtype = emHarvester) or (event_marker.emtype = emSpawn) then
        begin
          player := event_marker.player;
          if player >= cnt_players then
            player := 0;
          MapCanvas.Canvas.Pen.Color := mmap_player_colors[player];
          MapCanvas.Canvas.Brush.Color := mmap_player_colors[player];
          MapCanvas.Canvas.Rectangle(x*32, y*32, x*32+32, y*32+32);
          MapCanvas.Canvas.Pen.Color := clBlack;
          if event_marker.emtype = emReinforcement then
            MapCanvas.Canvas.TextOut(x * 32 + 12, y * 32 + 3, 'R')
          else if event_marker.emtype = emHarvester then
            MapCanvas.Canvas.TextOut(x * 32 + 12, y * 32 + 3, 'H')
          else if event_marker.emtype = emSpawn then
            MapCanvas.Canvas.TextOut(x * 32 + 12, y * 32 + 3, 'S');
          MapCanvas.Canvas.TextOut(x * 32 + 12, y * 32 + 17, inttostr(event_marker.index));
        end
        else if (event_marker.emtype = emTileTrigger) or (event_marker.emtype = emRevealMap) then
        begin
          MapCanvas.Canvas.Pen.Color := clGray;
          MapCanvas.Canvas.Brush.Color := clGray;
          MapCanvas.Canvas.Rectangle(x*32, y*32, x*32+32, y*32+32);
          MapCanvas.Canvas.Pen.Color := clBlack;
          if event_marker.emtype = emTileTrigger then
            MapCanvas.Canvas.TextOut(x * 32 + 12, y * 32 + 3, 'T')
          else if event_marker.emtype = emRevealMap then
            MapCanvas.Canvas.TextOut(x * 32 + 12, y * 32 + 3, 'M');
          MapCanvas.Canvas.TextOut(x * 32 + 12, y * 32 + 17, inttostr(event_marker.index));
        end;
      end;
    end;
  end;
  // Draw grid
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
  // Draw position marker on minimap
  MapCanvas.Canvas.CopyMode:=cmSrcCopy;
  MiniMap.Canvas.CopyRect(rect(0,0,128,128),MiniMapTmp.Canvas,rect(0,0,128,128));
  MiniMap.Canvas.Pen.Color:= $00FF00;
  MiniMap.Canvas.Brush.Style := bsClear;
  MiniMap.Canvas.Rectangle(mmap_border_x + map_canvas_left,mmap_border_y + map_canvas_top,mmap_border_x + map_canvas_left + map_canvas_width,mmap_border_y + map_canvas_top + map_canvas_height);
  MiniMap.Canvas.Brush.Style := bsSolid;
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
      tile_attr := get_tile_type(tileset_attributes[map_data[x,y].tile]);
      MiniMapTmp.Canvas.Pixels[x+mmap_border_x,y+mmap_border_y] := mmap_tile_colors[ord(tile_attr)];
    end;
  // Rendering structures
  for y:= 0 to map_height - 1 do
    for x:= 0 to map_width - 1 do
    begin
      value := map_data[x,y].special;
      if not structure_value_to_params(value,player,index,is_misc) then
        continue
      else if is_misc then
      begin
        MiniMapTmp.Canvas.Pixels[x+mmap_border_x,y+mmap_border_y] := mmap_misc_objects_colors[index];
      end else
      begin
        // Translate player number according to allocation index
        if Useallocationindexes1.Checked then
          player := mis_alloc_index[player];
        if player >= cnt_players then
          player := 0;
        // Render structure on map
        MiniMapTmp.Canvas.Pen.Color := mmap_player_colors[player];
        MiniMapTmp.Canvas.Brush.Color := mmap_player_colors[player];
        MiniMapTmp.Canvas.Pixels[x+mmap_border_x,y+mmap_border_y] := mmap_player_colors[player];
        MiniMapTmp.Canvas.Rectangle(x+mmap_border_x,y+mmap_border_y,x+mmap_border_x+structure_params[index].size_x,y+mmap_border_y+structure_params[index].size_y);
      end;
    end;
end;

procedure TMainWindow.load_map(filename: String);
var
  map_file: file of word;
  mis_filename: String;
  x, y: integer;
begin
  if not FileExists(filename) then
    exit;
  if UpperCase(Copy(filename, Length(filename)-2, 3)) <> 'MAP' then
  begin
    Application.MessageBox('Invalid file type', 'Load map error', MB_ICONERROR);
    exit;
  end;
  // Reset map data and event markers
  for x := 0 to 127 do
    for y := 0 to 127 do
    begin
      map_data[x,y].tile := 0;
      map_data[x,y].special := 0;
      mis_map_markers[x,y].emtype := emNone;
    end;
  Undo1.Enabled := false;
  Redo1.Enabled := false;
  undo_start := 0;
  undo_max := 0;
  undo_pos := 0;
  // Reading map file
  AssignFile(map_file, filename);
  Reset(map_file);
  Read(map_file, map_width);
  Read(map_file, map_height);
  for y := 0 to map_height - 1 do
    for x := 0 to map_width - 1 do
    begin
      Read(map_file, map_data[x,y].tile);
      Read(map_file, map_data[x,y].special);
    end;
  CloseFile(map_file);
  // Setting variables and status bar
  map_loaded := true;
  map_filename := filename;
  StatusBar.Panels[3].Text := filename;
  StatusBar.Panels[2].Text := inttostr(map_width)+' x '+inttostr(map_height);
  // Attempting to detect tileset from .mis file
  mis_filename := ExtractFileDir(filename)+'\_'+ExtractFileName(filename);
  mis_filename[length(mis_filename)-1]:= 'I';
  mis_filename[length(mis_filename)]:= 'S';
  load_misfile(mis_filename);
  // Rendering
  resize_map_canvas;
  render_minimap;
  render_map;
  calculate_power_and_statistics;
end;

procedure TMainWindow.save_map(filename: String);
var
  map_file: file of word;
  x, y: integer;
begin
  AssignFile(map_file, filename);
  ReWrite(map_file);
  Write(map_file, map_width);
  Write(map_file, map_height);
  for y := 0 to map_height - 1 do
    for x := 0 to map_width - 1 do
    begin
      Write(map_file, map_data[x,y].tile);
      Write(map_file, map_data[x,y].special);
    end;
  CloseFile(map_file);
end;

procedure TMainWindow.load_misfile(filename: String);
var
  mis_file: file of byte;
  tileset_name_buf: array[0..7] of char;
  tileset_name: String;
  num_events, num_conds: byte;
  event: array[0..71] of byte;
  condition: array[0..27] of byte;
  x, y: byte;
  i: integer;
begin
  if not FileExists(filename) then
    exit;
  AssignFile(mis_file, filename);
  Reset(mis_file);
  // Load tileset
  if Detectfrommis1.Checked then
  begin
    Seek(mis_file,$10598);
    BlockRead(mis_file,tileset_name_buf,8);
    tileset_name := String(tileset_name_buf);
    for i:= 1 to cnt_tilesets do
    begin
      if tileset_name = tilesets[i] then
        change_tileset(i);
    end;
  end;
  // Load allocation indexes
  Seek(mis_file,$50);
  BlockRead(mis_file,mis_alloc_index,8);

  // Load map markers (reinforcement destinations etc)
  Seek(mis_file,$10728);
  Read(mis_file,num_events);
  Read(mis_file,num_conds);
  Seek(mis_file,$EE58);
  BlockRead(mis_file, mis_events, 64*72);
  for i:= 0 to num_events - 1 do
  begin
    Move(mis_events[i], event, 72);
    if (event[13] = 0) or (event[13] = 18) or (event[13] = 14) then
    begin
      // Reinforcement, spawn, Reveal map
      x := event[0];
      y := event[4];
      if event[13] = 18 then
        mis_map_markers[x][y].emtype := emSpawn
      else if event[13] = 14 then
         mis_map_markers[x][y].emtype := emRevealMap
      else if (event[14] = 1) and (event[47] = 8) then
        mis_map_markers[x][y].emtype := emHarvester
      else
        mis_map_markers[x][y].emtype := emReinforcement;
      mis_map_markers[x][y].player := event[15];
      mis_map_markers[x][y].index := i;
    end;
  end;
  Seek(mis_file,$10058);
  for i:= 0 to num_conds - 1 do
  begin
    BlockRead(mis_file,condition,28);
    if condition[25] = 7 then
    begin
      // Unit in tile
      x := condition[12];
      y := condition[16];
      mis_map_markers[x][y].emtype := emTileTrigger;
      mis_map_markers[x][y].index := i;
    end;
  end;

  CloseFile(mis_file);
end;

procedure TMainWindow.load_tileatr(tileset: integer);
var
  tileatr_file: file of integer;
begin
  if not FileExists(current_dir+'/tilesets/'+tileatr_filenames[tileset]) then
    exit;
  AssignFile(tileatr_file, current_dir+'/tilesets/'+tileatr_filenames[tileset]);
  Reset(tileatr_file);
  BlockRead(tileatr_file, tileset_attributes, size_tileatr);
  CloseFile(tileatr_file);
end;

procedure TMainWindow.check_map_errors;
begin
  if mstat_num_worm_spawners = 0 then
    Application.MessageBox('You must place at least one Worm Spawner.', 'Map error', MB_ICONWARNING);
  if (mstat_num_player_starts <> 0) and (mstat_num_player_starts <> 8) then
    Application.MessageBox('Invalid number of Player Starts. Must be either 0 (for campaign maps) or 8 (for multiplayer maps).', 'Map error', MB_ICONWARNING);
end;

function TMainWindow.get_tile_type(value: integer): TileType;
begin
  if (value and $00006000) = 0 then
    result := ttImpassable
  else if (value and $00006000) = $00004000 then
    result := ttInfantryOnly
  else if (value and $40000000) = $40000000 then
    result := ttSlowdown
  else if (value and $20000000) = $20000000 then
    result := ttBuildable
  else
    result := ttNormal
end;

procedure TMainWindow.structure_params_to_value;
var
  value: word;
begin
  if MiscObjList.ItemIndex > -1 then
    value := misc_obj_values[MiscObjList.ItemIndex]
  else
    value := structure_params[StructureList.ItemIndex].values[PlayerSelect.ItemIndex];
  StructureValue.Text := inttostr(value);
end;


function TMainWindow.structure_value_to_params(value: word; var player,
  index: word; var is_misc: boolean): boolean;
var i, j: integer;
begin
  if value = 0 then
  begin
    result := false;
    exit;
  end;
  for i:= 0 to 9 do
  begin
    if value = misc_obj_values[i] then
    begin
      index := i;
      is_misc := true;
      result := true;
      exit;
    end;
  end;
  for i := 0 to 31 do
  begin
    for j:= 0 to cnt_players - 1 do
    begin
      if value = structure_params[i].values[j] then
      begin
        player := j;
        index := i;
        is_misc := false;
        result := true;
        exit;
      end;
    end;
  end;
  result := false;
end;

procedure TMainWindow.change_tileset(index: integer);
begin
  map_tileset := index;
  tileset_menuitems[map_tileset].Checked := true;
  // Load tileset graphics
  if FileExists(current_dir+'/tilesets/d2k_'+tilesets[map_tileset]+'.bmp') then
    graphics_tileset.LoadFromFile(current_dir+'/tilesets/d2k_'+tilesets[map_tileset]+'.bmp');
  // Load tileset attributes
  load_tileatr(map_tileset);
  StatusBar.Panels[1].Text := tilesets[map_tileset];
  if (TilesetDialog <> nil) and TilesetDialog.Visible then
    TilesetDialog.DrawTileset(nil);
  draw_cursor_image;
  render_minimap;
  render_map;
end;

procedure TMainWindow.calculate_power_and_statistics;
var
  output, need: array[0..cnt_players-1] of integer;
  x, y: integer;
  player, index: word;
  is_misc: boolean;
  tmp_power: integer;
begin
  mstat_num_worm_spawners := 0;
  mstat_num_player_starts := 0;
  mstat_num_spice_blooms := 0;
  for x := 0 to cnt_players - 1 do
  begin
    output[x] := 0;
    need[x] := 0;
  end;
  for y := 0 to map_height - 1 do
    for x := 0 to map_width -1 do
    begin
      if structure_value_to_params(map_data[x,y].special,player,index,is_misc) then
      begin
        if is_misc then
        begin
          if index = 3 then
            inc(mstat_num_worm_spawners)
          else if index = 4 then
            inc(mstat_num_player_starts)
          else if (index >= 5) and (index <= 9) then
            inc(mstat_num_spice_blooms)
        end else
        begin
          tmp_power := structure_params[index].power;
          if tmp_power > 0 then
            need[player] := need[player] + tmp_power
          else if tmp_power < 0 then
            output[player] := output[player] - tmp_power;
        end;
      end;
    end;
  for x := 0 to cnt_players - 1 do
  begin
    if (output[x] > 0) and (need[x] = 0) then
      mstat_player[x].power_percent := output[x] div 2 + 100
    else if (output[x] = 0) then
      mstat_player[x].power_percent := 0
    else
      mstat_player[x].power_percent := round((ln(output[x] / need[x] + 1) / ln(2)) * 100);
    mstat_player[x].power_output := output[x];
    mstat_player[x].power_need := need[x];
  end;
  StatusBar.Panels[4].Text := 'W: '+inttostr(mstat_num_worm_spawners)+'  S: '+inttostr(mstat_num_player_starts)+'  B: '+inttostr(mstat_num_spice_blooms);
  show_power;
end;

procedure TMainWindow.show_power;
var
  i: integer;
begin
  i := PlayerSelect.ItemIndex;
  StatusBar.Panels[5].Text := 'Power: '+inttostr(mstat_player[i].power_percent)+'%   ('+inttostr(mstat_player[i].power_output)+'/'+inttostr(mstat_player[i].power_need)+')';
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
  RbCustomBlock.Checked := true;
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
  RbCustomBlock.Checked := True;
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

procedure TMainWindow.fill_area(x, y: word);
var
  is_sand: boolean;
  replaces_sand: boolean;
  thick_spice: boolean;
  i: integer;
begin
  is_sand := false;
  thick_spice := (EditorPages.TabIndex = 0) and (StructureValue.Text = '2');
  replaces_sand := thick_spice or RbRock.Checked or RbDunes.Checked;
  for i := 0 to 9 do
    if map_data[x,y].tile = tiles_sand[i] then
    begin
      is_sand := true;
      break;
    end;
  if map_data[x,y].special <> 0 then
    is_sand := false;
  if (not is_sand and not replaces_sand) or (is_sand and replaces_sand) then
  begin
    if thick_spice then
      set_tile_value(x, y, map_data[x,y].tile, 2, false)
    else
      put_sand_rock_dunes(x, y)
  end else
    exit;
  if x > 0 then
    fill_area(x-1, y);
  if x < (map_width - 1) then
    fill_area(x+1, y);
  if y > 0 then
    fill_area(x, y-1);
  if y < (map_height - 1) then
    fill_area(x, y+1);
end;

procedure TMainWindow.put_sand_rock_dunes(x, y: word);
begin
  if RbSand.Checked then
    set_tile_value(x, y, tiles_sand[random(10)], 0, false)
  else if RbRock.Checked then
    set_tile_value(x, y, tiles_rock[random(15)], 0, false)
  else if RbDunes.Checked then
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
      BlockImage.Canvas.CopyRect(rect(x*32+border_x, y*32+border_y, x*32+32+border_x, y*32+32+border_y), graphics_tileset.Bitmap.Canvas,rect(tile_x*32, tile_y*32, tile_x*32+32, tile_y*32+32));
      CursorImage.Canvas.CopyRect(rect(x*32,y*32, x*32+32, y*32+32), graphics_tileset.Bitmap.Canvas,rect(tile_x*32, tile_y*32, tile_x*32+32, tile_y*32+32));
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

procedure TMainWindow.set_map_size(new_width, new_height: integer);
begin
  if (map_width = new_width) and (map_height = new_height) then
    exit;
  map_width := new_width;
  map_height := new_height;
  StatusBar.Panels[2].Text := inttostr(map_width)+' x '+inttostr(map_height);
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
  Undo1.Enabled := false;
  Redo1.Enabled := false;
  undo_start := 0;
  undo_max := 0;
  undo_pos := 0;
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
      if structure_value_to_params(map_data[x,y].special, player, index, is_misc) and (not is_misc) then
      begin
        // Change from -> to
        if player = player_from then
        begin
          if player_to = cnt_players then
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
  // Reset map data and event markers
  for x := 0 to 127 do
    for y := 0 to 127 do
    begin
      map_data[x,y].tile := 0;
      map_data[x,y].special := 0;
      mis_map_markers[x,y].emtype := emNone;
    end;
  Undo1.Enabled := false;
  Redo1.Enabled := false;
  undo_start := 0;
  undo_max := 0;
  undo_pos := 0;
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
  calculate_power_and_statistics;
  resize_map_canvas;
  render_minimap;
  render_map;
end;

end.
