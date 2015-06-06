unit mis_file;

interface

// Mis file constants
const cnt_mis_players = 8;

const unit_names_long: array[0..29] of string = (
  'Light infantry',
  'Trooper',
  'Engineer',
  'Thumper infantry',
  'Sardaukar',
  'Trike',
  'Raider',
  'Quad',
  'Harvester',
  'Combat tank (A)',
  'Combat tank (H)',
  'Combat tank (O)',
  'MCV',
  'Missile tank',
  'Deviator',
  'Siege tank',
  'Sonic tank',
  'Devastator',
  'Carryall',
  'Carryall (A)',
  'Ornithropter',
  'Stealth Fremen',
  'Fremen',
  'Saboteur',
  'Death Hand Missile',
  'Sandworm',
  'Frigate',
  'Grenadier',
  'Stealth Raider',
  'MP Sardaukar'
  );

// Mis file type definitions
type
  EventType = (etReinforcement, etStarportDelivery, etAllegiance, etLeave,
    etBerserk, etPlaySound, etSetBuildRate, etSetAttackBuildingRate, etSetCash,
    etSetTech, etMissionWin, etMissionFail, etBloxFile, etAttribFile,
    etRevealMap, etShowTimer, etHideTimer, etShowMessage, etUnitSpawn, etSetFlag);

type
  ConditionType = (ctBuildingExists, ctUnitExists, ctInterval, ctTimer,
    ctCasualties, ctBaseDestroyed, ctUnitsDestroyed, ctTileRevealed,
    ctSpiceHarvested, ctFlag);

type
   EventMarkerType = (emNone, emReinforcement, emHarvester, emUnitSpawn, emTileRevealed, emRevealMap);

type
  TEventMarkerTypeInfo = record
    letter: char;
    player_related: boolean;
  end;

type
  TEventMarker = record
    emtype: EventMarkerType;
    player: word;
    index: word;
    moved: boolean;
  end;

// Mis file type definition constants
const event_marker_type_info: array[0..5] of TEventMarkerTypeInfo =
  (
    (letter: ' '; player_related: false),
    (letter: 'R'; player_related: true),
    (letter: 'H'; player_related: true),
    (letter: 'S'; player_related: true),
    (letter: 'T'; player_related: false),
    (letter: 'M'; player_related: false)
  );

// Mis file variables
var
  mis_filename: String;
  mis_map_markers: array[0..127, 0..127] of TEventMarker;
  mis_events: array[0..63,0..71] of byte;
  mis_alloc_index: array[0..cnt_mis_players] of byte;

// Mis file functions and procedures
  function get_mis_filename(filename: String): String;
  procedure load_mis_file(filename: String);
  procedure create_empty_mis_file(filename: String);



implementation

uses Windows, Forms, SysUtils, main, tileset, map_defs;

function get_mis_filename(filename: String): String;
var
  tmp_filename: String;
begin
  tmp_filename := ExtractFileDir(filename)+'\_'+UpperCase(ExtractFileName(filename));
  tmp_filename[length(tmp_filename)-1]:= 'I';
  tmp_filename[length(tmp_filename)]:= 'S';
  result := tmp_filename;
end;

procedure load_mis_file(filename: String);
var
  mis_file: file of byte;
  tileset_name_buf: array[0..7] of char;
  tileset_name: String;
  num_events, num_conds: byte;
  event: array[0..71] of byte;
  event_type: EventType;
  condition: array[0..27] of byte;
  x, y: byte;
  i: integer;
  moved: boolean;
begin
  MainWindow.Createemptymisfile1.Enabled := not FileExists(filename);
  if not FileExists(filename) then
    exit;
  AssignFile(mis_file, filename);
  Reset(mis_file);
  // Load tileset
  Seek(mis_file,$10598);
  BlockRead(mis_file,tileset_name_buf,8);
  tileset_name := String(tileset_name_buf);
  for i:= 0 to cnt_tilesets-1 do
  begin
    if tileset_name = tilesets[i].name then
      tileset_change(i);
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
    event_type := EventType(event[13]);
    if (event_type = etReinforcement) or (event_type = etUnitSpawn) or (event_type = etRevealMap) then
    begin
      // Reinforcement, spawn, Reveal map
      x := event[0];
      y := event[4];
      // Move event marker one tile to right if this tile has already an event
      moved := false;
      while mis_map_markers[x][y].emtype <> emNone do
      begin
        x := (x + 1) mod map_width;
        moved := true;
      end;
      if event_type = etUnitSpawn then
        mis_map_markers[x][y].emtype := emUnitSpawn
      else if event_type = etRevealMap then
         mis_map_markers[x][y].emtype := emRevealMap
      else if (event[14] = 1) and (event[47] = 8) then
        mis_map_markers[x][y].emtype := emHarvester
      else
        mis_map_markers[x][y].emtype := emReinforcement;
      mis_map_markers[x][y].player := event[15];
      mis_map_markers[x][y].index := i;
      mis_map_markers[x][y].moved := moved;
    end;
  end;
  Seek(mis_file,$10058);
  for i:= 0 to num_conds - 1 do
  begin
    BlockRead(mis_file,condition,28);
    if condition[25] = byte(ctTileRevealed) then
    begin
      // Unit in tile
      x := condition[12];
      y := condition[16];
      mis_map_markers[x][y].emtype := emTileRevealed;
      mis_map_markers[x][y].index := i;
    end;
  end;

  CloseFile(mis_file);
  mis_filename := filename;
end;

procedure create_empty_mis_file(filename: String);
var
  mis_file: file of byte;
  i, j: integer;
  b: byte;
begin
  if FileExists(filename) then
  begin
    Application.MessageBox('Mission file for this map already exists.', 'Create empty .mis file', MB_ICONERROR);
    exit;
  end;
  AssignFile(mis_file, filename);
  ReWrite(mis_file);
  b := 0;
  for i := 0 to 68065 do
    Write(mis_file, b);
  // Write tileset name
  Seek(mis_file,$10598);
  BlockWrite(mis_file, tilesets[tileset_index].name[1], Length(tilesets[tileset_index].name));
  // Write tileatr file name
  Seek(mis_file,$10660);
  BlockWrite(mis_file, tilesets[tileset_index].tileatr_name[1], Length(tilesets[tileset_index].tileatr_name));
  // Write allocation indexes
  Seek(mis_file,$50);
  for i := 0 to 7 do
    Write(mis_file, i);
  // Write tech levels
  Seek(mis_file,$0);
  b := 8;
  for i := 0 to 7 do
    Write(mis_file, b);
  // Write starting money
  Seek(mis_file,$8);
  for i := 0 to 7 do
  begin
    b := $88;
    Write(mis_file, b);
    b := $13;
    Write(mis_file, b);
    b := 0;
    Write(mis_file, b);
    Write(mis_file, b);
  end;
  // Write diplomacy
  Seek(mis_file,$EE18);
  for i := 0 to 7 do
    for j := 0 to 7 do
    begin
      if i = j then b := 0 else b := 1;
      Write(mis_file, b);
    end;
  CloseFile(mis_file);
  Application.MessageBox('Mission file created', 'Create empty .mis file', MB_ICONINFORMATION);
  MainWindow.Createemptymisfile1.Enabled := false;
end;

end.
