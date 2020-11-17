unit _dispatcher;

interface

type
  TDispatcherRegisteredEvent = (
    // Map-related events
    evMapLoad,
    evMapResize,
    evMapShift,
    evMapTilesModify,
    evMapFilenameChange,
    // Mission-related eventd
    evMisLoad,
    evMisAllocIndexChange,
    evMisEventPositionChange,
    evMisDefenceAreaChange,
    // Initialize tilesets event
    evInitTilesets,
    // Tileset file load events
    evFLTilesetImage,
    evFLTileatrBin,
    evFLTilesetIni,
    // UIB string table file load events
    evFLTextUib,
    evFLSamplesUib,
    // Structures file load events
    evFLTemplatesBin,
    evFLBuilexpBin,
    evFLArmourBin,
    evFLSpeedBin,
    evFLTechposBin,
    evFLTiledataBin,
    evFLColoursBin,
    evFLMiscObjectsIni,
    evFLPlayersIni,
    // Graphics file load events
    evFLMiscObjectsBmp,
    evFLDataR16,
    // Setting change events
    evSCTranslateStructureNames,
    // Apply changes events
    evACTileAtrEditor,
    evACStructuresEditor
  );
  TDispatcherPendingAction = (
    // Update selection lists
    paUpdateTilesetList,
    paUpdateTextList,
    paUpdateSoundList,
    paUpdateStructuresList,
    paUpdateStructuresListTranslated,
    paUpdateMiscObjectList,
    paUpdatePlayerList,
    // Update various contents
    paUpdateTileset,
    paUpdateStructureControls,
    paUpdateMapDimensions,
    paUpdateMapName,
    paUpdateMissionLoadStatus,
    paUpdateMapStats,
    paUpdateEventMarkers,
    paUpdateMisAIProperties,
    paUpdatePlayerColours,
    paUpdateSpeedModifiers,
    // Update whole dialog contents
    paUpdateEventDialog,
    paUpdateTileAtrEditor,
    paUpdateStructuresEditor,
    // Rendering
    paRenderMap,
    paRenderMinimap,
    paRenderCursorImage
  );

type
  TDispatcher = class

  private
    // Pending action set
    pact: set of TDispatcherPendingAction;

  public
    procedure register_event(event: TDispatcherRegisteredEvent);
    procedure do_pending_actions;

  private
    procedure update_structures_list_translated;
    procedure update_player_list;
    procedure update_tileset;
    procedure update_map_name;
    procedure update_mission_load_status;
    procedure update_map_stats;
    procedure update_event_markers;
    procedure update_mis_ai_properties;

  end;

var
  Dispatcher: TDispatcher;

implementation

uses
  Forms, Classes, SysUtils, _utils, _settings, _structures, _misai, _map, _mission, main, set_dialog,
  test_map_dialog, tileset_dialog, block_preset_dialog,
  mission_dialog, event_dialog, map_stats_dialog,
  tileatr_editor, structures_editor;

{ TDispatcher }

procedure TDispatcher.register_event(event: TDispatcherRegisteredEvent);
begin
  case event of
    // Map-related events
    evMapLoad:                    pact := pact + [paUpdateMapDimensions, paUpdateMapName, paUpdateMapStats, paRenderMap, paRenderMinimap];
    evMapResize:                  pact := pact + [paUpdateMapDimensions, paUpdateMapStats, paUpdateEventMarkers, paUpdateEventDialog, paRenderMap, paRenderMinimap];
    evMapShift:                   pact := pact + [paUpdateMapStats, paUpdateEventMarkers, paUpdateEventDialog, paRenderMap, paRenderMinimap];
    evMapTilesModify:             pact := pact + [paUpdateMapStats, paRenderMap, paRenderMinimap];
    evMapFilenameChange:          pact := pact + [paUpdateMapName];
    // Mission-related events
    evMisLoad:                    pact := pact + [paUpdateStructureControls, paUpdateMissionLoadStatus, paUpdateEventMarkers, paUpdateEventDialog, paRenderMap, paRenderMinimap, paRenderCursorImage];
    evMisAllocIndexChange:        pact := pact + [paUpdateStructureControls, paRenderMap, paRenderMinimap, paRenderCursorImage];
    evMisEventPositionChange:     pact := pact + [paUpdateEventMarkers];
    evMisDefenceAreaChange:       if Settings.MarkDefenceAreas then pact := pact + [paRenderMap];
    // Initialize tilesets event
    evInitTilesets:               pact := pact + [paUpdateTilesetList];
    // Tileset file load events
    evFLTilesetImage:             pact := pact + [paUpdateTileset, paUpdateTileAtrEditor, paRenderMap, paRenderCursorImage];
    evFLTileatrBin:               pact := pact + [paUpdateTileset, paUpdateTileAtrEditor, paRenderMap, paRenderMinimap, paRenderCursorImage];
    evFLTilesetIni:               pact := pact + [paUpdateTileset, paUpdateTileAtrEditor, paRenderMinimap];
    // UIB string table file load events
    evFLTextUib:
      begin
                                  pact := pact + [paUpdateTextList, paUpdateEventDialog];
        if Settings.TranslateStructureNames then
                                  pact := pact + [paUpdateStructuresListTranslated];
      end;
    evFLSamplesUib:               pact := pact + [paUpdateSoundList];
    // Structures file load events
    evFLTemplatesBin:             pact := pact + [paUpdateStructuresList, paUpdateStructuresListTranslated, paUpdateStructureControls, paUpdateMisAIProperties, paUpdateMapStats, paUpdateEventDialog, paUpdateStructuresEditor, paRenderMap, paRenderMinimap, paRenderCursorImage];
    evFLBuilexpBin:               pact := pact + [paUpdateStructuresEditor];
    evFLArmourBin:                pact := pact + [paUpdateStructuresEditor];
    evFLSpeedBin:                 pact := pact + [paUpdateSpeedModifiers, paUpdateStructuresEditor];
    evFLTechposBin:               pact := pact + [paUpdateStructuresEditor];
    evFLTiledataBin:              pact := pact + [paUpdateMapStats, paRenderMap, paRenderMinimap, paRenderCursorImage];
    evFLColoursBin:               pact := pact + [paUpdatePlayerColours, paRenderMap, paRenderMinimap, paRenderCursorImage];
    evFLMiscObjectsIni:           pact := pact + [paUpdateMiscObjectList, paRenderMap, paRenderMinimap, paRenderCursorImage];
    evFLPlayersIni:               pact := pact + [paUpdatePlayerList, paUpdateEventDialog];
    // Graphics file load events
    evFLMiscObjectsBmp:           pact := pact + [paRenderMap, paRenderCursorImage];
    evFLDataR16:                  pact := pact + [paUpdateStructuresEditor, paRenderMap, paRenderCursorImage];
    // "Translate structure names" setting changed
    evSCTranslateStructureNames:  pact := pact + [paUpdateStructuresListTranslated];
    // Apply changes events
    evACTileAtrEditor:            pact := pact + [paRenderMap, paRenderMinimap, paRenderCursorImage];
    evACStructuresEditor:         pact := pact + [paUpdateStructuresList, paUpdateStructuresListTranslated, paUpdateStructureControls, paUpdateMisAIProperties, paUpdateMapStats, paUpdateSpeedModifiers, paUpdateEventDialog, paRenderMap, paRenderMinimap, paRenderCursorImage];
  end;
end;

procedure TDispatcher.do_pending_actions;
begin
  if (pact = []) then
    exit;
  // Update selection lists
  if paUpdateTilesetList        in pact then begin SetDialog.update_tileset_list; TileAtrEditor.update_tileset_list; end;
  if paUpdateTextList           in pact then TileAtrEditor.update_text_list;
  if paUpdateSoundList          in pact then begin EventDialog.update_sound_list; StructuresEditor.update_sound_list; end;
  if paUpdateStructuresList     in pact then EventDialog.update_structures_list;
  if paUpdateStructuresListTranslated in pact then update_structures_list_translated;
  if paUpdateMiscObjectList     in pact then MainWindow.update_misc_object_list;
  if paUpdatePlayerList         in pact then update_player_list;
  if paUpdateTileset            in pact then update_tileset;
  // Update various contents
  if paUpdateStructureControls  in pact then MainWindow.update_structure_controls;
  if paUpdateMapDimensions      in pact then MainWindow.update_map_dimensions;
  if paUpdateMapName            in pact then update_map_name;
  if paUpdateMissionLoadStatus  in pact then update_mission_load_status;
  if paUpdateMapStats           in pact then update_map_stats;
  if paUpdateEventMarkers       in pact then update_event_markers;
  if paUpdateMisAIProperties    in pact then update_mis_ai_properties;
  if paUpdatePlayerColours      in pact then MissionDialog.update_player_colors;
  if paUpdateSpeedModifiers     in pact then TileAtrEditor.update_speed_modifiers;
  // Update whole dialog contents
  if paUpdateEventDialog        in pact then EventDialog.update_contents;
  if paUpdateTileAtrEditor      in pact then TileAtrEditor.update_contents;
  if paUpdateStructuresEditor   in pact then StructuresEditor.update_contents;
  // Rendering
  if paRenderMap                in pact then MainWindow.render_map;
  if paRenderMinimap            in pact then MainWindow.render_minimap;
  if paRenderCursorImage        in pact then MainWindow.render_cursor_image;
  // Reset pending actions
  pact := [];
end;

procedure TDispatcher.update_structures_list_translated;
var
  i: integer;
  tmp_strings_buildings, tmp_strings_units: TStringList;
begin
  tmp_strings_buildings := TStringList.Create;
  tmp_strings_units := TStringList.Create;
  for i := 0 to Structures.building_type_mapping_count - 1 do
    tmp_strings_buildings.Add(Structures.get_building_type_str(Structures.building_type_mapping[i]));
  for i := 0 to Structures.templates.UnitTypeCount - 1 do
    tmp_strings_units.Add(Structures.get_unit_type_str(i));
  MainWindow.update_structures_list(tmp_strings_buildings, tmp_strings_units);
  MapStatsDialog.update_structures_list(tmp_strings_buildings, tmp_strings_units);
  tmp_strings_buildings.Destroy;
  tmp_strings_units.Destroy;
end;

procedure TDispatcher.update_player_list;
var
  player_list: TStringList;
  i: integer;
begin
  player_list := TStringList.Create;
  for i := 0 to CNT_PLAYERS - 1 do
    player_list.Add(inttostr(i) + ' - ' + Structures.player_names[i]);
  MainWindow.update_player_list(player_list);
  SetDialog.update_player_list(player_list);
  TestMapDialog.update_player_list(player_list);
  MapStatsDialog.update_player_list;
  MissionDialog.update_player_list(player_list);
  EventDialog.update_player_list(player_list);
  player_list.Destroy;
end;

procedure TDispatcher.update_tileset;
begin
  MainWindow.update_tileset;
  SetDialog.update_tileset;
  TilesetDialog.update_tileset;
  BlockPresetDialog.update_tileset;
  MissionDialog.update_tileset;
  StructuresEditor.update_tileset;
end;

procedure TDispatcher.update_map_name;
var
  map_name: string;
begin
  MainWindow.update_map_name;
  if Map.filename = '' then
    map_name := 'Untitled'
  else
    map_name := ChangeFileExt(ExtractFileName(Map.filename),'');
  Application.Title := 'D2kEditor - ' + map_name;
  MainWindow.Caption := 'Dune 2000 Map and Mission Editor - ' + map_name;
  MissionDialog.Caption := 'Mission settings - ' + map_name;
  EventDialog.Caption := 'Events and Conditions - ' + map_name;
end;

procedure TDispatcher.update_mission_load_status;
begin
  MainWindow.update_mission_load_status;
  if not Mission.mis_assigned then
    EventDialog.Close;
end;

procedure TDispatcher.update_map_stats;
begin
  Map.cache_map_stats;
  MainWindow.update_map_stats;
  MapStatsDialog.update_map_stats;
end;

procedure TDispatcher.update_event_markers;
begin
  Mission.cache_event_markers;
  if Settings.ShowEventMarkers then
    pact := pact + [paRenderMap];
end;

procedure TDispatcher.update_mis_ai_properties;
begin
  MisAI.cache_mis_ai_properties;
  MissionDialog.update_mis_ai_properties;
end;

end.
