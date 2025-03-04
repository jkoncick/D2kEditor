unit _dispatcher;

interface

type
  TDispatcherRegisteredEvent = (
    // Map events
    evMapLoad,
    evMapResize,
    evMapShift,
    evMapTilesModify,
    evMapFilenameChange,
    // Mission events
    evMisLoad,
    evMisEventsImport,
    evMisHouseIDChange,
    evMisEventPositionChange,
    evMisEventAreaChange,
    evMisDefenceAreaChange,
    // Mission ini events
    evMissionIniLoad,
    evMissionIniCustomTextChange,
    // Tileset events
    evFLLTilesetList,
    evFLTilesetImage,
    evFLTileatrBin,
    evFLTilesetIni,
    evTileatrModify,
    evTileatrFilenameChange,
    // Structures events
    evFLTemplatesBin,
    evFLBuilexpBin,
    evFLArmourBin,
    evFLSpeedBin,
    evFLTechposBin,
    evFLTiledataBin,
    evFLMiscObjectsIni,
    evFLSidesIni,
    evStructuresFilenameChange,
    evStructuresImportItem,
    // StructGraphics events
    evFLColoursBin,
    evFLDataR16,
    evFLMiscObjectsBmp,
    evLoadStructureImage,
    // Sounds events
    evFLSoundRs,
    // StringTable events
    evFLTextUib,
    evFLSamplesUib,
    evSamplesUibModify,
    // GameLists events
    evLoadGameLists,
    // EventConfig events
    evLoadEventTypeConfiguration,
    // Setting change events
    evSCTranslateStructureNames,
    // Apply changes events
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
    paUpdateSideList,
    paUpdateGameStructMembers,
    // Update various contents
    paUpdateTileset,
    paUpdateStructureControls,
    paUpdateMapDimensions,
    paUpdateMapName,
    paUpdateMissionLoadStatus,
    paUpdateMapStats,
    paUpdateEventMarkers,
    paUpdateEventAreas,
    paUpdateSideColours,
    paUpdateSpeedModifiers,
    paUpdateVariableNames,
    paUpdateGameLists,
    paUpdateEventTypeConfiguration,
    // Update mission dialog contents
    paUpdateMissionData,
    paUpdateMissionIniData,
    paUpdateMisAiValues,
    // Update whole dialog contents
    paUpdateEventDialog,
    paUpdateTileAtrEditor,
    paUpdateStructuresEditor,
    // Rendering
    paResetMapBlocksDrawn,
    paRenderMap,
    paRenderMinimap,
    paRenderCursorImage,
    // Debug window
    paUpdateDebugValues
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
    procedure update_side_list;
    procedure update_game_struct_members;
    procedure update_tileset;
    procedure update_map_name;
    procedure update_mission_load_status;
    procedure update_map_stats;
    procedure update_event_markers;
    procedure update_event_areas;
    procedure update_game_lists;

  end;

var
  Dispatcher: TDispatcher;

implementation

uses
  Forms, Classes, SysUtils, _utils, _settings, _tileset, _structures, _gamestructs, _map, _mission, _renderer, main, set_dialog,
  test_map_dialog, tileset_dialog, block_preset_dialog,
  mission_dialog, event_dialog, map_stats_dialog,
  tileatr_editor, structures_editor, debug_window;

{ TDispatcher }

procedure TDispatcher.register_event(event: TDispatcherRegisteredEvent);
begin
  case event of
    // Map events
    evMapLoad:                    pact := pact + [paUpdateMapDimensions, paUpdateMapName, paUpdateMapStats, paUpdateVariableNames, paRenderMap, paRenderMinimap, paResetMapBlocksDrawn];
    evMapResize:                  pact := pact + [paUpdateMapDimensions, paUpdateMapStats, paUpdateEventMarkers, paUpdateEventAreas, paUpdateEventDialog, paRenderMap, paRenderMinimap];
    evMapShift:                   pact := pact + [paUpdateMapStats, paUpdateEventMarkers, paUpdateEventAreas, paUpdateEventDialog, paRenderMap, paRenderMinimap];
    evMapTilesModify:             pact := pact + [paUpdateMapStats, paRenderMap, paRenderMinimap];
    evMapFilenameChange:          pact := pact + [paUpdateMapName];
    // Mission events
    evMisLoad:                    pact := pact + [paUpdateStructureControls, paUpdateMissionLoadStatus, paUpdateEventMarkers, paUpdateEventAreas, paUpdateMissionData, paUpdateMisAiValues, paUpdateEventDialog, paRenderMap, paRenderMinimap, paRenderCursorImage];
    evMisEventsImport:            pact := pact + [paUpdateEventMarkers, paUpdateEventAreas, paUpdateEventDialog];
    evMisHouseIDChange:           pact := pact + [paUpdateStructureControls, paRenderMap, paRenderMinimap, paRenderCursorImage];
    evMisEventPositionChange:     pact := pact + [paUpdateEventMarkers];
    evMisEventAreaChange:         pact := pact + [paUpdateEventAreas];
    evMisDefenceAreaChange:       if Settings.MarkDefenceAreas then pact := pact + [paRenderMap];
    // Mission ini events
    evMissionIniLoad:             pact := pact + [paUpdateMissionIniData, paUpdateEventDialog, paUpdateVariableNames];
    evMissionIniCustomTextChange: pact := pact + [paUpdateEventDialog];
    // Tileset events
    evFLLTilesetList:             pact := pact + [paUpdateTilesetList];
    evFLTilesetImage:             pact := pact + [paUpdateTileset, paUpdateTileAtrEditor, paRenderMap, paRenderCursorImage, paUpdateDebugValues];
    evFLTileatrBin:               pact := pact + [paUpdateTileset, paUpdateTileAtrEditor, paRenderMap, paRenderMinimap, paRenderCursorImage, paUpdateDebugValues];
    evFLTilesetIni:               pact := pact + [paUpdateTileset, paUpdateTileAtrEditor, paRenderMinimap, paUpdateDebugValues];
    evTileatrModify:              pact := pact + [paRenderMap, paRenderMinimap, paRenderCursorImage];
    evTileatrFilenameChange:      pact := pact + [paUpdateTileset, paUpdateTileAtrEditor, paUpdateDebugValues];
    // Structures events
    evFLTemplatesBin:             pact := pact + [paUpdateStructuresList, paUpdateStructuresListTranslated, paUpdateStructureControls, paUpdateGameStructMembers, paUpdateMapStats, paUpdateEventDialog, paUpdateStructuresEditor, paRenderMap, paRenderMinimap, paRenderCursorImage, paUpdateDebugValues];
    evFLBuilexpBin:               pact := pact + [paUpdateStructuresEditor, paUpdateDebugValues];
    evFLArmourBin:                pact := pact + [paUpdateStructuresEditor, paUpdateDebugValues];
    evFLSpeedBin:                 pact := pact + [paUpdateSpeedModifiers, paUpdateStructuresEditor, paUpdateDebugValues];
    evFLTechposBin:               pact := pact + [paUpdateStructuresEditor, paUpdateDebugValues];
    evFLTiledataBin:              pact := pact + [paUpdateMapStats, paRenderMap, paRenderMinimap, paRenderCursorImage, paUpdateDebugValues];
    evFLMiscObjectsIni:           pact := pact + [paUpdateMiscObjectList, paRenderMap, paRenderMinimap, paRenderCursorImage, paUpdateDebugValues];
    evFLSidesIni:                 pact := pact + [paUpdateSideList, paUpdateEventDialog, paUpdateDebugValues];
    evStructuresFilenameChange:   pact := pact + [paUpdateDebugValues];
    evStructuresImportItem:       pact := pact + [paUpdateStructuresEditor];
    // StructGraphics events
    evFLColoursBin:               pact := pact + [paUpdateSideColours, paRenderMap, paRenderMinimap, paRenderCursorImage, paUpdateDebugValues];
    evFLDataR16:                  pact := pact + [paUpdateStructuresEditor, paRenderMap, paRenderCursorImage, paUpdateDebugValues];
    evFLMiscObjectsBmp:           pact := pact + [paRenderMap, paRenderCursorImage, paUpdateDebugValues];
    evLoadStructureImage:         pact := pact + [paUpdateDebugValues];
    // Sounds events
    evFLSoundRs:                  pact := pact + [paUpdateStructuresEditor, paUpdateDebugValues];
    // StringTable events
    evFLTextUib:
      begin
                                  pact := pact + [paUpdateTextList, paUpdateEventDialog, paUpdateDebugValues];
        if Settings.TranslateStructureNames then
                                  pact := pact + [paUpdateStructuresListTranslated];
      end;
    evFLSamplesUib:               pact := pact + [paUpdateSoundList, paUpdateDebugValues];
    evSamplesUibModify:           pact := pact + [paUpdateSoundList];
    // GameLists events
    evLoadGameLists:              pact := pact + [paUpdateGameLists];
    // EventConfig events
    evLoadEventTypeConfiguration: pact := pact + [paUpdateEventTypeConfiguration];
    // "Translate structure names" setting changed
    evSCTranslateStructureNames:  pact := pact + [paUpdateStructuresListTranslated];
    // Apply changes events
    evACStructuresEditor:         pact := pact + [paUpdateStructuresList, paUpdateStructuresListTranslated, paUpdateStructureControls, paUpdateGameStructMembers, paUpdateMapStats, paUpdateSpeedModifiers, paUpdateEventDialog, paRenderMap, paRenderMinimap, paRenderCursorImage];
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
  if paUpdateSideList           in pact then update_side_list;
  if paUpdateGameStructMembers  in pact then update_game_struct_members;
  if paUpdateTileset            in pact then update_tileset;
  // Update various contents
  if paUpdateStructureControls  in pact then MainWindow.update_structure_controls;
  if paUpdateMapDimensions      in pact then MainWindow.update_map_dimensions;
  if paUpdateMapName            in pact then update_map_name;
  if paUpdateMissionLoadStatus  in pact then update_mission_load_status;
  if paUpdateMapStats           in pact then update_map_stats;
  if paUpdateEventMarkers       in pact then update_event_markers;
  if paUpdateEventAreas         in pact then update_event_areas;
  if paUpdateSideColours        in pact then MissionDialog.update_side_colors;
  if paUpdateSpeedModifiers     in pact then TileAtrEditor.update_speed_modifiers;
  if paUpdateVariableNames      in pact then EventDialog.update_variable_names;
  if paUpdateGameLists          in pact then update_game_lists;
  if paUpdateEventTypeConfiguration in pact then EventDialog.update_event_type_configuration;
  // Update mission dialog contents
  if paUpdateMissionData        in pact then MissionDialog.update_mission_data;
  if paUpdateMissionIniData     in pact then MissionDialog.update_mission_ini_data;
  if paUpdateMisAiValues        in pact then MissionDialog.update_mis_ai_values;
  // Update whole dialog contents
  if paUpdateEventDialog        in pact then EventDialog.update_contents;
  if paUpdateTileAtrEditor      in pact then TileAtrEditor.update_contents;
  if paUpdateStructuresEditor   in pact then StructuresEditor.update_contents;
  // Rendering
  if paResetMapBlocksDrawn      in pact then Renderer.reset_map_blocks;
  if paRenderMap                in pact then MainWindow.render_map;
  if paRenderMinimap            in pact then MainWindow.render_minimap;
  if paRenderCursorImage        in pact then MainWindow.render_cursor_image;
  // Debug window
  if paUpdateDebugValues        in pact then DebugWindow.update_debug_values;
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
  for i := 0 to Structures.building_group_mapping_count - 1 do
    tmp_strings_buildings.Add(Structures.get_building_group_str(Structures.building_group_mapping[i]));
  for i := 0 to Structures.templates.UnitGroupCount - 1 do
    tmp_strings_units.Add(Structures.get_unit_group_str(i));
  MainWindow.update_structures_list(tmp_strings_buildings, tmp_strings_units);
  MapStatsDialog.update_structures_list(tmp_strings_buildings, tmp_strings_units);
  tmp_strings_buildings.Destroy;
  tmp_strings_units.Destroy;
end;

procedure TDispatcher.update_side_list;
var
  side_list: TStringList;
  i: integer;
begin
  side_list := TStringList.Create;
  for i := 0 to CNT_SIDES - 1 do
    side_list.Add(inttostr(i) + ' - ' + Structures.side_names[i]);
  MainWindow.update_side_list(side_list);
  SetDialog.update_side_list(side_list);
  TestMapDialog.update_side_list(side_list);
  MapStatsDialog.update_side_list;
  MissionDialog.update_side_list(side_list);
  EventDialog.update_side_list(side_list);
  side_list.Destroy;
end;

procedure TDispatcher.update_game_struct_members;
begin
  GameStructs.cache_struct_member_names;
  pact := pact + [paUpdateMisAiValues];
end;

procedure TDispatcher.update_tileset;
begin
  Tileset.update_tileset_index;
  MainWindow.update_tileset;
  SetDialog.update_tileset;
  TilesetDialog.update_tileset;
  BlockPresetDialog.update_tileset;
  MissionDialog.update_tileset;
  EventDialog.update_tileset;
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
  begin
    MissionDialog.Close;
    EventDialog.Close;
  end;
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

procedure TDispatcher.update_event_areas;
begin
  Mission.cache_event_areas;
  if Settings.ShowEventAreas then
    pact := pact + [paRenderMap];
end;

procedure TDispatcher.update_game_lists;
begin
  MainWindow.update_game_lists;
  StructuresEditor.update_game_lists;
  TileAtrEditor.update_game_lists;
end;

end.
