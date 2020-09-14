unit _renderer;

interface

uses Windows, Graphics, Types, _map, _structures;

const constraint_type_color: array[0..8] of TColor = (clTeal, clOlive, clGray, clPurple, clTeal+$404040, clOlive+$404040, clGray+$404040, clPurple+$404040, clRed);
const constraint_side_rect: array[0..3, 0..3] of integer = (
  (8, 23, 16, 31),
  (16, 31, 8, 23),
  (8, 23, 0, 15),
  (0, 15, 8, 23)
);
const constraint_side_point: array[0..3, 0..1] of integer = (
  (14, 28),
  (28, 14),
  (14, 0),
  (0, 14)
);

const thin_spice_tiles: array[0..255] of word = (
  794,794,765,765,794,794,765,765,767,767,753,763,767,767,753,763,
  766,766,752,752,766,766,762,762,792,792,757,798,792,792,799,771,
  794,794,765,765,794,794,765,765,767,767,753,763,767,767,753,763,
  766,766,752,762,766,766,762,762,792,792,757,798,792,792,799,771,
  764,764,791,791,764,764,791,791,751,751,755,777,751,751,755,777,
  750,750,754,754,750,750,776,776,756,756,790,780,756,756,781,788,
  764,764,791,791,764,764,791,791,761,761,797,769,761,761,797,769,
  750,750,754,754,750,750,776,776,778,778,782,784,778,778,789,775,
  794,794,765,765,794,794,765,765,767,767,753,763,767,767,753,763,
  766,766,752,752,766,766,762,762,792,792,757,798,792,792,799,771,
  794,794,765,765,794,794,765,765,767,767,753,763,767,767,753,763,
  766,766,752,752,766,766,762,762,792,792,757,798,792,792,799,771,
  764,764,791,791,764,764,791,791,751,751,755,777,751,751,755,777,
  760,760,796,796,760,760,770,770,779,779,783,786,779,779,787,774,
  764,764,791,791,764,764,791,791,761,761,797,769,761,761,797,769,
  760,760,796,796,760,760,770,770,768,768,785,773,768,768,772,793
);

const thick_spice_pattern: array[0..7, 0..7] of byte = (
  (1,4,4,1,1,4,1,4),
  (4,4,2,4,2,2,4,1),
  (3,3,2,2,3,3,3,3),
  (2,3,4,2,1,4,1,3),
  (4,1,1,4,2,4,3,4),
  (2,1,2,3,3,4,4,3),
  (2,2,3,2,2,1,4,4),
  (4,3,4,2,4,4,4,3)
);

const thick_spice_tiles: array[0..3] of word = (300,301,320,321);

type EditingMarkerType = (emBuilding, emBuildingNoConcrete, emSingleObject, emSelectionArea, emPaintArea);

type
  CardinalArr = array[0..0] of cardinal;
  PCardinalArr = ^CardinalArr;

type
  TRenderer = class

  private
    // Differential rendering variables
    diffrender_old_left: word;
    diffrender_old_top: word;

    // Invalidation variables
    invalidated: boolean;
    inv_nothing: boolean;
    inv_rect: TRect;

    // Editing markers rendering variables
    bkup_bitmap: TBitmap;
    bkup_rect: TRect;
    bkup_valid: boolean;

  public
    procedure init;

    procedure invalidate_init;
    procedure invalidate_map_tile(x, y: word);

    procedure render_map_contents(cnv_target: TCanvas; cnv_left, cnv_top, cnv_width, cnv_height: word;
      data: TMapDataPtr; data_width, data_height: word;
      o_show_grid, o_mark_impassable, o_mark_buildable, o_mark_owner_side,
      o_use_alloc_indexes, o_show_event_markers, o_mark_defence_areas, o_show_unknown_specials,
      o_rendering_optimization: boolean);
    function is_spice(value: word): boolean;
    procedure draw_structure_image(cnv_target: TCanvas; dest_x, dest_y, min_x, min_y, max_x, max_y: integer; structure_image: TStructureImagePtr);
    procedure render_randomgen_data(cnv_target: TCanvas; cnv_left, cnv_top, x, y: word);
    procedure draw_cross(cnv_target: TCanvas; x1, x2, y1, y2: word; color: TColor; width: integer);
    procedure draw_ellipse(cnv_target: TCanvas; x1, y1, x2, y2: word; pcolor, bcolor: TColor; width: integer; text: String);
    procedure draw_point(cnv_target: TCanvas; x, y: word; color: TColor);

    procedure render_minimap_contents(bmp_target: TBitmap; data: TMapDataPtr; data_width, data_height: word;
      o_use_alloc_indexes: boolean);

    procedure remove_editing_marker(cnv_target: TCanvas);
    procedure draw_editing_marker(cnv_target: TCanvas; cnv_left, cnv_top, cnv_width, cnv_height: word;
      data: TMapDataPtr; mark_x, mark_y, mark_width, mark_height: word; marker_type: EditingMarkerType; building_occupied_tiles: Cardinal);

  end;

var
  Renderer: TRenderer;

implementation

uses SysUtils, Math, Forms, main, _mission, _tileset, _settings, Classes, _randomgen;

procedure TRenderer.init;
begin
  // Init backup image
  bkup_bitmap := TBitmap.Create;
  bkup_bitmap.Width := 128;
  bkup_bitmap.Height := 128;
end;

procedure TRenderer.invalidate_init;
begin
  if not invalidated then
    inv_nothing := true;
end;

procedure TRenderer.invalidate_map_tile(x, y: word);
begin
  if not invalidated then
  begin
    invalidated := true;
    inv_rect := Rect(x, y, x, y);
  end else
  begin
    inv_rect.Left := Min(inv_rect.Left, x);
    inv_rect.Top := Min(inv_rect.Top, y);
    inv_rect.Right := Max(inv_rect.Right, x);
    inv_rect.Bottom := Max(inv_rect.Bottom, y);
  end;
  inv_nothing := false;
end;

procedure TRenderer.render_map_contents(cnv_target: TCanvas; cnv_left, cnv_top, cnv_width, cnv_height: word;
  data: TMapDataPtr; data_width, data_height: word;
  o_show_grid, o_mark_impassable, o_mark_buildable, o_mark_owner_side,
  o_use_alloc_indexes, o_show_event_markers, o_mark_defence_areas, o_show_unknown_specials,
  o_rendering_optimization: boolean);
var
  min_x, min_y, max_x, max_y: integer;
  shift_count: word;
  x, y: integer;
  xx, yy: integer;
  actual_x, actual_y: integer;
  tile: word;
  special: word;
  tiledata_entry: TTileDataEntryPtr;
  player, player2: word;
  bottom_offset: integer;
  wall_frame: word;
  spice_tile_value: word;
  dest_rect: TRect;
  src_rect: TRect;
  tile_type: TileType;
  tile_attr: Cardinal;
  building_template: TBuildingTemplatePtr;
  unit_template: TUnitTemplatePtr;
  event_marker: ^TEventMarker;
  building_skirt: ^TBuildingSkirt;
  conc_tile, conc_tile_x, conc_tile_y: word;
  structure_image: TStructureImagePtr;
  was_already_loaded: boolean;
  house_color_pixels: integer;
  is_stealth: boolean;
begin
  if not Map.loaded then
    exit;
  min_x := 0;
  min_y := 0;
  max_x := cnv_width - 1;
  max_y := cnv_height - 1;
  // Rendering optimization
  if o_rendering_optimization then
  begin
    remove_editing_marker(cnv_target);
    // Horizontal scroll
    if (cnv_left <> diffrender_old_left) and (abs(cnv_left - diffrender_old_left) < cnv_width)  then
    begin
      shift_count := abs(cnv_left - diffrender_old_left);
      if cnv_left < diffrender_old_left then
      begin
        // Scrolling left
        max_x := shift_count - 1;
        dest_rect := rect(shift_count*32,0,cnv_width*32,cnv_height*32);
        src_rect := rect(0,0,cnv_width*32-shift_count*32,cnv_height*32);
      end else
      begin
        // Scrolling right
        min_x := max_x - shift_count + 1;
        src_rect := rect(shift_count*32,0,cnv_width*32,cnv_height*32);
        dest_rect := rect(0,0,cnv_width*32-shift_count*32,cnv_height*32);
      end;
      // Shifting part of map canvas
      cnv_target.CopyRect(dest_rect,cnv_target,src_rect);
    end else
    // Vertical scroll
    if (cnv_top <> diffrender_old_top) and (abs(cnv_top - diffrender_old_top) < cnv_height)  then
    begin
      shift_count := abs(cnv_top - diffrender_old_top);
      if cnv_top < diffrender_old_top then
      begin
        // Scrolling up
        max_y := shift_count - 1;
        dest_rect := rect(0,shift_count*32,cnv_width*32,cnv_height*32);
        src_rect := rect(0,0,cnv_width*32,cnv_height*32-shift_count*32);
      end else
      begin
        // Scrolling down
        min_y := max_y - shift_count + 1;
        src_rect := rect(0,shift_count*32,cnv_width*32,cnv_height*32);
        dest_rect := rect(0,0,cnv_width*32,cnv_height*32-shift_count*32);
      end;
      // Shifting part of map canvas
      cnv_target.CopyRect(dest_rect,cnv_target,src_rect);
    end else
    // Invalidated area
    if invalidated then
    begin
      min_x := Min(Max(inv_rect.Left - 1 - cnv_left, 0), cnv_width);
      max_x := Min(Max(inv_rect.Right + (MAX_BUILDING_SIZE - 1) - cnv_left, -1), cnv_width - 1);
      min_y := Min(Max(inv_rect.Top - 1 - cnv_top, 0), cnv_height);
      max_y := Min(Max(inv_rect.Bottom + (MAX_BUILDING_SIZE - 1) - cnv_top, -1), cnv_height - 1);
      // Nothing to render
      if (min_x > max_x) or (min_y > max_y) then
      begin
        invalidated := false;
        exit;
      end;
    end else
    if inv_nothing then
    begin
      // Nothing to render
      inv_nothing := false;
      exit;
    end;
    diffrender_old_left := cnv_left;
    diffrender_old_top := cnv_top;
    invalidated := false;
    inv_nothing := false;
  end;
  // Draw terrain
  for y:= min_y to max_y do
  begin
    for x:= min_x to max_x do
    begin
      xx := x + cnv_left;
      yy := y + cnv_top;
      special := data[xx, yy].special;
      // Draw spice like a normal terrain tile
      if special = 1 then
      begin
        spice_tile_value := 0;
        if is_spice(data[max(xx-1,0), max(yy-1,0)].special) then
          inc(spice_tile_value, 1);   // top-left
        if is_spice(data[xx, max(yy-1,0)].special) then
          inc(spice_tile_value, 2);   // top
        if is_spice(data[min(xx+1,data_width-1), max(yy-1,0)].special) then
          inc(spice_tile_value, 4);   // top-right
        if is_spice(data[max(xx-1,0), yy].special) then
          inc(spice_tile_value, 8);   // left
        if is_spice(data[min(xx+1,data_width-1), yy].special) then
          inc(spice_tile_value, 16);  // right
        if is_spice(data[max(xx-1,0), min(yy+1,data_height-1)].special) then
          inc(spice_tile_value, 32);  // bottom-left
        if is_spice(data[xx, min(yy+1,data_height-1)].special) then
          inc(spice_tile_value, 64);  // bottom
        if is_spice(data[min(xx+1,data_width-1), min(yy+1,data_height-1)].special) then
          inc(spice_tile_value, 128); // bottom-right
        tile := thin_spice_tiles[spice_tile_value];
      end else
      if special = 2 then
        tile := thick_spice_tiles[thick_spice_pattern[yy and 7, xx and 7]-1]
      else
        // No spice, draw terrain tile normally
        tile := data[x + cnv_left, y + cnv_top].tile;
      if tile = 65535 then
      begin
        // Blank tile
        cnv_target.Pen.Color := clBtnFace;
        cnv_target.Brush.Color := clBtnFace;
        cnv_target.Brush.Style := bsSolid;
        cnv_target.Rectangle(x*32,y*32,x*32+32,y*32+32);
      end else
        // Actual tile
        cnv_target.CopyRect(rect(x*32,y*32,x*32+32,y*32+32),Tileset.tileimage.Canvas,rect((tile mod 20)*32,(tile div 20 * 32),(tile mod 20)*32+32,(tile div 20 * 32+32)));
      // Draw tile markers
      if o_mark_impassable or o_mark_buildable or o_mark_owner_side then
      begin
        tile := data[x + cnv_left, y + cnv_top].tile;
        tile_type := Tileset.get_tile_type(tile);
        tile_attr := Tileset.attributes[tile];
        // Draw impassable/buildable tile marker
        if (tile_type = ttImpassable) and o_mark_impassable then
          draw_cross(cnv_target, x*32, x*32+31, y*32, y*32+31, clRed, 2)
        else if (tile_type = ttInfantryOnly) and o_mark_impassable then
          draw_cross(cnv_target, x*32, x*32+31, y*32, y*32+31, $4080FF, 2)
        else if (tile_type = ttBuildable) and ((tile_attr and taConcrete) <> 0) and o_mark_buildable then
          draw_cross(cnv_target, x*32, x*32+31, y*32, y*32+31, $00D0D0, 2)
        else if (tile_type = ttBuildable) and o_mark_buildable then
          draw_cross(cnv_target, x*32, x*32+31, y*32, y*32+31, $40FF80, 2);
        // Draw concrete owner marker
        if (tile_type = ttBuildable) and ((tile_attr and taConcrete) <> 0) and o_mark_owner_side then
        begin
          player := 0;
          if (tile_attr and taConcreteOwnerSideBit1) <> 0 then
            Inc(player, 1);
          if (tile_attr and taConcreteOwnerSideBit2) <> 0 then
            Inc(player, 2);
          if (tile_attr and taConcreteOwnerSideBit3) <> 0 then
            Inc(player, 4);
          if o_use_alloc_indexes then
            player := Mission.get_player_alloc_index(player);
          cnv_target.Pen.Color := Structures.player_info[player].color_inv;
          cnv_target.Brush.Color := cnv_target.Pen.Color;
          cnv_target.Brush.Style := bsSolid;
          cnv_target.Ellipse(x * 32 + 8, y * 32 + 8, x * 32 + 24, y * 32 + 24);
          cnv_target.Brush.Style := bsClear;
        end;
      end;
      //--render_randomgen_data(cnv_target, cnv_left, cnv_top, x, y);
    end;
  end;
  cnv_target.Pen.Width := 1;
  cnv_target.Brush.Style := bsClear;
  // Draw structures
  for y:= min_y - (MAX_BUILDING_SIZE - 1) to max_y + 1 do
  begin
    for x:= min_x - (MAX_BUILDING_SIZE - 1) to max_x + 1 do
    begin
      actual_x := x + cnv_left;
      actual_y := y + cnv_top;
      // Check if tile is out of map
      if (actual_x < 0) or (actual_x >= data_width) or (actual_y < 0) or (actual_y >= data_height) then
        continue;
      // Get special value
      special := data[actual_x, actual_y].special;
      if special <= 2 then
        continue;
      tiledata_entry := Structures.get_tiledata_entry(special);
      // Get player number
      player := Mission.get_player_alloc_index(tiledata_entry.player);
      player2 := IfThen(o_use_alloc_indexes, player, tiledata_entry.player);
      // Draw structure according to structure type
      if tiledata_entry.stype = ST_MISC_OBJECT then
      begin
        // Structure is Misc object
        if (x < min_x) or (x > max_x) or (y < min_y) or (y > max_y) then
          continue; // Do not draw it outside of rendering area
        src_rect := rect((tiledata_entry.index)*32,0,(tiledata_entry.index)*32+32,32);
        dest_rect := rect(x*32,y*32,x*32+32,y*32+32);
        cnv_target.Brush.Style := bsSolid;
        cnv_target.CopyMode := cmSrcAnd;
        cnv_target.CopyRect(dest_rect,Structures.graphics_misc_objects_mask.Canvas,src_rect);
        cnv_target.CopyMode := cmSrcPaint;
        cnv_target.CopyRect(dest_rect,Structures.graphics_misc_objects.Canvas,src_rect);
        cnv_target.CopyMode := cmSrcCopy;
      end else
      if tiledata_entry.stype = ST_BUILDING then
      begin
        // Structure is building
        building_template := Structures.get_building_template(tiledata_entry.index, player);
        if building_template <> nil then
        begin
          // Draw concrete under building
          if (building_template.Flags and BF_NO_CONCRETE) = 0 then
          begin
            for xx := 0 to MAX_BUILDING_SIZE - 1 do
              for yy := 0 to MAX_BUILDING_SIZE - 1 do
              begin
                // Do not draw concrete outside of rendering area
                if ((x + xx) < min_x) or ((x + xx) > max_x) or ((y + yy) < min_y) or ((y + yy) > max_y) then
                  continue;
                // Do not draw concrete on non-buildable tile
                if Tileset.get_tile_type(data[actual_x+xx, actual_y + yy].tile) <> ttBuildable then
                  continue;
                // Check if tile is occupied by building
                if (building_template.TilesOccupiedAll and (1 shl (yy * MAX_BUILDING_SIZE + xx))) = 0 then
                  continue;
                conc_tile := concrete_tiles[(actual_x+xx + actual_y+yy) mod Length(concrete_tiles)];
                conc_tile_x := conc_tile mod 20;
                conc_tile_y := conc_tile div 20;
                dest_rect := Rect((x + xx)*32, (y + yy)*32, (x + xx)*32 + 32, (y + yy)*32 + 32);
                src_rect := Rect(conc_tile_x*32, conc_tile_y*32, conc_tile_x*32+32, conc_tile_y*32+32);
                cnv_target.CopyRect(dest_rect, Tileset.tileimage.Canvas, src_rect);
              end;
          end;
          // Draw building's skirt
          if (building_template.Flags and BF_HAS_SKIRT) <> 0 then
          begin
            building_skirt := nil;
            if building_template.ArtWidth = 64 then
              building_skirt := Addr(BUILDING_SKIRT_2x2);
            if building_template.ArtWidth = 96 then
              building_skirt := Addr(BUILDING_SKIRT_3x2);
            if building_skirt <> nil then
            begin
              bottom_offset := (building_template.ArtHeight div 32) - building_skirt.size_y;
              for yy := 0 to building_skirt.size_y - 1 do
                for xx := 0 to building_skirt.size_x - 1 do
                begin
                  // Do not draw it outside of rendering area
                  if ((x + xx) < min_x) or ((x + xx) > max_x) or ((y + yy + bottom_offset) < min_y) or ((y + yy + bottom_offset) > max_y) then
                    continue;
                  dest_rect := Rect((x + xx)*32, (y + yy + bottom_offset)*32, (x + xx)*32 + 32, (y + yy + bottom_offset)*32 + 32);
                  if ((building_template.Flags and BF_NO_CONCRETE) = 0) and ((building_template.TilesOccupiedAll and (1 shl ((yy + bottom_offset) * MAX_BUILDING_SIZE + xx))) <> 0) and (Tileset.get_tile_type(data[actual_x+xx, actual_y + yy + bottom_offset].tile) = ttBuildable) then
                    src_rect := Rect((xx + building_skirt.conc_tile_x)*32, (yy + building_skirt.conc_tile_y)*32, (xx + building_skirt.conc_tile_x)*32+32, (yy + building_skirt.conc_tile_y)*32+32)
                  else
                    src_rect := Rect((xx + building_skirt.rock_tile_x)*32, (yy + building_skirt.rock_tile_y)*32, (xx + building_skirt.rock_tile_x)*32+32, (yy + building_skirt.rock_tile_y)*32+32);
                  cnv_target.CopyRect(dest_rect, Tileset.tileimage.Canvas, src_rect);
                end;
            end;
          end;
          // Draw building
          house_color_pixels := 0;
          if building_template.BuildingArt <> -1 then
          begin
            // Determine correct wall frame if structure links with wall
            wall_frame := 0;
            if (building_template.SpecialBehavior = 14) or (building_template.SpecialBehavior = 16) then
            begin
              // Checking left of wall
              if ((actual_x - 1) >= 0) and Structures.check_links_with_wall(data[actual_x - 1, actual_y].special) then
                wall_frame := wall_frame + 1;
              // Checking up of wall
              if ((actual_y - 1) >= 0) and Structures.check_links_with_wall(data[actual_x, actual_y - 1].special) then
                wall_frame := wall_frame + 2;
              // Checking right of wall
              if ((actual_x + 1) < data_width) and Structures.check_links_with_wall(data[actual_x + 1, actual_y].special) then
                wall_frame := wall_frame + 4;
              // Checking down of wall
              if ((actual_y + 1) < data_height) and Structures.check_links_with_wall(data[actual_x, actual_y + 1].special) then
                wall_frame := wall_frame + 8;
              wall_frame := WALL_FRAME_MAPPING[wall_frame];
            end;
            // Draw main building frame
            structure_image := Structures.get_structure_image(Structures.building_art_image_indexes[building_template.BuildingArt] + 1 + wall_frame, player2, false, false, was_already_loaded);
            if structure_image <> nil then
            begin
              draw_structure_image(cnv_target, x*32 + structure_image.offset_x + IfThen(building_template.SpecialBehavior = 16, building_template.ExitPoint1X, 0), y*32 + building_template.ArtHeight - structure_image.offset_y, min_x, min_y, max_x, max_y, structure_image);
              inc(house_color_pixels, structure_image.house_color_pixel_count);
            end;
            // Draw base building frame
            structure_image := Structures.get_structure_image(Structures.building_art_image_indexes[building_template.BuildingArt], player2, false, false, was_already_loaded);
            if structure_image <> nil then
            begin
              draw_structure_image(cnv_target, x*32 + structure_image.offset_x + IfThen(building_template.SpecialBehavior = 16, building_template.ExitPoint1X, 0), y*32 + building_template.ArtHeight - structure_image.offset_y, min_x, min_y, max_x, max_y, structure_image);
              inc(house_color_pixels, structure_image.house_color_pixel_count);
            end;
          end;
          // Draw barrel
          if building_template.BarrelArt <> -1 then
          begin
            // Draw main barrel frame
            structure_image := Structures.get_structure_image(Structures.building_art_image_indexes[building_template.BarrelArt] + 1, player2, false, false, was_already_loaded);
            if structure_image <> nil then
            begin
              draw_structure_image(cnv_target, x*32 + structure_image.offset_x + IfThen(building_template.SpecialBehavior = 16, building_template.ExitPoint1X, 0), y*32 + building_template.ArtHeight - structure_image.offset_y, min_x, min_y, max_x, max_y, structure_image);
              inc(house_color_pixels, structure_image.house_color_pixel_count);
            end;
          end;
          // Draw owner side marker
          if o_mark_owner_side and (house_color_pixels < 10) then
          begin
            cnv_target.Pen.Color := Structures.player_info[player2].color_inv;
            cnv_target.Brush.Color := cnv_target.Pen.Color;
            cnv_target.Brush.Style := bsSolid;
            cnv_target.Ellipse(x * 32 + 8, y * 32 + 8, x * 32 + 24, y * 32 + 24);
            cnv_target.Brush.Style := bsClear;
          end;
        end else
          draw_cross(cnv_target, x*32, x*32+31, y*32, y*32+31, $FF00FF, 2);
      end else
      if tiledata_entry.stype = ST_UNIT then
      begin
        // Structure is unit
        unit_template := Structures.get_unit_template(tiledata_entry.index, player);
        if unit_template <> nil then
        begin
          if unit_template.SpecialBehavior <> 5 then
          begin
            // Check if unit is stealth
            is_stealth := ((unit_template.Flags and $10) <> 0) or (unit_template.SpecialBehavior = 12) or (Structures.templates.UnitNameStrings[Structures.unit_side_versions[tiledata_entry.index, player]] = 'STEALTH RAIDER');
            // Draw unit
            if unit_template.UnitArt <> -1 then
            begin
              structure_image := Structures.get_structure_image(Structures.unit_art_image_indexes[unit_template.UnitArt], player2, true, is_stealth, was_already_loaded);
              if structure_image <> nil then
                draw_structure_image(cnv_target, x*32 + structure_image.offset_x, y*32 + structure_image.offset_y, min_x, min_y, max_x, max_y, structure_image);
            end;
            // Draw barrel
            if unit_template.BarrelArt <> -1 then
            begin
              structure_image := Structures.get_structure_image(Structures.unit_art_image_indexes[unit_template.BarrelArt], player2, true, is_stealth, was_already_loaded);
              if structure_image <> nil then
                draw_structure_image(cnv_target, x*32 + structure_image.offset_x, y*32 + structure_image.offset_y, min_x, min_y, max_x, max_y, structure_image);
            end;
          end else
          begin
            // Special case - Sandworm
            src_rect := rect(0,0,32,32);
            dest_rect := rect(x*32,y*32,x*32+32,y*32+32);
            cnv_target.Brush.Style := bsSolid;
            cnv_target.CopyMode := cmSrcAnd;
            cnv_target.CopyRect(dest_rect,Structures.graphics_misc_objects_mask.Canvas,src_rect);
            cnv_target.CopyMode := cmSrcPaint;
            cnv_target.CopyRect(dest_rect,Structures.graphics_misc_objects.Canvas,src_rect);
            cnv_target.CopyMode := cmSrcCopy;
          end;
        end else
          draw_cross(cnv_target, x*32, x*32+31, y*32, y*32+31, $FF00FF, 2);
      end else
      if o_show_unknown_specials then
      begin
        // Unknown special value
        cnv_target.Brush.Style := bsClear;
        cnv_target.TextOut(x * 32 + 2, y * 32 + 2, inttostr(special));
      end;
    end;
  end;
  // Draw event markers
  if o_show_event_markers then
  begin
    for y:= min_y to max_y do
      for x:= min_x to max_x do
      begin
        event_marker := addr(Mission.event_markers[x + cnv_left, y + cnv_top]);
        if event_marker.emtype = emNone then
          continue;
        if event_marker_type_info[ord(event_marker.emtype)].player_related then
        begin
          player := Min(event_marker.side, CNT_PLAYERS - 1);
          cnv_target.Pen.Color := Structures.player_info[player].color_inv;
          cnv_target.Brush.Color := Structures.player_info[player].color_inv;
        end else
        begin
          cnv_target.Pen.Color := clGray;
          cnv_target.Brush.Color := clGray;
        end;
        cnv_target.Rectangle(x*32, y*32, x*32+32, y*32+32);
        cnv_target.Pen.Color := clBlack;
        cnv_target.TextOut(x * 32 + 12, y * 32 + 3, event_marker_type_info[ord(event_marker.emtype)].letter);
        cnv_target.TextOut(x * 32 + 12, y * 32 + 17, inttostr(event_marker.index));
        if event_marker.moved then
          cnv_target.TextOut(x * 32 + 2, y * 32 + 10, '<');
      end;
  end;
  // Draw defence area markers
  if o_mark_defence_areas then
  begin
    cnv_target.Brush.Style := bsClear;
    cnv_target.pen.Width := 2;
    for x := 0 to cnt_players - 1 do
      for y := 0 to Mission.mis_data.ai_segments[x,7505] - 1 do
      begin
        cnv_target.Pen.Color := Structures.player_info[x].color_inv;
        cnv_target.Rectangle(
          (Mission.mis_data.ai_segments[x,7508+y*20] - cnv_left) * 32,
          (Mission.mis_data.ai_segments[x,7510+y*20] - cnv_top) * 32,
          (Mission.mis_data.ai_segments[x,7509+y*20] - cnv_left) * 32 + 32,
          (Mission.mis_data.ai_segments[x,7511+y*20] - cnv_top) * 32 + 32);
        cnv_target.TextOut(
          (Mission.mis_data.ai_segments[x,7508+y*20] - cnv_left) * 32 + 3,
          (Mission.mis_data.ai_segments[x,7510+y*20] - cnv_top) * 32 + 3,
          'Area' + inttostr(y+1));
      end;
  end;
  // Draw grid
  cnv_target.CopyMode:=cmSrcCopy;
  cnv_target.Pen.Color := Settings.GridColor;
  cnv_target.Pen.Width := 1;
  if o_show_grid then
  begin
    for x:= 0 to cnv_width do
    begin
      cnv_target.MoveTo(x*32,0);
      cnv_target.LineTo(x*32,cnv_height*32);
    end;
    for y:= 0 to cnv_height do
    begin
      cnv_target.MoveTo(0,y*32);
      cnv_target.LineTo(cnv_width*32,y*32);
    end;
  end;
end;

function TRenderer.is_spice(value: word): boolean;
begin
  result := (value = 1) or (value = 2);
end;

procedure TRenderer.draw_structure_image(cnv_target: TCanvas; dest_x, dest_y, min_x, min_y, max_x, max_y: integer; structure_image: TStructureImagePtr);
var
  dest_rect: TRect;
  src_rect: TRect;
begin
  src_rect := Rect(0, 0, structure_image.bitmap.Width, structure_image.bitmap.Height);
  dest_rect := Rect(dest_x, dest_y, dest_x + structure_image.bitmap.Width, dest_y + structure_image.bitmap.Height);
  // Restrict drawing to rendering area
  if (dest_rect.Left < (min_x * 32)) then
  begin
    src_rect.Left := src_rect.Left + (min_x * 32 - dest_rect.Left);
    dest_rect.Left := min_x * 32;
  end;
  if (dest_rect.Top < (min_y * 32)) then
  begin
    src_rect.Top := src_rect.Top + (min_y * 32 - dest_rect.Top);
    dest_rect.Top := min_y * 32;
  end;
  if (dest_rect.Right > (max_x * 32 + 32)) then
  begin
    src_rect.Right := src_rect.Right - (dest_rect.Right - (max_x * 32 + 32));
    dest_rect.Right := max_x * 32 + 32;
  end;
  if (dest_rect.Bottom > (max_y * 32 + 32)) then
  begin
    src_rect.Bottom := src_rect.Bottom - (dest_rect.Bottom - (max_y * 32 + 32));
    dest_rect.Bottom := max_y * 32 + 32;
  end;
  if src_rect.Left >= src_rect.Right then
    exit;
  if src_rect.Top >= src_rect.Bottom then
    exit;
  // Drawing structure
  cnv_target.Brush.Style := bsSolid;
  cnv_target.CopyMode := cmSrcAnd;
  cnv_target.CopyRect(dest_rect,structure_image.bitmap_mask.Canvas,src_rect);
  cnv_target.CopyMode := cmSrcPaint;
  cnv_target.CopyRect(dest_rect,structure_image.bitmap.Canvas,src_rect);
  cnv_target.CopyMode := cmSrcCopy;
end;

procedure TRenderer.render_randomgen_data(cnv_target: TCanvas; cnv_left, cnv_top, x, y: word);
var
  randomgen_tile_record: ^TTileRecord;
  pos_x, pos_y: integer;
  xx, yy: integer;
  i: integer;
  ctype: byte;
  checktype: byte;
  ccolor: TColor;
  checkcolor: TColor;
begin
  // Draw tile record from random map generator
  pos_x := x + cnv_left;
  pos_y := y + cnv_top;
  xx := x*32;
  yy := y*32;
  randomgen_tile_record := @RandomGen.tile_records[pos_x, pos_y];
  // Draw active constraints
  for i := 0 to 3 do
  begin
    if randomgen_tile_record.constraints[i].radius > 0 then
    begin
      ctype := randomgen_tile_record.constraints[i].c_type;
      ccolor := constraint_type_color[ctype] + (randomgen_tile_record.constraints[i].radius - 1) * $004000;
      if ctype <> constraint_type_connectionPoint then
        draw_cross(cnv_target, xx+constraint_side_rect[i,0], xx+constraint_side_rect[i,1], yy+constraint_side_rect[i,2], yy+constraint_side_rect[i,3], ccolor, 2)
      else
        draw_point(cnv_target, xx+constraint_side_point[i,0], yy+constraint_side_point[i,1], ccolor);
    end;
  end;
  // Draw last check for constraint
  if RandomGen.constraint_check_step <> 0 then
  begin
    for i := 0 to 3 do
    begin
      if randomgen_tile_record.constraint_checks[i].step <> RandomGen.constraint_check_step then
        continue;
      ctype := randomgen_tile_record.constraints[i].c_type;
      checktype := randomgen_tile_record.constraint_checks[i].c_type;
      checkcolor := constraint_type_color[checktype];
      // If there was violation, fill the ellipse with color
      if randomgen_tile_record.constraint_checks[i].violated then
        ccolor := constraint_type_color[ctype]
      else
        ccolor := $01000000;
      draw_ellipse(cnv_target, xx+constraint_side_rect[i,0], yy+constraint_side_rect[i,2], xx+constraint_side_rect[i,1], yy+constraint_side_rect[i,3], checkcolor, ccolor, 2, inttostr(randomgen_tile_record.constraint_checks[i].distance));
    end;
  end;

  cnv_target.Pen.Width := 1;
  cnv_target.Brush.Style := bsClear;
  // Draw connection points
  for i := 0 to RandomGen.num_active_connection_points - 1 do
  begin
    if (x + cnv_left <> RandomGen.active_connection_points[i].pos_x) or (y + cnv_top <> RandomGen.active_connection_points[i].pos_y) then
      continue;
    cnv_target.Pen.Color := clRed;
    cnv_target.Font.Color := clRed;
    cnv_target.Rectangle(xx, yy, xx+32, yy+32);
    cnv_target.TextOut(xx + 12, yy + 3, inttostr(i));
    cnv_target.TextOut(xx + 12, yy + 17, inttostr(RandomGen.active_connection_points[i].parent_step));
  end;
  // Draw blocks
  for i := 0 to RandomGen.current_step - 1 do
  begin
    if (x + cnv_left <> RandomGen.hist_placed_blocks[i].pos_x) or (y + cnv_top <> RandomGen.hist_placed_blocks[i].pos_y) then
      continue;
    cnv_target.Pen.Color := clLime;
    cnv_target.Font.Color := clLime;
    cnv_target.Rectangle(xx, yy, xx+32, yy+32);
    cnv_target.TextOut(xx + 12, yy + 3, inttostr(i));
    cnv_target.TextOut(xx + 12, yy + 17, inttostr(RandomGen.hist_placed_blocks[i].num_backtracked));
  end;
end;

procedure TRenderer.draw_cross(cnv_target: TCanvas; x1, x2, y1, y2: word; color: TColor; width: integer);
begin
  cnv_target.Pen.Color := color;
  cnv_target.Pen.Width := width;
  cnv_target.MoveTo(x1, y1);
  cnv_target.LineTo(x2, y2);
  cnv_target.MoveTo(x2, y1);
  cnv_target.LineTo(x1, y2);
end;

procedure TRenderer.draw_ellipse(cnv_target: TCanvas; x1, y1, x2, y2: word; pcolor, bcolor: TColor; width: integer; text: String);
begin
  cnv_target.Pen.Color := pcolor;
  cnv_target.Pen.Width := width;
  if (bcolor > $00FFFFFF) then
    cnv_target.Brush.Style := bsClear
  else begin
    cnv_target.Brush.Style := bsSolid;
    cnv_target.Brush.Color := bcolor;
  end;
  cnv_target.Ellipse(x1, y1, x2+1, y2+1);
  cnv_target.Font.Color := clLime;
  cnv_target.TextOut((x1+x2-cnv_target.TextWidth(text)) div 2, (y1+y2-cnv_target.TextHeight(text)) div 2 , text);
end;

procedure TRenderer.draw_point(cnv_target: TCanvas; x, y: word; color: TColor);
begin
  cnv_target.Pen.Color := color;
  cnv_target.Pen.Width := 1;
  cnv_target.Brush.Style := bsSolid;
  cnv_target.Brush.Color := color;
  cnv_target.Rectangle(x, y, x+4, y+4);
end;

procedure TRenderer.render_minimap_contents(bmp_target: TBitmap; data: TMapDataPtr; data_width, data_height: word; o_use_alloc_indexes: boolean);
var
  min_x, min_y, max_x, max_y: integer;
  x, y, xx, yy: integer;
  border_x, border_y: integer;
  index: integer;
  player, player2: word;
  tiledata_entry: TTileDataEntryPtr;
  building_template: TBuildingTemplatePtr;
  bmp_data: PCardinalArr;
begin
  min_x := 0;
  min_y := 0;
  max_x := data_width - 1;
  max_y := data_height - 1;
  if inv_nothing then
  begin
    // Nothing to render
    exit;
  end else
  if invalidated then
  begin
    // Render only invalidated area
    min_x := inv_rect.Left;
    max_x := Min(inv_rect.Right + (MAX_BUILDING_SIZE - 1), data_width - 1);
    min_y := inv_rect.Top;
    max_y := Min(inv_rect.Bottom + (MAX_BUILDING_SIZE - 1), data_height - 1);
  end else
  begin
    // Render whole minimap
    bmp_target.Canvas.Brush.Color := ClBtnFace;
    bmp_target.Canvas.Pen.Color := ClBtnFace;
    bmp_target.Canvas.Rectangle(0,0,max_map_width,max_map_height);
  end;
  border_x := (max_map_width - data_width) div 2;
  border_y := (max_map_height - data_height) div 2;
  bmp_data := bmp_target.ScanLine[bmp_target.height - 1];
  // Rendering terrain
  for y:= min_y to max_y do
    for x:= min_x to max_x do
    begin
      index := (bmp_target.height - (y+border_y) - 1) * bmp_target.width + x+border_x;
      bmp_data[index] := Tileset.get_tile_color(data[x,y].tile, data[x,y].special);
    end;
  // Rendering structures
  for y:= Max(min_y - (MAX_BUILDING_SIZE - 1), 0) to max_y do
    for x:= Max(min_x - (MAX_BUILDING_SIZE - 1), 0) to max_x do
    begin
      // Get structure parameters
      tiledata_entry := Structures.get_tiledata_entry(data[x,y].special);
      // Get player number
      player := Mission.get_player_alloc_index(tiledata_entry.player);
      player2 := IfThen(o_use_alloc_indexes, player, tiledata_entry.player);
      // Draw according to structure type
      if tiledata_entry.stype = ST_MISC_OBJECT then
      begin
        index := (bmp_target.height - (y+border_y) - 1) * bmp_target.width + x+border_x;
        bmp_data[index] := Structures.misc_object_info[tiledata_entry.index].color;
      end else
      if tiledata_entry.stype = ST_UNIT then
      begin
        index := (bmp_target.height - (y+border_y) - 1) * bmp_target.width + x+border_x;
        bmp_data[index] := Structures.player_info[player2].color;
      end else
      if tiledata_entry.stype = ST_BUILDING then
      begin
        building_template := Structures.get_building_template(tiledata_entry.index, player);
        if building_template = nil then
          continue;
        // Render building on map
        for yy := 0 to (MAX_BUILDING_SIZE - 1) do
          for xx := 0 to (MAX_BUILDING_SIZE - 1) do
          begin
            if (x + xx >= data_width) or (y + yy >= data_height) then
              continue;
            if (building_template.TilesOccupiedAll and (1 shl (yy * MAX_BUILDING_SIZE + xx))) <> 0 then
            begin
              index := (bmp_target.height - (y+border_y+yy) - 1) * bmp_target.width + x+border_x+xx;
              bmp_data[index] := Structures.player_info[player2].color;
            end;
          end;
      end;
    end;
end;

procedure TRenderer.remove_editing_marker(cnv_target: TCanvas);
var
  src_rect: TRect;
begin
  if not bkup_valid then
    exit;
  src_rect := Rect(0, 0, bkup_rect.Right - bkup_rect.Left, bkup_rect.Bottom - bkup_rect.Top);
  cnv_target.CopyRect(bkup_rect, bkup_bitmap.Canvas, src_rect);
  bkup_valid := false;
end;

procedure TRenderer.draw_editing_marker(cnv_target: TCanvas; cnv_left, cnv_top, cnv_width, cnv_height: word;
  data: TMapDataPtr; mark_x, mark_y, mark_width, mark_height: word; marker_type: EditingMarkerType; building_occupied_tiles: Cardinal);
var
  dest_rect: TRect;
  bkup_width, bkup_height: integer;
  x, y, xx, yy: integer;
  tile_type: TileType;
begin
  // Restore old backup
  remove_editing_marker(cnv_target);
  // Make new backup
  bkup_rect := Rect(Max(mark_x * 32 - cnv_left * 32, 0), Max(mark_y * 32 - cnv_top * 32, 0), Min((mark_x + mark_width - cnv_left) * 32 + 1, cnv_width * 32), Min((mark_y + mark_height - cnv_top) * 32 + 1, cnv_height * 32));
  bkup_width := bkup_rect.Right - bkup_rect.Left;
  bkup_height := bkup_rect.Bottom - bkup_rect.Top;
  dest_rect := Rect(0, 0, bkup_width, bkup_height);
  bkup_bitmap.Width := Max(bkup_bitmap.Width, bkup_width);
  bkup_bitmap.Height := Max(bkup_bitmap.Height, bkup_height);
  bkup_bitmap.Canvas.CopyRect(dest_rect, cnv_target, bkup_rect);
  bkup_valid := true;
  // Draw actual_marker
  if (marker_type = emBuilding) or (marker_type = emBuildingNoConcrete) then
  begin
    cnv_target.Brush.Style := bsBDiagonal;
    cnv_target.Pen.Style := psClear;
    for y := 0 to MAX_BUILDING_SIZE - 1 do
      for x := 0 to MAX_BUILDING_SIZE - 1 do
      begin
        xx := mark_x + x;
        yy := mark_y + y;
        if xx >= max_map_width then
          continue;
        if yy >= max_map_height then
          continue;
        if (building_occupied_tiles and (1 shl (y * MAX_BUILDING_SIZE + x))) = 0 then
          continue;
        tile_type := Tileset.get_tile_type(data[xx, yy].tile);
        if ((marker_type = emBuilding) and (tile_type = ttBuildable)) or (marker_type = emBuildingNoConcrete) then
          cnv_target.Brush.Color := $E0E0E0
        else
          cnv_target.Brush.Color := $0000E0;
        cnv_target.Rectangle((xx - cnv_left)*32, (yy - cnv_top)*32, (xx - cnv_left)*32+33, (yy - cnv_top)*32+33);
      end;
    cnv_target.Pen.Style := psSolid;
    cnv_target.Brush.Style := bsSolid;
  end else
  begin
    cnv_target.Brush.Style := bsClear;
    cnv_target.Pen.Width := 1;
    dest_rect := Rect((mark_x - cnv_left)*32, (mark_y - cnv_top)*32, (mark_x + mark_width - cnv_left)*32+1, (mark_y + mark_height - cnv_top)*32+1);
    if marker_type = emSingleObject then
    begin
      cnv_target.Pen.Color := clGreen;
      cnv_target.Pen.Style := psDot;
    end else
    if marker_type = emSelectionArea then
    begin
      cnv_target.Pen.Color := clRed;
    end else
    if marker_type = emPaintArea then
    begin
      cnv_target.Pen.Color := clBlue;
      cnv_target.Pen.Style := psDot;
    end;
    cnv_target.Rectangle(dest_rect);
    cnv_target.Pen.Style := psSolid;
  end;
end;

end.
