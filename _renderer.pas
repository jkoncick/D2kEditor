unit _renderer;

interface

uses Windows, Graphics, Types, _map;

type EditingMarkerType = (emBuilding, emBuildingNotOnBuildable, emSingleObject, emSelectionArea, emPaintArea);

type
  TRenderer = class

  private
    // Graphic data
    graphics_structures: TBitmap;
    graphics_structures_mask: TBitmap;
    graphics_misc_objects: TBitmap;
    graphics_misc_objects_mask: TBitmap;

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

    procedure load_or_create_mask(graph: TBitmap; mask: TBitmap; filename: String);

    procedure invalidate_init;
    procedure invalidate_map_tile(x, y: word);

    procedure render_map_contents(cnv_target: TCanvas; cnv_left, cnv_top, cnv_width, cnv_height: word;
      data: TMapDataPtr; data_width, data_height: word;
      o_show_grid, o_draw_concrete, o_mark_impassable, o_mark_buildable, o_show_unknown_specials,
      o_use_alloc_indexes, o_show_event_markers, o_mark_defence_areas,
      o_rendering_optimization: boolean);
    procedure render_minimap_contents(cnv_target: TCanvas; data: TMapDataPtr; data_width, data_height: word;
      o_use_alloc_indexes: boolean);

    procedure remove_editing_marker(cnv_target: TCanvas);
    procedure draw_editing_marker(cnv_target: TCanvas; cnv_left, cnv_top, cnv_width, cnv_height: word;
      data: TMapDataPtr; mark_x, mark_y, mark_width, mark_height: word; marker_type: EditingMarkerType);

  end;

var
  Renderer: TRenderer;

implementation

uses SysUtils, Math, Forms, main, _mission, _tileset, _structures, _settings, Classes;

procedure TRenderer.init;
var
  tmp_filename: String;
begin
  // Load graphics from files
  graphics_structures := TBitmap.Create;
  graphics_structures_mask := TBitmap.Create;
  graphics_misc_objects := TBitmap.Create;
  graphics_misc_objects_mask := TBitmap.Create;
  tmp_filename := current_dir + 'graphics/structures.bmp';
  if FileExists(tmp_filename) then
  begin
    graphics_structures.LoadFromFile(tmp_filename);
    load_or_create_mask(graphics_structures, graphics_structures_mask, current_dir + 'graphics/structures_mask.bmp');
  end else
    Application.MessageBox(PChar('Could not find graphics file ' + tmp_filename), 'Error loading graphics', MB_OK or MB_ICONWARNING);
  tmp_filename := current_dir + 'graphics/misc_objects.bmp';
  if FileExists(tmp_filename) then
  begin
    graphics_misc_objects.LoadFromFile(tmp_filename);
    load_or_create_mask(graphics_misc_objects, graphics_misc_objects_mask, current_dir + 'graphics/misc_objects_mask.bmp');
  end else
    Application.MessageBox(PChar('Could not find graphics file ' + tmp_filename), 'Error loading graphics', MB_OK or MB_ICONWARNING);
  // Init backup image
  bkup_bitmap := TBitmap.Create;
  bkup_bitmap.Width := 128;
  bkup_bitmap.Height := 128;
end;

procedure TRenderer.load_or_create_mask(graph, mask: TBitmap; filename: String);
var
  x, y: integer;
  black: TColor;
begin
  mask.PixelFormat := pf1bit;
  if FileExists(filename) then
    mask.LoadFromFile(filename)
  else begin
    mask.Width := graph.Width;
    mask.Height := graph.Height;
    black := graph.Canvas.Pixels[0,0];
    for y := 0 to graph.Height - 1 do
      for x := 0 to graph.Width - 1 do
      begin
        if graph.Canvas.Pixels[x,y] <> black then
          mask.Canvas.Pixels[x,y] := clBlack;
      end;
    mask.SaveToFile(filename);
  end;
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
  o_show_grid, o_draw_concrete, o_mark_impassable, o_mark_buildable, o_show_unknown_specials,
  o_use_alloc_indexes, o_show_event_markers, o_mark_defence_areas,
  o_rendering_optimization: boolean);
var
  min_x, min_y, max_x, max_y: integer;
  shift_count: word;
  x, y: integer;
  xx, yy: integer;
  actual_x, actual_y: integer;
  tile: word;
  special: word;
  player, index: word;
  is_misc: boolean;
  bottom_offset: word;
  wall_bitmap: word;
  dest_rect: TRect;
  src_rect: TRect;
  tile_attr: TileType;
  sinfo: ^TStructureInfo;
  event_marker: ^TEventMarker;
  bottom_style_type: ^TBottomStyleType;
  conc_tile, conc_tile_x, conc_tile_y: word;
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
      max_x := Min(Max(inv_rect.Right + (max_building_width - 1) - cnv_left, -1), cnv_width - 1);
      min_y := Min(Max(inv_rect.Top - 1 - cnv_top, 0), cnv_height);
      max_y := Min(Max(inv_rect.Bottom + (max_building_height - 1) - cnv_top, -1), cnv_height - 1);
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
      special := data[x + cnv_left, y + cnv_top].special;
      // Draw spice like a normal terrain tile
      if special = 1 then
        tile := Tileset.thin_spice_tile
      else if special = 2 then
        tile := Tileset.thick_spice_tile
      else
        // No spice, draw terrain tile normally
        tile := data[x + cnv_left, y + cnv_top].tile;
      cnv_target.CopyRect(rect(x*32,y*32,x*32+32,y*32+32),Tileset.tileimage.Canvas,rect((tile mod 20)*32,(tile div 20 * 32),(tile mod 20)*32+32,(tile div 20 * 32+32)));
      // Draw tile attribute markers
      if o_mark_impassable or o_mark_buildable then
      begin
        tile := data[x + cnv_left, y + cnv_top].tile;
        tile_attr := Tileset.get_tile_type(tile);
        if (tile_attr = ttImpassable) and o_mark_impassable then
          cnv_target.Pen.Color := clRed
        else if (tile_attr = ttInfantryOnly) and o_mark_impassable then
          cnv_target.Pen.Color := $4080FF
        else if (tile_attr = ttBuildable) and o_mark_buildable then
          cnv_target.Pen.Color := $40FF80
        else
          continue;
        cnv_target.Pen.Width := 2;
        cnv_target.MoveTo(x*32, y*32);
        cnv_target.LineTo(x*32+31, y*32+31);
        cnv_target.MoveTo(x*32+31, y*32);
        cnv_target.LineTo(x*32, y*32+31);
      end;
    end;
  end;
  cnv_target.Pen.Width := 1;
  cnv_target.Brush.Style := bsClear;
  // Draw structures
  for y:= min_y - (max_building_height - 1) to max_y + 1 do
  begin
    for x:= min_x - (max_building_width - 1) to max_x + 1 do
    begin
      actual_x := x + cnv_left;
      actual_y := y + cnv_top;
      // If tile is out of map
      if (actual_x < 0) or (actual_x >= data_width) or (actual_y < 0) or (actual_y >= data_height) then
        continue;
      special := data[actual_x, actual_y].special;
      // Do not draw spice tiles (already drawn during terrain rendering)
      if special <= 2 then
        continue;
      // Getting structure parameters
      if Structures.special_value_to_params(special,player,index,is_misc) then
      begin
        // Structure is not empty
        if is_misc then
        begin
          // Value is misc
          if (x < min_x) or (x > max_x) or (y < min_y) or (y > max_y) then
            continue; // Do not draw it outside of rendering area
          src_rect := rect((index-1)*32,0,(index-1)*32+32,32);
          dest_rect := rect(x*32,y*32,x*32+32,y*32+32);
          cnv_target.CopyMode := cmSrcAnd;
          cnv_target.CopyRect(dest_rect,graphics_misc_objects_mask.Canvas,src_rect);
          cnv_target.CopyMode := cmSrcPaint;
          cnv_target.CopyRect(dest_rect,graphics_misc_objects.Canvas,src_rect);
          cnv_target.CopyMode := cmSrcCopy;
        end else
        begin
          // Value is structure
          sinfo := Addr(Structures.structure_info[index]);
          // Draw concrete and building's bottom first
          if (o_draw_concrete) and (index < Structures.first_unit_index) and (not sinfo.not_on_buildable) then
          begin
            bottom_style_type := Addr(bottom_style_types[sinfo.bottom_style]);
            // Draw concrete under building
            for xx := 0 to sinfo.size_x - 1 do
              for yy := 0 to sinfo.size_y - 1 - bottom_style_type.size_y do
              begin
                // Do not draw concrete on all top tiles for 3x4 buildings (i.e. Heavy Factory)
                if (yy = 0) and (sinfo.size_y = 4) and ((xx = 0) or (xx = 2)) then
                  continue;
                // Do not draw concrete outside of rendering area
                if ((x + xx) < min_x) or ((x + xx) > max_x) or ((y + yy) < min_y) or ((y + yy) > max_y) then
                  continue;
                // Do not draw concrete on non-buildable tile
                if Tileset.get_tile_type(data[actual_x+xx, actual_y + yy].tile) <> ttBuildable then
                  continue;
                conc_tile := concrete_tiles[(actual_x + actual_y) mod Length(concrete_tiles)];
                conc_tile_x := conc_tile mod 20;
                conc_tile_y := conc_tile div 20;
                dest_rect := Rect((x + xx)*32, (y + yy)*32, (x + xx)*32 + 32, (y + yy)*32 + 32);
                src_rect := Rect(conc_tile_x*32, conc_tile_y*32, conc_tile_x*32+32, conc_tile_y*32+32);
                cnv_target.CopyRect(dest_rect, Tileset.tileimage.Canvas, src_rect);
              end;
            // Draw building's bottom
            for xx := 0 to bottom_style_type.size_x - 1 do
              for yy := 0 to bottom_style_type.size_y - 1 do
              begin
                bottom_offset := sinfo.size_y - bottom_style_type.size_y;
                // Do not draw it outside of rendering area
                if ((x + xx) < min_x) or ((x + xx) > max_x) or ((y + yy + bottom_offset) < min_y) or ((y + yy + bottom_offset) > max_y) then
                  continue;
                dest_rect := Rect((x + xx)*32, (y + yy + bottom_offset)*32, (x + xx)*32 + 32, (y + yy + bottom_offset)*32 + 32);
                if Tileset.get_tile_type(data[actual_x+xx, actual_y + yy + bottom_offset].tile) = ttBuildable then
                  src_rect := Rect((xx + bottom_style_type.conc_tile_x)*32, (yy + bottom_style_type.conc_tile_y)*32, (xx + bottom_style_type.conc_tile_x)*32+32, (yy + bottom_style_type.conc_tile_y)*32+32)
                else
                  src_rect := Rect((xx + bottom_style_type.rock_tile_x)*32, (yy + bottom_style_type.rock_tile_y)*32, (xx + bottom_style_type.rock_tile_x)*32+32, (yy + bottom_style_type.rock_tile_y)*32+32);
                cnv_target.CopyRect(dest_rect, Tileset.tileimage.Canvas, src_rect);
              end;
          end;
          // Draw actual structure
          dest_rect := rect(x*32,y*32,x*32+sinfo.size_x*32,y*32+sinfo.size_y*32);
          // Translate player number according to allocation index
          if o_use_alloc_indexes then
            player := Mission.mis_data.allocation_index[player];
          if player >= Structures.cnt_map_players then
            player := 0;
          if index = 0 then
          begin
            // Structure is wall
            wall_bitmap := 0;
            // Checking left of wall
            if ((actual_x - 1) >= 0) and Structures.check_links_with_wall(data[actual_x - 1, actual_y].special) then
              wall_bitmap := wall_bitmap + 1;
            // Checking up of wall
            if ((actual_y - 1) >= 0) and Structures.check_links_with_wall(data[actual_x, actual_y - 1].special) then
              wall_bitmap := wall_bitmap + 2;
            // Checking right of wall
            if ((actual_x + 1) < data_width) and Structures.check_links_with_wall(data[actual_x + 1, actual_y].special) then
              wall_bitmap := wall_bitmap + 4;
            // Checking down of wall
            if ((actual_y + 1) < data_height) and Structures.check_links_with_wall(data[actual_x, actual_y + 1].special) then
              wall_bitmap := wall_bitmap + 8;
            // Wall source rect
            src_rect := rect(0,wall_bitmap*32,32,wall_bitmap*32+32);
            index := 0;
          end else
            // Structure is not wall
            src_rect := rect(sinfo.pos_x*32,player*128+sinfo.pos_y*32,sinfo.pos_x*32+sinfo.size_x*32,player*128+sinfo.pos_y*32+sinfo.size_y*32);
          // Adjust render size
          src_rect.Top  := src_rect.Top  - sinfo.size_adjust.Top;
          dest_rect.Top := dest_rect.Top - sinfo.size_adjust.Top;
          src_rect.Left  := src_rect.Left  - sinfo.size_adjust.Left;
          dest_rect.Left := dest_rect.Left - sinfo.size_adjust.Left;
          src_rect.Bottom  := src_rect.Bottom  + sinfo.size_adjust.Bottom;
          dest_rect.Bottom := dest_rect.Bottom + sinfo.size_adjust.Bottom;
          src_rect.Right  := src_rect.Right  + sinfo.size_adjust.Right;
          dest_rect.Right := dest_rect.Right + sinfo.size_adjust.Right;
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
            continue;
          if src_rect.Top >= src_rect.Bottom then
            continue;
          // Drawing structure
          cnv_target.CopyMode := cmSrcAnd;
          cnv_target.CopyRect(dest_rect,graphics_structures_mask.Canvas,src_rect);
          cnv_target.CopyMode := cmSrcPaint;
          cnv_target.CopyRect(dest_rect,graphics_structures.Canvas,src_rect);
          cnv_target.CopyMode := cmSrcCopy;
        end;
      end
      // Draw unknown special value
      else if (special <> 0) and o_show_unknown_specials then
        cnv_target.TextOut(x * 32 + 2, y * 32 + 2, inttostr(special));
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
          player := event_marker.side;
          if player >= cnt_mis_players then
            player := 0;
          cnv_target.Pen.Color := Structures.map_player_info[player].color;
          cnv_target.Brush.Color := Structures.map_player_info[player].color;
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
    for x := 0 to cnt_mis_players - 1 do
      for y := 0 to Mission.mis_data.ai_segments[x,7505] - 1 do
      begin
        cnv_target.Pen.Color := Structures.map_player_info[x].color;
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

procedure TRenderer.render_minimap_contents(cnv_target: TCanvas; data: TMapDataPtr; data_width, data_height: word;
  o_use_alloc_indexes: boolean);
var
  min_x, min_y, max_x, max_y: integer;
  x, y: integer;
  border_x, border_y: integer;
  special: word;
  player, index: word;
  is_misc: boolean;
  sinfo: ^TStructureInfo;
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
    max_x := Min(inv_rect.Right + (max_building_width - 1), data_width - 1);
    min_y := inv_rect.Top;
    max_y := Min(inv_rect.Bottom + (max_building_height - 1), data_height - 1);
  end else
  begin
    // Render whole minimap
    cnv_target.Brush.Color := ClBtnFace;
    cnv_target.Pen.Color := ClBtnFace;
    cnv_target.Rectangle(0,0,max_map_width,max_map_height);
  end;
  border_x := (max_map_width - data_width) div 2;
  border_y := (max_map_height - data_height) div 2;
  // Rendering terrain
  for y:= min_y to max_y do
    for x:= min_x to max_x do
    begin
      cnv_target.Pixels[x+border_x,y+border_y] := Tileset.get_tile_color(data[x,y].tile);
    end;
  // Rendering structures
  for y:= Max(min_y - (max_building_height - 1), 0) to max_y do
    for x:= Max(min_x - (max_building_width - 1), 0) to max_x do
    begin
      special := data[x,y].special;
      if special = 1 then
        cnv_target.Pixels[x+border_x,y+border_y] := Tileset.thin_spice_color;
      if special = 2 then
        cnv_target.Pixels[x+border_x,y+border_y] := Tileset.thick_spice_color;
      if not Structures.special_value_to_params(special,player,index,is_misc) then
        continue
      else if is_misc then
      begin
        cnv_target.Pixels[x+border_x,y+border_y] := Structures.misc_object_info[index].color;
      end else
      begin
        sinfo := Addr(Structures.structure_info[index]);
        // Translate player number according to allocation index
        if o_use_alloc_indexes then
          player := Mission.mis_data.allocation_index[player];
        if player >= cnt_mis_players then
          player := 0;
        // Render structure on map
        cnv_target.Pen.Color := Structures.map_player_info[player].color;
        cnv_target.Brush.Color := Structures.map_player_info[player].color;
        cnv_target.Pixels[x+border_x,y+border_y] := Structures.map_player_info[player].color;
        cnv_target.Rectangle(x+border_x, y+border_y, Min(x+border_x+sinfo.size_x, data_width+border_x), Min(y+border_y+sinfo.size_y, data_height+border_y));
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
  data: TMapDataPtr; mark_x, mark_y, mark_width, mark_height: word; marker_type: EditingMarkerType);
var
  dest_rect: TRect;
  bkup_width, bkup_height: integer;
  x, y: integer;
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
  if (marker_type = emBuilding) or (marker_type = emBuildingNotOnBuildable) then
  begin
    cnv_target.Brush.Style := bsBDiagonal;
    cnv_target.Pen.Style := psClear;
    for y := mark_y to Min(mark_y + mark_height - 1, max_map_height - 1) do
      for x := mark_x to Min(mark_x + mark_width - 1, max_map_width - 1) do
      begin
        // Specific shape for 3*4 buildings (heavy factory etc.)
        if (mark_height = 4) and (y = mark_y) and ((x = mark_x) or (x = mark_x + 2)) then
          continue;
        tile_type := Tileset.get_tile_type(data[x, y].tile);
        if ((marker_type = emBuilding) and (tile_type = ttBuildable)) or ((marker_type = emBuildingNotOnBuildable) and (tile_type = ttImpassable)) then
          cnv_target.Brush.Color := $E0E0E0
        else
          cnv_target.Brush.Color := $0000E0;
        cnv_target.Rectangle((x - cnv_left)*32, (y - cnv_top)*32, (x - cnv_left)*32+33, (y - cnv_top)*32+33);
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
