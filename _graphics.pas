unit _graphics;

interface

uses Windows, Graphics, _utils;

// *****************************************************************************
// DATA.R16 file definitions
// *****************************************************************************

type
  TR16EntryHeader = packed record
    EntryType: byte;
    ImageWidth: integer;
    ImageHeight: integer;
    ImageOffsetX: integer;
    ImageOffsetY: integer;
    ImageHandle: cardinal;
    PaletteHandle: cardinal;
    BitsPerPixel: byte;
    FrameHeight: byte;
    FrameWidth: byte;
    Alignment: byte;
  end;

  TR16EntryHeaderPtr = ^TR16EntryHeader;

  TR16Palette = packed record
    Memory: integer;
    PaletteHandle2: integer;
    Colors: array[0..255] of word;
  end;

  TR16PalettePtr = ^TR16Palette;

const default_palette_colors: array[0..17] of integer = (31775, 0, 0, 2114, 4228, 6342, 8456, 10570, 12684, 14798, 17969, 20083, 22197, 24311, 26425, 28539, 30653, 32767);

// *****************************************************************************
// Internal graphical data structure definitions
// *****************************************************************************

type
  TStructureImage = record
    bitmap: TBitmap;
    bitmap_mask: TBitmap;
    bitmap_data: TWordArrayPtr;
    offset_x: SmallInt;
    offset_y: SmallInt;
    house_color_pixel_first_index: integer;
    house_color_pixel_count: word;
    current_house_color: SmallInt;
  end;

  TStructureImagePtr = ^TStructureImage;

  TStructureImageMappingEntry = record
    normal_index: smallint;
    stealth_index: smallint;
  end;

const STRUCTURE_IMAGE_ENTRY_NOT_LOADED = -1;
const STRUCTURE_IMAGE_ENTRY_EMPTY = -2;

const DATA_R16_FILE_ENTRY_POSITIONS_ALLOC_STEP = 1024;
const STRUCTURE_IMAGES_ALLOC_STEP = 128;
const HOUSE_COLOR_PIXEL_ALLOC_STEP = 8192;

const NUM_TECHNICAL_GRAPHICAL_ENTRIES = 206;
const NUM_EMPTY_UNIT_SIDEBAR_ICONS = 10;

const WALL_FRAME_MAPPING: array[0..15] of integer = (0, 2, 3, 12, 1, 6, 11, 7, 4, 13, 5, 9, 15, 10, 8, 14);

const CONCRETE_TILES: array[0..2] of word = (651, 671, 691);

type
  TBuildingSkirt = record
    size_x: integer;
    size_y: integer;
    rock_tile_x: integer;
    rock_tile_y: integer;
    conc_tile_x: integer;
    conc_tile_y: integer;
  end;

const BUILDING_SKIRT_2x2: TBuildingSkirt = (size_x: 2; size_y: 2; rock_tile_x: 17; rock_tile_y: 30; conc_tile_x:  9; conc_tile_y: 32);
const BUILDING_SKIRT_3x2: TBuildingSkirt = (size_x: 3; size_y: 2; rock_tile_x: 11; rock_tile_y: 30; conc_tile_x:  3; conc_tile_y: 32);

// *****************************************************************************
// TStructGraphics class
// *****************************************************************************

type
  TStructGraphics = class

  public
    // File names
    colours_bin_filename: String;
    data_r16_filename: String;
    graphics_misc_objects_filename: String;

    // COLOURS.BIN related data
    colours: array[0..CNT_SIDES-1,0..15] of word;
    house_colors: array[0..CNT_SIDES-1] of Cardinal;
    house_colors_inv: array[0..CNT_SIDES-1] of Cardinal;

    // DATA.R16 related data
    data_r16_file_contents: array of byte;
    data_r16_modified: boolean;
    data_r16_file_entry_positions: array of integer;
    data_r16_file_entry_count: integer;

    structure_images: array of TStructureImage;
    structure_images_count: integer;
    structure_image_entry_mapping: array of TStructureImageMappingEntry;
    house_color_pixel_indexes: array of word;
    house_color_pixel_shades: array of byte;
    house_color_pixel_count_total: integer;

    logpalette: PLogPalette;

    // Misc. objects graphics related data
    graphics_misc_objects: TBitmap;
    graphics_misc_objects_mask: TBitmap;

  public
    // General procedures
    procedure init;
    function colour_16bto32b(colour: word): cardinal;

    // COLOURS.BIN related procedures
    procedure load_colours_bin;
    // DATA.R16 related procedures
    procedure load_data_r16(force: boolean);
    procedure save_data_r16;
    procedure process_data_r16_entries;
    procedure clear_structure_image_data;
    procedure clear_last_structure_image(entry_index: integer; is_stealth: boolean);
    procedure load_structure_image(entry_index, house_index: integer; is_unit, is_stealth: boolean);
    procedure recolor_structure_image(image_index, house_index: integer);
    function get_structure_image(entry_index, house_index: integer; is_unit, is_stealth: boolean; var was_already_loaded: boolean): TStructureImagePtr;
    function get_structure_image_header(entry_index: integer): TR16EntryHeaderPtr;
    function get_structure_image_palette(entry_index: integer): TR16PalettePtr;
    function get_raw_structure_image(entry_index: integer): TBitmap;
    procedure set_default_palette_colors(entry_index: integer);
    function remap_image_colors(entry_index: integer; ini_filename: String): boolean;
    // Misc. objects related procedures
    procedure load_graphics_misc_objects;

    // DATA.R16 manipulation procedures
    function get_image_entry_position(entry_index: integer): integer;
    function get_image_entries_size(first_entry_index, num_entries: integer): integer;
    procedure resize_data_r16_contents(position, old_size, new_size: integer);
    procedure modify_image_data(entry_index: integer; var data; data_size: integer);
    procedure add_empty_image_entries(entry_index: integer; num_entries: integer);
    procedure remove_image_entries(entry_index: integer; num_entries: integer);
    procedure change_image_entry_count(first_entry_index, old_num_entries, new_num_entries: integer);
    procedure swap_image_entries(src_entry_index, dest_entry_index, src_num_entries, dest_num_entries: integer);
    procedure export_image_entries(filename: string; entry_index: integer; num_entries: integer);
    function import_image_entries(filename: string; entry_index: integer; num_entries: integer): boolean;
    procedure export_single_image(filename: string; entry_index: integer);
    procedure erase_single_image(entry_index: integer);
    function import_single_image(filename: string; entry_index: integer; is_icon: boolean): boolean;
  end;

var
  StructGraphics: TStructGraphics;

implementation

uses Forms, SysUtils, IniFiles, Classes, _settings, _dispatcher, _missionini, Math, pngimage;

procedure TStructGraphics.init;
begin
  GetMem(logpalette, Sizeof(TLogPalette) + Sizeof(TPaletteEntry) * 255);
  logpalette.palversion := $300;
  logpalette.palnumentries := 256;
  graphics_misc_objects := TBitmap.Create;
  graphics_misc_objects_mask := TBitmap.Create;
  load_colours_bin;
  load_data_r16(false);
  load_graphics_misc_objects;
end;

function TStructGraphics.colour_16bto32b(colour: word): cardinal;
begin
  result := 0;
  result := result or (((colour and $7C00) shr 10) shl 19) or (((colour and $7C00) shr 12) shl 16);
  result := result or (((colour and $03E0) shr  5) shl 11) or (((colour and $03E0) shr  7) shl  8);
  result := result or (((colour and $001F) shr  0) shl  3) or (((colour and $001F) shr  2) shl  0);
end;

procedure TStructGraphics.load_colours_bin;
var
  tmp_filename, tmp_filename2: String;
  color: Cardinal;
  i: integer;
begin
  // Step 1 - editor's internal file
  tmp_filename := current_dir + 'config\COLOURS.BIN';
  if Settings.LoadCustomColoursBin then
  begin
    // Step 2 - game's internal file
    tmp_filename2 := Settings.GamePath + '\Data\bin\COLOURS.BIN';
    if FileExists(tmp_filename2) then
      tmp_filename := tmp_filename2;
    // Step 3 - file under CustomCampaignData folder
    tmp_filename2 := Settings.GamePath + '\CustomCampaignData\' + MissionIni.CampaignFolder + '\Colours\' + MissionIni.ColoursFile;
    if FileExists(tmp_filename2) then
      tmp_filename := tmp_filename2;
  end;
  // Check if file exists
  if not FileExists(tmp_filename) then
  begin
    Application.MessageBox('Could not find file COLOURS.BIN', 'Error loading game file', MB_OK or MB_ICONERROR);
    exit;
  end;
  // This file is already loaded - do not load it again
  if tmp_filename = colours_bin_filename then
    exit;
  colours_bin_filename := tmp_filename;
  // Load COLOURS.BIN file
  load_binary_file(tmp_filename, colours, sizeof(colours));
  for i := 0 to CNT_SIDES - 1 do
  begin
    color := colour_16bto32b(colours[i, 8]);
    house_colors[i] := color;
    house_colors_inv[i] := ((color and $FF0000) shr 16) or (color and $00FF00) or ((color and $0000FF) shl 16);
  end;
  // Invalidate all preloaded structure images
  for i := 0 to structure_images_count - 1 do
    structure_images[i].current_house_color := -1;
  // Register event in dispatcher
  Dispatcher.register_event(evFLColoursBin);
end;

procedure TStructGraphics.load_data_r16(force: boolean);
var
  tmp_filename: String;
  data_r16_file: file of byte;
  data_r16_file_size: integer;
begin
  tmp_filename := find_file('Data\DATA.R16', 'graphics');
  if (tmp_filename = '') or ((tmp_filename = data_r16_filename) and not force) then
    exit;
  data_r16_filename := tmp_filename;

  // Clear all previously loaded data
  clear_structure_image_data;
  SetLength(data_r16_file_contents, 0);
  // Load file into buffer
  AssignFile(data_r16_file, tmp_filename);
  Reset(data_r16_file);
  data_r16_file_size := FileSize(data_r16_file);
  SetLength(data_r16_file_contents, data_r16_file_size);
  BlockRead(data_r16_file, data_r16_file_contents[0], data_r16_file_size);
  CloseFile(data_r16_file);
  data_r16_modified := false;
  // Parse file
  process_data_r16_entries;
  // Register event in dispatcher
  Dispatcher.register_event(evFLDataR16);
end;

procedure TStructGraphics.save_data_r16;
begin
  if (data_r16_filename = '') or not data_r16_modified then
    exit;
  if not manage_filesave(data_r16_filename, 'Data\DATA.R16', evStructuresFilenameChange) then
    exit;
  save_binary_file(data_r16_filename, data_r16_file_contents[0], Length(data_r16_file_contents));
  data_r16_modified := false;
end;

procedure TStructGraphics.process_data_r16_entries;
var
  data_r16_file_size: integer;
  index: integer;
  position: integer;
  first_byte: byte;
  header: TR16EntryHeaderPtr;
  image_data_size: integer;
  total_entry_size: integer;
begin
  index := 0;
  position := 0;
  data_r16_file_size := Length(data_r16_file_contents);
  while position < data_r16_file_size do
  begin
    first_byte := data_r16_file_contents[position];
    // Reallocate data_r16_file_entry_positions capacity if needed
    if index = Length(data_r16_file_entry_positions) then
      SetLength(data_r16_file_entry_positions, Length(data_r16_file_entry_positions) + DATA_R16_FILE_ENTRY_POSITIONS_ALLOC_STEP);
    data_r16_file_entry_positions[index] := position;
    if first_byte = 0 then
    begin
      inc(position);
      inc(index);
      continue;
    end;
    header := Addr(data_r16_file_contents[position]);
    image_data_size := header.ImageWidth * header.ImageHeight * (header.BitsPerPixel div 8);
    total_entry_size := sizeof(TR16EntryHeader) + image_data_size;
    if (header.PaletteHandle <> 0) and (header.BitsPerPixel = 8) and (header.EntryType = 1) then
      inc(total_entry_size, sizeof(TR16Palette));
    inc(position, total_entry_size);
    inc(index);
  end;
  data_r16_file_entry_count := index;
  // Initialize entry mapping
  SetLength(structure_image_entry_mapping, data_r16_file_entry_count);
  for index := 0 to data_r16_file_entry_count - 1 do
  begin
    structure_image_entry_mapping[index].normal_index := STRUCTURE_IMAGE_ENTRY_NOT_LOADED;
    structure_image_entry_mapping[index].stealth_index := STRUCTURE_IMAGE_ENTRY_NOT_LOADED;
  end;
end;

procedure TStructGraphics.clear_structure_image_data;
var
  i: integer;
begin
  // Clean entry position map
  SetLength(data_r16_file_entry_positions, 0);
  data_r16_file_entry_count := 0;
  // Clean structure images
  for i := 0 to structure_images_count - 1 do
  begin
    with structure_images[i] do
    begin
      if bitmap <> nil then
        bitmap.Destroy;
      bitmap := nil;
      if bitmap_mask <> nil then
        bitmap_mask.Destroy;
      bitmap_mask := nil;
      bitmap_data := nil;
    end;
  end;
  SetLength(structure_images, 0);
  structure_images_count := 0;
  SetLength(structure_image_entry_mapping, 0);
  // Clean house color pixels
  SetLength(house_color_pixel_indexes, 0);
  SetLength(house_color_pixel_shades, 0);
  house_color_pixel_count_total := 0;
end;

procedure TStructGraphics.clear_last_structure_image(entry_index: integer; is_stealth: boolean);
begin
  if structure_images_count = 0 then
    exit;
  dec(structure_images_count);
  with structure_images[structure_images_count] do
  begin
    if bitmap <> nil then
      bitmap.Destroy;
    bitmap := nil;
    if bitmap_mask <> nil then
      bitmap_mask.Destroy;
    bitmap_mask := nil;
    bitmap_data := nil;
    dec(house_color_pixel_count_total, house_color_pixel_count);
  end;
  if not is_stealth then
    structure_image_entry_mapping[entry_index].normal_index := STRUCTURE_IMAGE_ENTRY_NOT_LOADED
  else
    structure_image_entry_mapping[entry_index].stealth_index := STRUCTURE_IMAGE_ENTRY_NOT_LOADED;
end;

procedure TStructGraphics.load_structure_image(entry_index, house_index: integer; is_unit, is_stealth: boolean);
var
  already_loaded: boolean;
  header: TR16EntryHeaderPtr;
  image_data_8bpp: TByteArrayPtr;
  image_data_16bpp: TWordArrayPtr;
  palette: TR16PalettePtr;
  width, height, padded_width: Integer;
  tmp_bitmap, tmp_bitmap_mask: TBitmap;
  tmp_bitmap_data: TWordArrayPtr;
  tmp_offset_x, tmp_offset_y: integer;
  tmp_house_color_pixel_count: integer;
  x, y, image_pos, bitmap_pos: integer;
  pixel: byte;
begin
  // Check if this entry is already loaded
  if not is_stealth then
    already_loaded := structure_image_entry_mapping[entry_index].normal_index <> STRUCTURE_IMAGE_ENTRY_NOT_LOADED
  else
    already_loaded := structure_image_entry_mapping[entry_index].stealth_index <> STRUCTURE_IMAGE_ENTRY_NOT_LOADED;
  if already_loaded then
    exit;
  // Check if this entry is empty
  if data_r16_file_contents[data_r16_file_entry_positions[entry_index]] = 0 then
  begin
    // Empty entry
    if not is_stealth then
      structure_image_entry_mapping[entry_index].normal_index := STRUCTURE_IMAGE_ENTRY_EMPTY
    else
      structure_image_entry_mapping[entry_index].stealth_index := STRUCTURE_IMAGE_ENTRY_EMPTY;
    exit;
  end;
  // Load header and dimensions
  header := Addr(data_r16_file_contents[data_r16_file_entry_positions[entry_index]]);
  image_data_8bpp := Addr(data_r16_file_contents[data_r16_file_entry_positions[entry_index] + sizeof(TR16EntryHeader)]);
  image_data_16bpp := Addr(data_r16_file_contents[data_r16_file_entry_positions[entry_index] + sizeof(TR16EntryHeader)]);
  width := header.ImageWidth;
  height := header.ImageHeight;
  padded_width := width + (width mod 2);
  // Get palette
  palette := get_structure_image_palette(entry_index);
  // Create bitmap for internal image storage
  tmp_bitmap := TBitmap.Create;
  tmp_bitmap.Width := width;
  tmp_bitmap.Height := height;
  tmp_bitmap.PixelFormat := pf15bit;
  tmp_bitmap_data := tmp_bitmap.ScanLine[height - 1];
  // Load image pixels
  tmp_house_color_pixel_count := 0;
  image_pos := 0;
  for y := 0 to height - 1 do
    for x := 0 to width - 1 do
    begin
      bitmap_pos := (height - y - 1) * padded_width + x;
      if header.BitsPerPixel = 8 then
      begin
        pixel := image_data_8bpp[image_pos];
        // Elliminate shadows
        if pixel = 1 then
          pixel := 0;
        // If unit is stealth then whole shape will be drawn in house color
        if is_stealth and (pixel <> 0) then
          pixel := 248;
        // House colox pixel
        if pixel >= 240 then
        begin
          dec(pixel, 240);
          // Reallocate house_color_pixel_indexes and house_color_pixel_shades capacity if needed
          if (house_color_pixel_count_total + tmp_house_color_pixel_count) = Length(house_color_pixel_indexes) then
          begin
            SetLength(house_color_pixel_indexes, Length(house_color_pixel_indexes) + HOUSE_COLOR_PIXEL_ALLOC_STEP);
            SetLength(house_color_pixel_shades, Length(house_color_pixel_shades) + HOUSE_COLOR_PIXEL_ALLOC_STEP);
          end;
          house_color_pixel_indexes[house_color_pixel_count_total + tmp_house_color_pixel_count] := bitmap_pos;
          house_color_pixel_shades[house_color_pixel_count_total + tmp_house_color_pixel_count] := pixel;
          inc(tmp_house_color_pixel_count);
          tmp_bitmap_data[bitmap_pos] := colours[house_index, pixel];
        end else
          tmp_bitmap_data[bitmap_pos] := IfThen(pixel > 0, palette.colors[pixel], 31775); // If pixel is 0, forcefully use magenta color
      end else
        tmp_bitmap_data[bitmap_pos] := image_data_16bpp[image_pos];
      inc(image_pos);
    end;
  // Create mask from image bitmap
  tmp_bitmap_mask := TBitmap.Create;
  tmp_bitmap_mask.Assign(tmp_bitmap);
  tmp_bitmap_mask.Mask(clFuchsia);
  // Change transparent image pixels to black color
  if header.BitsPerPixel = 8 then
  begin
    image_pos := 0;
    for y := 0 to height - 1 do
      for x := 0 to width - 1 do
      begin
        if image_data_8bpp[image_pos] <= 1 then
          tmp_bitmap_data[(height - y - 1) * padded_width + x] := 0;
        inc(image_pos);
      end;
  end;
  // Compute offsets
  if not is_unit then
  begin
    tmp_offset_x := header.ImageOffsetX * -1
  end else
    tmp_offset_x := (header.FrameWidth div 2 - header.ImageOffsetX) - (header.FrameWidth - 32) div 2;
  if not is_unit then
  begin
    tmp_offset_y := IfThen(entry_index >= NUM_TECHNICAL_GRAPHICAL_ENTRIES, header.ImageOffsetY, height);
  end else
    tmp_offset_y := (header.FrameHeight div 2 - header.ImageOffsetY) - (header.FrameHeight - 32) div 2;
  // Save structure image
  if structure_images_count = Length(structure_images) then
    SetLength(structure_images, Length(structure_images) + STRUCTURE_IMAGES_ALLOC_STEP);
  with structure_images[structure_images_count] do
  begin
    bitmap := tmp_bitmap;
    bitmap_mask := tmp_bitmap_mask;
    bitmap_data := tmp_bitmap_data;
    offset_x := tmp_offset_x;
    offset_y := tmp_offset_y;
    house_color_pixel_first_index := house_color_pixel_count_total;
    house_color_pixel_count := tmp_house_color_pixel_count;
    current_house_color := house_index;
  end;
  inc(house_color_pixel_count_total, tmp_house_color_pixel_count);
  if not is_stealth then
    structure_image_entry_mapping[entry_index].normal_index := structure_images_count
  else
    structure_image_entry_mapping[entry_index].stealth_index := structure_images_count;
  inc(structure_images_count);
  Dispatcher.register_event(evLoadStructureImage);
end;

procedure TStructGraphics.recolor_structure_image(image_index, house_index: integer);
var
  i: integer;
begin
  with structure_images[image_index] do
  begin
    if current_house_color = house_index then
      exit;
    for i := 0 to house_color_pixel_count - 1 do
      bitmap_data[house_color_pixel_indexes[house_color_pixel_first_index + i]] := colours[house_index, house_color_pixel_shades[house_color_pixel_first_index + i]];
    current_house_color := house_index;
  end;
end;

function TStructGraphics.get_structure_image(entry_index, house_index: integer; is_unit, is_stealth: boolean; var was_already_loaded: boolean): TStructureImagePtr;
var
  image_index: integer;
begin
  was_already_loaded := false;
  result := nil;
  if data_r16_filename = '' then
    exit;
  if entry_index >= data_r16_file_entry_count then
    exit;
  // Check if image is already preloaded
  if not is_stealth then
    image_index := structure_image_entry_mapping[entry_index].normal_index
  else
    image_index := structure_image_entry_mapping[entry_index].stealth_index;
  // Image is already preloaded, but is empty
  if image_index = STRUCTURE_IMAGE_ENTRY_EMPTY then
    exit;
  // Image is already preloaded, recolor it and return it
  if image_index <> STRUCTURE_IMAGE_ENTRY_NOT_LOADED then
  begin
    recolor_structure_image(image_index, house_index);
    was_already_loaded := true;
    result := Addr(structure_images[image_index]);
    exit;
  end;
  // Image is not yet preloaded, load it now
  load_structure_image(entry_index, house_index, is_unit, is_stealth);
  if not is_stealth then
    image_index := structure_image_entry_mapping[entry_index].normal_index
  else
    image_index := structure_image_entry_mapping[entry_index].stealth_index;
  // Image is empty
  if image_index = STRUCTURE_IMAGE_ENTRY_EMPTY then
    exit;
  // Image is loaded now
  result := Addr(structure_images[image_index]);
end;

function TStructGraphics.get_structure_image_header(entry_index: integer): TR16EntryHeaderPtr;
begin
  if entry_index < data_r16_file_entry_count then
    result := Addr(data_r16_file_contents[data_r16_file_entry_positions[entry_index]])
  else
    result := nil;
end;

function TStructGraphics.get_structure_image_palette(entry_index: integer): TR16PalettePtr;
var
  palette_origin_index: integer;
  palette_origin_header: TR16EntryHeaderPtr;
begin
  result := nil;
  // Check if this image is empty
  if data_r16_file_contents[data_r16_file_entry_positions[entry_index]] = 0 then
    exit;
  // Find palette for this image
  palette_origin_index := entry_index;
  while data_r16_file_contents[data_r16_file_entry_positions[palette_origin_index]] <> 1 do
    dec(palette_origin_index);
  palette_origin_header := Addr(data_r16_file_contents[data_r16_file_entry_positions[palette_origin_index]]);
  result := Addr(data_r16_file_contents[data_r16_file_entry_positions[palette_origin_index] + sizeof(TR16EntryHeader) + palette_origin_header.ImageWidth * palette_origin_header.ImageHeight]);
end;

function TStructGraphics.get_raw_structure_image(entry_index: integer): TBitmap;
var
  header: TR16EntryHeaderPtr;
  image_data_8bpp: TByteArrayPtr;
  image_data_16bpp: TWordArrayPtr;
  palette: TR16PalettePtr;
  width, height, padded_width: Integer;
  tmp_bitmap: TBitmap;
  tmp_bitmap_data_8bpp: TByteArrayPtr;
  tmp_bitmap_data_16bpp: TWordArrayPtr;
  i, x, y, image_pos, bitmap_pos: integer;
  colour: Cardinal;
  palette_handle: HPALETTE;
begin
  result := nil;
  // Check if this image is empty
  if data_r16_file_contents[data_r16_file_entry_positions[entry_index]] = 0 then
    exit;
  // Load header and dimensions
  header := Addr(data_r16_file_contents[data_r16_file_entry_positions[entry_index]]);
  width := header.ImageWidth;
  height := header.ImageHeight;
  // Get palette
  palette := get_structure_image_palette(entry_index);
  // Create bitmap for internal image storage
  tmp_bitmap := TBitmap.Create;
  tmp_bitmap.Width := width;
  tmp_bitmap.Height := height;
  if header.BitsPerPixel = 8 then
  begin
    image_data_8bpp := Addr(data_r16_file_contents[data_r16_file_entry_positions[entry_index] + sizeof(TR16EntryHeader)]);
    padded_width := ((width + 3) div 4) * 4;
    // Init palette
    for i := 0 to 255 do
    begin
      colour := colour_16bto32b(palette.colors[i]);
      logpalette.palPalEntry[i].peRed := (colour shr 16) and $ff;
      logpalette.palPalEntry[i].peGreen := (colour shr 8) and $ff;
      logpalette.palPalEntry[i].peBlue := (colour shr 0) and $ff;
    end;
    palette_handle := CreatePalette(logpalette^);
    tmp_bitmap.PixelFormat := pf8bit;
    tmp_bitmap.Palette := palette_handle;
    DeleteObject(palette_handle);
    tmp_bitmap_data_8bpp := tmp_bitmap.ScanLine[height - 1];
    // Load image pixels
    image_pos := 0;
    for y := 0 to height - 1 do
      for x := 0 to width - 1 do
      begin
        bitmap_pos := (height - y - 1) * padded_width + x;
        tmp_bitmap_data_8bpp[bitmap_pos] := image_data_8bpp[image_pos];
        inc(image_pos);
      end;
  end else
  begin
    image_data_16bpp := Addr(data_r16_file_contents[data_r16_file_entry_positions[entry_index] + sizeof(TR16EntryHeader)]);
    padded_width := width + (width mod 2);
    tmp_bitmap.PixelFormat := pf15bit;
    tmp_bitmap_data_16bpp := tmp_bitmap.ScanLine[height - 1];
    // Load image pixels
    image_pos := 0;
    for y := 0 to height - 1 do
      for x := 0 to width - 1 do
      begin
        bitmap_pos := (height - y - 1) * padded_width + x;
        tmp_bitmap_data_16bpp[bitmap_pos] := image_data_16bpp[image_pos];
        inc(image_pos);
      end;
  end;
  result := tmp_bitmap;
end;

procedure TStructGraphics.set_default_palette_colors(entry_index: integer);
var
  palette: TR16PalettePtr;
  i: integer;
begin
  palette := get_structure_image_palette(entry_index);
  palette.Colors[0] := default_palette_colors[0];
  palette.Colors[1] := default_palette_colors[1];
  for i := 0 to 15 do
    palette.Colors[240 + i] := default_palette_colors[i + 2];
end;

function TStructGraphics.remap_image_colors(entry_index: integer; ini_filename: String): boolean;
var
  ini: TMemIniFile;
  strings: TStringList;
  from_colors, to_colors: array of byte;
  count: integer;
  header: TR16EntryHeaderPtr;
  image_data_8bpp: TByteArrayPtr;
  i, pixel_idx: integer;
begin
  result := false;
  // Load ini file
  ini := TMemIniFile.Create(ini_filename);
  strings := TStringList.Create;
  // Handle Remap_Colors section
  ini.ReadSection('Remap_Colors', strings);
  count := strings.Count;
  if count <> 0 then
  begin
    result := true;
    SetLength(from_colors, count);
    SetLength(to_colors, count);
    for i := 0 to count - 1 do
    begin
      from_colors[i] := strtoint(strings[i]);
      to_colors[i] := ini.ReadInteger('Remap_Colors', strings[i], 0);
    end;
    // Remap colors
    header := Addr(data_r16_file_contents[data_r16_file_entry_positions[entry_index]]);
    image_data_8bpp := Addr(data_r16_file_contents[data_r16_file_entry_positions[entry_index] + sizeof(TR16EntryHeader)]);
    for pixel_idx := 0 to header.ImageWidth * header.ImageHeight - 1 do
      for i := 0 to count - 1 do
        if image_data_8bpp[pixel_idx] = from_colors[i] then
        begin
          image_data_8bpp[pixel_idx] := to_colors[i];
          break;
        end;
    SetLength(from_colors, 0);
    SetLength(to_colors, 0);
  end;
  // Finalize
  strings.Destroy;
  ini.Destroy;
end;

procedure TStructGraphics.load_graphics_misc_objects;
var
  tmp_filename: String;
begin
  tmp_filename := find_file('graphics\misc_objects.bmp', 'graphics');
  if (tmp_filename = '') or (tmp_filename = graphics_misc_objects_filename) then
    exit;
  graphics_misc_objects_filename := tmp_filename;
  // Load graphics from files
  graphics_misc_objects.LoadFromFile(tmp_filename);
  // Create mask
  graphics_misc_objects_mask.Assign(graphics_misc_objects);
  graphics_misc_objects_mask.Mask(clBlack);
  // Register event in dispatcher
  Dispatcher.register_event(evFLMiscObjectsBmp);
end;

function TStructGraphics.get_image_entry_position(entry_index: integer): integer;
begin
  if (entry_index < data_r16_file_entry_count) and (entry_index >= 0) then
    result := data_r16_file_entry_positions[entry_index]
  else if entry_index = data_r16_file_entry_count then
    result := Length(data_r16_file_contents)
  else
    result := -1;
end;

function TStructGraphics.get_image_entries_size(first_entry_index, num_entries: integer): integer;
var
  i: integer;
begin
  result := 0;
  for i := first_entry_index to (first_entry_index + num_entries - 1) do
    if i < data_r16_file_entry_count then
      inc(result, get_image_entry_position(i+1) - get_image_entry_position(i))
    else
      break;
end;

procedure TStructGraphics.resize_data_r16_contents(position, old_size, new_size: integer);
var
  old_total_size: integer;
begin
  if new_size = old_size then
    exit;
  old_total_size := Length(data_r16_file_contents);
  if new_size > old_size then
  begin
    SetLength(data_r16_file_contents, old_total_size + new_size - old_size);
    Move(data_r16_file_contents[position + old_size], data_r16_file_contents[position + new_size], old_total_size - position - old_size);
  end else
  begin
    Move(data_r16_file_contents[position + old_size], data_r16_file_contents[position + new_size], old_total_size - position - old_size);
    SetLength(data_r16_file_contents, old_total_size + new_size - old_size);
  end;
end;

procedure TStructGraphics.modify_image_data(entry_index: integer; var data; data_size: integer);
var
  entry_size: integer;
  entry_position: integer;
begin
  entry_size := get_image_entries_size(entry_index, 1);
  entry_position := get_image_entry_position(entry_index);
  resize_data_r16_contents(entry_position, entry_size, data_size);
  Move(data, data_r16_file_contents[entry_position], data_size);
  if data_size <> entry_size then
  begin
    // Invalidate all cached data and re-process them again
    clear_structure_image_data;
    process_data_r16_entries;
  end;
  data_r16_modified := true;
end;

procedure TStructGraphics.add_empty_image_entries(entry_index, num_entries: integer);
var
  entry_position: integer;
begin
  if num_entries = 0 then
    exit;
  entry_position := get_image_entry_position(entry_index);
  resize_data_r16_contents(entry_position, 0, num_entries);
  FillChar(data_r16_file_contents[entry_position], num_entries, 0);
  // Invalidate all cached data and re-process them again
  clear_structure_image_data;
  process_data_r16_entries;
  data_r16_modified := true;
end;

procedure TStructGraphics.remove_image_entries(entry_index, num_entries: integer);
var
  entries_size: integer;
  entry_position: integer;
begin
  if num_entries = 0 then
    exit;
  entries_size := get_image_entries_size(entry_index, num_entries);
  entry_position := get_image_entry_position(entry_index);
  resize_data_r16_contents(entry_position, entries_size, 0);
  // Invalidate all cached data and re-process them again
  clear_structure_image_data;
  process_data_r16_entries;
  data_r16_modified := true;
end;

procedure TStructGraphics.change_image_entry_count(first_entry_index, old_num_entries, new_num_entries: integer);
begin
  if new_num_entries > old_num_entries then
    add_empty_image_entries(first_entry_index + old_num_entries, new_num_entries - old_num_entries)
  else if new_num_entries < old_num_entries then
    remove_image_entries(first_entry_index + new_num_entries, old_num_entries - new_num_entries);
end;

procedure TStructGraphics.swap_image_entries(src_entry_index, dest_entry_index, src_num_entries, dest_num_entries: integer);
var
  tmp_data: array of byte;
  src_entries_size, dest_entries_size: integer;
  src_entry_position, dest_entry_position: integer;
begin
  src_entries_size := get_image_entries_size(src_entry_index, src_num_entries);
  dest_entries_size := get_image_entries_size(dest_entry_index, dest_num_entries);
  src_entry_position := get_image_entry_position(src_entry_index);
  dest_entry_position := get_image_entry_position(dest_entry_index);
  if dest_entry_position < src_entry_position then
  begin
    SetLength(tmp_data, dest_entries_size);
    Move(data_r16_file_contents[dest_entry_position], tmp_data[0], dest_entries_size);
    Move(data_r16_file_contents[src_entry_position], data_r16_file_contents[dest_entry_position], src_entries_size);
    Move(tmp_data[0], data_r16_file_contents[dest_entry_position + src_entries_size], dest_entries_size);
  end else
  begin
    SetLength(tmp_data, src_entries_size);
    Move(data_r16_file_contents[src_entry_position], tmp_data[0], src_entries_size);
    Move(data_r16_file_contents[dest_entry_position], data_r16_file_contents[src_entry_position], dest_entries_size);
    Move(tmp_data[0], data_r16_file_contents[src_entry_position + dest_entries_size], src_entries_size);
  end;
  SetLength(tmp_data, 0);
  if (src_num_entries > 1) or (dest_num_entries > 1) or (src_entries_size <> dest_entries_size) then
  begin
    // Invalidate all cached data and re-process them again
    clear_structure_image_data;
    process_data_r16_entries;
  end;
  data_r16_modified := true;
end;

procedure TStructGraphics.export_image_entries(filename: string; entry_index, num_entries: integer);
var
  entries_size: integer;
  entry_position: integer;
begin
  if num_entries = 0 then
    exit;
  entries_size := get_image_entries_size(entry_index, num_entries);
  entry_position := get_image_entry_position(entry_index);
  save_binary_file(filename, data_r16_file_contents[entry_position], entries_size);
end;

function TStructGraphics.import_image_entries(filename: string; entry_index, num_entries: integer): boolean;
var
  entries_size: integer;
  entry_position: integer;
  buffer: array of byte;
  f: file of byte;
  size: integer;
  count, position: integer;
  first_byte: byte;
  header: TR16EntryHeaderPtr;
  image_data_size, total_entry_size: integer;
begin
  result := false;
  if num_entries = 0 then
    exit;
  // Read file into buffer
  AssignFile(f, filename);
  Reset(f);
  size := FileSize(f);
  SetLength(buffer, size);
  BlockRead(f, buffer[0], size);
  CloseFile(f);
  // Get number of image entries present in file
  count := 0;
  position := 0;
  while position < size do
  begin
    first_byte := buffer[position];
    if first_byte = 0 then
    begin
      inc(position);
      inc(count);
      continue;
    end;
    header := Addr(buffer[position]);
    image_data_size := header.ImageWidth * header.ImageHeight * (header.BitsPerPixel div 8);
    total_entry_size := sizeof(TR16EntryHeader) + image_data_size;
    if (header.PaletteHandle <> 0) and (header.BitsPerPixel = 8) and (header.EntryType = 1) then
      inc(total_entry_size, sizeof(TR16Palette));
    inc(position, total_entry_size);
    inc(count);
  end;
  // Check if number matches
  if num_entries <> count then
  begin
    Application.MessageBox(PChar(Format('The file you are importing has %d frames, but current art has %d frames. Change number of frames in current art first.', [count, num_entries])), 'Cannot import art', MB_ICONERROR or MB_OK);
    exit;
  end;
  // Modify data
  entries_size := get_image_entries_size(entry_index, num_entries);
  entry_position := get_image_entry_position(entry_index);
  resize_data_r16_contents(entry_position, entries_size, size);
  Move(buffer[0], data_r16_file_contents[entry_position], size);
  SetLength(buffer, 0);
  // Invalidate all cached data and re-process them again
  clear_structure_image_data;
  process_data_r16_entries;
  data_r16_modified := true;
  result := true;
end;

procedure TStructGraphics.export_single_image(filename: string; entry_index: integer);
var
  tmp_bitmap: TBitmap;
  PNG: TPNGObject;
begin
  tmp_bitmap := get_raw_structure_image(entry_index);
  if tmp_bitmap = nil then
    exit;
  if CompareText(ExtractFileExt(filename), '.PNG') = 0 then
  begin
    PNG := TPNGObject.Create;
    PNG.Assign(tmp_bitmap);
    PNG.SaveToFile(filename);
    PNG.Destroy;
  end else
    tmp_bitmap.SaveToFile(filename);
  tmp_bitmap.Destroy;
end;

procedure TStructGraphics.erase_single_image(entry_index: integer);
var
  zero_byte: byte;
begin
  zero_byte := 0;
  modify_image_data(entry_index, zero_byte, 1);
end;

function TStructGraphics.import_single_image(filename: string; entry_index: integer; is_icon: boolean): boolean;
var
  tmp_bitmap: TBitmap;
  PNG: TPNGObject;
  buffer: array of byte;
  header, orig_header: TR16EntryHeaderPtr;
  image_data: TByteArrayPtr;
  palette: TR16PalettePtr;
  i, x, y, image_pos, bitmap_pos: integer;
  padded_width: integer;
  tmp_bitmap_data: TByteArrayPtr;
  color: word;
begin
  result := false;
  tmp_bitmap := TBitmap.Create;
  if CompareText(ExtractFileExt(filename), '.PNG') = 0 then
  begin
    PNG := TPNGObject.Create;
    PNG.LoadFromFile(filename);
    if PNG.Header.ColorType = COLOR_PALETTE then
      PNG.RemoveTransparency;
    tmp_bitmap.Assign(PNG);
    PNG.Destroy;
  end else
    tmp_bitmap.LoadFromFile(filename);
  // Perform validation
  if is_icon and ((tmp_bitmap.Width <> 60) or (tmp_bitmap.Height <> 47)) then
  begin
    Application.MessageBox('Icon image must have exact size 60 x 47 pixels.', 'Cannot import image', MB_ICONERROR or MB_OK);
    tmp_bitmap.Destroy;
    exit;
  end;
  if tmp_bitmap.PixelFormat <> pf8bit then
  begin
    Application.MessageBox('Image must be in 8bpp (256-color paletted) format.', 'Cannot import image', MB_ICONERROR or MB_OK);
    tmp_bitmap.Destroy;
    exit;
  end;
  // Convert image into game format
  SetLength(buffer, sizeof(TR16EntryHeader) + tmp_bitmap.Width * tmp_bitmap.Height + sizeof(TR16Palette));
  header := Addr(buffer[0]);
  image_data := Addr(buffer[sizeof(TR16EntryHeader)]);
  palette := Addr(buffer[sizeof(TR16EntryHeader) + tmp_bitmap.Width * tmp_bitmap.Height]);
  // Get original header
  i := entry_index;
  repeat
    orig_header := get_structure_image_header(i);
    dec(i);
  until orig_header.EntryType <> 0;
  // Fill headers
  header.EntryType := 1;
  header.ImageWidth := tmp_bitmap.Width;
  header.ImageHeight := tmp_bitmap.Height;
  header.ImageOffsetX := orig_header.ImageOffsetX;
  header.ImageOffsetY := orig_header.ImageOffsetY;
  header.ImageHandle := orig_header.ImageHandle;
  header.PaletteHandle := orig_header.PaletteHandle;
  header.BitsPerPixel := 8;
  header.FrameHeight := orig_header.FrameHeight;
  header.FrameWidth := orig_header.FrameWidth;
  palette.Memory := $00111111;
  palette.PaletteHandle2 := orig_header.PaletteHandle;
  // Fill image data
  padded_width := ((tmp_bitmap.Width + 3) div 4) * 4;
  tmp_bitmap_data := tmp_bitmap.ScanLine[tmp_bitmap.Height - 1];
  image_pos := 0;
  for y := 0 to tmp_bitmap.Height - 1 do
    for x := 0 to tmp_bitmap.Width - 1 do
    begin
      bitmap_pos := (tmp_bitmap.Height - y - 1) * padded_width + x;
      image_data[image_pos] := tmp_bitmap_data[bitmap_pos];
      inc(image_pos);
    end;
  // Fill palette
  GetPaletteEntries(tmp_bitmap.Palette, 0, 256, logpalette.palPalEntry[0]);
  for i := 0 to 255 do
  begin
    color := 0;
    color := color or (logpalette.palPalEntry[i].peBlue  shr 3) shl 0;
    color := color or (logpalette.palPalEntry[i].peGreen shr 3) shl 5;
    color := color or (logpalette.palPalEntry[i].peRed   shr 3) shl 10;
    palette.Colors[i] := color;
  end;
  // Store image data
  modify_image_data(entry_index, buffer[0], Length(buffer));
  tmp_bitmap.Destroy;
  SetLength(buffer, 0);
  result := true;
end;

end.
