unit _colours;

interface

uses Classes, _utils;

type
  TColours = class

  public
    // COLOURS.BIN file list
    colours_file_list: TStringList;

    // COLOURS.BIN related data
    colours_bin_filename: String;
    colours_bin_file_index: integer;
    colours_data: array[0..CNT_SIDES-1,0..15] of word;
    colours_bin_modified: boolean;
    house_colors: array[0..CNT_SIDES-1] of Cardinal;
    house_colors_inv: array[0..CNT_SIDES-1] of Cardinal;

    clipboard_format: Cardinal;

  public
    procedure init;
    procedure load_colours_file_list;
    procedure load_colours_bin(campaign_folder, colours_bin_file: string);
    procedure load_colours_bin_by_index(file_index: integer);
    procedure save_colours_bin;
    procedure save_colours_bin_as(campaign_folder, colours_bin_file: string);
    procedure delete_current_file;
    function colour_16bto32b(colour: word): cardinal;
    function colour_swap_bytes(colour: cardinal): cardinal;
    procedure set_house_color(house: integer);
    function get_colour_32bit(house, index: integer): cardinal;
    procedure set_colour_32bit(house, index: integer; colour: cardinal);
    procedure set_row_from_gradient(house: integer; color1, color2: cardinal);
    procedure copy_row(house: integer);
    procedure paste_row(house: integer);
  end;

var
  Colours: TColours;

implementation

uses Windows, Forms, SysUtils, Math, Clipbrd, _settings, _dispatcher, _missionini, _resourcefile;

procedure TColours.init;
begin
  clipboard_format := RegisterClipboardFormat('D2kEditorColoursRow');
  colours_bin_file_index := -1;
  colours_file_list := TStringList.Create;
  load_colours_file_list;
  load_colours_bin('', '');
end;

procedure TColours.load_colours_file_list;
var
  SR1, SR2: TSearchRec;
  campaign_folder: string;
begin
  colours_file_list.Clear;
  colours_file_list.Add('COLOURS.BIN');
  if FindFirst(Settings.GamePath + '\CustomCampaignData\*', faDirectory, SR1) = 0 then
  begin
    repeat
      if ((SR1.Attr and faDirectory) <> 0) and (SR1.Name <> '.') and (SR1.Name <> '..') then
      begin
        campaign_folder := SR1.Name;
        if FindFirst(Settings.GamePath + '\CustomCampaignData\' + campaign_folder + '\Colours\*.BIN', 0, SR2) = 0 then
        begin
          repeat
            colours_file_list.Add(campaign_folder + '/' +SR2.Name);
          until FindNext(SR2) <> 0;
            FindClose(SR2);
        end;
      end;
    until FindNext(SR1) <> 0;
      FindClose(SR1);
  end;
  Dispatcher.register_event(evFLLColoursBinFileList);
end;

procedure TColours.load_colours_bin(campaign_folder, colours_bin_file: string);
var
  internal_filename, custom_filename, tmp_filename: string;
  compare_str: string;
  tmp_file_index, filenum, i: integer;
begin
  tmp_file_index := -1;
  internal_filename := Settings.GamePath + '\Data\bin\COLOURS.BIN';
  custom_filename := Settings.GamePath + '\CustomCampaignData\' + campaign_folder + '\Colours\' + colours_bin_file;
  if (campaign_folder <> '') and (colours_bin_file <> '') and FileExists(custom_filename) then
  begin
    tmp_filename := custom_filename;
    compare_str := campaign_folder + '/' + colours_bin_file;
    for i := 1 to colours_file_list.Count - 1 do
      if AnsiCompareText(compare_str, colours_file_list[i]) = 0 then
      begin
        tmp_file_index := i;
        break;
      end;
  end else
  begin
    if not FileExists(internal_filename) then
    begin
      Application.MessageBox('Could not find file COLOURS.BIN', 'Error loading game file', MB_OK or MB_ICONERROR);
      exit;
    end;
    tmp_filename := internal_filename;
    tmp_file_index := 0;
  end;
  // This file is already loaded - do not load it again
  if (tmp_filename = colours_bin_filename) and not colours_bin_modified then
    exit;
  colours_bin_filename := tmp_filename;
  colours_bin_file_index := tmp_file_index;
  // Load COLOURS.BIN file
  load_binary_file(tmp_filename, colours_data, sizeof(colours_data));
  colours_bin_modified := false;
  for i := 0 to CNT_SIDES - 1 do
    set_house_color(i);
  // Invalidate all preloaded structure images
  for filenum := 0 to Length(ResourceFile) - 1 do
    ResourceFile[filenum].invalidate_colors;
  // Register event in dispatcher
  Dispatcher.register_event(evFLColoursBin);
end;

procedure TColours.load_colours_bin_by_index(file_index: integer);
var
  campaign_folder, colours_bin_file: string;
begin
  if file_index = 0 then
    load_colours_bin('', '')
  else begin
    split_string_by_delimiter(colours_file_list[file_index], '/', campaign_folder, colours_bin_file);
    load_colours_bin(campaign_folder, colours_bin_file);
  end;
end;

procedure TColours.save_colours_bin;
begin
  if (colours_bin_filename = '') or not colours_bin_modified then
    exit;
  if not confirm_overwrite_original_file(colours_bin_filename, Settings.GamePath + '\Data\bin\COLOURS.BIN', false) then
    exit;
  save_binary_file(colours_bin_filename, colours_data, sizeof(colours_data));
  colours_bin_modified := false;
end;

procedure TColours.save_colours_bin_as(campaign_folder, colours_bin_file: string);
var
  ext: string;
  folder: string;
  i: integer;
  compare_str: string;
  found: boolean;
begin
  if (campaign_folder = '') or (colours_bin_file = '') then
    exit;
  ext := ExtractFileExt(colours_bin_file);
  if ext = '' then
    colours_bin_file := colours_bin_file + '.BIN'
  else if AnsiCompareText(ext, '.BIN') <> 0 then
  begin
    Application.MessageBox('The file must have .BIN extension', 'Error saving COLOURS.BIN', MB_OK or MB_ICONERROR);
    exit;
  end;
  folder := Settings.GamePath + '\CustomCampaignData\' + campaign_folder + '\Colours';
  ForceDirectories(folder);
  colours_bin_filename := folder + '\' + colours_bin_file;
  save_binary_file(colours_bin_filename, colours_data, sizeof(colours_data));
  colours_bin_modified := false;
  // Try to find target file in file list
  compare_str := campaign_folder + '/' + colours_bin_file;
  found := false;
  for i := 1 to colours_file_list.Count - 1 do
    if AnsiCompareText(compare_str, colours_file_list[i]) = 0 then
    begin
      colours_bin_file_index := i;
      found := true;
      break;
    end;
  // If not found, then reload colours file list
  if not found then
  begin
    load_colours_file_list;
    for i := 1 to colours_file_list.Count - 1 do
      if AnsiCompareText(compare_str, colours_file_list[i]) = 0 then
      begin
        colours_bin_file_index := i;
        break;
      end;
  end;
end;

procedure TColours.delete_current_file;
begin
  DeleteFile(colours_bin_filename);
  load_colours_file_list;
  load_colours_bin(MissionIni.CampaignFolder, MissionIni.ColoursFile);
end;

function TColours.colour_16bto32b(colour: word): cardinal;
begin
  result := 0;
  result := result or (((colour and $7C00) shr 10) shl 19) or (((colour and $7C00) shr 12) shl 16);
  result := result or (((colour and $03E0) shr  5) shl 11) or (((colour and $03E0) shr  7) shl  8);
  result := result or (((colour and $001F) shr  0) shl  3) or (((colour and $001F) shr  2) shl  0);
end;

function TColours.colour_swap_bytes(colour: cardinal): cardinal;
begin
  result := ((colour and $FF0000) shr 16) or (colour and $00FF00) or ((colour and $0000FF) shl 16);
end;

procedure TColours.set_house_color(house: integer);
var
  color: Cardinal;
begin
  color := colour_16bto32b(colours_data[house, 8]);
  house_colors[house] := color;
  house_colors_inv[house] := colour_swap_bytes(color);
end;

function TColours.get_colour_32bit(house, index: integer): cardinal;
begin
  result := colour_16bto32b(colours_data[house, index]);
end;

procedure TColours.set_colour_32bit(house, index: integer; colour: cardinal);
var
  temp_color: word;
begin
  temp_color := 0;
  temp_color := temp_color or (((colour and $FF0000) shr 19) shl 10);
  temp_color := temp_color or (((colour and $00FF00) shr 11) shl 5);
  temp_color := temp_color or (((colour and $0000FF) shr 3) shl 0);
  colours_data[house, index] := temp_color;
  if index = 8 then
    set_house_color(house);
  colours_bin_modified := true;
end;

procedure HSLtoRGB(H,S,L:Integer;var R,G,B:Integer);
var
  Sat,Lum : Double;
begin
  R := 0;
  G := 0;
  B := 0;
  if (H < 360) and (H >= 0) and (S <= 100) and (S >= 0) and (L <= 100) and (L >= 0) then begin
    if H <=60 then begin
      R := 255;
      G := Round((255/60)*H);
      B := 0;
    end
    else if H <=120 then begin
      R := Round(255-(255/60)*(H-60));
      G := 255;
      B := 0;
    end
    else if H <=180 then begin
      R := 0;
      G := 255;
      B := Round((255/60)*(H-120));
    end
    else if H <=240 then begin
      R := 0;
      G := Round(255-(255/60)*(H-180));
      B := 255;
    end
    else if H <=300 then begin
      R := Round((255/60)*(H-240));
      G := 0;
      B := 255;
    end
    else if H <360 then begin
      R := 255;
      G := 0;
      B := Round(255-(255/60)*(H-300));
    end;

    Sat := Abs((S-100)/100);
    R := Round(R-((R-128)*Sat));
    G := Round(G-((G-128)*Sat));
    B := Round(B-((B-128)*Sat));

    Lum := (L-50)/50;
    if Lum > 0 then begin
      R := Round(R+((255-R)*Lum));
      G := Round(G+((255-G)*Lum));
      B := Round(B+((255-B)*Lum));
    end
    else if Lum < 0 then begin
      R := Round(R+(R*Lum));
      G := Round(G+(G*Lum));
      B := Round(B+(B*Lum));
    end;
  end;
end;

procedure RGBtoHSL(R,G,B:Integer;var H,S,L:Integer);
var
  Delta : Double;
  CMax,CMin : Double;
  Red,Green,Blue,Hue,Sat,Lum : Double;
begin
  Red := R/255;
  Green := G/255;
  Blue := B/255;
  CMax := Max(Red,Max(Green,Blue));
  CMin := Min(Red,Min(Green,Blue));
  Lum := (CMax+CMin)/2;
  if CMax = CMin then begin
    Sat := 0;
    Hue := 0;
  end
  else begin
    if Lum < 0.5 then Sat := (CMax-CMin)/(CMax+CMin)
    else Sat := (cmax-cmin)/(2-cmax-cmin);
    delta := CMax-CMin;
    If Red = CMax then Hue := (Green-Blue)/Delta
    else if Green = CMax then Hue := 2+(Blue-Red)/Delta
    else Hue := 4.0+(Red-Green)/Delta;
    Hue := Hue / 6;
    If Hue < 0 then Hue := Hue + 1;
  end;
  H := Round(Hue*360);
  S := Round(Sat*100);
  L := Round(Lum*100);
end;

procedure TColours.set_row_from_gradient(house: integer; color1, color2: cardinal);
var
  r1, r2, g1, g2, b1, b2: integer;
  h1, h2, s1, s2, l1, l2: integer;
  hdiff, sdiff, ldiff: integer;
  r, g, b: integer;
  h, s, l: integer;
  i: integer;
  color: cardinal;
begin
  set_colour_32bit(house, 0, color1);
  set_colour_32bit(house, 15, color2);
  r1 := (color1 shr 16) and 255;
  r2 := (color2 shr 16) and 255;
  g1 := (color1 shr 8) and 255;
  g2 := (color2 shr 8) and 255;
  b1 := (color1 shr 0) and 255;
  b2 := (color2 shr 0) and 255;
  RGBtoHSL(r1, g1, b1, h1, s1, l1);
  RGBtoHSL(r2, g2, b2, h2, s2, l2);
  hdiff := h2 - h1;
  sdiff := s2 - s1;
  ldiff := l2 - l1;
  for i := 1 to 14 do
  begin
    h := h1 + (hdiff * i) div 15;
    s := s1 + (sdiff * i) div 15;
    l := l1 + (ldiff * i) div 15;
    HSLtoRGB(h, s, l, r, g, b);
    color := (r shl 16) or (g shl 8) or (b shl 0);
    set_colour_32bit(house, i, color);
  end;
end;

procedure TColours.copy_row(house: integer);
var
  handle: THandle;
  data_ptr: TByteArrayPtr;
begin
  OpenClipboard(Application.Handle);
  EmptyClipboard;
  handle := GlobalAlloc(GMEM_DDESHARE or GMEM_MOVEABLE, sizeof(colours_data[house]));
  data_ptr := GlobalLock(handle);
  Move(colours_data[house], data_ptr[0], sizeof(colours_data[house]));
  GlobalUnLock(handle);
  SetClipboardData(clipboard_format, handle);
  CloseClipboard;
end;

procedure TColours.paste_row(house: integer);
var
  handle: THandle;
  data_ptr: TByteArrayPtr;
begin
  if not Clipboard.HasFormat(clipboard_format) then
    exit;
  OpenClipboard(Application.Handle);
  handle := GetClipboardData(clipboard_format);
  data_ptr := GlobalLock(handle);
  Move(data_ptr[0], colours_data[house], sizeof(colours_data[house]));
  GlobalUnLock(handle);
  CloseClipboard;
  set_house_color(house);
  colours_bin_modified := true;
end;

end.
