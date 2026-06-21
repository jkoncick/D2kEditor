unit _colours;

interface

uses _utils;

type
  TColours = class

  public
    // File names
    colours_bin_filename: String;

    // COLOURS.BIN related data
    colours: array[0..CNT_SIDES-1,0..15] of word;
    house_colors: array[0..CNT_SIDES-1] of Cardinal;
    house_colors_inv: array[0..CNT_SIDES-1] of Cardinal;

  public
    procedure init;
    function colour_16bto32b(colour: word): cardinal;
    procedure load_colours_bin;
  end;

var
  Colours: TColours;

implementation

uses Windows, Forms, SysUtils, _settings, _dispatcher, _missionini, _resourcefile;

procedure TColours.init;
begin
  load_colours_bin;
end;

function TColours.colour_16bto32b(colour: word): cardinal;
begin
  result := 0;
  result := result or (((colour and $7C00) shr 10) shl 19) or (((colour and $7C00) shr 12) shl 16);
  result := result or (((colour and $03E0) shr  5) shl 11) or (((colour and $03E0) shr  7) shl  8);
  result := result or (((colour and $001F) shr  0) shl  3) or (((colour and $001F) shr  2) shl  0);
end;

procedure TColours.load_colours_bin;
var
  tmp_filename, tmp_filename2: String;
  color: Cardinal;
  filenum, i: integer;
begin
  // Step 1 - game's internal file
  tmp_filename := Settings.GamePath + '\Data\bin\COLOURS.BIN';
  // Step 2 - file under CustomCampaignData folder
  tmp_filename2 := Settings.GamePath + '\CustomCampaignData\' + MissionIni.CampaignFolder + '\Colours\' + MissionIni.ColoursFile;
  if FileExists(tmp_filename2) then
    tmp_filename := tmp_filename2;
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
  for filenum := 0 to Length(ResourceFile) - 1 do
    ResourceFile[filenum].invalidate_colors;
  // Register event in dispatcher
  Dispatcher.register_event(evFLColoursBin);
end;

end.
