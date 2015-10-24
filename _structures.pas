unit _structures;

interface

uses Graphics;

const cnt_map_players = 7;

type
  TStructureInfo = record
    name: String;
    offs_x: word; // X-offset in image
    offs_y: word; // Y-offset in image
    size_x: word; // Structure width
    size_y: word; // Structure height
    overfl: word; // Sprite overflow (1 = up (for buildings), 2 = infantry, 3 = wide (harvester, MCV))
    lnwall: boolean;  // Structure links with wall
    power: SmallInt; // Power the structure gives/needs
    values: array[0..cnt_map_players-1] of word; // Map special values
  end;

type
  TMapPlayerInfo = record
    name: String;
    color: TColor;
  end;

type TObjectStatsGroup = (sgNone, sgWormSpawners, sgPlayerStarts, sgSpiceBlooms);

type
  TMiscObjectInfo = record
    name: String;
    value: word;
    color: TColor;
    stats_group: TObjectStatsGroup;
  end;

const first_unit_index = 18;

const structure_info: array[0..31] of TStructureInfo =
  (
    (name: 'Wall';                  offs_x:  0; offs_y: 0; size_x: 1; size_y: 1; overfl:  0; lnwall:  true; power:   0; values:(  4,204,404,580,620,660,700)),
    (name: 'Wind Trap';             offs_x:  1; offs_y: 0; size_x: 2; size_y: 3; overfl:  1; lnwall: false; power:-200; values:(  5,205,405,581,621,661,701)),
    (name: 'Construction Yard';     offs_x:  3; offs_y: 0; size_x: 3; size_y: 3; overfl:  1; lnwall: false; power: -20; values:(  8,208,408,582,622,662,702)),
    (name: 'Barracks';              offs_x:  6; offs_y: 0; size_x: 2; size_y: 3; overfl:  1; lnwall: false; power:  30; values:( 11,211,411,583,623,663,703)),
    (name: 'Refinery';              offs_x:  8; offs_y: 0; size_x: 3; size_y: 3; overfl:  1; lnwall: false; power:  75; values:( 14,214,414,584,624,664,704)),
    (name: 'Outpost';               offs_x: 11; offs_y: 0; size_x: 3; size_y: 3; overfl:  1; lnwall: false; power: 125; values:( 17,217,417,585,625,665,705)),
    (name: 'Light Factory';         offs_x: 14; offs_y: 0; size_x: 3; size_y: 3; overfl:  1; lnwall: false; power: 125; values:( 63,263,463,587,627,667,707)),
    (name: 'Silo';                  offs_x: 17; offs_y: 0; size_x: 1; size_y: 1; overfl:  0; lnwall: false; power:  15; values:( 69,269,469,589,629,668,708)),
    (name: 'Heavy Factory';         offs_x: 18; offs_y: 0; size_x: 3; size_y: 4; overfl:  0; lnwall: false; power: 150; values:( 72,272,472,590,630,669,709)),
    (name: 'Repair Pad';            offs_x: 21; offs_y: 0; size_x: 3; size_y: 3; overfl:  0; lnwall: false; power:  50; values:( 75,275,475,591,631,670,710)),
    (name: 'Gun Turret';            offs_x: 24; offs_y: 0; size_x: 1; size_y: 1; overfl:  1; lnwall:  true; power:  50; values:( 78,278,478,592,632,671,711)),
    (name: 'High Tech Factory';     offs_x: 25; offs_y: 0; size_x: 3; size_y: 4; overfl:  0; lnwall: false; power:  75; values:(120,320,520,593,633,672,712)),
    (name: 'Rocket Turret';         offs_x: 28; offs_y: 0; size_x: 1; size_y: 1; overfl:  1; lnwall:  true; power:  60; values:(123,323,523,594,634,673,713)),
    (name: 'IX Research Centre';    offs_x: 29; offs_y: 0; size_x: 3; size_y: 4; overfl:  0; lnwall: false; power: 175; values:(126,326,526,595,635,674,714)),
    (name: 'Starport';              offs_x: 32; offs_y: 0; size_x: 3; size_y: 3; overfl:  0; lnwall: false; power: 150; values:(129,329,529,596,636,675,715)),
    (name: 'Palace';                offs_x: 35; offs_y: 0; size_x: 3; size_y: 3; overfl:  1; lnwall: false; power: 200; values:(132,332,532,588,628,676,716)),
    (name: 'Sietch';                offs_x: 38; offs_y: 0; size_x: 2; size_y: 2; overfl:  0; lnwall: false; power:  50; values:(  0,  0,  0,597,637,  0,  0)),
    (name: 'Modified Outpost';      offs_x: 40; offs_y: 0; size_x: 3; size_y: 3; overfl:  1; lnwall: false; power: 100; values:(  0,218,418,  0,  0,666,  0)),
    (name: 'Light Infantry';        offs_x: 43; offs_y: 0; size_x: 1; size_y: 1; overfl:  2; lnwall: false; power:   0; values:(180,360,560,598,638,677,717)),
    (name: 'Trooper';               offs_x: 44; offs_y: 0; size_x: 1; size_y: 1; overfl:  2; lnwall: false; power:   0; values:(181,361,561,599,639,678,718)),
    (name: 'St. Fremen / Saboteur'; offs_x: 45; offs_y: 0; size_x: 1; size_y: 1; overfl:  2; lnwall: false; power:   0; values:(182,362,562,600,640,679,719)),
    (name: 'Sardaukar / Fremen';    offs_x: 46; offs_y: 0; size_x: 1; size_y: 1; overfl:  2; lnwall: false; power:   0; values:(183,363,563,601,641,  0,  0)),
    (name: 'Engineer';              offs_x: 47; offs_y: 0; size_x: 1; size_y: 1; overfl:  2; lnwall: false; power:   0; values:(184,364,564,602,642,680,720)),
    (name: 'Harvester';             offs_x: 44; offs_y: 1; size_x: 1; size_y: 1; overfl:  3; lnwall: false; power:   0; values:(185,365,565,603,643,681,721)),
    (name: 'MCV';                   offs_x: 46; offs_y: 1; size_x: 1; size_y: 1; overfl:  3; lnwall: false; power:   0; values:(186,366,566,604,644,682,722)),
    (name: 'Trike / Raider';        offs_x: 43; offs_y: 3; size_x: 1; size_y: 1; overfl:  0; lnwall: false; power:   0; values:(187,367,567,605,645,683,723)),
    (name: 'Quad';                  offs_x: 44; offs_y: 3; size_x: 1; size_y: 1; overfl:  0; lnwall: false; power:   0; values:(188,368,568,606,646,684,724)),
    (name: 'Combat Tank';           offs_x: 45; offs_y: 3; size_x: 1; size_y: 1; overfl:  0; lnwall: false; power:   0; values:(189,369,569,607,647,685,725)),
    (name: 'Missile Tank';          offs_x: 46; offs_y: 3; size_x: 1; size_y: 1; overfl:  0; lnwall: false; power:   0; values:(190,370,570,608,648,686,726)),
    (name: 'Siege Tank';            offs_x: 47; offs_y: 3; size_x: 1; size_y: 1; overfl:  1; lnwall: false; power:   0; values:(191,371,571,609,649,687,727)),
    (name: 'Carryall';              offs_x: 17; offs_y: 1; size_x: 1; size_y: 1; overfl:  0; lnwall: false; power:   0; values:(192,372,572,610,650,688,728)),
    (name: 'House Specific Tank';   offs_x: 47; offs_y: 1; size_x: 1; size_y: 1; overfl:  1; lnwall: false; power:   0; values:(194,374,574,  0,652,  0,  0))
  );

const map_player_info: array[0..7] of TMapPlayerInfo =
  (
    (name: 'Atreides';    color: $84614A),
    (name: 'Harkonnen';   color: $3231C6),
    (name: 'Ordos';       color: $63824A),
    (name: 'Emperor';     color: $6B0063),
    (name: 'Fremen';      color: $747274),
    (name: 'Smugglers';   color: $00106B),
    (name: 'Mercenaries'; color: $08728C),
    (name: 'Sandworm';    color: $406088)
  );

const misc_object_info: array[0..7] of TMiscObjectInfo =
  (
    (name: 'Nothing';                 value: 0;  color: $000000; stats_group: sgNone;),
    (name: 'Worm Spawner';            value: 20; color: $FF00FF; stats_group: sgWormSpawners;),
    (name: 'Player Start';            value: 23; color: $FFFF00; stats_group: sgPlayerStarts;),
    (name: 'Spice Bloom';             value: 45; color: $0000FF; stats_group: SgSpiceBlooms;),
    (name: 'Spice Bloom (Finite 41)'; value: 41; color: $0000B0; stats_group: SgSpiceBlooms;),
    (name: 'Spice Bloom (Finite 42)'; value: 42; color: $0000C0; stats_group: sgSpiceBlooms;),
    (name: 'Spice Bloom (Finite 43)'; value: 43; color: $0000D0; stats_group: sgSpiceBlooms;),
    (name: 'Spice Bloom (Finite 44)'; value: 44; color: $0000E0; stats_group: sgSpiceBlooms;)
  );

function special_value_to_params(value: word; var player: word; var index: word; var is_misc: boolean): boolean;

implementation

function special_value_to_params(value: word; var player, index: word; var is_misc: boolean): boolean;
var i, j: integer;
begin
  if value = 0 then
  begin
    result := false;
    exit;
  end;
  for i:= 0 to Length(misc_object_info) - 1 do
  begin
    if value = misc_object_info[i].value then
    begin
      index := i;
      is_misc := true;
      result := true;
      exit;
    end;
  end;
  for i := 0 to Length(structure_info) - 1 do
  begin
    for j:= 0 to cnt_map_players - 1 do
    begin
      if value = structure_info[i].values[j] then
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

end.
