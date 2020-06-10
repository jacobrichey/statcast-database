# Statcast Documentation
###### Data sourced from Baseball Savant.
### Plate Appearance Information
gameid - mlbam gameid\
game_year – year\
game_date – date of the game
game_type - type of game (e.g. Regular Season, World Series, etc.)\
home_team – home team\
vis_team – visiting team\
pa_number – game pa number\
pitch_number - total pitch number of the plate appearance\
inning\
is_bottom\
batterid – mlbam batter id\
batter_name\
is_lhb – is left handed batter\
pitcherid – mlbam pitcher id\
pitcher_name\
is_lhp – is left handed pitcher\
bat_score_before – score of batting team before plate appearance event\
bat_score_after – score of batting team after plate appearance event\
field_score – field team score during batting event\
baseout_state_before – basecode and outs before plate appearance event (e.g. 010 1 = runner on 2nd, one out)\
baseout_state_after – basecode and outs after plate appearance event (e.g. 001 2 = runner on 3rd, two outs)\
event_type - resulting event of plate appearance\
event_description - plate appearance description from game day\
pitch_description - description of resulting pitch
### Pitch Details
balls – balls when pitch was thrown\
strikes – strikes when pitch was thrown\
is_last_pitch - was it the last pitch in the plate appearance\
is_bip - was the ball put in play\
is_stk - was the pitch a strike\
pitch_type – the type of pitch derived from Statcast (CH - changeup; CU - curveball; EP - eephus; FA - fastball; FC - fastball (cutter); FF - fastball (4-seam); FO - forkball; FS - fastball (split finger); FT - fastball (2-seam); IN - intentional ball; KC - knuckle curve; KN - knuckleball; PO - pitchout; SC - screwball; SI - sinker; SL - slider; UN - unknown) \
attack_region - zone location of the ball when it crosses the plate from the catcher's perspective (see http://tangotiger.net/strikezone/zone%20chart.png) \
plate_x - horizontal position of the ball in feet when it crosses home plate from the catcher's perspective\
plate_z - vertical position of the ball in feet when it crosses home plate from the catcher's perspective\
sz_top - top of the batter's strike zone in feet set by the operator when the ball is halfway to the plate\
sz_bot - bottom of the batter's strike zone set in feet by the operator when the ball is halfway to the plate\
rel_plate_x - zone center at 0, with 1 denoting the strike zone perimeter\
rel_plate_z - zone center at 0, with 1 denoting the strike zone perimeter (dependent on batter height)
### Batting Details
bb_type - batted ball type (ground_ball, line_drive, fly_ball, popup) \
estimated_ba_using_speedangle - estimated batting avg based on launch angle and exit velocity (NA for '08-'14) \
estimated_woba_using_speedangle - estimated wOBA based on launch angle and exit velocity (NA for '08-'14) \
woba_value - wOBA value based on result of play\
woba_denom - wOBA denominator based on result of play (NA for '08-'14) \
babip_value - BABIP value based on result of play\
iso_value - ISO value based on result of play\
launch_speed – exit velocity in mph of batted ball as tracked by Statcast (NA for '08-'14) \
launch_angle – launch angle in mph of batted ball as tracked by Statcast (NA for '08-'14) \
launch_speed_angle - launch speed/angle zone based on launch angle and exit velocity (1 = weak, 2 = topped, 3 = under, 4 = flare/burner, 5 = solid contact, 6 = barrel) [NA for '08-'14] \
hc_x - hit coordiante X of batted ball\
hc_y - hit coordiante Y of batted ball\
hit_distance – projected hit distance of the batted ball in feet (NA for '08-'14) \
spray_angle – spray angle in degrees\
cdrv_24 - context-dependent run value, based on RE24 (see http://tangotiger.com/index.php/site/article/statcast-lab-swing-take-and-a-primer-on-run-value) \
cnrv_24 - context-neutral run value, based on RE24 \
cdrv_288 - context-dependent run value, based on RE288 \
cnrv_288 - context-neutral run value, based on RE288
### Pitching Details
release_speed – release speed of pitch in mph\
effective_speed - derived speed based on the extension of the pitcher's release (NA for '08-'14) \
release_spin – spin rate of pitch in rpm as tracked by Statcast (NA for '08-'14)\
release_pos_x – horizontal release position of ball measured in feet from catcher's perspective (NA for '08-'14) \
release_pos_y - release position of pitch measured in feet from catcher's perspective (NA for '08-'14) \
release_pos_z - vertical release position of ball measured in feet from catcher's perspective (NA for '08-'14) \
release_extension - release extension of pitch in feet as tracked by Statcast (NA for '08-'14) \
pfx_x - horizontal movement in feet from the catcher's perspective\
pfx_z - vertical movement in feet from the catcher's perpsective\
vx0 - the velocity of the pitch, in feet per second, in x-dimension, determined at y=50 feet\
vy0 - the velocity of the pitch, in feet per second, in y-dimension, determined at y=50 feet\
vz0 - the velocity of the pitch, in feet per second, in z-dimension, determined at y=50 feet\
ax - the acceleration of the pitch, in feet per second per second, in x-dimension, determined at y=50 feet\
ay - the acceleration of the pitch, in feet per second per second, in y-dimension, determined at y=50 feet\
az - the acceleration of the pitch, in feet per second per second, in z-dimension, determined at y=50 feet
### Runners and Defensive Alignment
run_on_1b – id of runner on first pre-pitch\
run_on_2b – id of runner on second pre-pitch\
run_on_3b – id of runner on third pre-pitch\
hit_location - position of first fielder to touch the ball\
fielderid – mlbam fielder id of first fielder to touch the ball\
fielder_name - name of first fielder to touch the ball\
fielder_2 – mlbam id of catcher (NA for '08-'14)\
fielder_3 – mlbam id of first baseman (NA for '08-'14)\
fielder_4 – mlbam id of second baseman (NA for '08-'14)\
fielder_5 – mlbam id of third baseman (NA for '08-'14)\
fielder_6 – shortstop id (NA for '08-'14)\
fielder_7 – left fielder id (NA for '08-'14)\
fielder_8 – centerfielder id (NA for '08-'14)\
fielder_9 – right fielder id (NA for '08-'14)\
if_fielding_alignment - infield fielding alignmeent at the time of the pitch (NA for '08-'14)\
of_fielding_alignment - outfield fielding alignment at the time of the pitch (NA for '08-'14)
