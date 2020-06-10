# Jacob Richey
# June 2020
# Compiling historical MLB play-by-play data from Baseball Savant

### Statcast Documentation ----------------------------------------------------
# # Plate Appearance Information ----
# gameid – mlbam gameid
# game_year – year
# game_date – date of the game
# game_type - type of game (e.g. Regular Season, World Series, etc.)
# home_team – home team
# vis_team – visiting team
# pa_number – game pa number
# pitch_number - total pitch number of the plate appearance
# inning
# is_bottom
# batterid – mlbam batter id
# batter_name
# is_lhb – is left handed batter
# pitcherid – mlbam pitcher id
# pitcher_name
# is_lhp – is left handed pitcher
# bat_score_before – score of batting team before plate appearance event
# bat_score_after – score of batting team after plate appearance event
# field_score – field team score during batting event
# baseout_state_before – basecode and outs before plate appearance event (e.g. 010 1 = runner on 2nd, one out)
# baseout_state_after – basecode and outs after plate appearance event (e.g. 001 2 = runner on 3rd, two outs)
# event_type - resulting event of plate appearance
# event_description - plate appearance description from game day
# pitch_description - description of resulting pitch
# # Pitch Details ----
# balls – balls when pitch was thrown
# strikes – strikes when pitch was thrown
# is_last_pitch - was it the last pitch in the plate appearance
# is_bip - was the ball put in play
# is_stk - was the pitch a strike
# pitch_type – the type of pitch derived from Statcast
# CH - changeup
# CU - curveball
# EP - eephus
# FA - fastball
# FC - fastball (cutter)
# FF - fastball (4-seam)
# FO - forkball
# FS - fastball (split finger)
# FT - fastball (2-seam)
# IN - intentional ball
# KC - knuckle curve
# KN - knuckleball
# PO - pitchout
# SC - screwball
# SI - sinker
# SL - slider
# UN - unknown
# attack_region - zone location of the ball when it crosses the plate from the catcher's perspective (http://tangotiger.net/strikezone/zone%20chart.png)
# plate_x – horizontal position of the ball in feet when it crosses home plate from the catcher's perspective
# plate_z – vertical position of the ball in feet when it crosses home plate from the catcher's perspective
# sz_top - top of the batter's strike zone in feet set by the operator when the ball is halfway to the plate
# sz_bot - bottom of the batter's strike zone in feet set by the operator when the ball is halfway to the plate
# rel_plate_x - zone center at 0, with 1 denoting the strike zone perimeter
# rel_plate_z - zone center at 0, with 1 denoting the strike zone perimeter (dependent on batter height)
# # Batting Details ----
# bb_type - batted ball type (ground_ball, line_drive, fly_ball, popup)
# estimated_ba_using_speedangle - estimated batting avg based on launch angle and exit velocity (NA for '08-'14)
# estimated_woba_using_speedangle - estimated wOBA based on launch angle and exit velocity (NA for '08-'14)
# woba_value - wOBA value based on result of play
# woba_denom - wOBA denominator based on result of play (NA for '08-'14)
# babip_value - BABIP value based on result of play
# iso_value - ISO value based on result of play
# launch_speed – exit velocity in mph of batted ball as tracked by Statcast (NA for '08-'14)
# launch_angle – launch angle in mph of batted ball as tracked by Statcast (NA for '08-'14)
# launch_speed_angle - launch speed/angle zone based on launch angle and exit velocity (1 = weak, 2 = topped, 3 = under, 4 = flare/burner, 5 = solid contact, 6 = barrel) [NA for '08-]
# hc_x - hit coordiante X of batted ball
# hc_y - hit coordiante Y of batted ball
# hit_distance – projected hit distance of the batted ball in feet (NA for '08-'14)
# spray_angle – spray angle in degrees
# cdrv_24 - context-dependent run value, based on RE24 (http://tangotiger.com/index.php/site/article/statcast-lab-swing-take-and-a-primer-on-run-value)
# cnrv_24 - context-neutral run value, based on RE24
# cdrv_288 - context-dependent run value, based on RE288
# cnrv_288 - context-neutral run value, based on RE288
# # Pitching Details ----
# release_speed – release speed of pitch in mph
# effective_speed - derived speed based on the extension of the pitcher's release (NA for '08-'14)
# release_spin – spin rate of pitch in rpm as tracked by Statcast (NA for '08-'14)
# release_pos_x – horizontal release position of ball measured in feet from catcher's perspective (NA for '08-'14)
# release_pos_y - release position of pitch measured in feet from catcher's perspective (NA for '08-'14)
# release_pos_z - vertical release position of ball measured in feet from catcher's perspective (NA for '08-'14)
# release_extension - release extension of pitch in feet as tracked by Statcast (NA for '08-'14)
# pfx_x - horizontal movement in feet from the catcher's perspective
# pfx_z - vertical movement in feet from the catcher's perpsective
# vx0 - the velocity of the pitch, in feet per second, in x-dimension, determined at y=50 feet
# vy0 - the velocity of the pitch, in feet per second, in y-dimension, determined at y=50 feet
# vz0 - the velocity of the pitch, in feet per second, in z-dimension, determined at y=50 feet
# ax - the acceleration of the pitch, in feet per second per second, in x-dimension, determined at y=50 feet
# ay - the acceleration of the pitch, in feet per second per second, in y-dimension, determined at y=50 feet
# az - the acceleration of the pitch, in feet per second per second, in z-dimension, determined at y=50 feet
# # Runners and Defensive Alignment ----
# run_on_1b – id of runner on first pre-pitch
# run_on_2b – id of runner on second pre-pitch
# run_on_3b – id of runner on third pre-pitch
# hit_location - position of first fielder to touch the ball
# fielderid – mlbam fielder id of first fielder to touch the ball
# fielder_name - name of first fielder to touch the ball
# fielder_2 – mlbam id of catcher (NA for '08-'14)
# fielder_3 – mlbam id of first baseman (NA for '08-'14)
# fielder_4 – mlbam id of second baseman (NA for '08-'14)
# fielder_5 – mlbam id of third baseman (NA for '08-'14)
# fielder_6 – shortstop id (NA for '08-'14)
# fielder_7 – left fielder id (NA for '08-'14)
# fielder_8 – centerfielder id (NA for '08-'14)
# fielder_9 – right fielder id (NA for '08-'14)
# if_fielding_alignment - infield fielding alignmeent at the time of the pitch (NA for '08-'14)
# of_fielding_alignment - outfield fielding alignment at the time of the pitch (NA for '08-'14)

### Scrape and Format ----
library(baseballr)
library(tidyverse)

setwd("~/data_personal/statcast_database")
# from http://github.com/chadwickbureau/register
register <- read_csv("chadwick_register.csv") %>%
  filter(!is.na(key_mlbam)) %>%
  mutate(name = paste(name_first, name_last))

# scrape online statcast data from Baseball Savant for specified season
# Credit to Bill Petti for statcast scraping functions (baseballr)
scrape_statcast <- function(season) {
  
  # create weeks of dates for season from mar - nov
  # includes spring training + postseason
  dates <- seq.Date(as.Date(paste0(season, '-03-01')),
                    as.Date(paste0(season, '-12-01')), by = 'week')
  
  date_grid <- tibble(start_date = dates, 
                      end_date = dates + 6)
  
  # create 'safe' version of scrape_statcast_savant in case week doesn't process
  safe_savant <- safely(scrape_statcast_savant)
  
  # loop over each row of date_grid, and collect each week in a df
  payload <- map(.x = seq_along(date_grid$start_date), 
                 ~{message(paste0('\nScraping week of ', date_grid$start_date[.x], '...\n'))
                   
                   payload <- safe_savant(start_date = date_grid$start_date[.x], 
                                          end_date = date_grid$end_date[.x], type = 'pitcher')
                   
                   return(payload)
                 })
  
  payload_df <- map(payload, 'result')
  
  # eliminate results with an empty dataframe
  number_rows <- map_df(.x = seq_along(payload_df), 
                        ~{number_rows <- tibble(week = .x, 
                                                number_rows = length(payload_df[[.x]]$game_date))}) %>%
    filter(number_rows > 0) %>%
    pull(week)
  
  payload_df_reduced <- payload_df[number_rows]
  
  combined <- payload_df_reduced %>%
    bind_rows()
  
  return(combined)
}

format_statcast <- function(df) {
  df <- df %>%
    mutate(pitch_type = ifelse(pitch_type == "", "UN", pitch_type),
           game_type = case_when(
             game_type == "E" ~ "Exhibition",
             game_type == "S" ~ "Spring Training",
             game_type == "R" ~ "Regular Season",
             game_type == "F" ~ "Wild Card",
             game_type == "D" ~ "Divisional Series",
             game_type == "L" ~ "League Championship Series",
             game_type == "W" ~ "World Series"),
           is_lhb = ifelse(stand == "L", "1", "0"),
           is_lhp = ifelse(p_throws == "L", "1", "0"),
           fielderid = case_when(
             hit_location == "1" ~ as.character(pitcher),
             hit_location == "2" ~ as.character(fielder_2),
             hit_location == "3" ~ as.character(fielder_3),
             hit_location == "4" ~ as.character(fielder_4),
             hit_location == "5" ~ as.character(fielder_5),
             hit_location == "6" ~ as.character(fielder_6),
             hit_location == "7" ~ as.character(fielder_7),
             hit_location == "8" ~ as.character(fielder_8),
             hit_location == "9" ~ as.character(fielder_9)),
           fielderid = as.numeric(fielderid),
           is_bottom = ifelse(inning_topbot == "Bot", "1", "0"),
           spray_angle = round(atan((hc_x-125.42)/(198.27-hc_y))*180/pi*.75, 1),
           home_team = case_when(
             home_team == "FLA" ~ "MIA",
             home_team == "KC" ~ "KCR",
             home_team == "SD" ~ "SDP",
             home_team == "SF" ~ "SFG",
             home_team == "TB" ~ "TBR",
             TRUE ~ home_team),
           away_team = case_when(
             away_team == "FLA" ~ "MIA",
             away_team == "KC" ~ "KCR",
             away_team == "SD" ~ "SDP",
             away_team == "SF" ~ "SFG",
             away_team == "TB" ~ "TBR",
             TRUE ~ away_team),
           run_on_1b = ifelse(on_1b == "0", NA, on_1b),
           run_on_2b = ifelse(on_2b == "0", NA, on_2b),
           run_on_3b = ifelse(on_3b == "0", NA, on_3b),
           is_bip = ifelse(type == "X", 1, 0),
           is_stk = ifelse(type == "S", 1, 0),
           basecode_before = case_when(
             is.na(run_on_1b) & is.na(run_on_2b) & is.na(run_on_3b) ~ "000",
             is.na(run_on_1b) & is.na(run_on_2b) ~ "001",
             is.na(run_on_1b) & is.na(run_on_3b) ~ "010",
             is.na(run_on_2b) & is.na(run_on_3b) ~ "100",
             is.na(run_on_3b) ~ "110",
             is.na(run_on_2b) ~ "101",
             is.na(run_on_1b) ~ "011",
             TRUE ~ "111")) %>%
    rename(vis_team = away_team,
           batterid = batter,
           pitcherid = pitcher,
           event_type = events,
           event_description = des,
           pitch_description = description,
           outs_before = outs_when_up,
           hit_distance = hit_distance_sc,
           pa_number = at_bat_number,
           bat_score_before = bat_score,
           gameid = game_pk,
           field_score = fld_score,
           release_spin = release_spin_rate) %>%
    arrange(game_date, gameid, pa_number, pitch_number)
  
  df <- df %>%
    group_by(gameid, pa_number, batterid) %>%
    mutate(pitch_number = row_number(),
           is_last_pitch = ifelse(row_number() == n(), 1, 0),
           balls = pmin(pitch_number - cumsum(lag(is_stk, default = 0)) - 1, 3),
           strikes = pmin(cumsum(lag(is_stk, default = 0)), 2)) %>%
    group_by(gameid, inning, is_bottom) %>%
    mutate(outs_after = ifelse(row_number() == n() & 
                                 inning == 9 & is_bottom == 1 & 
                                 str_detect(pitch_description, "score"),
                               outs_before,
                               lead(outs_before, default = 3)),
           basecode_after = ifelse(row_number() == n() & 
                                     inning == 9 & is_bottom == 1 & 
                                     str_detect(pitch_description, "score"),
                                   basecode_before,
                                   lead(basecode_before, default = "000")),
           baseout_state_before = paste(basecode_before, outs_before),
           baseout_state_after = paste(basecode_after, outs_after)) %>%
    group_by(gameid) %>%
    mutate(pa_number = cumsum(lag(is_last_pitch, default = 0)) + 1,
           bat_score_after = 
             ifelse(row_number() == n(),
                    ifelse(is_bottom == 1, 
                           ifelse(str_detect(pitch_description, "score"), 
                                  away_score + 1, home_score), away_score),
                    ifelse(is_bottom == 1, lead(home_score), 
                           lead(away_score)))) %>%
    ungroup() %>%
    left_join(select(register, key_mlbam, name), 
              by = c("batterid" = "key_mlbam")) %>%
    rename(batter_name = name) %>%
    left_join(select(register, key_mlbam, name), 
              by = c("pitcherid" = "key_mlbam")) %>%
    rename(pitcher_name = name) %>%
    left_join(select(register, key_mlbam, name), 
              by = c("fielderid" = "key_mlbam")) %>%
    rename(fielder_name = name) %>%
    arrange(game_date, gameid, pa_number, pitch_number)
  
  re24 <- df %>%
    group_by(game_date, gameid, inning, is_bottom) %>%
    mutate(runs_roi = max(bat_score_after) - bat_score_before) %>%
    ungroup() %>%
    group_by(baseout_state_before) %>%
    summarise(mean_roi = mean(runs_roi)) %>%
    rename(state = baseout_state_before) %>%
    rbind(c("000 3", 0)) %>%
    mutate(mean_roi = as.numeric(mean_roi))
  
  re288 <- df %>%
    group_by(game_date, gameid, inning, is_bottom) %>%
    mutate(runs_roi = max(bat_score_after) - bat_score_before,
           full_state_before = paste0(baseout_state_before, " ", 
                                      balls, "-", strikes)) %>%
    ungroup() %>%
    group_by(full_state_before) %>%
    summarise(mean_roi = mean(runs_roi)) %>%
    rename(state = full_state_before) %>%
    rbind(c("000 3 0-0", 0)) %>%
    mutate(mean_roi = as.numeric(mean_roi))
  
  df <- df %>%
    mutate(full_state_before = paste0(baseout_state_before, " ", balls, "-", 
                                      strikes),
           full_state_after = case_when(
             is_last_pitch == 1 ~ paste0(baseout_state_after, " 0-0"),
             strikes == 2 & is_stk == 1 ~ paste0(baseout_state_before, " ", 
                                                 balls, "-", strikes),
             is_stk == 1 ~ paste0(baseout_state_before, " ", balls, "-", 
                                  strikes + 1),
             is_stk == 0 ~ paste0(baseout_state_before, " ", balls + 1, "-", 
                                  strikes),
             TRUE ~ "ERROR")) %>%
    left_join(re24, by = c("baseout_state_before" = "state"), keep = TRUE) %>%
    rename(re24_before = mean_roi) %>%
    left_join(re24, by = c("baseout_state_after" = "state"), keep = TRUE) %>%
    rename(re24_after = mean_roi) %>%
    mutate(re24_before = ifelse(is_last_pitch == 1, re24_before, NA),
           re24_after = ifelse(is_last_pitch == 1, re24_after, NA),
           cdrv_24 = ifelse(is_last_pitch == 1, 
                            bat_score_after - bat_score_before + 
                              re24_after - re24_before, NA)) %>%
    group_by(event_type) %>%
    mutate(cnrv_24 = mean(cdrv_24)) %>%
    left_join(re288, by = c("full_state_before" = "state"), keep = TRUE) %>%
    rename(re288_before = mean_roi) %>%
    left_join(re288, by = c("full_state_after" = "state"), keep = TRUE) %>%
    rename(re288_after = mean_roi) %>%
    mutate(cn_event = ifelse(is_last_pitch == 1, paste0(balls, "-", strikes, 
                                                        " ", event_type),
                             ifelse(is_stk == 1, paste0(balls, "-", strikes, 
                                                        " strike"),
                                    paste0(balls, "-", strikes, " ball"))),
           cdrv_288 = ifelse(is_last_pitch == 1, 
                             bat_score_after - bat_score_before + 
                               re288_after - re288_before, 
                             re288_after - re288_before)) %>%
    group_by(cn_event) %>%
    mutate(cnrv_288 = mean(cdrv_288)) %>%
    ungroup()
  
  df <- df %>%
    mutate(rel_plate_x = plate_x / ((17/2 + 1.456) / 12),
           rel_plate_z = (plate_z - (sz_bot - 1.456/12)) /
             (sz_top - sz_bot + 1.456*2/12) * 2 - 1,
           attack_region = case_when(
             abs(rel_plate_x) < 0.67 & abs(rel_plate_z) < 0.67 ~ "Heart",
             abs(rel_plate_x) < 1.33 & abs(rel_plate_z) < 1.33 ~ "Shadow",
             abs(rel_plate_x) < 2.00 & abs(rel_plate_z) < 2.00 ~ "Chase",
             TRUE ~ "Waste"),
           attack_region = ifelse(is.na(plate_x) | is.na(plate_z), NA, attack_region)) %>%
    select(gameid, game_year, game_date, game_type, home_team, vis_team, 
           pa_number, pitch_number, inning, is_bottom, batterid, batter_name, 
           is_lhb, pitcherid, pitcher_name, is_lhp, bat_score_before, 
           bat_score_after, field_score, baseout_state_before, 
           baseout_state_after, event_type, event_description, 
           pitch_description, balls, strikes, is_last_pitch, is_bip, is_stk, 
           pitch_type, attack_region, plate_x, plate_z, sz_top, sz_bot, 
           rel_plate_x, rel_plate_z, bb_type, estimated_ba_using_speedangle, 
           estimated_woba_using_speedangle, woba_value, woba_denom, 
           babip_value, iso_value, launch_speed, launch_angle,
           launch_speed_angle, hc_x, hc_y, hit_distance, spray_angle, cdrv_24, 
           cnrv_24, cdrv_288, cnrv_288, release_speed, effective_speed, 
           release_extension, pfx_x, pfx_z, vx0, vy0, vz0, ax, ay, az, 
           hit_location, run_on_1b, run_on_2b, run_on_3b, fielderid, 
           fielder_name, fielder_2, fielder_3, fielder_4, fielder_5, fielder_6, 
           fielder_7, fielder_8, fielder_9, if_fielding_alignment, 
           of_fielding_alignment)
  
  return(df)
}

# 2019 example
payload_statcast <- scrape_statcast(2019)
statcast_2019 <- format_statcast(payload_statcast)
write.csv(statcast_2019, "statcast_2019.csv", row.names = FALSE)
         