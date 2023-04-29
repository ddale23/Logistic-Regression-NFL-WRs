## This R Script will provide both datasets that will be used in our project## 

## Load in packages that we need
library(tidyverse)
library(nflfastR)
library(nflverse)


## Starting Contingency Table Dataset

# Getting every WR draft pick from 2012 through 2018
season_picks = c(2012:2018)

draft_df = data.frame()

for (season_draft in season_picks){
  data_hold = 
    load_draft_picks(seasons = season_draft) %>%
      filter(position == "WR") %>%
    select(season, round, pick, team, gsis_id, pfr_player_id, pfr_player_name, age)
  draft_df = rbind(draft_df, data_hold)
}

# Edit Column Names of draft_df 
colnames(draft_df) = paste(colnames(draft_df),"draft",sep="_")


# Getting Week 1 Rosters of every NFL WR Room from 2016 through 2022
seasons_rosters = c(2016:2022)

rosters_df = data.frame()

for (i in seasons_rosters){
  data_hold = 
    load_rosters_weekly(seasons = i) %>% 
      filter(week == 1, position == "WR", rookie_year == i - 4, !is.na(draft_club)) %>%
    select(season, full_name, team, draft_club, gsis_id, pfr_id)
  rosters_df = rbind(rosters_df, data_hold)
}

# Edit Column Names of rosters_df 
colnames(rosters_df) = paste(colnames(rosters_df),"roster",sep="_")


## Convert all Team Abbreviations to consistent format, accounting for team movement 

# Clean roster_df team abbreviations
rosters_df = 
  rosters_df %>% 
    mutate(clean_team_enter_fifth = 
             case_when(
               team_roster == "OAK" ~ "LV",
               team_roster == "SD" ~ "LAC",
               team_roster == "SDG" ~ "LAC",
               team_roster == "STL" ~ "LAR",
               team_roster == "LA" ~ "LAR",
               team_roster == "GNB" ~ "GB",
               team_roster == "SFO" ~ "SF",
               team_roster == "NOR" ~ "NO",
               team_roster == "NWE" ~ "NE",
               team_roster == "TAM" ~ "TB",
               TRUE ~ team_roster),
           clean_draft = 
             case_when(
               draft_club_roster  == "OAK" ~ "LV",
               draft_club_roster  == "SD" ~ "LAC",
               draft_club_roster == "SDG" ~ "LAC",
               draft_club_roster  == "STL" ~ "LAR",
               draft_club_roster == "LA" ~ "LAR",
               draft_club_roster  == "GNB" ~ "GB",
               draft_club_roster  == "SFO" ~ "SF",
               draft_club_roster  == "NOR" ~ "NO",
               draft_club_roster  == "NWE" ~ "NE",
               draft_club_roster  == "TAM" ~ "TB",
               TRUE ~ draft_club_roster))

# Clean draft_df team abbreviations
draft_df = 
  draft_df %>% 
  mutate(clean_team_draft = 
           case_when(
             team_draft == "OAK" ~ "LV",
             team_draft == "SD" ~ "LAC",
             team_draft == "SDG" ~ "LAC",
             team_draft == "STL" ~ "LAR",
             team_draft == "LA" ~ "LAR",
             team_draft == "GNB" ~ "GB",
             team_draft == "SFO" ~ "SF",
             team_draft == "NOR" ~ "NO",
             team_draft == "NWE" ~ "NE",
             team_draft == "TAM" ~ "TB",
             TRUE ~ team_draft))


# Left join roster_df to draft_df
joined_master_draft = 
  left_join(draft_df, rosters_df, by = c("gsis_id_draft" = "gsis_id_roster")) %>% 
    select(season_draft, round_draft, pick_draft, clean_team_draft, gsis_id_draft, pfr_player_name_draft, age_draft, season_roster,
           clean_team_enter_fifth) %>%
  mutate(same_team = case_when(clean_team_draft == clean_team_enter_fifth ~ 1, 
                               clean_team_draft != clean_team_enter_fifth~ 0,
                               is.na(clean_team_enter_fifth) ~ 0)) 

# Joined_master_draft complete!



### Next is to create WR Statistics for their first 4 season in the NFL, for the second dataset

# Getting season level stats for each player from 2012 through 2021
pbp_seasons = c(2012:2021)

stat_df = data.frame()

for (i in pbp_seasons){
  pbp_for_stats = load_pbp(seasons = i)
  data_hold = calculate_player_stats(pbp_for_stats, weekly = FALSE)  
  data_hold = data_hold %>%
    mutate(season = i)
  
  stat_df = rbind(stat_df, data_hold)
}


# Getting every Week 1 Roster from 2012 to 2021
stats_rosters = c(2012:2021)

stat_roster_df = data.frame()

for (i in stats_rosters){
  data_hold = 
    load_rosters_weekly(seasons = i) %>% 
    filter(week == 1) %>%
    select(season, full_name, team, gsis_id, pfr_id)
  stat_roster_df = rbind(stat_roster_df, data_hold)
}

# Join together two datasets & then select columns we want
joined_master_team_stats = left_join(stat_roster_df, stat_df, by = c("gsis_id" = "player_id", "season" )) 

joined_master_team_stats =
  joined_master_team_stats %>% 
  select(season, full_name, team, gsis_id, games, receptions, targets, receiving_yards, receiving_tds, receiving_air_yards, 
         receiving_yards_after_catch, target_share, air_yards_share, wopr) 
  
# Join master_draft dataset back to master_team_stats, create a season order column, filter out any players who aren't in master_draft
# then create a logic column to then filter out any players whose stats should not be included in final dataset
joined_master_team_stats =
  left_join(joined_master_team_stats, joined_master_draft, by = c("gsis_id" = "gsis_id_draft")) %>%
  filter(!is.na(season_draft)) %>%
  group_by(gsis_id) %>%
  mutate(season_order = rank(season)) %>%
  ungroup() %>%
  mutate(logic_col = case_when(season >= season_draft & (season < season_roster | is.na(season_roster) & season_order <= 4
                                                         ) ~ 1, TRUE ~ 0 )) %>%
  filter(logic_col == 1 & !is.na(games))

# Summarize dataset for totals from first 4 seasons for each distinct player
final_player_stats = 
  joined_master_team_stats %>% 
    group_by(gsis_id) %>%
    summarize(num_seasons = n_distinct(season), total_receptions = sum(receptions), total_targets = sum(targets),
              total_rec_yards = sum(receiving_yards), total_receiving_tds = sum(receiving_tds), 
              total_receiving_air_yards = sum(receiving_air_yards), total_yac = sum(receiving_yards_after_catch),
              avg_tgt_share = mean(target_share), avg_ay_share = mean(air_yards_share), avg_wopr = mean(wopr)
              )


# Join statistics back to master_draft for complete dataset
completed_stats = left_join(final_player_stats, joined_master_draft, by = c("gsis_id" = "gsis_id_draft"))




  
  






















