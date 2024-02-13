# Team-seasons
### Offense ####
agg_off <- pbp %>%
  group_by(season, posteam) %>%
  summarise(pass_epa_play = mean(pass_epa , na.rm = T) ,
            rush_epa_play = mean(rush_epa , na.rm = T) ,
            epa_play = mean(epa , na.rm = T) ,
  ) %>% filter(!is.na(posteam))
### Defense ####
agg_def <- pbp %>%
  group_by(season , defteam ) %>%
  summarise(def_pass_epa = mean(epa[play_type_nfl == "PASS"] , na.rm = T)*-1 ,
            def_rush_epa = mean(epa[play_type_nfl == "RUSH"] , na.rm = T)*-1
  ) %>% filter(!is.na(defteam))
### Combine into team_agg base ####
epa_season <-  left_join(agg_off, agg_def , join_by(season,  posteam == defteam))


# Play-level epa view
pbp %>% select(season, week , home_team , away_team , posteam , epa , desc) %>% View()
