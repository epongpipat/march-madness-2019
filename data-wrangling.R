# functions to manipulate data
# metric data manipulation ----
df_regular_season <- read_csv("data/RegularSeasonDetailedResults.csv")
# add TeaamID and TeamName dataset
df_teams <- read_csv("data/Teams.csv")

# select only the TeamID and TeamName vars
df_teams <- df_teams %>% 
  select(TeamID, TeamName) 

# create df for win and loss TeamID and TeamNames to append to regular season df
df_wteams <- df_teams                         
colnames(df_wteams) <- c("WTeamID", "WTeamName")
df_lteams <- df_teams                        
colnames(df_lteams) <- c("LTeamID", "LTeamName")

# add actual names using inner_join
# remove TeamID
df_regular_season <- df_regular_season %>%
  inner_join(., df_wteams, by = "WTeamID") %>%
  inner_join(., df_lteams, by = "LTeamID") %>%
  select(-contains("TeamID"))

metric_data_manipulation <- function(data) {
  data %>%
    select(season = Season,
           n_day = DayNum,
           team_name = TeamName,
           score = Score,
           points_made = FGM,
           points_attempted = FGA,
           points_3_made = FGM3,
           points_3_attempted = FGA3,
           points_free_throws_made = FTM,
           points_free_throws_attempted = FTA,
           rebound_offensive = OR,
           rebound_defensive = DR,
           assist = Ast,
           turnovers = TO,
           steals = Stl,
           blocks = Blk,
           personal_fouls = PF) %>%
    mutate(index = 1:nrow(data),
           points_2_made = points_made - points_3_made,
           points_2_attempted = points_attempted - points_3_attempted,
           points_missed = points_attempted - points_made,
           points_2_missed = points_2_attempted - points_2_made,
           points_3_missed = points_3_attempted - points_3_made,
           points_free_throws_missed = points_free_throws_attempted - points_free_throws_made,
           points_made_percentage = points_made / points_attempted * 100,
           points_2_made_percentage = points_2_made / points_2_attempted * 100,
           points_3_made_percentage = points_3_made / points_3_attempted * 100,
           points_free_throws_made_percentage = points_free_throws_made / points_free_throws_attempted * 100)
}

# separate wins and losses
df_regular_season_win <- df_regular_season %>%
  select(Season, DayNum, contains("W"))
colnames(df_regular_season_win) <- gsub("W", "", colnames(df_regular_season_win))

df_regular_season_loss <- df_regular_season %>%
  select(Season, DayNum, contains("L"))
colnames(df_regular_season_loss) <- gsub("L", "", colnames(df_regular_season_loss))

# include outcome
df_regular_season_win <- metric_data_manipulation(df_regular_season_win) %>% 
  mutate(outcome = "win")

df_regular_season_loss <- metric_data_manipulation(df_regular_season_loss) %>% 
  mutate(outcome = "loss")

df_win_num <- df_regular_season_win %>%
  select(-c(season, n_day, team_name, outcome, index))

df_loss_num <- df_regular_season_loss %>%
  select(-c(season, n_day, team_name, outcome, index))

df_reg_season_diff <- df_win_num-df_loss_num

df_reg_season_diff <- df_reg_season_diff %>%
  mutate(season = df_regular_season_win$season,
         n_day = df_regular_season_win$n_day,
         team_name = df_regular_season_win$team_name,
         index = df_regular_season_win$index)
glimpse(df_reg_season_diff)

# combine together using bind_rows 
df_regular_season_all <- bind_rows(df_regular_season_win, df_regular_season_loss) %>% 
  mutate(outcome = as_factor(outcome))

glimpse(df_regular_season_all)


df_regular_season_all_total <- df_regular_season_all %>%
  select(-n_day, -index) %>%
  rowwise() %>%
  mutate(n_win = ifelse(outcome == "win", 1, 0)) %>%
  ungroup() %>%
  select(-outcome) %>%
  group_by(season, team_name) %>%
  summarise_all(sum) %>%
  ungroup()
glimpse(df_regular_season_all_total)

df_regular_season_all_mean <- df_regular_season_all %>%
  select(-n_day, -index, -outcome) %>%
  group_by(season, team_name) %>%
  summarise_all(mean) %>%
  ungroup()
glimpse(df_regular_season_all_mean)

df_reg_season_diff_total <- df_reg_season_diff %>%
  select(-c(index, n_day)) %>%
  group_by(season, team_name) %>%
  summarise_all(sum)
glimpse(df_reg_season_diff_total)

df_reg_season_diff_mean <- df_reg_season_diff %>%
  select(-c(index, n_day)) %>%
  group_by(season, team_name) %>%
  summarise_all(mean)
glimpse(df_reg_season_diff_mean)