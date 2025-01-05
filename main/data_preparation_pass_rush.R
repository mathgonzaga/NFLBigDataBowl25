  library(data.table)
  library(ggplot2)
  library(readr)
  library(magrittr)
  library(lest)
  library(forcats)
  library(arrow)
  plays <- as.data.table(read_csv('data/plays.csv'))
  player_play <- as.data.table(read_csv('data/player_play.csv'))
  tracking <- read_parquet('data/tracking.parquet')
  players <- as.data.table(read_csv('data/players.csv'))
  games <- as.data.table(read_csv('data/games.csv'))
  
  
  

  
  target <- player_play[, c('gameId', 'playId', 'nflId', 'wasInitialPassRusher')]
  
  
  plays[, 
        yards_to_endzone := case_when(
          possessionTeam == yardlineSide ~ 100 - yardlineNumber,
          TRUE ~ yardlineNumber )]
  
  plays_aux <- plays[, c('gameId', 'playId', 'down', 'yardsToGo', 
                         'possessionTeam', 'defensiveTeam', 'yards_to_endzone',
                         'expectedPoints','isDropback', 'pff_passCoverage')]
  
  df <- merge(tracking, target, by = c('gameId', 'playId', 'nflId'))
  
  df <- merge(df, plays_aux, by = c('gameId', 'playId'))
  
  
  df <- df[(isDropback == TRUE) & (frameType == 'BEFORE_SNAP') & (club == defensiveTeam)]
  
  
  df[, lineset := ifelse((event != 'line_set') | (is.na(event)), 0, 1)]
  
  df <- df[order(gameId, playId, nflId, frameId)]
  
  df[, after_lineset := cumsum(lineset), by = c('gameId', 'playId', 'nflId') ]
  
  df <- df[after_lineset >= 1]
  
  
  df[, coverage := case_when(
    pff_passCoverage %like% 'Cover-3' ~ 'Cover-3',
    pff_passCoverage %like% 'Cover-1' ~ 'Cover-1',
    pff_passCoverage %like% 'Cover-0' ~ 'Cover-0',
    pff_passCoverage %like% 'Cover-2' ~ 'Cover-2',
    pff_passCoverage %like% '6' ~ 'Cover-6',
    pff_passCoverage %like% 'Cover-2' ~ 'Cover-2',
    pff_passCoverage %like% '2-Man' ~ '2-Man',
    pff_passCoverage %like% 'Quarters' ~ 'Quarters',
    pff_passCoverage %like% 'Red Zone' ~ 'Red Zone',
    TRUE ~ 'Other'
  )]
  # ------------------ Dados de Personnel -------------
  
  
  positions <- players[, c('nflId', 'position')]
  
  df <- merge(df, positions, by = 'nflId')
  
  
  df[, position_group := case_when(
    position %in% c('C', 'G', 'T') ~ 'OL',
    position %in% c('RB') ~ 'RB',
    position %in% c('FB') ~ 'FB',
    position %in% c('TE') ~ 'TE',
    position %in% c('QB') ~ 'QB',
    position %in% c('WR') ~ 'WR',
    position %in% c('DT','NT') ~ 'IDL',
    position %in% c('DE') ~ 'DE',
    position %in% c('OLB') ~ 'OLB',
    position %in% c('ILB','MLB', 'LB') ~ 'ILB',
    position %in% c('CB') ~ 'CB',
    position %in% c('FS','SS','DB') ~ 'S')
  ]
  
  
  positions[, position_group := case_when(
    position %in% c('C', 'G', 'T') ~ 'OL',
    position %in% c('RB') ~ 'RB',
    position %in% c('FB') ~ 'FB',
    position %in% c('TE') ~ 'TE',
    position %in% c('QB') ~ 'QB',
    position %in% c('WR') ~ 'WR',
    position %in% c('DT','NT') ~ 'IDL',
    position %in% c('DE') ~ 'DE',
    position %in% c('OLB') ~ 'OLB',
    position %in% c('ILB','MLB', 'LB') ~ 'ILB',
    position %in% c('CB') ~ 'CB',
    position %in% c('FS','SS', 'DB') ~ 'S')
  ]
  
  player_play <- merge(player_play, positions, by = 'nflId')
  personnel <- player_play[
    ,.(count = .N),
    by = c('gameId', 'playId','position_group')
  ] %>%
    na.omit() %>% 
    dcast(gameId + playId ~ position_group,fill = 0 )
  
  personnel[, DB := CB + S]
  personnel[, DL := IDL + DE]
  personnel[, DL_OLB := IDL + DE + OLB]
  personnel[, LB := ILB + OLB]
  
  df <- merge(df, personnel, by = c('gameId', 'playId'))
  
  # ------------------ Consolidação de tracking -------------
  
  
  ball_snap <- tracking[(club == 'football')] 
  ball_snap[, x_ball := x]
  ball_snap[, y_ball := y]
  ball_tracking <- ball_snap[, c('playId', 'gameId','frameId', 'x_ball', 'y_ball')]
  
  df <- merge(df, ball_tracking, by = c('playId','gameId','frameId') )
  
  df[, x := abs(x - x_ball)]
  df[, y := ifelse(playDirection == 'right', (y - y_ball), (y_ball - y))]
  df[, distance_to_ball := sqrt(x ^ 2 + y ^ 2)]
  df[, o := ifelse(playDirection == 'right', 360 - o, o)]
  df[, dir := ifelse(playDirection == 'right', 360 - dir, dir)]
  
  # ------------------- Informacao da Semana ----------------
  
  
  df <- df[order(gameId, playId, nflId)]
  
  write_parquet(df, 'data/base_data.parquet')
  
