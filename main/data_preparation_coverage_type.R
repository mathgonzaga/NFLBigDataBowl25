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

# -------------- COVERAGE TYPE -----------------


# -------------- Plays Preparation -----------------

plays <- plays[!is.na(offenseFormation), c(
  'gameId', 'playId', 'playDescription', 'yardsToGo', 'down',  'quarter', 'possessionTeam', 
  'defensiveTeam', 'yardlineSide', 'yardlineNumber', 'expectedPoints',
  'isDropback','pff_passCoverage')]


plays[, 
      yards_to_endzone := case_when(
        possessionTeam == yardlineSide ~ 100 - yardlineNumber,
        TRUE ~ yardlineNumber
      )]


plays <- plays[isDropback == TRUE]
plays[, coverage := case_when(
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



positions <- players[, c('nflId', 'position')]

player_play <- merge(player_play, positions, by = 'nflId')

player_play[, position_group := case_when(
  position %in% c('C', 'G', 'T') ~ 'OL',
  position %in% c('RB') ~ 'RB',
  position %in% c('FB') ~ 'FB',
  position %in% c('TE') ~ 'TE',
  position %in% c('QB') ~ 'QB',
  position %in% c('WR') ~ 'WR',
  position %in% c('DT','NT') ~ 'IDL',
  position %in% c('DE') ~ 'DE',
  position %in% c('OLB') ~ 'OLB',
  position %in% c('ILB','MLB') ~ 'ILB',
  position %in% c('CB') ~ 'CB',
  position %in% c('FS','SS') ~ 'S')
]

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

personnel <- personnel[, c('gameId', 'playId', 'DB', 'DL', 'DL_OLB', 'LB')]


tracking <- merge(tracking, plays, by = c('playId', 'gameId'))



ball_snap <- tracking[(club == 'football')] 


tracking <- tracking[(frameType == 'BEFORE_SNAP') & (club == defensiveTeam)]

ball_snap[, x_ball := x]
ball_snap[, y_ball := y]
ball_tracking <- ball_snap[, c('playId', 'gameId', 'frameId', 'x_ball', 'y_ball')]


tracking <- merge(tracking, ball_tracking, c('playId', 'gameId', 'frameId'))

tracking[, lineset := ifelse((event != 'line_set') | (is.na(event)), 0, 1)]

tracking <- tracking[order(gameId, playId, nflId, frameId)]

tracking[, after_lineset := cumsum(lineset), by = c('gameId', 'playId', 'nflId') ]

tracking <- tracking[after_lineset >= 1]



tracking[, distance_to_ball := sqrt(((x - x_ball)) ^ 2 + ((y - y_ball) ^ 2))]
def_tracking <- tracking[,.(
  defense_width = max(y) - min(y),
  defense_length = max(x) - min(x),
  defense_spreadness_y = sd(y),
  defense_spreadness_x = sd(x),
  defense_spreadness = sd(distance_to_ball),
  yards_to_endzone = min(yards_to_endzone)
)
, by = c('playId', 'gameId', 'frameId')]

def_tracking[, relative_defense_length :=  defense_length/yards_to_endzone]
def_tracking[, relative_defense_spreadness_x :=  defense_spreadness_x/yards_to_endzone]
def_tracking[, relative_defense_spreadness :=  defense_spreadness/yards_to_endzone]



def_snap <- merge(tracking, players[, c('nflId', 'position')], by = 'nflId')

db <- def_snap[position %in% c('CB', 'FS', 'SS'),]
cb <- def_snap[position %in% c('CB'),]
s <- def_snap[position %in% c('FS', 'SS'),]

db <- db[,.(
  db_deepness = mean(abs(x - x_ball)),
  relative_db_deepness = mean(abs(x - x_ball))/mean(yards_to_endzone)
)
, by = c('playId', 'gameId', 'frameId')]

cb <- cb[,.(
  cb_deepness = mean(abs(x - x_ball)),
  relative_cb_deepness = mean(abs(x - x_ball))/mean(yards_to_endzone),
  cb_width = max(y) - min(y)
),  by = c('playId', 'gameId', 'frameId')]

s <- s[,.(
  s_deepness = mean(abs(y - y_ball)),
  closest_safety_distance_x = min(abs(x - x_ball)),
  closest_safety_distance = min(distance_to_ball),
  far_safety_distance_x = max(abs(x - x_ball)),
  far_safety_distance = max(distance_to_ball),
  s_deepness_relative = mean(abs(x - x_ball))/mean(yards_to_endzone),
  closest_safety_distance_x_relative = min(abs(x - x_ball))/mean(yards_to_endzone),
  closest_safety_distance_relative = min(distance_to_ball)/mean(yards_to_endzone),
  far_safety_distance_x_relative = max(abs(x - x_ball))/mean(yards_to_endzone),
  far_safety_distance_relative = max(distance_to_ball)/mean(yards_to_endzone)
), by = c('playId', 'gameId', 'frameId')]

def_tracking <- merge(def_tracking, db, all.x = T) %>% merge(s, all.x = T) %>% merge(cb, all.x = T)

def_tracking[, yards_to_endzone := NULL ]


df <- plays %>% merge(personnel, by = c('gameId', 'playId')) %>% 
  merge(def_tracking, by = c('gameId', 'playId'))

write_parquet(df, 'data/coverage.parquet')


