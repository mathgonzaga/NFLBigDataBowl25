library(data.table)
library(ggplot2)
library(readr)
library(magrittr)

plays <- as.data.table(read_csv('data/plays.csv'))

plays <- plays[, c('gameId', 'playId', 'playDescription', 'yardsToGo', 'down',
          'possessionTeam', 'defensiveTeam', 'absoluteYardlineNumber',
          'offenseFormation', 'receiverAlignment','passResult', 'passLength',
          'playAction', 'dropbackType','passLocationType', 'timeToThrow',
          'yardsGained','expectedPointsAdded', 'pff_runConceptPrimary',
          'pff_runConceptSecondary','pff_runPassOption', 'pff_passCoverage',
          'pff_manZone', 'rushLocationType')]


player_play <- as.data.table(read_csv('data/player_play.csv'))

plays_aux <- player_play[,
            .(rushers = sum(wasInitialPassRusher,na.rm = T),
              pressure = sum(causedPressure, na.rm = T),
              mean_getoff = mean(getOffTimeAsPassRusher, na.rm = T),
              min_getoff = min(getOffTimeAsPassRusher, na.rm = T),
              shift = sum(shiftSinceLineset, na.rm = T),
              motion = sum(motionSinceLineset, na.rm = T)
              )
            ,
            by = c('gameId', 'playId')]

routes <- player_play[,
            .(count = .N),
            by = c('gameId', 'playId','routeRan')
            ] %>%
  na.omit %>% 
  dcast(gameId + playId ~ routeRan,fill = 0 )


  
  
plays <- merge(plays, plays_aux, by = c('gameId', 'playId'))
plays <- merge(plays, routes, by = c('gameId', 'playId'), all.x = T)

plays[, playtype := ifelse(is.na(dropbackType), 'run', 'dropback')]



plays[,.(epa = mean(expectedPointsAdded), n = .N),by = 'pff_passCoverage'] %>% View()

plays[playtype == 'dropback',.(epa = mean(expectedPointsAdded), n = .N),by = 'pff_passCoverage'] %>% View()
plays[playtype == 'run',.(epa = mean(expectedPointsAdded), n = .N),by = 'pff_passCoverage'] %>% View()

plays[playtype == 'dropback',.(epa = mean(expectedPointsAdded), n = .N),by = 'rushers'] %>% View()

plays[playtype == 'dropback',.(epa = mean(expectedPointsAdded), n = .N),by = 'motion'] %>% View()

plays[playtype == 'dropback',.(epa = mean(expectedPointsAdded), n = .N),by = 'shift'] %>% View()

plays[playtype == 'dropback',.(epa = mean(expectedPointsAdded), n = .N),by = 'receiverAlignment'] %>% View()


plays[,.(epa = mean(expectedPointsAdded), n = .N),by = 'WHEEL'] %>% View()

df <- read_csv('data/tracking_week_1.csv')
