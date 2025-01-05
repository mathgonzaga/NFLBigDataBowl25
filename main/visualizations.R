library(data.table)
library(ggplot2)
library(readr)
library(magrittr)
library(lest)
library(forcats)
library(arrow)
library(tidymodels)
library(dplyr)
library(lightgbm)
library(bonsai)
library(ingredients)
library(gganimate)
library(sportyR)
df_pr <- setDT(read_parquet('data/scored_pass_rush_data.parquet'))
df_c <- setDT(read_parquet('data/scored_coverage_data.parquet'))

plays <- as.data.table(read_csv('data/plays.csv'))
tracking <- read_parquet('data/tracking.parquet')
tracking <- setDT(tracking)


# Team Scores ###################






epa <- plays[, c('gameId', 'playId','expectedPointsAdded')]

pr <- df_pr[, .(pred = last(pred), 
          wasInitialPassRusher = last(wasInitialPassRusher)),
      by = c('playId', 'gameId', 'nflId', 'displayName', 'club')]

#pr[, err := wasInitialPassRusher - pred]
#pr[, .(mean(err)), by = 'displayName'] %>% View()


pr <- df_pr[, .(
  pr_previsibility = sd(pred), 
  pr_error = mean(abs(wasInitialPassRusher  - pred))
  ),
            by = c('playId', 'gameId')]



c <-  df_c[, .(pred_class = last(.pred_class), 
                coverage = last(coverage)),
            by = c('playId', 'gameId', 'defensiveTeam')]


c2 <- melt(df_c[, c('playId', 'gameId', 'defensiveTeam', ".pred_2-Man",
              ".pred_Cover-0", ".pred_Cover-1",                  
              ".pred_Cover-2", ".pred_Cover-3",                    
              ".pred_Cover-6", ".pred_Other",                       
              ".pred_Quarters", ".pred_Red Zone")],
     id.vars = c('playId', 'gameId', 'defensiveTeam'))

c2 <- c2[, .(cov_previsibility = max(value)), by = c('playId', 'gameId', 'defensiveTeam')]


c <- merge(c, c2)

c[, correct_coverage := ifelse(pred_class == coverage, 1, 0)]

scores <- merge(c, pr, by = c('playId', 'gameId'))

scores <- merge(scores, epa, by = c('playId', 'gameId'))


tab <- scores[,.(cov_prev = mean(cov_previsibility),
          pr_prev = mean(pr_previsibility),
          epa = mean(expectedPointsAdded) ), by = 'defensiveTeam'] 


tab[, cov_imprev := 1 - (cov_prev)]
tab[, pr_imprev := 1 - (pr_prev)]


tab[, cov_imprev_p := 1 - rank(cov_prev)/32]
tab[, pr_imprev_p := 1 - rank(pr_prev)/32]
tab[, def_imprev_p := (cov_imprev + pr_imprev)/2]

ggplot(tab, aes(x = (pr_imprev), y = (cov_imprev))) + 
  geom_hline(aes(yintercept = mean(cov_imprev))) +
  geom_vline(aes(xintercept = mean(pr_imprev))) +
  nflplotR::geom_nfl_logos(aes(team_abbr = defensiveTeam), height = 0.07) +
  ggthemes::theme_hc() +
  scale_x_continuous(name = 'Pass Rush Imprevisibility', labels = scales::percent) +
  scale_y_continuous(name = 'Coverage Imprevisibility', labels = scales::percent) +
  ggtitle('Pass Rush and Coverage Imprevisibility')

ggsave('plots/imprevisibility_metric.png', height = 8, width = 8, dpi = 100)


tab[, def_imprev := scale(cov_imprev) + scale(pr_imprev)]


ggplot(tab, aes(x = def_imprev, y = epa)) + 
  geom_hline(aes(yintercept = mean(epa))) +
  geom_vline(aes(xintercept = mean(def_imprev))) +
  geom_smooth(method = 'lm', alpha = 0) + 
  nflplotR::geom_nfl_logos(aes(team_abbr = defensiveTeam), height = 0.07) +
  ggthemes::theme_hc() +
  scale_x_continuous(name = 'Defensive Imprevisibility Score', labels = scales::percent) +
  scale_y_continuous(name = 'EPA per Play',transform = 'reverse') +
  ggtitle('Defensive Imprevisibility and EPA per play') 
ggsave('plots/cov_epa_metric.png', height = 8, width = 8, dpi = 100)







#  Individual prediction ######################

df_pr <- setDT(read_parquet('data/scored_pass_rush_data.parquet'))

game <- '2022092503'
play <- '3114'


player <- df_pr[gameId == game  & playId %in% play & displayName == 'Damar Hamlin']
train <- readRDS('data/train.rds')
Xtrain <- copy(train)
Xtrain[, wasInitialPassRusher := NULL]
recipe <- readRDS('data/recipe.rds')
model_pr <- readRDS('data/model_pass_rush.rds')

player2 <- bake(recipe, player)


pred_nn <- function(model, newdata){
  predict(model, newdata, type = 'prob')$.pred_1
}


explainer <- DALEX::explain(model_pr, Xtrain, as.numeric(train$wasInitialPassRusher) - 1, 
                            predict_function = pred_nn)



mpart <- DALEX::variable_attribution(explainer, player2[1,])
plot(mpart)


mpart <- DALEX::variable_attribution(explainer, player2[68,])
plot(mpart)




df_c <- setDT(read_parquet('data/scored_coverage_data.parquet'))

game <- '2022092503'
play <- '3114'


cover <- df_c[gameId == game  & playId %in% play ]

train <- readRDS('data/train_coverage.rds')
Xtrain <- copy(train)
Xtrain[, coverage := NULL]

recipe <- readRDS('data/recipe_coverage.rds')
model_cov <- readRDS('data/model_coverage.rds')


cover2 <- bake(recipe, cover)



explainer <- DALEX::explain(model_cov, Xtrain, train$coverage)



mpart <- DALEX::variable_attribution(explainer, cover2[1,], )
mpart <- mpart %>% filter(label %like% 'Cover-1')
 plot(mpart)

mpart <- DALEX::variable_attribution(explainer, cover2[68,], )
mpart <- mpart %>% filter(label %like% 'Cover-1')
plot(mpart)



# Play Viz ##########################






tracking[, lineset := ifelse((event != 'line_set') | (is.na(event)), 0, 1)]

tracking <- tracking[order(gameId, playId, nflId, frameId)]

tracking[, after_lineset := cumsum(lineset), by = c('gameId', 'playId', 'nflId') ]

tracking <- tracking[after_lineset >= 1]



nfl_field <- geom_football("nfl", x_trans = 60, y_trans = 26.6667, xlims = c(50,110),
                           display_range = 'in_bounds_only')

nfl_field
 
#

df_pr <- df_pr[, c('playId', 'gameId', 'frameId', 'nflId', 'pred', 'wasInitialPassRusher')]

df <- merge(tracking, df_pr, c('playId', 'gameId', 'frameId', 'nflId'), all.x = T)
 

game <- '2022092503'
play <- '3114'


plays <- as.data.table(read_csv('data/plays.csv'))

description <- plays[gameId == game  & playId %in% play]$playDescription
off_team <- plays[gameId == game  & playId %in% play]$possessionTeam
def_team <- plays[gameId == game  & playId %in% play]$defensiveTeam
ex <- df[gameId == game  & playId %in% play]



frames_ex <- uniqueN(ex$frameId)
ex[, color := lest::case_when(
  club == 'football' ~ 'brown',
  club == 'BUF' ~ 'blue3',
  club == 'MIA' ~ 'white')]

play_anim <- nfl_field +
  geom_point(
    data = ex,
    aes(x, y),
    color = ex$color,
    size = 7
  ) +
   geom_label(data = ex, aes(x = x, y = y, label = round(pred, 2)),alpha = 0.3, size = 7 ) +
   ggtitle(paste0('Off Team: ', off_team, '   -   ', 'Def Team: ', def_team), 
           description) + 
  transition_states(states = frameId, state_length = 20) 


play_anim


exc <- df_c[gameId == game  & playId %in% play]
true <- unique(exc$coverage)
exc <- exc[, c('frameId', ".pred_2-Man", ".pred_Cover-0", 
        ".pred_Cover-1", ".pred_Cover-2", ".pred_Cover-3",                     
        ".pred_Cover-6", ".pred_Other", ".pred_Quarters",
        ".pred_Red Zone")]

aux <- unique(ex[, c('frameId')])

exc <- merge(aux, exc, by = 'frameId', all.x = T)


for(col in names(exc)[2:ncol(exc)]){
  exc[, (col) := ifelse(is.na(get(col)), last(get(col), na_rm = T), get(col))]
}


exc <- melt(exc, id.vars = 'frameId')




exc[, variable:= stringr::str_remove_all(variable, '.pred_')]




cov <- ggplot() + geom_col(data = exc, aes( x = value, y = variable), fill = 'navy') +
  scale_x_continuous(limits = c(0,1), labels = scales::percent, name = 'Probability') + 
  ylab('Coverage Scheme') + 
  ggtitle('Coverage Scheme Probability', paste0('True Coverage: ', true )) + 
  transition_states(states = frameId, state_length = 1) + ggthemes::theme_hc() 



play_anim2 <- animate(play_anim, renderer = magick_renderer(), width = 800, height = 800)
cov2 <- animate(cov, renderer = magick_renderer(), height = 800, width = 400)



i=1
new_gif <- magick::image_append(c(play_anim2[i], cov2[i]))

new_gif

for(i in 2:100){
  combined <- magick::image_append(c(play_anim2[i], cov2[i]))
  new_gif <- c(new_gif, combined)
}


magick::image_write(new_gif, format="gif", path="plots/bills_dolphins.gif")
