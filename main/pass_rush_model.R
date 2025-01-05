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
library(gt)

df <- read_parquet('data/base_data.parquet')

df <- df[!is.na(wasInitialPassRusher)]
df <- df[position != 'WR']
df[, yardage_group := case_when(
  yardsToGo <= 3 ~ 'Short',
  yardsToGo <= 7 ~ 'Medium',
  yardsToGo > 7 ~ 'Long'
  )]

df[, field_position := case_when(
  yards_to_endzone >= 90 ~ 'Own 10',
  yards_to_endzone >= 50 ~ 'Own Field',
  yards_to_endzone > 20 ~ 'Opp Field',
  yards_to_endzone <= 20  ~ 'Redzone'
)]

aux <- unique(df[, c('playId', 'gameId', 'nflId','wasInitialPassRusher', 'club', 'position_group', 
              'down', 'yardage_group', 'field_position')])

aux[, avg_rushers := 11* cummean(wasInitialPassRusher), by = c('club')]
aux[, position_rushers_pct := cummean(wasInitialPassRusher), by = c('club', 'position_group')]
aux[, avg_context_rushers := 11* cummean(wasInitialPassRusher), by = c('club', 'down', 'yardage_group', 'field_position')]
aux[, context_position_rushers_pct := cummean(wasInitialPassRusher), by = c('club', 'down', 'yardage_group', 'field_position', 'position_group')]
aux[, avg_player_rushing_pct := cummean(wasInitialPassRusher), by = c('nflId')]


aux[, avg_rushers := shift(avg_rushers, type = 'lag'), by = c('club')]
aux[, position_rushers_pct := shift(position_rushers_pct, type = 'lag'), by = c('club', 'position_group')]
aux[, avg_context_rushers := shift(avg_context_rushers, type = 'lag'), by = c('club', 'down', 'yardage_group', 'field_position')]
aux[, context_position_rushers_pct := shift(context_position_rushers_pct, type = 'lag'), by = c('club', 'down', 'yardage_group', 'field_position', 'position_group')]
aux[, avg_player_rushing_pct := shift(avg_player_rushing_pct, type = 'lag'), by = c('club', 'nflId')]

df <- merge(df, aux, by = c(
  'playId', 'gameId', 'nflId','wasInitialPassRusher', 'club', 'position_group',
  'down', 'yardage_group', 'field_position'))

df_base <- df[, c(
  'playId',
  'gameId',
  'frameId',
  'nflId',
  'displayName',
  'club',
  'x',
  'y',
  's',
  'a',
  'dis',
  'o',
  'dir',
  'down',
  'yardsToGo',
  'yards_to_endzone',
  'position_group',
  'distance_to_ball',
  'yardage_group',
  'field_position',
  'avg_rushers',
  'position_rushers_pct',
  'avg_context_rushers',
  'context_position_rushers_pct',
  'avg_player_rushing_pct',
  'wasInitialPassRusher'
)]

key_cols <- c('playId', 'gameId', 'frameId', 'nflId', 'displayName', 'club')
cols_modeling <- setdiff(names(df_base), key_cols)
df_base <- df_base[order(gameId, playId, nflId)]

games <- unique(df_base$gameId)


set.seed(1212)
train_games <- sample(games, size = 0.7 * length(games))
test_games <- setdiff(games, train_games)

df_train <- df_base[gameId %in% train_games]
df_test <- df_base[gameId %in% test_games]

dfm_train <- df_train[, ..cols_modeling]
dfm_test <- df_test[, ..cols_modeling]

recipe <- recipe(wasInitialPassRusher ~ ., data = dfm_train) %>%
  step_dummy(all_nominal_predictors()) %>% 
  step_impute_mean(all_numeric_predictors()) %>% 
  step_normalize(all_predictors()) %>% 
  prep()

saveRDS(recipe, 'data/recipe.rds')
train <- setDT(juice(recipe))
train[, wasInitialPassRusher := factor(wasInitialPassRusher)]
test <- setDT(bake(recipe, dfm_test))
test[, wasInitialPassRusher := factor(wasInitialPassRusher)]

vif <- car::vif(glm(formula = wasInitialPassRusher ~ ., data = train, family = 'binomial'))


train_cols <- names(train)[vif < 10]


write_rds(train, 'data/train.rds')


model <- mlp(epochs = 1000, hidden_units = 10, penalty = 0.01, learn_rate = 0.1) %>% 
  set_engine("brulee", validation = 0) %>% 
  set_mode("classification")

model <- fit(model, formula = wasInitialPassRusher ~ ., data = train )
saveRDS(model,'data/model_pass_rush.rds')


predictions <- predict(model, test, type = 'prob')$.pred_1

df_test[, pred := predictions]

mean(abs(df_test$pred - (as.numeric(df_test$wasInitialPassRusher) - 1)))

df_test[, binary_pred := factor(ifelse(pred  >= 0.5, 1, 0))]
df_test[, wasInitialPassRusher := factor(wasInitialPassRusher)]

prec <- precision(df_test, truth = wasInitialPassRusher, estimate = (binary_pred))

rec <- recall(df_test, truth = wasInitialPassRusher, estimate = (binary_pred))

acc <- accuracy(df_test, truth = wasInitialPassRusher, estimate = (binary_pred))

roc_auc(df_test,
        pred, truth = wasInitialPassRusher, event_level = 'second')

df_test[, numeric_pr := (as.numeric(df_test$wasInitialPassRusher) - 1)]


metrics <- c()
position <- c()
for(pg in unique(df_test$position_group)){
aux <- roc_auc(df_test[position_group == pg],
               pred, truth = wasInitialPassRusher, event_level = 'second')
metrics <- c(metrics, aux$.estimate)
position <- c(position, pg)
}



roc_auc <- tibble(`Position Group` = unique(df_test$position_group),
                      `ROC AUC` = metrics) %>% arrange( -`ROC AUC`)

gt(roc_auc) %>% gt::fmt_percent('ROC AUC') %>% gt::tab_header('Pass Rusher Prediction Model ROC AUC by position') %>% 
gtsave('plots/roc_auc_pass_rush.png')


pred_nn <- function(model, newdata){
  predict(model, newdata, type = 'prob')$.pred_1
}


vi <- vip::vi_permute(model, train = train, target = 'wasInitialPassRusher', metric = 'roc_auc',
                pred_wrapper = pred_nn, event_level = 'second', sample_size = 10000)
vip::vip(object =vi , num_features = 10 )



Xtrain <- copy(train)
Xtrain[, wasInitialPassRusher := NULL]
explainer <- DALEX::explain(model, Xtrain, as.numeric(train$wasInitialPassRusher) - 1, 
               predict_function = pred_nn)


vimp <- DALEX::variable_importance(explainer)
plot(vimp)

mp <- DALEX::model_profile(explainer,variables = c('avg_player_rushing_pct',
                                                   'x',
                                                   'distance_to_ball',
                                                   'position_group_IDL',
                                                   'position_rushers_pct',
                                                   'position_group_S',
                                                   'context_position_rushers_pct',
                                                   'position_group_DE',
                                                   'dir'
                                                   ))
plot(mp)


# mpart <- DALEX::variable_attribution(explainer, train[1,])
# plot(mpart)

df_test[, target := as.numeric(wasInitialPassRusher) - 1]



ggplot(df_test, aes(x = pred, y = target)) + 
  geom_smooth(method = 'gam') + geom_abline(aes(intercept = 0, slope = 1)) +
  xlab('Model Defined Pass Rusher Probability') + ylab('Actual Pass Rusher Probability') +
  ggtitle('Calibration - Pass Rusher Probability Model') + ggthemes::theme_hc()

ggsave('plots/pass_rush_probability_calibration.png', height = 12, width = 12)

#ggplot(df_test,  aes(x = pred)) + geom_density()


ggplot(df_test,  aes(x = pred)) + geom_histogram() + 
  facet_wrap(~position_group)

ggplot(df_test, aes(x = pred, y = target)) + 
  geom_smooth(method = 'gam',se = T) + geom_abline(aes(intercept = 0, slope = 1))+ 
  facet_wrap(~position_group) +
  xlab('Model Defined Pass Rusher Probability') + 
  ylab('Actual Pass Rusher Probability') +
  ggtitle('Calibration - Pass Rusher Probability Model') + 
  ggthemes::theme_hc()

ggsave('plots/pass_rush_probability_calibration_by_position.png', height = 12, width = 12)



base <- setDT(bake(recipe, df_base))

predictions <- predict(model, base, type = 'prob')$.pred_1

df_base[, pred := predictions]


write_parquet(df_base, 'data/scored_pass_rush_data.parquet')
