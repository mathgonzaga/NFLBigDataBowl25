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
library(patchwork)
library(gt)
df <- read_parquet('data/coverage.parquet')

df <- df[!is.na(coverage)]
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

aux <- unique(df[, c('playId', 'gameId',  'defensiveTeam',
                     'down', 'yardage_group', 'coverage')])

aux[, helper := 1]

aux = dcast(aux, playId + gameId  + defensiveTeam + down + yardage_group ~ coverage, fill = 0)

aux[, cover_2_man_rate := cummean(`2-Man`), by = c('defensiveTeam', 'down', 'yardage_group')]
aux[, cover_0_rate := cummean(`Cover-0`), by = c('defensiveTeam', 'down', 'yardage_group')]
aux[, cover_1_rate := cummean(`Cover-1`), by = c('defensiveTeam', 'down', 'yardage_group')]
aux[, cover_2_rate := cummean(`Cover-2`), by = c('defensiveTeam', 'down', 'yardage_group')]
aux[, cover_3_rate := cummean(`Cover-3`), by = c('defensiveTeam', 'down', 'yardage_group')]
aux[, cover_6_rate := cummean(`Cover-6`), by = c('defensiveTeam', 'down', 'yardage_group')]
aux[, other_rate := cummean(`Other`), by = c('defensiveTeam', 'down', 'yardage_group')]
aux[, quarters_rate := cummean(`Quarters`), by = c('defensiveTeam', 'down', 'yardage_group')]
aux[, redzone_rate := cummean(`Red Zone`), by = c('defensiveTeam', 'down', 'yardage_group')]

aux2 <- aux[, c('playId', 'gameId', 'defensiveTeam', 'down', 'yardage_group',
                'cover_0_rate', 'cover_1_rate', 'cover_2_rate', 'cover_3_rate',
                'cover_6_rate', 'other_rate', 'quarters_rate', 'redzone_rate')]

aux[, cover_2_man_rate := shift(cover_2_man_rate, type = 'lag'), by = c('defensiveTeam', 'down', 'yardage_group')]
aux[, cover_0_rate := shift(cover_0_rate, type = 'lag'), by = c('defensiveTeam', 'down', 'yardage_group')]
aux[, cover_1_rate := shift(cover_1_rate, type = 'lag'), by = c('defensiveTeam', 'down', 'yardage_group')]
aux[, cover_2_rate := shift(cover_2_rate, type = 'lag'), by = c('defensiveTeam', 'down', 'yardage_group')]
aux[, cover_3_rate := shift(cover_3_rate, type = 'lag'), by = c('defensiveTeam', 'down', 'yardage_group')]
aux[, cover_6_rate := shift( cover_6_rate, type = 'lag'), by = c('defensiveTeam', 'down', 'yardage_group')]
aux[, other_rate := shift(other_rate, type = 'lag'), by = c('defensiveTeam', 'down', 'yardage_group')]
aux[, quarters_rate := shift(quarters_rate, type = 'lag'), by = c('defensiveTeam', 'down', 'yardage_group')]
aux[, redzone_rate := shift(redzone_rate, type = 'lag'), by = c('defensiveTeam', 'down', 'yardage_group')]

df <- merge(df, aux2, by = c(
  'playId', 'gameId', 'defensiveTeam','down', 'yardage_group' ))

df_base <- df[, c(
  'playId',
  'gameId',
  'frameId',
  'defensiveTeam',
  'coverage',
  "DB",
  "DL",                               
  "DL_OLB",
  "LB",                               
  "defense_width",                     
  "defense_length",                   
  "defense_spreadness_y",              
  "defense_spreadness_x",
  "defense_spreadness",             
  "relative_defense_length",
  "relative_defense_spreadness_x",   
  "relative_defense_spreadness",
  "db_deepness",                     
  "relative_db_deepness",
  "s_deepness",                      
  "closest_safety_distance_x",
  "closest_safety_distance",          
  "far_safety_distance_x",
  "far_safety_distance",               
  "s_deepness_relative",
  "closest_safety_distance_x_relative",
  "closest_safety_distance_relative",
  "far_safety_distance_x_relative",    
  "far_safety_distance_relative",
  "cb_deepness",                       
  "relative_cb_deepness",
  "cb_width",                      
  "field_position",
  "cover_0_rate",                      
  "cover_1_rate",
  "cover_2_rate",                      
  "cover_3_rate",
  "cover_6_rate",                      
  "other_rate",
  "quarters_rate",                  
  "redzone_rate"                      
)]

key_cols <- c('playId', 'gameId', 'frameId','defensiveTeam')
cols_modeling <- setdiff(names(df_base), key_cols)
df_base <- df_base[order(gameId, playId, frameId)]

games <- unique(sort(df_base$gameId))

set.seed(363116)
train_games <- sample(games, size = 0.7 * length(games))
test_games <- setdiff(games, train_games)
df_train <- df_base[gameId %in% train_games]
df_test <- df_base[gameId %in% test_games]

dfm_train <- df_train[, ..cols_modeling]
dfm_test <- df_test[, ..cols_modeling]

recipe <- recipe(coverage ~ ., data = dfm_train) %>%
  step_dummy(all_nominal_predictors()) %>% 
  step_impute_mean(all_numeric_predictors()) %>% 
  step_normalize(all_predictors()) %>% 
  prep()

saveRDS(recipe, 'data/recipe_coverage.rds')

train <- setDT(juice(recipe))
train[, coverage := factor(coverage)]
test <- setDT(bake(recipe, dfm_test))
test[, coverage := factor(coverage)]


saveRDS(train,'data/train_coverage.rds')


model <- mlp(epochs = 1000, hidden_units = 10, penalty = 0.01, learn_rate = 0.1) %>% 
  set_engine("brulee", validation = 0) %>% 
  set_mode("classification")

model <- fit(model, formula = coverage ~ ., data = train )
saveRDS(model, 'data/model_coverage.rds')



predictions <- predict(model, test, type = 'prob')

df_test <- cbind(df_test, predictions)

df_test[, class_pred := predict(model, test, type = 'class')]

mean(df_test$class_pred == df_test$coverage)

df_test[, acerto := ifelse(class_pred == coverage, 1, 0)]

df_test[, .(mean(acerto), .N), by = 'coverage']

df_test[, .(mean(acerto), .N), by = 'class_pred']


probs_df <- df_test[,
  c(
    "playId",
    "gameId",
    "frameId",
  'coverage',
  ".pred_Cover-0",
  ".pred_Cover-1",                    
  ".pred_Cover-2",
  ".pred_Cover-3",
  ".pred_Cover-6",
  ".pred_Other",
  ".pred_Quarters",
  ".pred_Red Zone",
  ".pred_2-Man"
  )]


probs_df[, id := 1]
probs_df <- dcast(probs_df,  `.pred_Cover-0`+  `.pred_Cover-1`+  `.pred_Cover-2`+  `.pred_Cover-3`+  `.pred_Cover-6` +
        `.pred_Other`+`.pred_Quarters` + `.pred_Red Zone` + `.pred_2-Man` + playId + gameId + frameId ~ coverage, fill = 0)


g1 <- ggplot(probs_df, aes(x = `.pred_Cover-0`, y = `Cover-0`)) + 
  geom_smooth(method = 'gam') + geom_abline(aes(intercept = 0, slope = 1)) + 
  xlab('Model Defined Probability of Coverage')+
  ylab('Actual Probability of Coverage') +
  ggtitle('Cover-0') +
  ggthemes::theme_hc()

g2 <- ggplot(probs_df, aes(x = `.pred_Cover-1`, y = `Cover-1`)) + 
  geom_smooth(method = 'gam') + geom_abline(aes(intercept = 0, slope = 1)) +
  xlab('Model Defined Probability of Coverage')+
  ylab('Actual Probability of Coverage') +
  ggtitle('Cover-1') +
  ggthemes::theme_hc()

g3 <- ggplot(probs_df, aes(x = `.pred_Cover-2`, y = `Cover-2`)) + 
  geom_smooth(method = 'gam') + geom_abline(aes(intercept = 0, slope = 1)) + 
  xlab('Model Defined Probability of Coverage')+
  ylab('Actual Probability of Coverage') +
  ggtitle('Cover-2') +
  ggthemes::theme_hc()


g4 <- ggplot(probs_df, aes(x = `.pred_Cover-3`, y = `Cover-3`)) + 
  geom_smooth(method = 'gam') + geom_abline(aes(intercept = 0, slope = 1)) +
  xlab('Model Defined Probability of Coverage')+
  ylab('Actual Probability of Coverage') +
  ggtitle('Cover-3') +
  ggthemes::theme_hc()

g5 <- ggplot(probs_df, aes(x = `.pred_Cover-6`, y = `Cover-6`)) + 
  geom_smooth(method = 'gam') + geom_abline(aes(intercept = 0, slope = 1)) +
  xlab('Model Defined Probability of Coverage')+
  ylab('Actual Probability of Coverage') +
  ggtitle('Cover-6') +
  ggthemes::theme_hc()

g6 <- ggplot(probs_df, aes(x = `.pred_Other`, y = `Other`)) + 
  geom_smooth(method = 'gam') + geom_abline(aes(intercept = 0, slope = 1)) +
  xlab('Model Defined Probability of Coverage')+
  ylab('Actual Probability of Coverage') +
  ggtitle('Other') +
  ggthemes::theme_hc()

g7 <- ggplot(probs_df, aes(x = `.pred_Quarters`, y = `Quarters`)) + 
  geom_smooth(method = 'gam') + geom_abline(aes(intercept = 0, slope = 1)) +
  xlab('Model Defined Probability of Coverage')+
  ylab('Actual Probability of Coverage') +
  ggtitle('Quarters') +
  ggthemes::theme_hc()

g8 <- ggplot(probs_df, aes(x = `.pred_Red Zone`, y = `Red Zone`)) + 
  geom_smooth(method = 'gam') + geom_abline(aes(intercept = 0, slope = 1)) +
  xlab('Model Defined Probability of Coverage')+
  ylab('Actual Probability of Coverage') +
  ggtitle('Redzone') +
  ggthemes::theme_hc()

g9 <- ggplot(probs_df, aes(x = `.pred_2-Man`, y = `2-Man`)) + 
  geom_smooth(method = 'gam') + geom_abline(aes(intercept = 0, slope = 1)) +
  xlab('Model Defined Probability of Coverage')+
  ylab('Actual Probability of Coverage') +
  ggtitle('2-Man') +
  ggthemes::theme_hc()

(g1 | g2 |g3 ) / (g4 | g5 | g6) / (g7 | g8 | g9) 

ggsave('plots/calibration_by_coverage.png', height = 12, width = 12)
predictions_list <- melt(predictions)
ggplot(predictions_list,  aes(x = value)) + geom_density() + facet_wrap(~variable, scales = 'free_y') +
  xlab('prediction') + ylab('frequency') + ggthemes::theme_hc()

ggsave('plots/prediction_distribution_coverage.png', height = 12, width =12)





df_test[, right_prediction_probability := lest::case_when(
  coverage == 'Cover-0' ~ `.pred_Cover-0`,
  coverage == 'Cover-1' ~ `.pred_Cover-1`,
  coverage == 'Cover-2' ~ `.pred_Cover-2`,
  coverage == 'Cover-3' ~ `.pred_Cover-3`,
  coverage == 'Cover-6' ~ `.pred_Cover-6`,
  coverage == 'Other' ~ `.pred_Other`,
  coverage == 'Quarters' ~ `.pred_Quarters`,
  coverage == 'Red Zone' ~ `.pred_Red Zone`
)]

df_test[, error := 1 - right_prediction_probability]

df_test[, .(mean(error, na.rm = T)), by = 'coverage']
pred_acc <- df_test[, .(mean(error, na.rm = T)), by = 'class_pred']

mean(pred_acc$V1)

pred_metrics <- tibble(`Predicted Class` = pred_acc$class_pred,
                  `Error` = pred_acc$V1) %>% arrange( -`Error`)

gt(pred_metrics) %>% gt::fmt_percent('Error') %>% 
  gt::tab_header('Coverage Model Precision by error') %>% 
  gtsave('plots/tab_precision_coverage.png')


df_test[, .(mean(error, na.rm = T)), by = 'defensiveTeam'] %>% View()




Xtrain <- copy(train)
Xtrain[, coverage := NULL]
explainer <- DALEX::explain(model, Xtrain, train$coverage)


vimp <- DALEX::variable_importance(explainer)


plot(vimp)
vimp

       
mp <- DALEX::model_profile(explainer,
                           variables = c(
                             "cb_deepness",
                             "cover_2_rate",
                             "cover_3_rate",                     
                             "db_deepness",
                             "cover_1_rate",                      
                            "defense_length",
                            "quarters_rate",                     
                            "cover_6_rate", 
                            "s_deepness",                        
                             "cover_0_rate",
                            "cb_width",
                            "far_safety_distance_x",
                            "other_rate",
                            "closest_safety_distance"           
                           ))
plot(mp)


base <- setDT(bake(recipe, df_base))


predictions <- predict(model, base, type = 'prob')

predictions_class <- predict(model, base, type = 'class')

df_base <- cbind(df_base, predictions)
df_base <- cbind(df_base, predictions_class)


write_parquet(df_base, 'data/scored_coverage_data.parquet')
# Visualizacao de jogadas especificas