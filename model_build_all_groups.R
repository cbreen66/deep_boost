source("./racingdata/race_functions.R")

# Model build
train_data = read.csv("./racingdata/train_data_1112.csv") %>%
  mutate(winner = as.factor(winner))

# Read all traindata files
# file = list.files("./racingdata/traindata", full.names=TRUE)
# DT = do.call(rbind, lapply(file, fread))

# Derive race ID
# train_data$race_id = paste0(train_data$race_month, train_data$race_year,
  #                          train_data$track, train_data$dist, train_data$group,
   #                         train_data$fieldsize)

#train_data = mutate(train_data, stamina_index = (sire_index + damsire_index) / dist,
 #                   stays = avdist / dist)

# train_data2 = train_data %>% group_by(id) %>%
#arrange(id, -race_year, -race_month) %>%
#  mutate(temp_lag = lag(finish, n=1)) %>%
#  mutate(consist = sd(temp_lag, na.rm=T)) %>%
#  select(-temp_lag)

# Replace frequencies
#track_calc = train_data %>% group_by(track) %>%
 # summarise(track_freq = n() / nrow(train_data))
#trial_calc = train_data %>% group_by(trial) %>%
 # summarise(trial_freq = n() / nrow(train_data))
#train_calc = train_data %>% group_by(trainer) %>%
#  summarise(train_freq = n() / nrow(train_data))

# Recode ground feature
train_data$ground = fct_recode(train_data$ground, '1' = 'Fm', '1' = 'GF', '2' = 'Gd', '2' = 'GS', '3' = 'Y',
                              '3' = 'Sft', '4' = 'VSft', '4' = 'Hy', '4' = 'H')
#train_data$ground = as.numeric(as.character(train_data$ground))

# Correlation matrix
cor_mat = cor(na.omit(select_if(train_data, sapply(train_data, is.numeric))))
high_cors = c('avfinish', 'group1', 'trax', 'mean_sp', 'rating')
train_data = select_if(train_data, !names(train_data) %in% high_cors)

set.seed(54321)
d_split = createDataPartition(train_data$winner, list=FALSE, p=0.75)
train_set = train_data[d_split, ]
valid_set = train_data[-d_split, ]

h2o.init()
h2o_train = as.h2o(train_set)
h2o_valid = as.h2o(valid_set)

dep = "winner"
indep = names(h2o_train[ ,!(names(h2o_train) %in% c("horse_name", "id",
                                                    "finish", "race_id", "winner",
                                                    "race_year", "trial", "track"
))])

# GBM model
deep_boost = h2o.gbm(x = indep, y=dep, model_id = "deep_boost",
                     training_frame = h2o_train, validation_frame = h2o_valid,
                     sample_rate = 1,
                     seed = 5432154321,
                     col_sample_rate = 0.3,
                     stopping_rounds = 10,
                     stopping_metric = "AUC",
                     stopping_tolerance = 0.001,
                     learn_rate = 0.01,
                     learn_rate_annealing = 0.999,
                     ntrees = 90,
                     max_depth = 11,
                     min_rows = 30,
                     nbins = 10,
                     nfolds = 5
                     )
h2o.auc(h2o.performance(deep_boost, h2o_valid))
h2o_two = as.h2o(y2017)
tale2 = h2o.performance(deep_boost, h2o_two)
h2o.auc(tale2)
imps = deep_boost@model$variable_importances

deep_future = h2o.deeplearning(x = indep, y=dep, model_id = "deep_future",
                          training_frame = h2o_train,
                          validation_frame = h2o_valid,
                          stopping_rounds = 5,
                          stopping_metric = "AUC",
                          stopping_tolerance = 0.01,
                          activation = 'RectifierWithDropout',
                          hidden = 40,
                          epochs = 18
                          )
h2o.auc(h2o.performance(deep_future, h2o_valid)) 

library(xgboost)
dtrain = xgb.DMatrix(data.matrix(select(train_set, -winner, -horse_name, -id, -finish)),
                                 label = train_set$winner)
dval = xgb.DMatrix(data.matrix(select(valid_set, -winner, -horse_name, -id, -finish)),
                               label = valid_set$winner)

param = list(max_depth = 6, eta = 0.2)
watchlist = list(train = dtrain, eval = dval)
xgb_boom = xgb.train(params = param,
                     objective = "binary:logistic",
                     data = dtrain,
                     watchlist,
                     gamma = 0.1,
                     eval_metric = "auc",
                     nrounds = 100,
                     early_stopping_rounds = 10)
test1 = read.csv("./racingdata/traindata/train_data_arc.csv", sep = ";") %>% 
  select(names(train_set))
preds = predict(xgb_boom, test1)

