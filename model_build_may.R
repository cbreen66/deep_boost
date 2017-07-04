source("./racingdata/race_functions.R")

# Model build
train_data = read.csv("./racingdata/train_data_040717.csv") %>%
  mutate(place = as.factor(place))

# Derive race ID
# train_data$race_id = paste0(train_data$race_month, train_data$race_year,
  #                          train_data$track, train_data$dist, train_data$group,
   #                         train_data$fieldsize)
# t2 = read.csv("./racingdata/traindata/train_data_vase.csv", sep=";")

#train_data$trial = fct_lump(train_data$trial, 20)
# train_data$wcol = ifelse(train_data$finish == 1 | train_data$finish >= 6, 2,1)
#train_data = mutate(train_data, stamina_index = (sire_index + damsire_index) / dist,
 #                   stays = avdist / dist)

#y2017 = filter(train_data, race_year == 2017)
#train_data = filter(train_data, race_year != 2017)

set.seed(54321)
d_split = createDataPartition(train_data$id, list=FALSE, p=0.75)
train_set = train_data[d_split, ]
valid_set = train_data[-d_split, ]

h2o_train = as.h2o(train_set)
h2o_valid = as.h2o(valid_set)

dep = "place"
indep = names(h2o_train[ ,!(names(h2o_train) %in% c("horse_name", "id",
                                                    "finish", "wcol", "place", "race_id"
))])

deep_scream = h2o.deeplearning(x = indep, y = dep, model_id = "deep_scream",
                               training_frame = h2o_train, validation_frame = h2o_valid,
                               nfolds = 7, epochs = 17,
                               hidden = c(40,40),
                               activation = "MaxoutWithDropout",
                               keep_cross_validation_predictions = TRUE,
                               keep_cross_validation_fold_assignment = TRUE,
                               fold_assignment = 'Modulo',
                               rho = 0.999,
                               epsilon = 1e-9,
                               weights_column = "wcol",
                               stopping_rounds = 5,
                               stopping_metric = "AUC",
                               stopping_tolerance = 0.01
                               )
tale1 = h2o.performance(deep_scream, h2o_valid)
h2o.auc(tale1)
plot(deep_scream)
h2o_two = as.h2o(y2017)
tale2 = h2o.performance(deep_scream, h2o_two)
h2o.auc(tale2)

# GBM model
deep_boost = h2o.gbm(x = indep, y=dep, model_id = "deep_boost",
                     training_frame = h2o_train, validation_frame = h2o_valid,
                     keep_cross_validation_predictions = TRUE,
                     keep_cross_validation_fold_assignment = TRUE,
                     fold_assignment = 'Modulo',
                     stopping_rounds = 5,
                     stopping_metric = "AUC",
                     stopping_tolerance = 0.001,
                     learn_rate = 0.02,
                     weights_column = "wcol",
                     ntrees = 80,
                     nfolds = 7
                     )
h2o.auc(h2o.performance(deep_boost, h2o_valid))
h2o_two = as.h2o(y2017)
tale2 = h2o.performance(deep_boost, h2o_two)
h2o.auc(tale2)
imps = deep_boost@model$variable_importances

num_trees = 100
deep_boost_grid = h2o.gbm(x = indep, y=dep, model_id = "deep_boost",
                          training_frame = h2o_train, validation_frame = h2o_valid,
                          stopping_rounds = 5,
                          stopping_metric = "AUC",
                          stopping_tolerance = 0.01,
                          learn_rate = 2 / num_trees,
                          learn_rate_annealing = 0.999,
                          max_depth = 6,
                          min_rows = 50,
                          sample_rate = 0.8,
                          histogram_type = "RoundRobin",
                          weights_column = "wcol",
                          ntrees = num_trees,
                          nfolds = 7
)
h2o.auc(h2o.performance(deep_boost_grid, h2o_valid)) 
