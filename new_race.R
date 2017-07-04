# Generate predictions and training data for new race
# 1 Lay only model
# 2 3 places maximum
# Coolmore excluded
source("./racingdata/race_functions.R")


# Get and parse new data
new_data = read.csv("./racingdata/total_data_polly.csv", sep=";", stringsAsFactors = F,
                    strip.white = T, na.strings = c("NA", "-")) %>%
            rename(rating = OR) %>%
            mutate(WGT = gsub(" p| h| 1", "", WGT))
new_data = parse_race_data(new_data)
names(new_data) = tolower(names(new_data))
new_data = new_data %>% select(-x, -x.1) %>%
  mutate(date = dmy(date))

# Race_day_data
race_day_data = read.csv("./racingdata/race_day_polly.csv", sep=";") %>%
  mutate(date = dmy(date))

new_hist = new_data %>% group_by(id) %>%
  summarise(avfinish = mean(finish, na.rm=T),
            avdist = mean(dist),
            cont1 = sum(group == "G1"), cont2 = sum(group == "G2"),
            cont3 = sum(group == "G3"),
            group1 = sum(group == "G1" & finish == 1),
            group2 = sum(group == "G2" & finish == 1),
            group3 = sum(group == "G3" & finish == 1),
            win1600 = sum(dist == 8 & finish == 1),
            win2000 = sum(dist == 10 & finish == 1),
            win2400 = sum(dist == 12 & finish == 1),
            run2400 = sum(dist == 12),
            run2000 = sum(dist == 10),
            run1600 = sum(dist == 8),
            total_race = length(finish),
            total_win = sum(finish == 1),
            win_percent = sum(finish == 1) / length(finish),
            form1 = nth(finish, 1),
            form2 = nth(finish, 2),
            form3 = nth(finish, 3),
            form4 = nth(finish, 4),
            high_or = max(rating, na.rm = T),
            groundwin = sum(ground == race_day_data$ground[[1]] & finish %in% c(1,2,3)),
            year_race = sum(year(date) == year(race_day_data$date[[1]])),
            trax = length(unique(track)),
            trial = nth(track, 1),
            grounds = length(unique(ground)),
            mean_sp = mean(fn),
            consist = sd(finish),
            days_since = race_day_data$date[[1]] - nth(date, 1)
  )


# Full historical data
predict_data = merge(new_hist, race_day_data, by = "id") %>%
  mutate(days_since = as.numeric(days_since),
         high_or = as.numeric(high_or))

# Derived features
predict_data = predict_data %>%
  make_racelevel() %>%
  mutate(ratingminusweight = (rating - weight)) %>%
         make_derived()
write.csv(predict_data, file = paste0("./racingdata/traindata/train_data_",
                                      format(Sys.time(), "%Y%m%d"), ".csv"),
          row.names=FALSE)
          
# PRedict using H2O
h2o.init()
deep_boost = h2o.loadModel("./racingdata/boost_model/deep_boost")
library(assertthat); assert_that(setdiff(deep_boost@model$variable_importances$variable,
                                         names(predict_data)) == 0)

h2o_arca = as.h2o(predict_data)
preds1 = h2o.predict(deep_boost, h2o_arca)
preds_frame = as.data.frame(h2o.cbind(h2o_arca, preds1)) %>%
  select(id, horse_name, p1) %>% 
  mutate(odds = 1 / p1) %>%
  arrange(-p1)
write.csv(preds_frame, file = paste0("./racingdata/predictions/predictions_",
                                      format(Sys.time(), "%Y%m%d"), ".csv"),
          row.names=F)






