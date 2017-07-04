library(h2o)
library(tidyr)
library(forcats)
library(lubridate)
library(stringr)
library(caret)
library(dplyr)

# Functions for webscraping and parsing racing data

scrapescrape <- function(x) {
  
  link <- paste0("https://www.racingpost.com/profile/horse/",x)
  
  Frame1 <- readHTMLTable(link, which=2)
  Frame1[Frame1==" "] <- NA
  Frame1 <- na.omit(Frame1)
  Frame1$id = rep(x, times = nrow(Frame1))
  message(paste0("Completed id = ", x))
  Frame1
}

parse_race_data = function(data) {
  data = separate(data, 'race.conditions', into=c("track", "group",
                                                  "prize"), sep=" ")
  data$dist <- fct_recode(data$distance,
                          '8' = '1m', '8.5' = '1m½f', '9.5' = '1m1½f', '9' = '1m1f',
                          '10' = '1m2f', '5' = '5f', '6' = '6f', '7.5' = '7½f', '7' = '7f',
                          '10.5' = '1m2½f', '11' = '1m3f', '12.5' = '1m4½f', '12' = '1m4f',
                          '14' = '1m6f', '11.5' = '1m3½f', '13.5' = '1m5½f',
                          '14.5' = '1m6½f', '15.5' = '1m7½f', '16' = '2m', '16.5' = '2m½f',
                          '17' = '2m1f', '18' = '2m2f', '20' = '2m4f', '15' = '1m7f',
                          '6.5' = '6½f', '4.5' = '4½f', '13' = '1m5f', '5.5' = '5½f',
                          '18.5' = '2m2½f', '20.5' = '2m4½f')
  data$dist = as.character(data$dist)
  data$dist = as.numeric(data$dist)
  data$group <- substr(data$group, nchar(data$group)-1, nchar(data$group))

  data$SP = gsub("EvensF", "1/1", data$SP)
  data$SP = gsub("F", "", data$SP)
  data$SP = gsub("J", "", data$SP)
  data$SP = gsub("C", "", data$SP)
  
  data$finish = sub("[/].*", "", data$race.outcome)
  data$finish = as.numeric(data$finish)
  
  data$tongue_strap = ifelse(grepl("t", data$WGT), 1,0)
  data$blinkers = ifelse(grepl("b", data$WGT), 1,0)
  data$WGT = gsub("t | b | [ 1]", "", data$WGT)
  
  fracs <- sapply(data$SP, function(x) eval(parse(text=x)))
  fracs = as.numeric(unlist(fracs))
  data$fn = fracs + 1
  data$prize = data$distance = data$race.outcome = NULL
  data
}
historical_data = function(race_date, id_input, race_ground) {
  mapply_data %>% group_by(id) %>% filter(date < race_date & id == id_input) %>%
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
              groundwin = sum(ground == race_ground & finish %in% c(1,2,3)),
              year_race = sum(year(date) == year(race_date)),
              trax = length(unique(track)),
              trial = nth(track, 1),
              jox = length(unique(jockey)),
              grounds = length(unique(ground)),
              ts = sum(tongue_strap),
              blinx = sum(blinkers),
              mean_sp = mean(fn),
              years = length(unique(year(date)),
              days_since = race_date - nth(date, 1))
    )
}

make_derived = function(data) {
  data = data %>%
    mutate(nog = if_else(group1 + group2 + group3 == 0, 1,0),
           race_month = month(date),
           race_year = year(date),
           stamina_index = (sire_index + damsire_index) / dist,
           stays = avdist / dist
    )
}

make_racelevel = function(data) {
  data = data %>% dplyr::group_by(track, date) %>%
    dplyr::mutate(fieldsize = length(avfinish),
           findiff = (mean(avfinish, na.rm=T) - avfinish),
           spdiff = (mean(mean_sp, na.rm=T) - mean_sp),
           form_diff = mean(form1 + form2 + form3 + form4, na.rm=T) -
             (form1 + form2 + form3 + form4),
           gr1_diff = mean(group1, na.rm=T) - group1
           )
}

horse_metadata = function(id) {
  
  url = paste0("http://www.racingpost.com/horses/horse_home.sd?horse_id=", id)
  x <- read_html(GET(url, add_headers('user-agent' = 'r')))
  
  horse_name = x %>% html_node("h1") %>%
    html_text() %>%
    as.character()
  horse_name = sub(" *\\(.*", "", horse_name)
  horse_name = sub("\\n ", "", horse_name)
  
  horse_trainer = x %>% html_node("#detailedInfo div .White") %>%
    html_text() %>% as.character()
  horse_trainer = gsub(" $","", horse_trainer, perl=T)
  
  horse_sire = x %>% html_node("li:nth-child(2) b:nth-child(1) .White") %>%
    html_text() %>% as.character()
  horse_sire = sub(" *\\(.*", "", horse_sire)
  
  horse_damsire = x %>% html_node("b:nth-child(3) .White") %>%
    html_text() %>% as.character()
  horse_damsire = sub(" *\\(.*", "", horse_damsire)
  
  breeding = x %>% html_node("#detailedInfo li:nth-child(2)") %>%
    html_text() %>% as.character()
  re <- "\\(([^()]+)\\)"
  breeding = gsub(re, "\\1", str_extract_all(breeding, re)[[1]])
  breeding = gsub("f", "", breeding)
  sire_index = as.numeric(breeding[2])
  damsire_index = as.numeric(breeding[5])
  
  foaling_date = x %>% html_node("li:nth-child(1) b") %>%
    html_text() %>% as.character()
  foaling_date = gsub(re, "\\1", str_extract_all(foaling_date, re)[[1]])
  
  frame1 = data.frame(id, horse_name, horse_trainer, horse_sire, horse_damsire,
                      sire_index, damsire_index, foaling_date)
  message(paste0("Completed id =", id))
  frame1
}

geo_mean = function (x) {
  exp(mean(log(x), na.rm=T))
}

