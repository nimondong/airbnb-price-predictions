
# REPLACE WITH OWN DIRECTORY

data <- read_csv("data/data_pt1/data_pt1.csv")

walk_scores = list()

for(i in 1:nrow(data)){
  walk_scores[i] = list(getWS(data$longitude[i], data$latitude[i], "PASTE IN API KEY GIVEN IN EMAIL"))
}

walk_score_df <- walk_scores %>%
  sapply(unclass) %>% 
  as.data.frame() %>% 
  t() %>% 
  as.data.frame() %>% 
  lapply(unlist) %>% 
  as.data.frame(stringsAsFactors = FALSE) %>% 
  remove_rownames()

# SAVE IN FOLDER
write_csv(walk_score_df, "data/data_pt1/walk_score_df.csv")