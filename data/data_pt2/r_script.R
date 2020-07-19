
# REPLACE WITH OWN DIRECTORY

data <- read_csv("data/data_pt2/data_pt2.csv")

walk_scores = list()

for(i in 1:nrow(data)){
  walk_scores[i] = list(getWS(data$longitude[i], data$latitude[i], "ffd1c56f9abcf84872116b4cc2dfcf31"))
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
write_csv(walk_score_df, "data/data_pt2/walk_score_2_df.csv")
