# DATA CLEANING

# Loading Packages --------------------------------------------------------
library(tidyverse)
library(readr)
library(janitor)
library(stringr)
library(stringi)
library(fastDummies)
library(corrplot)
library(ggmap)
library(walkscoreAPI)

# Initial Data Cleaning ---------------------------------------------------

airbnb_data <- read_csv("data/unprocessed/chicago.csv") %>%
  mutate(location = "Chicago") %>%
  clean_names()

# converting dollar amounts from chr to dbl 
airbnb_data$price <- as.numeric(gsub('[$,]', '', airbnb_data$price))
airbnb_data$extra_people <- as.numeric(gsub('[$,]', '', airbnb_data$extra_people))
airbnb_data$cleaning_fee <- as.numeric(gsub('[$,]', '', airbnb_data$cleaning_fee)) 
airbnb_data$security_deposit <- as.numeric(gsub('[$,]', '', airbnb_data$security_deposit)) 
airbnb_data$cancellation_policy[which(airbnb_data$cancellation_policy =="super_strict_30")] <- "strict"
airbnb_data$cancellation_policy[which(airbnb_data$cancellation_policy =="super_strict_60")] <- "strict"
airbnb_data$cancellation_policy[which(airbnb_data$cancellation_policy =="strict_14_with_grace_period")] <- "strict"

cleaned_airbnb_data <- airbnb_data  %>% 
  as_tibble() %>%
  select(id, location, longitude, latitude, property_type, room_type, accommodates,
         bathrooms, bedrooms, beds, amenities, price, security_deposit, cleaning_fee,
         guests_included, extra_people, minimum_nights, maximum_nights, number_of_reviews,
         review_scores_accuracy, review_scores_cleanliness, review_scores_checkin, 
         review_scores_communication, review_scores_location, review_scores_value, 
         review_scores_rating, host_is_superhost, instant_bookable, cancellation_policy) %>%
  
  mutate(property_type = replace(property_type, property_type %in% c("Condominium", "Serviced apartment", "Loft"), "Apartment"),
         property_type = replace(property_type, property_type %in% c("Townhouse", "Guesthouse", "Villa", "Bungalow"), "House"),
         bathrooms = replace(bathrooms, is.na(bathrooms), median(airbnb_data$bathrooms, na.rm = TRUE)),
         bedrooms = replace(bedrooms, is.na(bedrooms), median(airbnb_data$bedrooms, na.rm = TRUE)),
         beds = replace(beds, is.na(beds), median(airbnb_data$beds, na.rm = TRUE)),
         tv = if_else(str_detect(amenities, paste(c("TV", "Cable TV"), collapse = "|")), 1, 0),
         air_conditioning = if_else(str_detect(amenities, paste(c("Air conditioning", "Central air conditioning"), collapse = "|")), 1, 0),
         balcony_patio = if_else(str_detect(amenities, paste(c("Balcony", "Patio"), collapse = "|")), 1, 0),
         bed_linen = if_else(str_detect(amenities, paste(c("Bed linens"), collapse = "|")), 1, 0),
         outdoor_space = if_else(str_detect(amenities, paste(c("Garden", "Outdoor", "Sun loungers", "Terrace"), collapse = "|")), 1, 0),
         breakfast = if_else(str_detect(amenities, paste(c("Breakfast"), collapse = "|")), 1, 0),
         coffee_machine = if_else(str_detect(amenities, paste(c("Coffee maker", "Espresso machine"), collapse = "|")), 1, 0),
         cooking_equip = if_else(str_detect(amenities, paste(c("Cooking basics"), collapse = "|")), 1, 0),
         white_goods = if_else(str_detect(amenities, paste(c("Dishwasher", "Dryer", "Washer"), collapse = "|")), 1, 0),
         child_friendly = if_else(str_detect(amenities, paste(c("Family/kid friendly", "Children", "children"), collapse = "|")), 1, 0),
         parking = if_else(str_detect(amenities, paste(c("parking"), collapse = "|")), 1, 0),
         greet_by_host = if_else(str_detect(amenities, paste(c("Host greets you"), collapse = "|")), 1, 0),
         internet = if_else(str_detect(amenities, paste(c("Internet", "Pocket wifi", "Wifi"), collapse = "|")), 1, 0),
         long_term_stay = if_else(str_detect(amenities, paste(c("Long term stays allowed"), collapse = "|")), 1, 0),
         pets = if_else(str_detect(amenities, paste(c("Pets", "pet", "Cat(s)", "Dog(s)"), collapse = "|")), 1, 0),
         private_entrance = if_else(str_detect(amenities, paste(c("Private entrance"), collapse = "|")), 1, 0),
         security_system = if_else(str_detect(amenities, paste(c("Safe", "Security system"), collapse = "|")), 1, 0),
         self_checkin = if_else(str_detect(amenities, paste(c("Self check-in"), collapse = "|")), 1, 0),
         gym = if_else(str_detect(amenities, paste(c("Exercise equipment", "Gym", "gym"), collapse = "|")), 1, 0),
         security_deposit = replace(security_deposit, is.na(security_deposit), 0),
         cleaning_fee = replace(cleaning_fee, is.na(cleaning_fee), 0),
        
         review_scores_rating = replace(review_scores_rating, is.na(review_scores_rating), 101),
         review_scores_rating_under80 = if_else(review_scores_rating < 80, 1, 0),
         review_scores_rating_80_94 = if_else(review_scores_rating >= 80 & review_scores_rating <= 94, 1, 0),
         review_scores_rating_95_100 = if_else(review_scores_rating > 95 & review_scores_rating <= 100, 1, 0),
         review_scores_rating_unknown = if_else(review_scores_rating == 101, 1, 0)
  ) %>%
  filter(property_type == "Apartment" | property_type == "House",
         room_type == "Entire home/apt" | room_type == "Private room",
         cancellation_policy == "flexible" | cancellation_policy == "moderate" | cancellation_policy == "strict") %>%
  drop_na(host_is_superhost) %>%
  select(-amenities,
         -review_scores_accuracy, 
         -review_scores_cleanliness, 
         -review_scores_checkin, 
         -review_scores_communication, 
         -review_scores_location, 
         -review_scores_value)

write_csv(cleaned_airbnb_data, "data/processed/cleaned_airbnb_data.csv")


# count 
count <- features %>%
  count(amenities, sort = TRUE) 

# missing
table(is.na(cleaned_airbnb_data$state))

missing <- as_tibble(map(cleaned_airbnb_data, ~sum(is.na(.)))) %>%
  t() %>%
  as.data.frame()

# data types
lapply(cleaned_airbnb_data,class)


# PROPERTY TYPE: classifying property types as either house or apartment -> looking for property types with 1k+ instances
# ROOM TYPE: filtered for only entire house/apt or private room
# BATHROOMS: replaced missing values with median
# BEDROOMS: replaced missing values with median
# BEDS: replaced missing values with median
# AMENTIES: created dummy variable
# PRICE: adjusted to numeric dollars
# SECURITY DEPOSIT: replaced missing values with zero, adjusted to numeric dollars
# CLEANING FEE: replaced missing values with zero, adjusted to numeric dollars
# RATINGS: replaced missing values with unknown buckets / create different dummies for each bin (uknown, 0-8, 9, 10)
# SUPER HOST: drop missing rows
# CANCEL POLICY: condensed to three buckets


# Creating Model Building Data --------------------------------------------

# hot-one encoding
model_building_data <- cleaned_airbnb_data %>%
  dummy_cols(select_columns = c("property_type", "room_type", "host_is_superhost", "instant_bookable"), remove_first_dummy = TRUE) %>%
  dummy_cols(select_columns = c("cancellation_policy")) %>%
  select(-property_type,
         -room_type,
         -host_is_superhost,
         -instant_bookable,
         -location,
         -cancellation_policy) 

# looking at variable correlations
model_building_data %>%
  cor() %>%
  corrplot(type = "lower")

# removing corrlated columns
model_building_data <- model_building_data %>%
  select(-bedrooms, 
         -beds, 
         -bathrooms, 
         -guests_included)

# log transformation

model_building_data <- model_building_data %>%
  mutate(accommodates = replace(accommodates, accommodates == 0, 0.001),
         price = replace(price, price == 0, 0.001),
         security_deposit = replace(security_deposit, security_deposit == 0, 0.001),
         cleaning_fee = replace(cleaning_fee, cleaning_fee == 0, 0.001),
         extra_people = replace(extra_people, extra_people == 0, 0.001),
         minimum_nights = replace(minimum_nights, minimum_nights == 0, 0.001),
         maximum_nights = replace(maximum_nights, maximum_nights == 0, 0.001),
         number_of_reviews = replace(number_of_reviews, number_of_reviews == 0, 0.001),
         
         accommodates = log(accommodates),
         price = log(price),
         security_deposit = log(security_deposit),
         cleaning_fee = log(cleaning_fee),
         extra_people = log(extra_people),
         minimum_nights = log(minimum_nights),
         maximum_nights = log(maximum_nights),
         number_of_reviews = log(number_of_reviews))


chicago_model_data <- model_building_data

# pulling walk/transit/bike scores

data_pt1 <- chicago_model_data %>%
  slice(1:4092)

write_csv(data_pt1, "data/data_pt1/data_pt1.csv")
  
data_pt2 <- chicago_model_data%>%
  slice(4093:8183)

write_csv(data_pt2, "data/data_pt2/data_pt2.csv")
  
walk_score_1 <- read_csv("data/data_pt1/walk_score_df.csv")
walk_score_2 <- read_csv("data/data_pt2/walk_score_2_df.csv")

walk_scores <- walk_score_1 %>%
  rbind(walk_score_2) %>%
  select(walkscore)

model_data <- model_building_data %>%
  cbind(walk_scores)

# compiling final dataset
write_csv(model_data , "data/processed/model_data.csv")





