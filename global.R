library(data.table)
library(dplyr)


# Load DF for line graph
member_time_df <- fread('member_time_df.csv', drop = 1)
member_time_df$joined <- as.Date(member_time_df$joined)



# Load DF for members by category and group
member_catGroup_df <- fread('member_catGroup_df.csv', drop = 1)
member_catGroup_df$created <- as.Date(member_catGroup_df$created)

# Load DF for event data
event_details_df <- fread('event_details_df.csv')
event_details_df <- event_details_df[,-1]
event_details_df$creation_date <- as.Date(event_details_df$creation_date)
event_details_df$venue_rating <- round(event_details_df$venue_rating, 1)
event_details_df$rating_count <- round(event_details_df$rating_count)


# Creating DF for area chart
# create dummy df with all the dates so that area chart wont have white spaces
dummy_catYear_df <- data.frame(cbind(
                                rep(rep(seq(2010,2017), each = 12), each = 3),
                                rep(rep(seq(1,12), 8), each = 3)  , # months
                                rep(sort(unique(event_details_df$category_name)), 96)),stringsAsFactors = F)

colnames(dummy_catYear_df) <- c('year', 'month', 'category_name')

dummy_catYear_df$year <- as.integer(dummy_catYear_df$year)
dummy_catYear_df$month <- as.integer(dummy_catYear_df$month)

dummy_catYear_df <- dummy_catYear_df %>%
  arrange(year,month)



## Load original event data DF and join with dummy variabe for complete DF to be plotted
category_details_df <- event_details_df %>%
  mutate(year = year(creation_date),
         month = month(creation_date))  %>%
  group_by(year,month, category_name) %>%
  summarise(total_events_created = n()) %>%
  full_join(dummy_catYear_df, by = c('year', 'month', 'category_name')) %>%
  mutate(date = paste(year,month,'01', sep='-'))

category_details_df$date <- as.Date(category_details_df$date)
category_details_df[is.na(category_details_df)] <- 0
  


