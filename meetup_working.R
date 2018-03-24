### TEST APP USING
# runApp('C:/Users/Kenny/Desktop/NYCDSA/meetup', display.mode = "showcase")

library(data.table)
library(dplyr)
library(ggplot2)
install.packages('shiny')
library(shiny)


setwd('C:/Users/moyke/Desktop/NYCDSA/meetup_backup_files')

# vector of unread files
unread_files <- list.files(pattern = '*.csv')

# create a list of read files
myfiles <- lapply(unread_files , fread)

# split list of data frames to individual data frames
category_df <- myfiles[[1]]
cities_df <- myfiles[[2]]
events_df <- myfiles[[3]]
groups_df <- myfiles[[4]]
groups_topics_df <- myfiles[[5]]
members_df <- myfiles[[6]]
members_topics_df <- myfiles[[7]]
topics_df <- myfiles[[8]]
venues_df <- myfiles[[9]]

members_df <- fread('6members.csv')
length(unique(members_df$member_id))
unique(members_df$hometown)
View(head(members_df, 20))

# create members_ts data frame for coutn of members

members_df$joined <- as.Date(members_df$joined)

members_joined <- members_df %>%
  select(joined, member_id) %>%
  group_by(joined) %>%
  summarise(total_members = n())

head(members_joined,20)

members_joined$joined <- as.Date(members_joined$joined)

members_joined <- members_joined %>%
  group_by(joined) %>%
  summarise(total_members = sum(total_members))

View(head(members_joined, 20))

members_joined <- members_joined %>%
  filter(joined >= '2010-10-01' & joined <= '2017-10-31')

 

min(members_joined$joined)
max(members_joined$joined)

## write.csv(members_joined, 'members_joined.csv')


members_joined <- fread('members_joined.csv')

members_plot <- ggplot(members_joined, aes(x = joined, y = total_members)) + geom_line() 
  

members_plot



test <- members_joined %>%
  mutate(month = month(joined),
         year = year(joined))  %>%
  group_by(year, month) %>%
  summarise(member_count = sum(total_members))

## 22764 in year 2010
members_joined %>%
  mutate(month = month(joined),
         year = year(joined))  %>%
  filter(year >= 2011 & year <= 2015) %>%
  ggplot(aes(x = month, y = total_members)) +
  geom_bar(stat = 'identity')

#########################################################################################

# create category_members df
# neeed category, groups and members df


member_details_df <- members_df %>% 
  full_join(groups_df, by = 'group_id') %>%
  select(member_id, member_name, joined, group_id, group_name, category_id) %>%
  full_join(category_df, by = 'category_id') %>%
  select(member_id, member_name, joined, group_id, group_name, category_id, category_name)


member_details_df$joined <- as.Date(member_details_df$joined)


# write.csv(member_details_df, 'members_details_df.csv') 

member_details_df <- fread('members_details_df.csv', drop = 1)

member_time_df <- member_details_df %>%
  group_by(joined) %>%
  summarise(total_members = n())

# write.csv(member_time_df, 'member_time_df.csv') 

member_details_df <- fread('members_details_df.csv', drop = 1)




###############################################################################

###### group data frame, what are the top groups in each data frame WITHOUT DATE RANGE
group_details_df <- fread('group_details_df.csv', drop = 1)
groups_df <- fread('4groups.csv')

member_catGroup_df <- member_details_df %>%
  group_by(category_name, group_name) %>%
  summarise(total_members = n()) %>%
  left_join(groups_df, by = 'group_name') %>%
  select(category_name, group_name, total_members, city, created)

# clean  city
unique(member_catGroup_df$city)
member_catGroup_df$city <- gsub('New York', 'New York City', member_catGroup_df$city, fixed = TRUE)
member_catGroup_df$city <- gsub('West New York', 'New York City', member_catGroup_df$city, fixed = TRUE)
member_catGroup_df$city <- gsub('New York City City', 'New York City', member_catGroup_df$city, fixed = TRUE)

member_catGroup_df$city <- gsub('South San Francisco', 'San Francisco', member_catGroup_df$city, fixed = TRUE)

member_catGroup_df$city <- gsub('West Chicago', 'Chicago', member_catGroup_df$city, fixed = TRUE)
member_catGroup_df$city <- gsub('Chicago Ridge', 'Chicago', member_catGroup_df$city, fixed = TRUE)
member_catGroup_df$city <- gsub('Chicago Heights', 'Chicago', member_catGroup_df$city, fixed = TRUE)
member_catGroup_df$city <- gsub('North Chicago', 'Chicago', member_catGroup_df$city, fixed = TRUE)

# convert cerated column to date type
member_catGroup_df$created <- as.Date(member_catGroup_df$created) 

write.csv(member_catGroup_df,'member_catGroup_df.csv')


# 
member_details_df %>%
  arrange(joined) %>%
  head()


###########################################################################################

events_df <- fread('3events.csv')
venues_df <- fread('9venues.csv')
groups_df <- fread('4groups.csv')
category_df <- fread('1categories.csv')

# show all events that ocurred during selected time frame
# need groups df,  and group topics

event_details_df <- events_df %>%  
  # join venues to events to get group_id
  left_join(venues_df, by = 'venue_id') %>%
  select(event_id, event_name, creation_date = created, duration, rsvp = yes_rsvp_count, venue_id, venue_name,
         rating, rating_count, venue.city, address_1, lat, lon, group_id) %>%
  
  # join with groups to get category id
  inner_join(groups_df, by = 'group_id') %>%
  select(event_id, event_name, creation_date, duration, rsvp, venue_id, venue_name, venue_rating = rating.x, rating_count, 
         venue.city, address_1, lat = lat.x, long = lon.x, group_id, group_name, group_rating = rating.y, category_id) %>%
  
  # join with category to get category name
  inner_join(category_df, by = 'category_id') %>%
  select( event_name, creation_date, duration, rsvp, venue_name, venue_rating, rating_count, venue.city, address_1, lat, long, group_name, group_rating, category_name)
  


event_details_df$creation_date <- as.Date(event_details_df$creation_date)
event_details_df$venue_rating <- as.numeric(event_details_df$venue_rating)
event_details_df$rating_count <- as.numeric(event_details_df$rating_count)
event_details_df$lat <- round(as.numeric(event_details_df$lat),8)
event_details_df$long <- round(as.numeric(event_details_df$long),8)
event_details_df$rsvp <- as.numeric(event_details_df$rsvp)
event_details_df$duration <- as.numeric(event_details_df$duration)

# convert duration from hours to seconds
event_details_df <- event_details_df %>%
mutate(duration = duration/3600)


# clean venue city
unique(event_details_df$venue.city)
event_details_df$venue.city <- gsub('New York', 'New York City', event_details_df$venue.city, fixed = TRUE)
event_details_df$venue.city <- gsub('New York City City', 'New York City', event_details_df$venue.city, fixed = TRUE)
event_details_df$venue.city <- gsub('New York City,', 'New York City', event_details_df$venue.city, fixed = TRUE)
event_details_df$venue.city <- gsub('New York City,NY', 'New York City', event_details_df$venue.city, fixed = TRUE)
event_details_df$venue.city <- gsub('New York CityNY', 'New York City', event_details_df$venue.city, fixed = TRUE)

event_details_df$venue.city <- gsub('San Francisco, CA 94103', 'San Francisco', event_details_df$venue.city, fixed = TRUE)
event_details_df$venue.city <- gsub('san francisco', 'San Francisco', event_details_df$venue.city, fixed = TRUE)
event_details_df$venue.city <- gsub('SAN FRANCISCO', 'San Francisco', event_details_df$venue.city, fixed = TRUE)

event_details_df$venue.city <- gsub('Chicago, IL', 'Chicago', event_details_df$venue.city, fixed = TRUE)


# write.csv(event_details_df, 'event_details_df.csv')



############################################################################

# fidn a group by category
# need groups df, cities df (for lat and lon)  and group topics

group_details_df <- fread('group_details_df.csv', drop = 1)

group_details_df <- groups_topics_df %>% 
  # join topic and group dfs to get topic name and group name
  full_join(groups_df, by = 'group_id') %>%
  select(topic_id, topic_name, group_id, group_name, category_id, city_id) %>%
  
  # join to category df to get category name
  full_join(category_df, by = 'category_id') %>%
  select(topic_id, topic_name, group_id, group_name, category_id, category_name, city_id) %>%
  
  # join with city id to get lat and lon
  full_join(cities_df, by = 'city_id') %>%
  select(topic_id, topic_name, group_id, group_name, category_id, category_name, city_id, city, member_count, state, latitude, longitude) %>%
  filter(group_name != 'NA')

options(digits=10)
group_details_df$latitude <- as.numeric(group_details_df$latitude)
group_details_df$longitude <- as.numeric(group_details_df$longitude )
## write.csv(group_details_df, 'group_details_df.csv')


##########################################################################################3


##create dummy df to combine with event_details_df 
dummy_catYear_df <- data.frame(cbind(rep(sort(unique(event_details_df$category_name)), each  = 96),
                                     rep(seq(2010,2017), 32)), stringsAsFactors = F)
colnames(dummy_catYear_df) <- c('category_name','year')
dummy_catYear_df$year <- as.integer(dummy_catYear_df$year)


category_details_df <- event_details_df %>%
  mutate(year = year(creation_date),
         month = month(creation_date))  %>%
  group_by(year,month, category_name) %>%
  summarise(total_events_created = n()) %>%
  full_join(dummy_catYear_df, by = c('year', 'category_name'))
category_details_df[is.na(category_details_df)] <- 0



#########################################################################################

style = "font-family: 'Lobster', cursive;
        font-weight: 500; line-height: 1.1; 
color: #E74C3C;"

