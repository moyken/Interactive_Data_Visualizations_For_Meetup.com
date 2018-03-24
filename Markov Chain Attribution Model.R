# Load Packages
install.packages("googleAnalyticsR")
library(googleAnalyticsR)
install.packages("googleAuthR")
library(googleAuthR)
install.packages("ChannelAttribution")
library(ChannelAttribution)
install.packages("tidyr")
library(tidyr)
install.packages("markovchain")
library(markovchain)

# authenticate
ga_auth(new_user = TRUE)

# GA View ID
my_id <- 140458927

# Data Time Frame
from = "2017-02-01"
to = "2017-02-27"

# Creating Filter objects to be used later
truegoals <- "mcf:conversionGoalNumber==001" 
allvisits <- "mcf:conversionGoalNumber==003"

# Pulling ALL the paths taken to the site
visit_data <- google_analytics(id = my_id,
                               start = from, end = to,
                               metrics = "totalConversions",
                               dimensions = "basicChannelGroupingPath",
                               filters = "mcf:conversionGoalNumber==003",
                               type = "mcf")

# Transforming number of paths to numeric
visit_data[,2] <- as.numeric(visit_data[,2])

# Confirm Total number of paths taken
sum(visit_data$totalConversions)

# Cleaning Channel Path Labels
visit_data[,1] <- gsub(" / ", "/", visit_data[,1]) # remove spaces
visit_data[,1] <- gsub("CLICK:", "", visit_data[,1]) # remove Clicks
visit_data[,1] <- gsub(":CLICK", "", visit_data[,1]) # remove Clicks
visit_data[,1] <- gsub("NA:", "", visit_data[,1]) # remove NA
visit_data[,1] <- gsub(":NA", "", visit_data[,1]) # remove NA
visit_data[,1] <- gsub("Paid Search", "Paid_Search", visit_data[,1]) # adjusting paid search
visit_data[,1] <- gsub("Organic Search", "Organic_Search", visit_data[,1]) # adjusting organic search
visit_data[,1] <- gsub("Social Network", "Social_Network", visit_data[,1]) # adjusting social network
visit_data[,2] <- as.numeric(visit_data[,2])

# Rename Columns
colnames(visit_data) <- c("Path", "Visits")

#sort by conversions
visit_data <- visit_data[order(-visit_data$Visits),]

# View Data
View(visit_data)





# Pulling data on only the paths that resulted in conversions
goal_data <- google_analytics(id = my_id,
                              start = from, end = to,
                              metrics = "totalConversions",
                              dimensions = c("basicChannelGroupingPath"),
                              filters = "mcf:conversionGoalNumber==001",
                              type = "mcf")

# Transforming total paths taken to numeric
goal_data[,2] <- as.numeric(goal_data[,2])

# Confirm tota number of web conversions
sum(goal_data$totalConversions)

# Cleaning channel path labels
goal_data[,1] <- gsub(" / ", "/", goal_data[,1]) #remove spaces
goal_data[,1] <- gsub("CLICK:", "", goal_data[,1]) #remove Clicks
goal_data[,1] <- gsub(":CLICK", "", goal_data[,1]) #remove Clicks
goal_data[,1] <- gsub("NA:", "", goal_data[,1]) #remove NA
goal_data[,1] <- gsub(":NA", "", goal_data[,1]) #remove NA
goal_data[,1] <- gsub("Paid Search", "Paid_Search", goal_data[,1]) #adjusting paid search
goal_data[,1] <- gsub("Organic Search", "Organic_Search", goal_data[,1]) #adjusting organic search
goal_data[,1] <- gsub("Social Network", "Social_Network", goal_data[,1]) #adjusting social network
goal_data[,2] <- as.numeric(goal_data[,2])

# Rename Columns
colnames(goal_data) <- c("Path", "Conversions")

# Order goal paths by number of conversions 
goal_data <- goal_data[order(-goal_data$Conversions),]

# View final data on paths that lead to conversions
View(goal_data)





# MERGE BOTH DATA FRAMES for final list of paths taken (both converted and non converted)
all_data <- merge.data.frame(visit_data,goal_data, by = "Path", all=T)

# Clean NAs
all_data[is.na(all_data$Conversions), "Conversions"] <- 0
all_data <- all_data[order(-all_data$Conversions),]

View(all_data)

# Calculate conversion rate
all_data$rate <- all_data$Conversions / all_data$Visits

# Creating null column: visits witout conversions
all_data$null <- all_data$Visits - all_data$Conversions





# APPLY MARKOV MODEL
mm <- markov_model(all_data, var_path = "Path",
                   var_conv = "Conversions",
                   var_null = "null",
                   order = 1)

# # Attribute credit to each channel based on the impact of removing it
attribution <- mm$removal_effects
attribution[,2] <- as.numeric(attribution[,2])
attribution$attribution <- attribution$removal_effect/ sum(attribution$removal_effects)
View(attribution)

# Create markov chain diagram
graph_data <- markov_model(all_data, var_path = "Path",
                           var_conv = "Conversions",
                           var_null = "null",
                           order = 1,
                           out_more = TRUE)

graph_matrix <- graph_data$transition_matrix



# Create dummy data frame so that all channels are represented in Channel_from and channel_to
dummy <- data.frame(
  channel_from = c('(start)', '(conversion)', '(null)'),
  channel_to = c('(start)', '(conversion)', '(null)'),
  transition_probability = c(0,1,1))

# Combine dummy and graph data
trans_df <- rbind(graph_matrix, dummy)

# Order the channels
trans_df$channel_from <- factor(trans_df$channel_from,
                                levels = c('(start)','(conversion)','(null)', 'Organic_Search','Paid_Search','Direct','Referral','Email','(unavailable)'))

trans_df$channel_to <- factor(trans_df$channel_to,
                              levels = c('(start)','(conversion)','(null)', 'Organic_Search','Paid_Search','Direct','Referral','Email','(unavailable)'))

str(trans_df)



# create a matrix to be graphed
spread_data <- spread(trans_df, channel_to, transition_probability)

matrix_data <- matrix(data = as.matrix(spread_data[,-1]),
                      nrow = nrow(spread_data[,-1]), ncol = ncol(spread_data[,-1]),
                      dimnames = list(c(as.character(spread_data[,1])), c(colnames(spread_data[,-1]))))

matrix_data[is.na(matrix_data)] <- 0

matrix_data1 <- new("markovchain", transitionMatrix = matrix_data)

plot(matrix_data1,edge.arrow.size = 0.35)





# Compare with heuristic model

hm <- heuristic_models(all_data, 
                       var_path = "Path",
                       var_conv = "Conversions")
View(hm)


# Merge Markov chain and heuristic results in one DF
modeled <- merge.data.frame(hm, mm, all=T, by="channel_name")
colnames(modeled) <- c("Channel", "First", "Last", "Linear", "Markov")
View(modeled)
str(modeled)
model <- modeled[,c(1:3,5)]



###################################################3

# transform dataset so you can graph it comparisons
install.packages("reshape")
install.packages("ggplot2")
library(reshape)
library(ggplot2)

melted <- melt(model, id="Channel")


# plot total conversions
ggplot(melted, aes(Channel, value, fill = variable)) +
  geom_bar(stat = 'identity', position = 'dodge') +
  ggtitle("TOTAL CONVERSIONS")
theme(axis.title.x = element_text(vjust = -2))+
  theme(axis.title.y = element_text(vjust = +2))+
  theme(plot.title=element_text(size = 20))
