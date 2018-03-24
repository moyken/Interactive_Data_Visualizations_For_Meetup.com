library(ggplot2)
library(dplyr)
library(leaflet)
library(data.table)
library(DT)
library(RColorBrewer)

function(input, output) {
  
  #################################################################################
  ### Analysis Overview (tab 1)
  ##################################################################################
  
  
  ###################################################################################
  # Ceating Line Graph of Members Joining Meetup groups
  
  # Update members_data DF based on user date input
  members_time_data <- reactive({ member_time_df %>%
      filter(joined >= input$dateRange[1] & joined <= input$dateRange[2]) %>%
      select(joined, groups_joined = total_members)
  })
  
  # Plot Line Graph based on updated members DF above
  output$members_line <- renderPlot({
    ggplot(members_time_data(), aes(x = joined, y = groups_joined, group = 1)) +
      geom_line(color = 'orangered') +
      ggtitle("Group Activity By Time") +
      theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
            panel.background = element_blank(),
            panel.grid.major.y = element_line(color = 'grey'),
            axis.title = element_text(size = 15),
            text = element_text(size = 12)) +
      labs(x = 'Date', y = 'Members Joined A Group',
           caption = "*Data includes members who've joined groups since Jun. 2002")
  })
  
  
  
  ################################################################################### 
  # Creating Stacked Area Chart of number of events created colored by Category
  
  # Update event_details_df based on date selected
  cat_events_data <- reactive({  category_details_df %>%
      filter(date >= input$dateRange[1] & date <= input$dateRange[2])
  })
  
  # plot area chart of event trends by catgeory dataframe
  output$cat_events_plot <- renderPlot({
    ggplot(cat_events_data(), aes(x = date, y = total_events_created, fill = category_name)) +
      geom_area(alpha = 0.5) +
      ggtitle('Meetup Category Trends') +
      theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
            legend.position = c(.25,.85),
            legend.background = element_blank(),
            legend.direction = 'horizontal',
            legend.title = element_blank(),
            panel.background = element_blank(), 
            panel.grid.major.y = element_line(color = 'grey'),
            panel.grid.minor.y = element_line(color = 'grey'),
            axis.title = element_text(size = 15),
            text = element_text(size = 12)) +
      labs(x = 'Date', y = 'Events Created',
           caption = "Data includes events created since Oct. 2010")
  })
  
  ###################################################################################
  # Creating bar graph of Category popularity
  
  # Update category DF based on user date input
  cat_group_data <- reactive({ member_catGroup_df %>%
      group_by(category_name) %>%
      filter(created >= input$dateRange[1] & created <= input$dateRange[2]) %>%
      summarise(total_groups = n()) %>%
      arrange(desc(total_groups)) %>%
      head(5)
  })
  
  
  # Plot bar chart based on updated data above
  output$cat_group_plot <- renderPlot({
    ggplot(cat_group_data(), aes(x = reorder(category_name, -total_groups), y = total_groups)) +
      geom_bar(stat = 'identity', aes(fill = category_name)) +
      geom_text(aes(label = total_groups, vjust = -0.5)) +
      ggtitle('Top Categories By Groups Created') +
      theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
            panel.background = element_blank(),
            legend.position = 'none',
            axis.title = element_text(size = 15),
            # axis.text.x = element_text(angle = 15),
            text = element_text(size = 12)) +
      labs(x = 'Category Name', y = 'Groups Created',
           caption = '*Data includes groups created since Oct. 2002') +
      scale_fill_manual(values = c('#A50F15','#CB181D','#EF3B2C','#FB6A4A','#FC9272','#FCBBA1','#FEE0D2','#FFF5F0','#67000D'))
  })
  
  
  
  
  ###################################################################################
  ### Data for SCATTER PLOT OF RSVPS VS EVENT DURATION
  
  # Update event data based on user date input
  event_perf_data <- reactive({ event_details_df %>%
      left_join(member_catGroup_df, by = 'group_name') %>%
      select(category_name = category_name.x, event_name, creation_date, duration, rsvp, total_members) %>%
      filter(creation_date >= input$dateRange[1] & creation_date <= input$dateRange[2] & duration < 20)
  })
  
  
  # Plot scatter plot of events by event duration and RSVP count
  output$scat_plot <- renderPlot({
    ggplot(event_perf_data(), aes(x = duration, y = rsvp, color = category_name, size = total_members)) +
      geom_point(alpha = 0.5, position = 'jitter') +
      scale_size_area() + 
      ggtitle('Event Performance') +
      theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
            legend.position = c(.70,.83),
            legend.background = element_blank(),
            legend.direction = 'horizontal',
            # legend.title = element_blank(),
            panel.background = element_blank(),
            panel.grid.major = element_line(color = 'grey'),
            panel.grid.minor = element_line(color ='grey'),
            axis.title = element_text(size = 15),
            text = element_text(size = 12)) +
      guides(color = guide_legend(title="Category"), size = guide_legend(title = 'Member Volume')) +
      labs(x = 'Event Duration (hours)', y = 'RSVP Count',
           caption = "*Data includes event created since Oct. 2010")
  })
  
  
  #################################################################################
  ### Find a Group (Tab 2)
  #################################################################################
  
  # Update Most Popular Groups DF by selected City and Category of Meetup
  topGroups_data <- reactive ({ member_catGroup_df %>%
      filter(city == input$groupCity & category_name == input$catName) %>%
      arrange(desc(total_members)) %>%
      head(10)
  })
  
  # Plot bar chart based on updated data above
  output$topGroups_plot <- renderPlot({
    ggplot(topGroups_data(), aes(x = reorder(group_name, total_members), y = total_members)) +
      geom_bar(stat = 'identity', aes(fill = group_name), color = 'black') +
      geom_text(aes(label = total_members, hjust = -0.15)) +
      ggtitle("Top 10 Groups by Members Joined") +
      theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
            panel.background = element_blank(),
            legend.position = 'none',
            axis.title = element_text(size = 15),
            text = element_text(size = 15)) +
      labs(x = 'Group Name', y = 'Members Joined',
           caption = 'Timeframe: Oct. 2002 - Oct. 2017') +
      coord_flip() +
      scale_fill_brewer(palette = 'Reds')
  })
  
  
  
  
  #################################################################################
  ### Find a Venue (Tab 3)
  #################################################################################
  
  # Update Popular Venue DF based on selected City and Category
  event_data <- reactive({ 
    event_details_df %>%
      filter(venue.city == input$city & 
               category_name == input$category,
             lat != 0,
             long != 0)
  })
  
  # Plot a map of all the venue locations that match selected inputs
  output$category_map <- renderLeaflet({
    leaflet(event_data()) %>%
      addProviderTiles('OpenStreetMap.DE') %>%
      addMarkers(lng = ~long,
                 lat = ~lat,
                 popup = ~venue_name)
  })
  
  # Show an info table of the most popular Venue locations
  output$venueTable <- DT::renderDataTable( 
    (event_details_df %>% 
       filter(venue.city == input$city &
                category_name == input$category) %>%
       select('Venue Name' = venue_name, Ratings = rating_count, 'Avg Rating' = venue_rating, Address = address_1) %>%
       arrange(desc(Ratings, 'Avg Rating')) %>%
       unique()),
    rownames = FALSE,
    options = list(pageLength = 15)
  )
  
  
}











