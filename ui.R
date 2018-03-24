library(dplyr)
library(leaflet)
library(shinythemes)
library(DT)
library(shiny)



navbarPage(theme = shinytheme("simplex"),
           
           #logo
           title = img(src = 'http://www.bullhornleadership.com/wp-content/uploads/2017/08/meetup_logo.png',
                       width = 80),
           
           
           tabPanel('Analysis Overview',
                    style = "font-family: 'Lobster', cursive;
                    line-height: 1.1; 
                    color: #E74C3C;",
                    
                    
                    
                    # TEST
                    tags$head(
                      tags$style("body {background-color: white; }")
                    ),
                    
                    h3('Adjust Time Range'),
                    dateRangeInput('dateRange',
                                   label = 'Select Date Range',
                                   start = '2010-01-01' , end = '2017-10-31',
                                   width = 1250),
                    
                    br(),
                    
                    fluidRow(
                      # line graph of member joins
                      column(6,  plotOutput('members_line'), height = 450), 
                      
                      
                      # scatterplot
                      column(6,  plotOutput('cat_group_plot'), height = 450)
                    ),
                    
                    br(),
                    br(),
                    br(),
                    
                    fluidRow(
                      
                      # area chart of category trends
                      column(6, plotOutput('cat_events_plot'), height = 450), 
                      
                      
                      # scatterplot
                      column(6,  plotOutput('scat_plot'), height = 450)
                      
                    )
                    
                    
           ),  
           
           tabPanel(title = 'Find A Group',
                    style = "font-family: 'Lobster', cursive;
                    line-height: 1.1; 
                    color: #E74C3C;",
                    
                    
                    
                    h3('Find Popular Groups'),
                    
                    selectizeInput('groupCity',
                                   label = 'Select City',
                                   choices = unique((member_catGroup_df[,sort(city)])),
                                   width = 1250),
                    
                    
                    selectizeInput('catName',
                                   label = 'Select Category',
                                   choices = unique((member_catGroup_df[,sort(category_name)])),
                                   width = 1250),
                    
                    
                    mainPanel( width = 12,
                               
                               # histogram count of members for each category
                               plotOutput('topGroups_plot')
                    )
                    
           ), 
           
           tabPanel('Find A Venue',
                    style = "font-family: 'Lobster', cursive;
                    line-height: 1.1; 
                    color: #E74C3C;",
                    
                    sidebarLayout(
                      
                      mainPanel(width = 6,
                                
                                
                                h3('Find Popular Venues'),
                                
                                # these widgets to help find groups in your city
                                selectizeInput('city',
                                               label = 'Select City',
                                               choices = unique((event_details_df[,sort(venue.city)])),
                                               width = 600),
                                
                                selectizeInput('category',
                                               label = 'Select Category',
                                               choices = unique(event_details_df[,sort(category_name)]),
                                               width = 600),
                                
                                leafletOutput("category_map")
                                
                                
                      ),
                      
                      mainPanel(width = 6,
                                
                                br(), 
                                
                                DT::dataTableOutput('venueTable')
                                
                      )
                    )
           )
)



