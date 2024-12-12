#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(shinythemes)
library(bslib)

# Define UI for application that draws a histogram
fluidPage( theme =shinytheme("flatly"),
           navbarPage(title = "The Enron Message App ",
                      
                      tabPanel("User Sender",
                               fluidRow(
                                 column(5,
                                        card(
                                          sliderInput("bins",
                                                      label = "Choose your top:",
                                                      min = 0,
                                                      max = 20,
                                                      value = 20,
                                                      step = 5
                                          ),
                                          selectInput("statuslist",
                                                      label = "Select status:",
                                                      choices = list(
                                                        "All" = "All", "CEO" = "CEO", "Director" = "Director",
                                                        "Employee" = "Employee", "In House Lawyer" = "In House Lawyer",
                                                        "Manager" = "Manager", "Managing Director" = "Managing Director",
                                                        "N/A" = "N/A", "President" = "President", "Trader" = "Trader",
                                                        "Vice President" = "Vice President"
                                                      ),
                                                      selected = "All"
                                          ),
                                          uiOutput("TopActiveEmailersTitle_"),
                                          tableOutput("TopActiveEmailers_")
                                        )
                                 ),
                                 
                                 column(7,
                                        card(
                                          tags$h3("Message Distribution by Status"),
                                          # Stacking the outputs vertically in the card
                                          plotOutput("MsgByStatus_", width = "500px", height = "500px"),
                                          tableOutput("TableMsgByStatus_"),
                                          tableOutput("TableStatusSummary_")
                                        )
                                 )
                               )
                      ),
                      
                      
                      
                      tabPanel("User Recipient",
                               fluidRow(
                                 column(5,
                                        card(
                                          sliderInput("bins_recip",
                                                      label = "Choose your top:",
                                                      min = 0,
                                                      max = 20,
                                                      value = 20,
                                                      step = 5
                                          ),
                                          selectInput("statuslist_recip",
                                                      label = "Select status:",
                                                      choices = list(
                                                        "All" = "All", "CEO" = "CEO", "Director" = "Director",
                                                        "Employee" = "Employee", "In House Lawyer" = "In House Lawyer",
                                                        "Manager" = "Manager", "Managing Director" = "Managing Director",
                                                        "N/A" = "N/A", "President" = "President", "Trader" = "Trader",
                                                        "Vice President" = "Vice President"
                                                      ),
                                                      selected = "All"
                                          ),
                                          uiOutput("TopActiveRecipientTitle_"),
                                          tableOutput("TopActiveRecipient_")
                                        )
                                 ),
                                 
                                 column(7,
                                        card(
                                          tags$h3("Message received  histogram"),
                                          # Stacking the outputs vertically in the card
                                          plotOutput("Histo_Msg_Received_", width = "600px", height = "400px"),
                                        ),
                                        card(
                                          tags$h3("Message received by status and rtype"),
                                          plotOutput("StackedBar_Status_Rtype_", width = "600px", height = "400px")
                                        ),
                                 )
                               )
                      ),
                      
                      
                      
                      
                      tabPanel(
                        # Application title
                        "Sent vs Received",
                        
                        # Sidebar with a slider input for number of bins
                        card( 
                          radioButtons( 
                            inputId = "Bar_or_Box", 
                            label = "Choose plot type", 
                            choices = list( 
                              "Bar Plots" = 1 , 
                              "Box Plots" = 2 
                            ) 
                          ), 
                          layout_columns( 
                            card(
                                 h3(textOutput("leftplotTitle_")),
                                 plotOutput("leftplot_")), 
                            card(
                                 h3(textOutput("rightplotTitle_")),
                                 plotOutput("rightplot_")) 
                          ), col_widths = c(6,6)
                         )
                        
                        ),
                      
                      
                      
                      
                      tabPanel("Temporal Dynamic",
                               
                               sidebarPanel( #dateRangeInput(inputId = "rangedate", label = "Date Range", start ="1999-01-01",
                                                            #end="2002-12-31", min="1999-01-01", max = "2002-12-31",
                                                            #weekstart = 1, language="en", separator = "to", autoclose = TRUE),
                                             selectInput( 
                                               "SelectTrend", 
                                               "Select options below:", 
                                               list("General Trend by date" = "GTD",
                                                    "General Trend by month_year" = "GTMY",
                                                    "Status Emailing Out Trend" = "SEO",
                                                    "Exchange compositon : user/non user" = "NUNUEX") 
                                                        )
                                            ),
                               mainPanel(
                                 h3(textOutput("TemporalDynamicsTrends_Title")),
                                 plotOutput("TemporalDynamicsTrends_"),
                                 width = 9
                                        )
                      ),
                      
                      
                      
                      
                      tabPanel("User Interaction Dynamic", tags$h3("Interaction between user from Jan 2001 to June 2002"),
                               plotOutput("Grid_StatusVSStatusData")
                      ),
                      
                      
                      
                      
                      
                      tabPanel("Content Exploration",
                                 tags$h3("Messages content exploration"),
                                 layout_columns( 
                                   card(
                                     tags$h4("Distribution of extracted words"),
                                     plotOutput("PlotWordPieData")), 
                                   card(
                                     tags$h4("Words of interest trend over time"),
                                     plotOutput("PlotWordLineTrendData")) 
                                 ))
                      ))
