#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(lubridate)
library(bslib)
library(ggiraph)
library(stringr)
library(tidyr)

load('C:\\Users\\MarieLaureJovial\\Downloads\\R BIG DATA PROCESSING\\Enron.Rdata')

#---------------------------TABLES USED AND TRANSFORMATION-------------------------------------------

#Replacing the null status in employeelist by N/A
employeelist$status[is.na(employeelist$status)] <- "N/A"

#Making sure that the date column is explotable for future visuals
message$date <- trimws(ymd(message$date))

#Adding a fullName column to employeelist
employeelistFN <- mutate(employeelist, fullName = paste(employeelist$firstName, employeelist$lastName))


    # ----------- TABLE : EmployeeMessageSent-----------------------------------

#Join of employeelist table and message table on Email_id
EmployeelistFN1 <-
  employeelistFN %>%
  select(eid,Email_id,status,fullName)%>%
  inner_join(message, by=c("Email_id"="sender"))
dim(EmployeelistFN1)

#Join of employeelist table and message table  on Email3
EmployeelistFN3 <-
  employeelistFN %>%
  filter(!is.na(Email3) & Email3 !="")%>%
  select(eid,Email3,status,fullName)%>%
  inner_join(message, by=c("Email3"="sender"))
dim(EmployeelistFN3)

#Appending EmployeelistFN1 and EmployeelistFN3
EmployeeMessageSent <- bind_rows(EmployeelistFN1, EmployeelistFN3)

    # ----------- TABLE : MsgByStatus----------------------------------

MsgByStatus <- EmployeeMessageSent %>% 
  group_by(status) %>% 
  summarise(TotalSent = n()) %>%
  arrange(desc(TotalSent))%>% 
  mutate(TotalSentPercentage= round(TotalSent/sum(TotalSent)*100))

    # ----------- TABLE : AllUser_wEmail----------------------------------

EmployeePartID <- employeelistFN %>% select (fullName, eid, status, Email_id) %>% rename(Email=Email_id) %>% mutate(EmailIndex = "Email_id")
EmployeePart2 <- employeelistFN %>% filter(!is.na(Email2) & Email2 !="") %>% select(fullName,eid,status, Email2) %>% rename(Email=Email2) %>% mutate(EmailIndex = "Email2")
EmployeePart3 <- employeelistFN %>% filter(!is.na(Email3) & Email3 !="") %>% select (fullName, eid, status, Email3) %>% rename(Email=Email3)  %>% mutate(EmailIndex = "Email3")
EmployeePart4 <- employeelistFN %>% filter(!is.na(EMail4) & EMail4 !="") %>% select (fullName, eid, status, EMail4) %>% rename(Email=EMail4) %>% mutate(EmailIndex = "EMail4")
AllUser_wEmail <- bind_rows(EmployeePartID, EmployeePart2, EmployeePart3, EmployeePart4)

  
    # ----------- TABLE : UserRecipient----------------------------------
UserRecipient <- merge(recipientinfo, AllUser_wEmail,by.x = "rvalue", by.y = "Email")


    # ------------ TABLES FOR PANE SENT VS RECEIVED ---------------------


#Number message sent by status (in value and percentage) - renaming the table for consistency
MsgSentByStatus <- MsgByStatus

#Number message rceived by status (in value and percentage)
MsgReceibedByStatus<- UserRecipient %>% 
  filter(rtype!="BCC")%>% 
  group_by(status)%>%
  summarise(TotalReceived = n())%>%
  mutate(TotalReceivedPercentage= round(TotalReceived/sum(TotalReceived)*100))

#Number user by status (in value and percentage)
EmployeeByStatus <- employeelistFN %>%
  group_by(status)%>%
  summarise(TotalUser = n())%>%
  mutate(TotalUserPercentage= round(TotalUser/sum(TotalUser)*100))


#Merge of the 3 tables below
StatusTable <-  MsgSentByStatus %>%
  inner_join(MsgReceibedByStatus, by = "status") %>%
  inner_join(EmployeeByStatus, by = "status")


#----------------------TABLE : CompleteDataset (merge of the 4 original table) -------

message$date <- trimws(ymd(message$date))
AllMessage_AllSender <- left_join(message,AllUser_wEmail, by = c("sender" = "Email"))

AllReceivers <- left_join(recipientinfo,AllUser_wEmail, by = c("rvalue" = "Email"))

AllMessage_AllSender_AllReceivers <- full_join(AllMessage_AllSender,AllReceivers, by = c("mid" = "mid")) %>% 
  rename(eid_Emailer = eid.x, fullName_Emailer = fullName.x, status_Emailer = status.x, EmailIndex_Emailer =EmailIndex.x, eid_Recipient = eid.y,fullName_Recipient = fullName.y, status_Recipient = status.y, EmailIndex_Recipient =EmailIndex.y)                         

AllMessage_AllSender_AllReceivers_wContent <- full_join(AllMessage_AllSender_AllReceivers,referenceinfo, by = c("mid" = "mid")) %>% 
  filter(year(date)>=1999 & year(date)<=2002)     



#Adding a column to indicate if the sender/receiver was a user or not
#Adding a column to identify the user status involved in an exchange
#Adding a column to help group data by month date if needed
AllMessage_AllSender_AllReceivers_wContent$date <- as.Date(AllMessage_AllSender_AllReceivers_wContent$date, "%Y-%m-%d")

CompleteDataset <- AllMessage_AllSender_AllReceivers_wContent %>% 
  mutate(ExchangeCat = case_when(
    is.na(eid_Emailer) & is.na(eid_Recipient) ~ "NON user - NON user",
    !is.na(eid_Emailer) & is.na(eid_Recipient) ~ "user - NON user",
    is.na(eid_Emailer) & !is.na(eid_Recipient) ~ "NON user - user",
    !is.na(eid_Emailer) & !is.na(eid_Recipient) ~ "user - user"
  ))%>%
  mutate(ExchangeStatus = paste(status_Emailer,status_Recipient,sep = "-"))%>%
  mutate(month_year = floor_date(date, "month"))




#----------------------TABLE AND VARIABLE: related to content exploration ---------------------------


WordOfInterest <- "fraud|bankrupcy|layoffs|debt|lawsuit|litigation|resignation" # _Inserting to be research_

ContentCompleteDataset <- CompleteDataset %>%
  filter(!is.na(reference)) %>%  
  mutate(
    word_exist = str_detect(tolower(reference), WordOfInterest), #binary column detecting the existence of the word
    word_count = str_count(tolower(reference), WordOfInterest), # counting how many times the terms appears
    word_extract = str_extract(tolower(reference), WordOfInterest) #extracting the first word found
  ) 


ContentCompleteDataset_TRUE <- ContentCompleteDataset %>%
  filter(word_exist == TRUE)




# Define server logic required to draw a histogram
function(input, output, session) {
  
#---------------------------  PANE 1: USER SENDER --------------------------------------
  
  
  
    #---------- VISUAL ON PANE 1: Interactive table with top user sender 

    output$TopActiveEmailers_ <- renderTable({

      MsgByEid <- EmployeeMessageSent %>% group_by(fullName, status) %>% summarise(TotalMessageSent = n()) %>% arrange(desc(TotalMessageSent))
      LastSent <- EmployeeMessageSent %>% select (fullName,date) %>% group_by(fullName) %>% mutate (LatestEmail= max(date))  %>% distinct(fullName, LatestEmail, .keep_all = FALSE)
      TopSender_LatestEmailSent<-merge(MsgByEid,LastSent) %>% arrange(desc(TotalMessageSent))
      TopSender_LatestEmailSent_bystatus <- if (input$statuslist == "All") {head(TopSender_LatestEmailSent, input$bins)} else {head(filter(TopSender_LatestEmailSent, status == input$statuslist),input$bins)}
      TopSender_LatestEmailSent_bystatus_ranked <- cbind(Rank=seq(1,nrow(TopSender_LatestEmailSent_bystatus)),TopSender_LatestEmailSent_bystatus)
      TopSender_LatestEmailSent_bystatus_ranked
      },striped = TRUE)
    
    output$TopActiveEmailersTitle_ <- renderUI({
      topCount <- input$bins
      maxCount <- min(topCount, 20)
      status_sender <-input$statuslist
      
      title <- paste("Top", maxCount, status_sender," Senders")
      tags$h3(title)  
    })
      

    #---------- VISUAL ON PANE 1 : Pie Chart message distribution by status

    
    layout(matrix(1:2,2,1))
    output$MsgByStatus_ <- renderPlot({
      
        ggplot(data = MsgByStatus, aes(x = '', y = TotalSent, fill = status)) +
        geom_bar(stat = 'identity', width = 1) +  # Create bars to form a pie
        geom_text(aes(label = paste0(TotalSentPercentage, '%')), 
                  position = position_stack(vjust = 0.5), color = 'black', size = 6) +  # Add percentage labels
        coord_polar(theta = 'y') +  # Turn the bar chart into a pie chart
        theme_void() +  # Remove background
        scale_fill_brewer(palette = "Paired") +  # Choose a color palette for better readability
        theme(legend.title = element_blank(),  # Remove legend title for clarity
              legend.text = element_text(size = 12))  # Adjust legend text size

      
    })

    # ----------- VISUAL ON PANE 1 : Table of message distribution
    
    output$TableMsgByStatus_ <- renderTable({
      TableMsgByStatus <- cbind(c("STATUS", "TOTAL MSG SENT", "TOTAL SENT IN %"),(t(MsgByStatus)))
      TableMsgByStatus
    }, bordered=TRUE)
    
    # ----------- VISUAL ON PANE 1 : Table of status distribution     
    
    output$TableStatusSummary_ <- renderTable({
      status_summary <- employeelist %>%
        group_by(status) %>%
        summarise(count = n()) %>%
        mutate(status_percentage = round((count / sum(count)) * 100,2))
      status_summary <- cbind(c("STATUS", "NB USERS", "USERS IN %"),(t(status_summary)))
      status_summary
      
    }, bordered=TRUE)
  

              
  #---------------------------  PANE 2: USER SENDER ----------------------------------------- 
    
    
    # ----------- VISUAL 1 ON PANE 2 : Interactive table with top user recipient
    
    
    output$TopActiveRecipient_<- renderTable({
      
      
      MsgByUserRecipient<- UserRecipient %>% group_by(fullName, status)%>% summarise(TotalReceived = n(), .groups = "keep")%>% arrange(desc(TotalReceived))
      TopRecipient_bystatus <- if (input$statuslist_recip == "All") {head(MsgByUserRecipient, input$bins_recip)} else {head(filter(MsgByUserRecipient, status == input$statuslist_recip),input$bins_recip)}
      TopRecipient_bystatus_ranked <- cbind(Rank=seq(1,nrow(TopRecipient_bystatus)),TopRecipient_bystatus)
      TopRecipient_bystatus_ranked
    },striped = TRUE)
    
    output$TopActiveRecipientTitle_ <- renderUI({
      topCount_recip <- input$bins_recip
      maxCount_recip <- min(topCount_recip, 20)
      status_recip <- input$statuslist_recip
      
      title <- paste("Top", maxCount_recip, status_recip, " Recipients")
      tags$h3(title)  
    })
    
    
    # ----------- VISUAL 2 ON PANE 2 : Histogram message received
    
    
     output$Histo_Msg_Received_ <- renderPlot({
      ggplot(data=MsgByUserRecipient)+
        geom_histogram(aes(x=TotalReceived),fill='lightblue')+
        ggtitle("Message Received Frequency")
      
 
    })
     
     
     # ----------- VISUAL 3 ON PANE 2 : Stacked bar Rtype by status
     
     
     MsgRtypeByUserStatus<- UserRecipient %>% 
       group_by(status, rtype)%>% 
       summarise(TotalReceived = n())
     
     output$StackedBar_Status_Rtype_ <- renderPlot({
       ggplot(MsgRtypeByUserStatus, aes(x = status, y = TotalReceived, fill = rtype)) +
         geom_bar(stat = "identity") +
         labs(x = "Status", y = "TotalReceived") +
         theme_minimal() +
         scale_fill_brewer(palette = "Greens")+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
       
       
     })
     
    
    
    
  #---------------------------  PANE 3: SENT VS RECEIVED --------------------------------------
     
     # ----------- VISUAL 1 ON PANE 3 (LEFT SIDE - visible choice Bar Plot) :
     
     StatusTable_long2 <- StatusTable %>%
       pivot_longer(
         cols = c(TotalReceived, TotalSent),
         names_to = "Metric",
         values_to = "Amount"
       )
     
     
     # Create the grouped bar plot with dodged bars
     TotalvsReceivedPlot<- ggplot(StatusTable_long2, aes(x = factor(status), y = Amount, fill = Metric)) +
       geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +  
       labs(
         title = "Total Send vs Total Received by Status",
         x = "Status", 
         y = "Total Messages", 
         fill = "Metric"
       ) +
       theme_minimal()+
       scale_fill_brewer(palette = "Set2")+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
     
     
     
     # ----------- VISUAL 1BIS ON PANE 3 (LEFT SIDE - visible choice Box Plot) :
     
     
     MsgByEid <- EmployeeMessageSent %>% group_by(fullName, status) %>% summarise(TotalMessageSent = n()) %>% arrange(desc(TotalMessageSent))
     
     SentBoxplot_status <- ggplot(data=MsgByEid)+ geom_boxplot(aes(x=factor(status),y=MsgByEid$TotalMessageSent))+
       ylim(0,6500) + 
       theme(axis.text.x = element_text(angle = 45, hjust = 1))+
       labs(x = "Status", y = "Total Messages Sent")
     
     
     
     
     # ----------- CODE FOR LEFT SIDE 

     output$leftplot_ <- renderPlot({
       if (input$Bar_or_Box == 1) {TotalvsReceivedPlot} else {SentBoxplot_status}
       
     })
     
     output$leftplotTitle_ <- renderText({
       if (input$Bar_or_Box == 1) {"Total Send vs Total Received by Status"} else {"Distribution of Total Messages Sent by Status"}
       })
     

     # ----------- VISUAL 2 ON PANE 3 (RIGHT SIDE - visible choice Bar Plot)

     # Reshape the data to long format
     StatusTable_long <- StatusTable %>%
       pivot_longer(
         cols = c(TotalReceivedPercentage, TotalSentPercentage, TotalUserPercentage),
         names_to = "Metric",
         values_to = "Percentage"
       )
     
     # Create the grouped bar plot with dodged bars
     TotalvsReceivedPlot_Percent <- ggplot(StatusTable_long, aes(x = factor(status), y = Percentage, fill = Metric)) +
       geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +  # Use position_dodge
       labs(
         title = "Part of the total in % by Status for each metric",
         x = "Status", 
         y = "Percentage", 
         fill = "Metric"
       ) +
       theme_minimal()+
       scale_fill_brewer(palette = "Set2")+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
     
     
     
     
     
     
     # ----------- VISUAL 2BIS ON PANE 3 (RIGHT SIDE - visible choice Box Plot)
     
     RceivedBoxplot_status <- ggplot(data=MsgByUserRecipient)+ 
       geom_boxplot(aes(x=factor(status),y=MsgByUserRecipient$TotalReceived))+
       ylim(0,12000) + 
       theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
       labs(x = "Status", y = "Total Messages Received")
     
     
     
     # ----------- CODE FOR RIGHT SIDE 
     
     output$rightplot_ <- renderPlot({
       if (input$Bar_or_Box == 1) {TotalvsReceivedPlot_Percent} else {RceivedBoxplot_status}
       
     })
     
     output$rightplotTitle_ <- renderText({
       if (input$Bar_or_Box == 1) {"Part of the total in % by Status for each metric"} else {"Distribution of Total Messages Received by Status"}
     })
     
     
     
     
   
  #---------------------------  PANE 4: TEMPORAL DYNAMIC --------------------------------------
     
     # ----------- VISUAL 1 AND 2 ON PANE 4 (General Trend and General Trend by Month-Date) :
     
     
     
     
     # Calculate the number of unique `mid` per date
     trend_message <-
       CompleteDataset %>%
       group_by(date) %>%
       summarize(unique_mids = n_distinct(mid), .groups = 'drop')%>%  # Count unique `mid`
       arrange(desc(unique_mids))
       

     # Calculate the number of unique `mid` per month-year
 
      trend_message_byMD <-
       trend_message %>%
       mutate(month_year = floor_date(date, "month")) %>%
       group_by(month_year) %>%  # Group by month_year
       summarize(total_unique_mids = sum(unique_mids), .groups = 'drop')  %>%
       arrange(desc(total_unique_mids))


     MsgTrendbyDate <- ggplot(data = trend_message, aes(x = date, y = unique_mids)) +
       geom_line(color = "blue", linewidth = 1) +
       scale_x_date(
         date_labels = "%b %Y",  
         date_breaks = "4 month" 
       ) +
       labs(
         title = "Trend of Messent Sent Over Time",
         x = "Date (Day level)",
         y = "Number of Unique MID"
       ) +
       theme_minimal() +
       theme(
         axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels
         ,
         plot.margin = margin(t = 20, r = 20, b = 80, l = 20)  # Increase bottom margin
       )
     
     
     
     MsgTrendbyMonthYear <- ggplot(data = trend_message_byMD, aes(x = month_year, y = total_unique_mids)) +
       geom_line(color = "blue", linewidth = 1) +
       labs(
         title = "Trend of Messent Sent Over Time",
         x = "Date (Month level)",
         y = "Number of Unique MID"
       ) +
       theme_minimal() +
       theme(
         axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate x-axis labels
         ,
         plot.margin = margin(t = 20, r = 20, b = 80, l = 20)  # Increase bottom margin
       )
     
  
     # ----------- VISUAL 3 PANE 4 - Genereal Trend Month Year by status :

     
     StatusEmailingOut <- CompleteDataset %>%
       filter(ExchangeCat=="user - NON user" & EmailIndex_Emailer == "Email_id")%>%
       group_by (month_year,status_Emailer)%>%
       summarize(SentTo_NONuser= n(),.groups = "keep")
     StatusEmailingOut
     
     # Plot using the summarized data
     PlotStatusEmailingOut <- ggplot(data = StatusEmailingOut, aes(x = month_year, y = SentTo_NONuser, color = status_Emailer, group = status_Emailer)) +
       geom_line(size = 1) +  # Use lines to show trends
       scale_x_date(
         date_labels = "%b %Y",  # Format x-axis labels as "Month Year"
         date_breaks = "2 month", # Break x-axis by every month
         limits = c(as.Date("2000-01-01"), as.Date("2002-05-31"))  # Set date limits
       ) +
       labs(
         title = "Email sent to non registered users Over Time by status_Emailer",
         x = "Month-Year",
         y = "Email Sent to NON-users",
         color = "Emailer Status"
       ) +
       theme_minimal() +
       theme(
         axis.text.x = element_text(angle = 80, hjust = 1)  # Rotate x-axis labels for readability
       )
     
     
     # ----------- VISUAL 4 PANE 4 - Genereal Trend Month Year by status :
     

     ExchangeCat_trend <- CompleteDataset %>%
       group_by(month_year, ExchangeCat) %>%
       summarize(count = n(), .groups = 'drop') %>%
       mutate(percent = count / sum(count) * 100)  # Calculate percentages
     
     
     # Plot using the transformed data
     PlotExchangeCat_trend <- ggplot(data = ExchangeCat_trend, aes(x = month_year, y = percent, fill = ExchangeCat)) +
       geom_bar(stat = "identity", position = "fill") +  # 100% stacked bar chart
       scale_x_date(
         date_labels = "%b %Y",  # Format x-axis labels as "Month Year"
         date_breaks = "1 month",  # Break x-axis by every month
         limits = c(as.Date("2000-01-01"), as.Date("2002-05-31"))  # Set date limits
       ) +
       labs(
         title = "100% Stacked Bar Chart of Exchange Categories Over Time",
         x = "Date",
         y = "Percentage",
         fill = "Exchange Category"
       ) +
       theme_minimal() +
       theme(
         axis.text.x = element_text(angle = 75, hjust = 1),  # Rotate x-axis labels for readability
         legend.position = "top"  # Position legend at the top
       )

     
     # ----------- CODE FOR PANE 4  :
     
     output$TemporalDynamicsTrends_Title <- renderText({ 
       if (input$SelectTrend == "GTD") {
         "Message sent by Date"
       } else if (input$SelectTrend == "GTMY") {
         "Message sent monthly aggregated"
       } else if (input$SelectTrend == "SEO") {
         "Trend of Message sent from user to non-user by sender status"
       } else if (input$SelectTrend == "NUNUEX") {
         "Composition of the exchanges over time"
       }
       })

     output$TemporalDynamicsTrends_ <- renderPlot({
       if (input$SelectTrend == "GTD") {
         MsgTrendbyDate
       } else if (input$SelectTrend == "GTMY") {
         MsgTrendbyMonthYear
       } else if (input$SelectTrend == "SEO") {
         PlotStatusEmailingOut
       } else if (input$SelectTrend == "NUNUEX") {
         PlotExchangeCat_trend
       }
     })
     
  #---------------------------  PANE 5: INTERACTION DYNAMIC --------------------------------------
     
     StatusVSStatusData <- CompleteDataset %>%
       # Convert month_year to Date if it is not already
       mutate(month_year = as.Date(month_year, format = "%Y-%m-%d")) %>%  
       group_by(month_year, status_Emailer, status_Recipient) %>%
       filter(
         status_Emailer %in% c("President", "CEO", "Vice President", "Director","N/A","Manager","Employee")  &
           status_Recipient %in% c("President", "CEO", "Vice President", "Director", "N/A","Manager","Employee") &
           month_year >= as.Date("2001-01-01") &  
           month_year <= as.Date("2002-06-30")    
       ) %>%
       summarise(count = n(), .groups = 'keep')
     
     output$Grid_StatusVSStatusData <- renderPlot({
     
       ggplot(data=StatusVSStatusData)+
       geom_point(aes(x = month_year, y=count))+
       geom_line(aes(x = month_year, y=count))+
       facet_grid(status_Emailer~status_Recipient)+
       scale_y_continuous(
         limits = c(0, 300),  # Set the y-axis limits from 0 to 300
         expand = c(0, 0)     # Prevent additional space beyond the limits
       ) +
       labs(
         title = "Interaction From Status_Emailer To Status_Recipient Over Time",
         x = "Month-Year",  # Label for x-axis
         y = "Count"        # Label for y-axis
       ) +
       theme_minimal() +
       theme(
         plot.title = element_text(hjust = 0.5, face = "bold"),# Center and bold the title
         axis.text.x = element_text(angle = 45, hjust = 1)
            ) # Rotate x-axis labels by 45 degrees
     
     })
     
     
     
  #---------------------------  PANE 6: CONTENT EXPLORATION --------------------------------------
     
     # ----------- VISUAL 1 ON PANE 6 (Distribution fo extracted world)   
     
     WordPieData <- ContentCompleteDataset_TRUE %>% 
       group_by(word_extract) %>% 
       summarise(WordExtractedCount = n()) %>%
       arrange(desc(WordExtractedCount))%>% 
       mutate(WordExtractedCountPercentage= round(WordExtractedCount/sum(WordExtractedCount)*100))
     
     
     
     # Pie representation of the distribution**
     
     output$PlotWordPieData <- renderPlot({
     ggplot(data = WordPieData, aes(x = '', y = WordExtractedCount, fill = word_extract)) +
       geom_bar(stat = 'identity', width = 1) +  # Create bars to form a pie
       geom_text(aes(label = paste0(WordExtractedCountPercentage, '%')), 
                 position = position_stack(vjust = 0.5), color = 'black', size = 5) +  # Add percentage labels
       coord_polar(theta = 'y') +  # Turn the bar chart into a pie chart
       theme_void() +  # Remove background
       scale_fill_brewer(palette = "Set3") +  # Choose a color palette for better readability
       theme(legend.text = element_text(size = 10))
     
     })
     
 
     
     # ----------- VISUAL 2 ON PANE 6 (Bar - Words of interest trend over time)  
     
     WordLineTrendData <- ContentCompleteDataset_TRUE %>%
       select(month_year, word_extract) %>%
       group_by(month_year, word_extract) %>%
       mutate(WordExtractedCount = n()) %>%
       arrange(month_year) %>%
       distinct(month_year, word_extract, .keep_all = TRUE)
     
     output$PlotWordLineTrendData <- renderPlot({
       ggplot(WordLineTrendData, aes(x = month_year, y = WordExtractedCount, fill = word_extract)) +
       geom_bar(stat = "identity") +
       scale_x_date(
         date_labels = "%b %Y",  # Format x-axis labels as "Month Year"
         date_breaks = "3 month", # Break x-axis by every month
         limits = c(as.Date("2000-01-01"), as.Date("2002-05-31"))  # Set date limits
       ) +
       labs(x = "Word of interest", y = "Occurence") +
       theme_minimal() +
       scale_fill_brewer(palette = "Set3")+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
     
     })
    
    
}
