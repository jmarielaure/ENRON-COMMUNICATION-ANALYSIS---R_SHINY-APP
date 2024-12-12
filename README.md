# ENRON-COMMUNICATION-ANALYSIS---R_SHINY-APP
Analyzing Enron's employees emails around the time of  the investigation for fraud and money laudering. Creating a related shiny App


## CONTEXT 
Enron data set, which contains all email communications between 149 employees of the  company from 1999 to 2002.
This specific time window  is the denser period in term of sent emails and corresponds to a critical
period for the company. Indeed, after the announcement early September 2001 that the company
was “in the strongest and best shape that it has ever been in”, the Securities and Exchange
Commission (SEC) opened an investigation on October, 31th for fraud and the company finally
filed for bankruptcy on December, 2nd, 2001. 
By this time, it was the largest bankruptcy in U.S. history and resulted in more than 4,000 lost jobs.

## OBJECTIVE 
Exploratory analysis of the emails in relation to the events using R to perform statistical analysis 
and visualize the findings in a Shiny App

## LEARNINGS: 
- Statisctical analysis with R language
- Writing R language
- Creating visualizations 
- Creating a shiny App ( redaction of a ui and server file)


## DATASET
The data set contains 4 different relational databases:

  - employeelist: the list of the Enron employees and their email addresses, and their occupation within the company
  - message: all emails exchanged between 1999 and 2002
  - recipientinfo: the recipients (TO, CC, BCC) of each message
  - referenceinfo: the object and content of each message

## PROJECT CONTENT 
### R markdown analysis 
In this analysis divided in 4 part , we answered questions related to :
- the communication activity ( main email senders/recipient, distribution of mail sendings, usage of secondary email adresses )
- the status influence on different metrics
- The evolution of communication over time in correlation with major events such as start of investigation or shareholder equity reduction
- a basic analysis content of these emails
All analysis and chart were commented. 

### Shiny App : ui and server file 
Relevant chars were selected to create a Shiny App based on the 4 part analysis. 

## AREAS OF IMPROVEMENT AND LIMITATIONS 
Many messages were sent / received by person out of the organisation on which we had no information about. 
3 last pane of the shniy app could be done by using dynamic raph to allow a more self-service analysis oriented application 


