#Script of the Rshiny app to display the key visualization of the Enron company email analysis.

#libraries
library(shinydashboard)
library(shinyWidgets)
library(shiny)
library(tidyverse)
library(circlize)
library(ggpubr)
library(patchwork)
library(gridExtra)
library(grid)
library(gtable)
library(ggbreak)
library(knitr)


### the global

#data loading
load(file = "C:/Users/marie/Documents/DSTI_Cours/R_big_Data/Exam/Enron_project/Enron.Rdata")

#data cleaning
employeelist_2 <- employeelist %>% 
  select(-c(Email2, EMail4)) %>% #the variable we don't need in the data
  transform(eid = as.factor(eid)) %>% #data type change for the variable eid to factor
  mutate(status = if_else((status == "N/A"), NA, status)) #homogenized the declaration of the NA in the variable status

message_2 <- message %>%
  select(-c(message_id)) %>% #withdraw the variable we don't need
  transform(#change the data type for factor
    mid = as.factor(mid),
    sender = as.factor(sender),
    subject = as.factor(subject)) %>%
  #add the year variable in the table from the date
  mutate(year = as.factor(format(date, "%Y"))) %>% 
  #filter to keep only the date from 1999 to 2002
  filter(year %in% c(1999 : 2002)) %>% #drop the year variable which is no more useful in the data
  select(-year)

recipientinfo_2 <- recipientinfo %>%
  #change the variable data type for factor
  transform(rid = as.factor(rid),
            rvalue = as.factor(rvalue),
            mid = as.factor(mid))

referenceinfo_2 <- referenceinfo %>%
  #change the variable data type for factor
  transform(rfid = as.factor(rfid),
            mid = as.factor(mid))

#Merged dataframe
#first between the recipientinfo_2 and message_2
df_message <- left_join(recipientinfo_2, message_2, #the table we merged 
                        by = "mid") #the variable used for merging

#then between the new table df_message and the referenceinfo_2
df_message <- left_join(df_message, referenceinfo_2,
                        by = "mid")

df_message <- df_message %>% filter( #withdrawn the lines where we have a missing value for the sender, date, and subject variables.
  is.na(sender) == FALSE,
  is.na(subject) == FALSE,
  is.na(date) == FALSE)

employee_merge_final <- employeelist_2 %>% 
  select(Email_id, status) %>% #keep only the variables we need
  mutate(status_sender = status) %>% #rename the status variable to know to who is attached the status
  select(-status)

#merged with the df_message table 
df_message_status <- left_join(df_message, employee_merge_final, 
                               join_by(sender == Email_id))

employee_merge_final2 <- employeelist_2 %>% 
  select(Email3, status) %>% #keep only the variables we need
  mutate(status_sender_email3 = status) %>% #rename the status variable to know to who is attached the status
  select(-status)

#merged with the df_message table 
df_message_status <- left_join(df_message_status, employee_merge_final2, 
                               join_by(sender == Email3))

df_message_status <- df_message_status %>% mutate(
  #replace the NA value in the variable by the value in the 2nd variable
  status_sender = if_else((is.na(status_sender) == TRUE), status_sender_email3, status_sender)) %>% select(-status_sender_email3) #drop the variable

employee_merge_final_recipient <- employeelist_2 %>% 
  select(Email_id, status) %>% #keep only the variables we need
  mutate(status_recipient = status) %>% #rename the status variable to know to who is attached the status
  select(-status)

#merged with the df_message table 
df_message_status <- left_join(df_message_status, employee_merge_final_recipient, 
                               join_by(rvalue == Email_id))

employee_merge_final_recipient2 <- employeelist_2 %>% 
  select(Email3, status) %>% #keep only the variables we need
  mutate(status_recipient_email3 = status) %>% #rename the status variable to know to who is attached the status
  select(-status)

#merged with the df_message table 
df_message_status <- left_join(df_message_status, employee_merge_final_recipient2, 
                               join_by(rvalue == Email3))

df_message_status <- df_message_status %>% mutate(
  #replace the NA value in the variable by the value in the 2nd variable
  status_recipient = if_else((is.na(status_recipient) == TRUE), status_recipient_email3, status_recipient)) %>% 
  select(-status_recipient_email3) #drop the variable

df_message_status <- df_message_status %>% 
  #withdraw the variable which are identifier
  select(-c(mid, rfid, rid)) %>%
  #change the name of the recipient email variable
  mutate(recipient = rvalue,
         year = format(date,"%Y"), 
         month = format(date, "%m")) %>% 
  transform( #to put the variable in wright type
    year = as.factor(year),
    month = as.factor(month)) %>%
  #order the different variable
  select(date, year, month, sender, status_sender, rtype, recipient, status_recipient, subject, reference)


#Kept only what is needed for the app to create the visualization
rm(employee_merge_final_recipient2, employee_merge_final_recipient, employee_merge_final2, employee_merge_final, employeelist, message, message_2,df_message, recipientinfo, 
   recipientinfo_2, referenceinfo, referenceinfo_2)

#for visualize the email flux between the different worker status
per_year <- df_message_status %>% select(date, year, status_sender, status_recipient) %>%
  filter(!is.na(status_sender) & !is.na(status_recipient)) %>%
  mutate(#to enhance the clarity we group certain status with similar level of responsability together
         status_sender = case_when(
           status_sender %in% c("Managing Director", "Manager", "Director") ~ "Manger - Director",
           status_sender %in% c("CEO", "Vice President", "President") ~ "CEO - President",
           .default = status_sender),
         status_recipient = case_when(
           status_recipient %in% c("Managing Director", "Manager", "Director") ~ "Manger - Director",
           status_recipient %in% c("CEO", "Vice President", "President") ~ "CEO - President",
           .default = status_recipient)) %>%
  group_by(date,status_sender, status_recipient) %>%
  mutate(number_exchange = n()) %>% ungroup() %>%
  distinct(date, status_sender, status_recipient, number_exchange, year)

#For visualize and compare the most active employee to the other member of his group and the total Enron worker
#count the number of email send by jeff dasovich per day
jeff_stat_send <- df_message_status %>% filter(sender == "jeff.dasovich@enron.com") %>%
  #we count the number of different email subject send per day
  group_by(date, subject) %>% 
  summarise(email_count = n(), .groups = "drop") %>%
  mutate(source = "Jeff Dasovich") %>% transform(source = as.factor(source))

#count the number of email send by Enron's worker per day
sender_stat <- df_message_status %>% 
  #we count the number of different email subject send per day by each sender
  group_by(date, sender, subject) %>% 
  summarise(email_count = n(), .groups = "drop") %>%
  mutate(source = "Enron's worker") %>% select(-sender) %>% transform(source = as.factor(source))

#count the number of email send by Employee status per day
statuts_stat_send <- df_message_status %>% filter(status_sender == "Employee") %>% 
  #we count the number of different email subject send per day by each sender of status employee
  group_by(date, sender, subject) %>% 
  summarise(email_count = n(), .groups = "drop") %>%
  mutate(source = "Employee status") %>% transform(source = as.factor(source))

#statistics on the jeff dasovich email receive per day
jeff_stat_rec <- df_message_status %>% filter(recipient == "jeff.dasovich@enron.com") %>%
  group_by(date) %>% 
  summarise(email_count = n(), .groups = "drop") %>%
  mutate(source = "Jeff Dasovich") %>% transform(source = as.factor(source))

#statistics on the email send per day by the enron's worker
recipient_stat <- df_message_status %>% group_by(date, recipient) %>% 
  summarise(email_count = n(), .groups = "drop") %>%
  mutate(source = "Enron's worker") %>% select(-recipient) %>% transform(source = as.factor(source))

#statistics on the email send per day by the enron's worker who have an employee statuts
statuts_stat_rec <- df_message_status %>% filter(status_recipient == "Employee") %>% group_by(date) %>% 
  summarise(email_count = n(), .groups = "drop") %>%
  mutate(source = "Employee status") %>% transform(source = as.factor(source))

#to study the email subject and content
#topics list 

topic_meeting <- c("message|origin|pleas|email|thank|attach|file|copi|inform|receiv|thank|all|time|meet|look|week|day|dont|vinc|talk")

topic_business_process <- c("enron|deal|agreement|chang|contract|corp|fax|houston|date|america|risk|analy|confidential|correction")

topic_core_business <- c("market|gas|price|power|company|energy|trade|busi|servic|manag")

topic_enron_event <- c("bankrup|SEC|MTM|fear")

#for the analyse of email subject and content
email_subject_send <- df_message_status %>% distinct(date, year, month, sender, status_sender, subject, reference) %>%
  mutate(#count the number of email which contain at least one word in the list of each topic
    topic_meeting = if_else(str_detect(subject, topic_meeting), 1, 0),
    topic_business_process = if_else(str_detect(subject, topic_business_process), 1, 0),
    topic_core_business = if_else(str_detect(subject, topic_core_business), 1, 0),
    topic_enron_event = if_else(str_detect(subject, topic_enron_event), 1, 0),
    email_mark_to_market = if_else(str_detect(reference,"mark-to-market"), 1, 0),
    email_10K_report = if_else(str_detect(reference, "10-K"), 1, 0),
    email_losing_money = if_else(str_detect(reference, "losing money"), 1, 0),
    email_SEC_investigation = if_else(str_detect(reference, "SEC"), 1, 0),
    email_fear_feeling = if_else(str_detect(reference, "fears"), 1, 0),
    email_correction = if_else(str_detect(reference,"correction"),1,0),
    email_bankruptcy = if_else(str_detect(reference, "bankruptcy"), 1, 0),
    #to get the date in year/month
    year_month = as.Date(paste0(year,"-",month,"-01")))

email_subject_rec <- df_message_status %>% distinct(date, year, month, recipient, status_recipient, subject, reference) %>%
  mutate(#count the number of email which contain at least one word in the list of each topic
    topic_meeting = if_else(str_detect(subject, topic_meeting), 1, 0),
    topic_business_process = if_else(str_detect(subject, topic_business_process), 1, 0),
    topic_core_business = if_else(str_detect(subject, topic_core_business), 1, 0),
    topic_enron_event = if_else(str_detect(subject, topic_enron_event), 1, 0),
    email_mark_to_market = if_else(str_detect(reference,"mark-to-market"), 1, 0),
    email_10K_report = if_else(str_detect(reference, "10-K"), 1, 0),
    email_losing_money = if_else(str_detect(reference, "losing money"), 1, 0),
    email_SEC_investigation = if_else(str_detect(reference, "SEC"), 1, 0),
    email_fear_feeling = if_else(str_detect(reference, "fears"), 1, 0),
    email_correction = if_else(str_detect(reference,"correction"),1,0),
    email_bankruptcy = if_else(str_detect(reference, "bankruptcy"), 1, 0),
    #to get the date in year/month
    year_month = as.Date(paste0(year,"-",month,"-01")))

#study the email send per worker status
status_email_subject_send <- email_subject_send %>%
  #we focus on the worker which their status are know
  filter(!is.na(status_sender)) %>%
  #compute the sum of each topics for each year studied
  group_by(year_month, status_sender) %>%
  mutate(
    sum_topic_meeting = sum(topic_meeting),
    sum_topic_business_process = sum(topic_business_process),
    sum_topic_core_business = sum(topic_core_business),
    sum_topic_enron_event = sum(topic_enron_event),
    #for the email we use na.rm = TRUE to allow the sum to be done
    sum_email_mark_to_market = sum(email_mark_to_market, na.rm = TRUE),
    sum_email_10K_report = sum(email_10K_report, na.rm = TRUE),
    sum_email_losing_money = sum(email_losing_money, na.rm = TRUE),
    sum_email_SEC_investigation = sum(email_SEC_investigation, na.rm = TRUE),
    sum_email_fear_feeling = sum(email_fear_feeling, na.rm = TRUE),
    sum_email_correction = sum(email_correction, na.rm = TRUE),
    sum_email_bankruptcy = sum(email_bankruptcy, na.rm = TRUE)
  ) %>% ungroup() %>%
  #keep one line per year
  distinct(year_month, status_sender, sum_topic_meeting, sum_topic_business_process, sum_topic_core_business, sum_topic_enron_event, sum_email_mark_to_market,
           sum_email_10K_report,
           sum_email_losing_money,
           sum_email_SEC_investigation,
           sum_email_fear_feeling,
           sum_email_correction,
           sum_email_bankruptcy)

#pivot the data frame
status_email_subject_send <- status_email_subject_send %>%
  pivot_longer(
    cols = 3:length(status_email_subject_send),
    names_to = "topic_email",
    values_to = "value")


#study the email received per status
status_email_subject_rec <- email_subject_rec %>%
  #we focus on the worker which their status are know
  filter(!is.na(status_recipient)) %>%
  #compute the sum of each topics for each year studied
  group_by(year_month, status_recipient) %>%
  mutate(
    sum_topic_meeting = sum(topic_meeting),
    sum_topic_business_process = sum(topic_business_process),
    sum_topic_core_business = sum(topic_core_business),
    sum_topic_enron_event = sum(topic_enron_event),
    #for the email we use na.rm = TRUE to allow the sum to be done
    sum_email_mark_to_market = sum(email_mark_to_market, na.rm = TRUE),
    sum_email_10K_report = sum(email_10K_report, na.rm = TRUE),
    sum_email_losing_money = sum(email_losing_money, na.rm = TRUE),
    sum_email_SEC_investigation = sum(email_SEC_investigation, na.rm = TRUE),
    sum_email_fear_feeling = sum(email_fear_feeling, na.rm = TRUE),
    sum_email_correction = sum(email_correction, na.rm = TRUE),
    sum_email_bankruptcy = sum(email_bankruptcy, na.rm = TRUE)
  ) %>% ungroup() %>%
  #keep one line per year and month
  distinct(year_month, status_recipient, sum_topic_meeting, sum_topic_business_process, sum_topic_core_business, sum_topic_enron_event, sum_email_mark_to_market,
           sum_email_10K_report,
           sum_email_losing_money,
           sum_email_SEC_investigation,
           sum_email_fear_feeling,
           sum_email_correction,
           sum_email_bankruptcy)

#pivot the data frame
status_email_subject_rec <- status_email_subject_rec %>%
  pivot_longer(
    cols = 3:length(status_email_subject_rec),
    names_to = "topic_email",
    values_to = "value")


#for the analyze of the person imply in the Enron event
person_of_interest_send <- email_subject_send %>%
  filter(str_detect(sender,"jeff.dasovich|andrew.baker|tim.belden|andrew.fastow|lfastow|vkaminski|jordan.mintz|jeff.skilling|sherron.watkins|richard.causey|greg.whalley")) %>%
  mutate(
    #identify the person who sent the email
    email_label_sender = case_when(
      sender == "jeff.dasovich@enron.com" ~ "Jeff Dasovich",
      sender == "kenneth.lay@enron.com" ~ "Kenneth Lay",
      sender == "jeff.skilling@enron.com" ~ "Jeffrey Skilling",
      sender == "andrew.baker@enron.com" ~ "Andrew Baker",
      sender == "tim.belden@enron.com" ~ "Timothy Belden", 
      sender %in% c("lfastow@pop.pdq.net", "lfastow@pdq.net") ~ "Lea Fastow",
      sender == "andrew.fastow@enron.com" ~ "Andrew Fastow",
      sender %in% c("vkaminski@enron.com", "vkaminski@aol.com", "vkaminski@palm.net") ~ "Vincent Kaminski",
      sender == "jordan.mintz@enron.com" ~ "Jordan Mintz",
      sender == "sherron.watkins@enron.com" ~ "Sherron Watkins",
      sender == "richard.causey@enron.com" ~ "Richard Causey", #chief account officer wikipedia source
      sender == "greg.whalley@enron.com" ~ "Greg Whalley", #president and COO of Enron wholesale service
      .default = sender)) %>% 
  #to compute the number of email sent in each topics by the person whose are directly involved in the Enron scandal
  group_by(year, email_label_sender) %>%
  mutate(#compute the sum for each group
    sum_topic_meeting = sum(topic_meeting),
    sum_topic_business_process = sum(topic_business_process),
    sum_topic_core_business = sum(topic_core_business),
    sum_topic_enron_event = sum(topic_enron_event),
    #for the email we use na.rm = TRUE to allow the sum to be done
    sum_email_mark_to_market = sum(email_mark_to_market, na.rm = TRUE),
    sum_email_10K_report = sum(email_10K_report, na.rm = TRUE),
    sum_email_losing_money = sum(email_losing_money, na.rm = TRUE),
    sum_email_SEC_investigation = sum(email_SEC_investigation, na.rm = TRUE),
    sum_email_fear_feeling = sum(email_fear_feeling, na.rm = TRUE),
    sum_email_correction = sum(email_correction, na.rm = TRUE),
    sum_email_bankruptcy = sum(email_bankruptcy, na.rm = TRUE)) %>% ungroup() %>%
  #keep one line per year and month
  distinct(year, email_label_sender, sum_topic_meeting, sum_topic_business_process, sum_topic_core_business,sum_topic_enron_event,sum_email_mark_to_market,
           sum_email_10K_report,
           sum_email_losing_money,
           sum_email_SEC_investigation,
           sum_email_fear_feeling,
           sum_email_correction,
           sum_email_bankruptcy) %>% 
  #filter to get only the date with email exchange for at least one of those topics
  filter((sum_topic_business_process != 0)|(sum_topic_meeting != 0)|(sum_topic_core_business !=0)|
           (sum_email_mark_to_market!=0)|(sum_email_10K_report != 0)|
           (sum_email_losing_money!=0)|(sum_email_SEC_investigation!=0)|(sum_email_fear_feeling!=0)|
           (sum_email_correction!=0)|(sum_email_bankruptcy!=0)) 

person_of_interest_send <- person_of_interest_send %>% pivot_longer(
    cols = 3:length(person_of_interest_send),
    names_to = "topic_email",
    values_to = "value")

person_of_interest_reciveid <- email_subject_rec %>%
  filter(str_detect(recipient,"jeff.dasovich|andrew.baker|tim.belden|andrew.fastow|lfastow|vkaminski|jordan.mintz|jeff.skilling|sherron.watkins|richard.causey|greg.whalley")) %>%
  mutate(
    #identify the person who sent the email
    email_label_recipient = 
      case_when(
        recipient %in% c("jeff.dasovich@enron.com","jeff_dasovich@ees.enron.com") ~ "Jeff Dasovich",
        recipient == "kenneth.lay@enron.com" ~ "Kenneth Lay",
        recipient %in% c("jeff.skilling@enron.com","jeff_skilling@enron.com") ~ "Jeffrey Skilling",
        recipient == "andrew.baker@enron.com" ~ "Andrew Baker",
        recipient %in% c("tim.belden@enron.com", "tim_belden@pgn.com") ~ "Timothy Belden",
        recipient %in% c("lfastow@pop.pdq.net", "lfastow@pdq.net") ~ "Lea Fastow",
        recipient %in% c("andrew.fastow@enron.com", "andrew.fastow@ljminvestments.com") ~ "Andrew Fastow",
        recipient %in% c("vkaminski@enron.com", "vkaminski@aol.com","vkaminski@aol .com", "vkaminski@palm.net",
                         "vkaminski@ol.com") ~ "Vincent Kaminski",
        recipient %in% c("jordan.mintz@enron.com","jordan_mintz@enron.com") ~ "Jordan Mintz",
        recipient == "sherron.watkins@enron.com" ~ "Sherron Watkins",
        recipient == "richard.causey@enron.com" ~ "Richard Causey", #chief account officer wikipedia source
        recipient == "greg.whalley@enron.com" ~ "Greg Whalley", #president and COO of Enron wholesale service
        .default = recipient)) %>% 
  #to compute the number of email sent in each topics by the person whose are directly involved in the Enron scandal
  group_by(year, email_label_recipient) %>%
  mutate(#compute the sum for each group
    sum_topic_meeting = sum(topic_meeting),
    sum_topic_business_process = sum(topic_business_process),
    sum_topic_core_business = sum(topic_core_business),
    sum_topic_enron_event = sum(topic_enron_event),
    #for the email we use na.rm = TRUE to allow the sum to be done
    sum_email_mark_to_market = sum(email_mark_to_market, na.rm = TRUE),
    sum_email_10K_report = sum(email_10K_report, na.rm = TRUE),
    sum_email_losing_money = sum(email_losing_money, na.rm = TRUE),
    sum_email_SEC_investigation = sum(email_SEC_investigation, na.rm = TRUE),
    sum_email_fear_feeling = sum(email_fear_feeling, na.rm = TRUE),
    sum_email_correction = sum(email_correction, na.rm = TRUE),
    sum_email_bankruptcy = sum(email_bankruptcy, na.rm = TRUE)) %>% ungroup() %>%
  #keep one line per year and month
  distinct(year, email_label_recipient, sum_topic_meeting, sum_topic_business_process, sum_topic_core_business, sum_topic_enron_event, sum_email_mark_to_market,
           sum_email_10K_report,
           sum_email_losing_money,
           sum_email_SEC_investigation,
           sum_email_fear_feeling,
           sum_email_correction,
           sum_email_bankruptcy) %>%
  #filter to get only the date with email exchange for at least one of those topics
  filter((sum_topic_business_process != 0)|(sum_topic_meeting != 0)|(sum_topic_core_business !=0)|
           (sum_email_mark_to_market!=0)|(sum_email_10K_report != 0)|
           (sum_email_losing_money!=0)|(sum_email_SEC_investigation!=0)|(sum_email_fear_feeling!=0)|
           (sum_email_correction!=0)|(sum_email_bankruptcy!=0))

#pivot the table
person_of_interest_reciveid <-person_of_interest_reciveid %>%
  pivot_longer(
    cols = 3:length(person_of_interest_reciveid),
    names_to = "topic_email",
    values_to = "value"
  )

#the color and label for the different visualisations
status_color <- c(
  "Employee" = "pink",
  "CEO - President" = "orange",
  "Trader" = "springgreen3",
  "Manger - Director" = "violetred4",
  "In House Lawyer" = "purple4")


#the list of category studied and their related color in each plot
topic_colors <- c("sum_email_SEC_investigation" = "darkred",
                  "sum_email_10K_report" = "pink",
                  "sum_email_bankruptcy" = "springgreen4",
                  "sum_email_correction" = "salmon",
                  "sum_email_mark_to_market" = "purple",
                  "sum_email_losing_money" = "turquoise",
                  "sum_email_fear_feeling" = "violetred",
                  "sum_topic_business_process" = "steelblue4",
                  "sum_topic_core_business" = "orchid",
                  "sum_topic_meeting" = "chocolate4",
                  "sum_topic_enron_event" = "yellowgreen")



#the list of category and their related label on the plot  
topic_label <- c("sum_email_SEC_investigation" = "SEC Investigation email",
                 "sum_email_10K_report" = "10-K report email",
                 "sum_email_bankruptcy" = "Bankruptcy email",
                 "sum_email_correction" = "Correction email",
                 "sum_email_mark_to_market" = "mark-to-market process email",
                 "sum_email_losing_money" = "Losing money email",
                 "sum_email_fear_feeling" = "Fear feeling email",
                 "sum_topic_business_process" = "Business process email topic",
                 "sum_topic_core_business" = "Core Business email topic",
                 "sum_topic_meeting" = "Meeting email topic",
                 "sum_topic_enron_event" = "Enron Event")

month_label <- c("01" = "January","02" = "February","03" = "March","04" = "April","05" = "May","06" = "June","07" = "July","08" = "August",
                 "09" = "September","10" = "October","11" = "November","12" = "December")

month_color <- c("01" = "lightgreen","02" = "lightsalmon4","03" = "lightblue","04" = "greenyellow","05" = "cyan","06" = "darkgreen","07" = "lavender",
                 "08" = "plum","09" = "coral","10" = "honeydew4","11" = "hotpink","12" = "indianred")

status_list <- c("Employee", "CEO", "Manager", "Director", "Vice President", "Trader", "President", "Managing Director", "In House Lawyer")

enron_worker_rec <- c("Jeff Dasovich", "Jeffrey Skilling", "Timothy Belden","Lea Fastow","Andrew Fastow",
                  "Vincent Kaminski","Jordan Mintz","Sherron Watkins","Richard Causey", "Greg Whalley")



#### front end UI

#dashboard of the Enron email analysis
ui <- dashboardPage(
  dashboardHeader(
    title = "Enron Company"
  ),
  #the different page in the dashboard
  dashboardSidebar(
    sidebarMenu(id = "tabs",
      menuItem("General", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Email exchange", tabName = "workerExchange", icon = icon("envelope")),
      menuItem("Email content analysis", tabName = "email", icon = icon("file-lines")),
      menuItem("Email analyze per status", tabName = "status", icon = icon("business-time")),
      menuItem("The most active Enron worker", tabName = "active", icon = icon("sun")),
      menuItem("Potential fraud actor", tabName = "actor", icon = icon("circle-exclamation"))),
      #designe the side bar for the status tab
      conditionalPanel(
        #date range slicer
        condition = "input.tabs == 'status'",
        dateRangeInput("date_range",
                       label = "Select the period: ",
                       start = min(email_subject_send$year_month),
                       end = max(email_subject_send$year_month),
                       format = "yyyy-mm",
                       startview = "year"),
        #status selector
        selectizeInput(
          "status_choice", "Select status:",
          choices = unique(status_email_subject_send$status_sender),
          selected = NULL),
      #topic selector
      selectizeInput(
        "subject_choice", "Select subject topic:",
        choices = topic_label,
        selected = topic_label,
        multiple = TRUE)
      )
    ),
  
  #the body of the dashboard
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(
        #name of the tab
        tabName = "dashboard",
              #the title for it
              h4("General information about the company"),
        fluidRow(
          column(width = 9,
                 switchInput(inputId = "show_na", value = FALSE, label = "show NA"),
                 plotOutput("NbWorker"))),
      br(),
              fluidRow(column(
                width = 9,
                selectInput("year", "Select years:", 
                                       choices = sort(unique(df_message_status$year)),
                                       selected = unique(df_message_status$year)[1],
                                       multiple = TRUE),
                           plotOutput("nbEmail")))),
      
      #2nd tab content
      tabItem(tabName = "workerExchange",
              h4("Study of the email exchange between Enron's worker"),
              fluidRow(
              tabBox(width = 6,
                title = "Email send and received visualization",
                id = "tab1", 
                tabPanel("Send", plotOutput("StatusSend")),
                tabPanel("Received", plotOutput("StatusRec"))
              ),
              tabBox(width = 6,
                title = "Email send and received descriptive statistics",
                     id = "tab2",
                     tabPanel("Statistic Send", tableOutput("StatSend")),
                     tabPanel("Statistic Received", tableOutput("StatRec")))),
      br(),
      fluidRow(column(3, h4("Email exchange flux in 1999"), plotOutput("flux1999")),
               column(3, h4("Email exchange flux in 2000"), plotOutput("flux2000")),
               column(3, h4("Email exchange flux in 2002"), plotOutput("flux2001")),
               column(3, h4("Email exchange flux in 2002"), plotOutput("flux2002")))
      ),

      
      # 3rd tab content
      tabItem(tabName = "active",
              h4("The most active email sender of the Enron company, Jeff Dasovich"),
              fluidRow(
                tabBox(width = 12,
                  id = "tab3",
                       tabPanel("Send", plotOutput("Top10Send")),
                       tabPanel("Received", plotOutput("Top10Rec")))),
              fluidRow(
                tabBox(width = 12, id = "tab4",
                  tabPanel("Send", 
                           fluidRow(
                             column(width = 4, plotOutput("JeffStatusSendViz")), 
                           column(width = 4, plotOutput("JeffWorkerSendViz")),
                           column(width = 4, tableOutput("JeffSendStat")))),
                  tabPanel("Received",
                           fluidRow(
                           column(width = 4, plotOutput("JeffRecViz")),
                           column(width = 4, plotOutput("JeffWorkerRecViz")),
                           column(width = 4, tableOutput("JeffRecStat")))))
              )),
      
      # 4th tab content
      tabItem(tabName = "email",
              h4 ("Analyse of the email subject and content about the Enron event and the core business"),
              fluidRow(
                column(12,
                       #this will apply to the both plot
                       dateRangeInput("date_range",
                                      label = "Select the period: ",
                                      start = min(email_subject_send$year_month),
                                      end = max(email_subject_send$year_month),
                                      format = "yyyy-mm",
                                      startview = "year")),
                
                fluidRow(column(width = 6, 
                                selectizeInput(
                                  "topic_choice", "Select subject topic:",
                                  choices = topic_label[8:11],
                                  selected = topic_label[8:11],
                                  multiple = TRUE),
                                plotOutput("EmailSubject")),
                         
                         column(width = 6, 
                                selectizeInput(
                                  "subject_choice", "Select subject topic:",
                                  choices = topic_label[1:7],
                                  selected = topic_label[1:7],
                                  multiple = TRUE),
                                plotOutput("EmailContent"))))),
      
      # 5th tab content
      tabItem(tabName = "status",
              h4("Study of the Enron worker status email send/received and their subject/content."),
              
              fluidRow(
                tabBox(width = 12,
                       id = "tab5",
                       tabPanel("Send", plotOutput("nbEmailSendStatus")
                       ),
                       tabPanel("Received",  plotOutput("nbEmailRecStatus")))),
              
              br(),
              
              fluidRow(
                tabBox(width=12,
                       id = "tab4",
                       tabPanel("Send", plotOutput("SubjectEmailSendStatus")),
                       tabPanel("Received", plotOutput("SubjectEmailRecStatus"))
                  
                ))),
      
      # 6th tab content
      tabItem(tabName = "actor",
              h4("Email send/received by Enron worker knows for being involved in the fiscal fraud and/or the bankruptcy"),
                fluidRow(box(title = "Email send",
                         selectizeInput(
                           "sender_choice", "Select the worker:",
                           choices = unique(person_of_interest_send$email_label_sender),
                           selected = NULL),
                         plotOutput("EnronWorkerSend")
                         ),
                         box(title = "Email received",
                         selectizeInput(
                           "rec_choice", "Select the worker:",
                           choices = enron_worker_rec,
                           selected = NULL),
                         plotOutput("EnronWorkerRec")
                         ))
                  
                  )))
    )
    
  

### Back-end server

server <- function(input, output){
  
  #the plot in the tab General
  
  output$NbWorker <- renderPlot({
    
    employeelist_2 %>% select(status) %>% #select the needed variable
      {if(input$show_na != TRUE) filter(.,!is.na(status)) else .} %>%
      group_by(status) %>% count() %>% #count the number of employee per status
      ungroup() %>%
      #calculate the percentage for each status
      mutate(perc = `n`/sum(`n`),
             labels = scales::percent(perc)) %>%
      #bar chart
      ggplot(aes(reorder(status, perc ,sum),perc, fill = status)) +
      geom_bar(stat = "identity") +
      #to invert the axis's position
      coord_flip()+ 
      #customize the theme, title and axis labels
      geom_text(aes(label = labels), vjust = 0.5, size = 4) + #display the percentage for each category at the end of the corresponding bar
      scale_y_continuous(labels = scales::percent_format())+
      ggtitle("Number of employee per status in Enron company")+
      labs(y = "Percentage (%)",x = "Employee status") +
      scale_fill_brewer(palette = "Set3", 
                        #to display the NA in grey on the graph
                        na.value = "grey50")+
      theme(legend.position = "none")
  })
  
  #allow choose the year we want to display int the plot
  filtered_year <- reactive({
    df_message_status %>%filter(year %in% input$year)
  })
  
  output$nbEmail <- renderPlot({
    
    data <- filtered_year() %>% group_by(year,month) %>%
      summarise(n= n(), .groups = "drop")
  
    
      ggplot(data, aes(month, n, group = year, color = year))+
      geom_line(size = 1)+
      scale_y_continuous(labels = scales::label_comma())+
      labs(title = "Number of email send/receive per month by the Enron's worker",
           x = "Month",
           y = "Email count per month")+
      scale_fill_brewer(palette = "Set3")
  })
  
  #the plot and table in the email exchange
  
  output$StatusSend <- renderPlot({
    #compute the number of email send per day per employee statuts
    violin_worker_send <- df_message_status %>% filter(!is.na(status_sender)) %>%
      group_by(date, status_sender) %>%
      summarise(email_count = n(), .groups = "drop")
    
    #violin plot 
    ggplot(violin_worker_send, aes(as.factor(status_sender), email_count, fill = as.factor(status_sender))) +
      geom_violin(trim = FALSE) +
      geom_boxplot(width = 0.1, outlier.shape = NA, color = "white")+
      ylim(c(0,250))+
      stat_compare_means(method = "anova", label.y = 250, size = 4)+
      labs(title = "Comparison of the number of email send in function of the enron's worker statuts",
           x = "Source",
           y = "Email count per day") +
      theme(legend.position = "none")
    
  })
  
  output$StatusRec <- renderPlot({
    #compute the number of email send per day per employee statuts
    violin_worker_rec <- df_message_status %>% filter(!is.na(status_recipient)) %>%
      group_by(date, status_recipient) %>%
      summarise(email_count = n(), .groups = "drop")
    
    #violin plot 
    ggplot(violin_worker_rec, aes(as.factor(status_recipient), email_count, fill = as.factor(status_recipient))) +
      geom_violin(trim = FALSE) +
      geom_boxplot(width = 0.1, outlier.shape = NA, color = "white")+
      ylim(c(0,250))+
      stat_compare_means(method = "anova", label.y = 250, size = 4)+
      labs(title = "Comparison of the number of email received in function of the enron's worker statuts",
           x = "Source",
           y = "Email count per day") +
      theme(legend.position = "none")
    
  })
  
  output$StatSend <- renderTable({
  df_message_status %>% filter(!is.na(status_sender)) %>%
      group_by(date, status_sender) %>%
      summarise(email_count = n(), .groups = "drop") %>% 
      ungroup() %>%
      group_by(status_sender)%>%
      summarise(
        mean = mean(email_count),
        sd = sd(email_count),
        min = min(email_count),
        Q1 = quantile(email_count, 0.25),
        Q3 = quantile(email_count, 0.75),
        max = max(email_count)
      )
    
  })
  
  output$StatRec <- renderTable({
    df_message_status %>% filter(!is.na(status_recipient)) %>%
      group_by(date, status_recipient) %>%
      summarise(email_count = n(), .groups = "drop")%>% 
      ungroup() %>%
      group_by(status_recipient)%>%
        summarise(
          mean = mean(email_count),
          sd = sd(email_count),
          min = min(email_count),
          Q1 = quantile(email_count, 0.25),
          Q3 = quantile(email_count, 0.75),
          max = max(email_count))
  })
  
  output$flux1999 <- renderPlot({
    year_1999 <- as.data.frame(per_year %>% filter(year == 1999) %>%
                                 group_by(status_sender, status_recipient) %>%
                                 mutate(sum = sum(number_exchange)) %>% ungroup() %>%
                                 distinct(status_sender, status_recipient, sum) %>%
                                 filter(status_sender != status_recipient) %>%
                                 arrange(status_sender, status_recipient))
    
    adjacencyData_99 <-with(year_1999, table(status_sender, status_recipient))
    chordDiagram(adjacencyData_99, transparency = 0.5, grid.col = status_color)
  })
  
  output$flux2000 <- renderPlot({
    year_2000 <- as.data.frame(per_year %>% filter(year == 2000) %>%
                                 group_by(status_sender, status_recipient) %>%
                                 mutate(sum = sum(number_exchange)) %>% ungroup() %>%
                                 distinct(status_sender, status_recipient, sum) %>%
                                 filter(status_sender != status_recipient) %>%
                                 arrange(status_sender, status_recipient))
    
    adjacencyData_00 <-with(year_2000, table(status_sender, status_recipient))
    chordDiagram(adjacencyData_00, transparency = 0.5, grid.col = status_color)
    
  })
  
  output$flux2001 <- renderPlot({
    
    year_2001 <- as.data.frame(per_year %>% filter(year == 2001) %>%
                                 group_by(status_sender, status_recipient) %>%
                                 mutate(sum = sum(number_exchange)) %>% ungroup() %>%
                                 distinct(status_sender, status_recipient, sum) %>%
                                 filter(status_sender != status_recipient) %>%
                                 arrange(status_sender, status_recipient))
    
    adjacencyData_01 <-with(year_2001, table(status_sender, status_recipient))
    chordDiagram(adjacencyData_01, transparency = 0.5, grid.col = status_color)
    
  })
  
  output$flux2002 <- renderPlot({
    
    year_2002 <- as.data.frame(per_year %>% filter(year == 2002) %>%
                                 group_by(status_sender, status_recipient) %>%
                                 mutate(sum = sum(number_exchange)) %>% ungroup() %>%
                                 distinct(status_sender, status_recipient, sum) %>%
                                 filter(status_sender != status_recipient) %>%
                                 arrange(status_sender, status_recipient))
    
    adjacencyData_02 <-with(year_2002, table(status_sender, status_recipient))
    chordDiagram(adjacencyData_02, transparency = 0.5, grid.col = status_color)
  })
  
  #the plot for the most active employee in the company
  output$Top10Send <- renderPlot({
    df_message_status %>% group_by(sender)%>% count() %>% #to count the number of email send per email address
      ungroup() %>%
      #calculate the percentage for each sender
      mutate(perc = round(`n`/sum(`n`),3),
             labels = scales::percent(perc)) %>% 
      arrange(desc(n)) %>% head(10) %>% #to get only the 10 email address with the most important number of email send
      #bar chart
      ggplot(aes(reorder(sender, perc, sum), perc, fill = sender)) +
      geom_bar(stat="identity") +
      coord_flip() +
      #graph title and label
      geom_text(aes(label = labels), vjust = 0.5, size = 4) + #display the percentage for each category at the end of the corresponding bar
      scale_y_continuous(labels = scales::percent_format())+  
      labs(title = "Top 10 Enron's employee email sender")+
      xlab("Employee's email addres")+
      ylab("Email send per sender (%)") +
      scale_fill_brewer(palette = "Set3")+
      theme(legend.position = "none",
            plot.margin = margin(10, 10, 10, 20))
  })
  
  output$Top10Rec <- renderPlot({
    df_message_status %>% filter(rtype == "TO") %>% #select only the email of the direct concerned receiver
      group_by(recipient)%>% count() %>% #to count the number of email send per email address
      ungroup() %>%
      #calculate the percentage for each sender
      mutate(perc = round(`n`/sum(`n`),4),
             labels = scales::percent(perc)) %>% 
      arrange(desc(n)) %>% head(10) %>% #to get only the 10 email address with the most important number of email send
      #bar chart
      ggplot(aes(reorder(recipient, perc, sum), perc, fill = recipient)) +
      geom_bar(stat="identity") +
      coord_flip() +
      #graph title and label
      geom_text(aes(label = labels), vjust = 0.5, size = 4) + #display the percentage for each category at the end of the corresponding bar
      scale_y_continuous(labels = scales::percent_format())+ 
      labs(title = "Top 10 Enron's employee email receiver",
           subtitle = "Only principal receiver")+
      xlab("Employee's email address")+
      ylab("Email recived per recipient (%)") +
      scale_fill_brewer(palette = "Set3")+
      theme(legend.position = "none",
            plot.margin = margin(10, 10, 10, 20))
  })
  
  output$JeffStatusSendViz <- renderPlot({
    violin_plot1 <- bind_rows(jeff_stat_send, statuts_stat_send)
    
    ggplot(violin_plot1, aes(as.factor(source), email_count, fill = as.factor(source))) +
      geom_violin(trim = FALSE) +
      geom_boxplot(width = 0.1, outlier.shape = NA, color = "white")+
      #display the comparative statistic on the violin plot
      stat_compare_means(method = "t.test", label.y = 380)+
      labs(title = "Comparison of the email send between 
       Jeff Dasovitch and the Enron's Employee",
           x = "Source",
           y = "Email count per day") +
      #to better see the violin plot we break the y axis
      coord_cartesian(ylim=c(0, 400))+
      #set up the color for each resources
      scale_fill_manual(values = c(
        "Jeff Dasovich" = "tomato2",
        "Employee status" = "yellowgreen"))+
      #withdraw the legend form the plot
      theme(legend.position = "none")
  })
  
  output$JeffWorkerSendViz <- renderPlot({
    violin_plot2 <- bind_rows(jeff_stat_send, sender_stat)
    
    ggplot(violin_plot2, aes(as.factor(source), email_count, fill = as.factor(source))) +
      geom_violin(trim = FALSE) +
      geom_boxplot(width = 0.1, outlier.shape = NA, color = "white")+
      stat_compare_means(method = "t.test", label.y = 280)+
      coord_cartesian(ylim = c(0, 300))+
      labs(title = "Comparison of the email count between 
       Jeff Dasovitch and the Enron's worker",
           x = "Source",
           y = "Email count per day") +
      scale_fill_manual(#set up the color for each resources
        values = c(
          "Jeff Dasovich" = "tomato2",
          "Enron's worker" = "cyan"))+
      theme(legend.position = "none")
    
  })
  output$JeffSendStat <- renderTable({
    
    violin_plot <- bind_rows(jeff_stat_send, sender_stat, statuts_stat_send)
    
    violin_plot %>% group_by(source)%>%
      summarise(
        mean = mean(email_count),
        sd = sd(email_count),
        min = min(email_count),
        Q1 = quantile(email_count, 0.25),
        Q3 = quantile(email_count, 0.75),
        max = max(email_count))
  })
  
  output$JeffRecViz <- renderPlot({
    
    violin_plot1 <- bind_rows(jeff_stat_rec, statuts_stat_rec)
    
    ggplot(violin_plot1, aes(as.factor(source), email_count, fill = as.factor(source))) +
      geom_violin(trim = FALSE) +
      geom_boxplot(width = 0.1, outlier.shape = NA, color = "white")+
      #compared statisticaly the 2 group to see if the difference is significant or not
      stat_compare_means(method = "t.test", label.y = max(violin_plot1$email_count) + 2)+
      labs(title = "Comparison of the email count between 
       Jeff Dasovitch and the Enron's Employee",
           x = "Source",
           y = "Email count per day") +
      theme(legend.position = "none")+
      scale_fill_manual(#set up the color for each resources
        values = c(
          "Jeff Dasovich" = "tomato2",
          "Employee status" = "yellowgreen"
        ))
  })
  
  output$JeffWorkerRecViz <- renderPlot({
    violin_plot2 <- bind_rows(jeff_stat_rec, recipient_stat)
    
    ggplot(violin_plot2, aes(as.factor(source), email_count, fill = as.factor(source))) +
      geom_violin(trim = FALSE) +
      geom_boxplot(width = 0.1, outlier.shape = NA, color = "white")+
      ylim(c(-10,350))+
      stat_compare_means(method = "t.test", label.y = 300)+
      labs(title = "Comparison of the email count between 
       Jeff Dasovitch and the Enron's worker",
           x = "Source",
           y = "Email count per day") +
      theme(legend.position = "none")+
      scale_fill_manual(#set up the color for each resources
        values = c(
          "Jeff Dasovich" = "tomato2",
          "Enron's worker" = "cyan"
        ))
  })
  
  output$JeffRecStat <- renderTable({
    violin_plot <- bind_rows(jeff_stat_rec, recipient_stat, statuts_stat_rec)
    
    violin_plot %>% group_by(source) %>%
      summarise(
        mean = mean(email_count),
        median = median(email_count),
        sd = sd(email_count),
        min = min(email_count),
        Q1 = quantile(email_count, 0.25),
        Q3 = quantile(email_count, 0.75),
        max = max(email_count)
      )
    
  })
  
  #email subject and content analysis
  
  email_subject_filtered <- reactive({
    
    #verify the element exist
    req(input$date_range)
    
    email_subject_filtered <- email_subject_send %>% filter(
      year_month >= input$date_range[1], #for year_month is greater than the min date in the range
      year_month <= input$date_range[2]) #for year_month is smaller than the min date in the range
  })
  
  
  output$EmailSubject <- renderPlot({
    
    email_subject_filtered() %>% group_by(year_month) %>%
      mutate(
        sum_topic_meeting = sum(topic_meeting),
        sum_topic_business_process = sum(topic_business_process),
        sum_topic_core_business = sum(topic_core_business),
        sum_topic_enron_event = sum(topic_enron_event)) %>% ungroup() %>%
      #keep one line per year and month
      distinct(year_month, sum_topic_meeting, sum_topic_business_process, sum_topic_core_business, sum_topic_enron_event) %>% 
      #change the orientation of the data set
      pivot_longer(
        cols = 2:5,
        names_to = "topics",
        values_to = "value") %>%
      filter(topics %in% names(topic_label[topic_label %in% input$topic_choice])) %>%
      #scatter plot and trend line
      ggplot(aes(year_month,value, color=topics))+
      geom_line(size = 1)+
      #label, axis, and legend
      labs(color = "Email subject topics",
           title = "Email topics in function of the year",
           x = "year",
           y = "Number of email per topics") +
      #to display the year and month, every 3 months for a better reading
      scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months")+
      scale_color_manual(#to get only the customization for the email categories
        values = topic_colors[8:11],
        labels = topic_label[8:11])
        
  })
  output$EmailContent <- renderPlot({
    
    
    email_subject_filtered() %>% group_by(year_month) %>%
      mutate(
        sum_email_mark_to_market = sum(email_mark_to_market, na.rm = TRUE),
        sum_email_10K_report = sum(email_10K_report, na.rm = TRUE),
        sum_email_losing_money = sum(email_losing_money, na.rm = TRUE),
        sum_email_SEC_investigation = sum(email_SEC_investigation, na.rm = TRUE),
        sum_email_fear_feeling = sum(email_fear_feeling, na.rm = TRUE),
        sum_email_correction = sum(email_correction, na.rm = TRUE),
        sum_email_bankruptcy = sum(email_bankruptcy, na.rm = TRUE)
      ) %>% ungroup() %>%
      #keep one line per year and month
      distinct(year_month, sum_email_mark_to_market, sum_email_10K_report, sum_email_losing_money, sum_email_SEC_investigation, sum_email_fear_feeling,
               sum_email_correction, sum_email_bankruptcy) %>% 
      pivot_longer(
                 cols = 2:8,
                 names_to = "email",
                 values_to = "value") %>%
      filter(email %in% names(topic_label[topic_label %in% input$subject_choice])) %>%
      #scatter plot and trend line
      ggplot(aes(year_month,value, color=email))+
      geom_line(size = 1)+
      #label, axis, and legend
      labs(color = "Email content key words",
           title = "Email key word about the Enron event in function of the year",
           x = "year",
           y = "Number of email per key words") +
      #to display the year and month, every 3 months for a better reading
      scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months")+
      scale_color_manual(#to get only the customization for the email categories
        values = topic_colors[1:7],
        labels = topic_label[1:7])
    
  })
  
  #analyze the email for each status
  
  output$nbEmailSendStatus <- renderPlot({
    
    df_message_status %>% filter(status_sender == input$status_choice) %>% 
      group_by(year,month)%>%
      count() %>% 
      ggplot(aes(month, n, fill = month))+
      geom_bar(stat = "identity") +
      facet_grid(~year)+
      labs(title = paste("Email send per month for each year by the", input$status_choice),
           y = "Email count per month")+
      scale_fill_manual(
        values = month_color,
        labels = month_label)+
      theme(legend.position = "bottom",
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.x = element_blank())
    
  })
           
             
  output$nbEmailRecStatus <- renderPlot({
    
    df_message_status %>% filter(status_recipient == input$status_choice) %>% 
      group_by(year,month)%>%
      count() %>% 
      ggplot(aes(month, n, fill = month))+
      geom_bar(stat = "identity") +
      facet_grid(~year)+
      labs(title = paste("Email received per month for each year by the", input$status_choice),
           y = "Email count per month")+
      scale_fill_manual(
        values = month_color,
        labels = month_label)+
      theme(legend.position = "bottom",
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.x = element_blank())
  })
  

  
  
  output$SubjectEmailSendStatus <- renderPlot({
    
   status_email_subject_send %>% 
      filter(
        year_month >= input$date_range[1], #for year_month is greater than the min date in the range
        year_month <= input$date_range[2], #for year_month is smaller than the min date in the range
    status_sender == input$status_choice, #for selecting the status
    topic_email %in% names(topic_label[topic_label %in% input$subject_choice]) #for selecting the topics
    )%>% 
      ggplot(aes(year_month,value, color = topic_email))+
      geom_line(size = 1) +
      scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months")+    
      labs(fill = "Email key words and topics",
           title = paste("Email send by", input$status_choice, ", content and subject analyze"),
           y = "Email count")+
      scale_color_manual(values = topic_colors,
                         labels = topic_label)+
      theme(legend.text.position = "bottom")})
  
  
  output$SubjectEmailRecStatus <- renderPlot({
    
    status_email_subject_rec %>% 
      filter(
        year_month >= input$date_range[1], #for year_month is greater than the min date in the range
        year_month <= input$date_range[2], #for year_month is smaller than the min date in the range
        status_recipient == input$status_choice, #for selecting the status
        topic_email %in% names(topic_label[topic_label %in% input$subject_choice]) #for selecting the topics
      )%>% 
      ggplot(aes(year_month,value, color = topic_email))+
      geom_line(size = 1) +
      scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months")+    
      labs(fill = "Email key words and topics",
           title = paste("Email received by", input$status_choice, ", content and subject analyze"),
           y = "Email count")+
      scale_color_manual(values = topic_colors,
                         labels = topic_label)+
      theme(legend.text.position = "bottom")
    
  })
  
  
  
  #analyze the content and subject of email for specific enron worker
  output$EnronWorkerSend <- renderPlot({
    
    person_of_interest_send %>% filter(email_label_sender == input$sender_choice) %>% 
      ggplot(aes(topic_email,value, fill = topic_email))+
      geom_bar(stat = "identity") +
      facet_grid(~year)+
      labs(fill = "Email topics",
           title = paste("Email topics send by", input$sender_choice),
           y = "Email count per subject topics")+
      scale_fill_manual(values = topic_colors,
                        labels = topic_label)+
      theme(legend.text.position = "bottom",
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank())
    
  })
  
  output$EnronWorkerRec <- renderPlot({
    
    person_of_interest_reciveid %>% filter(email_label_recipient == input$rec_choice)%>% 
      ggplot(aes(topic_email,value, fill = topic_email))+
      geom_bar(stat = "identity") +
      facet_grid(~year)+
      labs(fill = "Email content key words and topics",
           title = paste("Email topics received by", input$rec_choice),
           y = "Email count per category research")+
      scale_fill_manual(values = topic_colors,
                        labels = topic_label)+
      theme(legend.text.position = "bottom",
            axis.title.x = element_blank(),
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank())
    
  })
  
}

### the App

shinyApp(ui, server)
