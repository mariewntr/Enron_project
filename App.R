#Script of the Rshiny app to display the key visualization of the Enron company email analysis.

#libraries
library(shinydashboard)
library(shinyWidgets)
library(bslib)
library(shiny)
library(tidyverse)
library(circlize)
library(wordcloud)
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
  mutate(recipient = gsub(" ", "", df_message_status$rvalue),
         sender = gsub(" ", "", df_message_status$sender),
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
           #group the status at the head of the company together
           status_sender %in% c("Managing Director", "Manager", "Director") ~ "Manger - Director",
           status_sender %in% c("CEO", "Vice President", "President") ~ "CEO - President",
           #if they aren't in those list we kept the original status
           .default = status_sender),
         status_recipient = case_when(
           status_recipient %in% c("Managing Director", "Manager", "Director") ~ "Manger - Director",
           status_recipient %in% c("CEO", "Vice President", "President") ~ "CEO - President",
           .default = status_recipient)) %>%
  group_by(date,status_sender, status_recipient) %>%
  mutate(number_exchange = n()) %>% ungroup() %>%
  distinct(date, status_sender, status_recipient, number_exchange, year)

#For visualize and compare the most active employee to the other member of his group and the total Enron worker
#plot the top 10 sender/receiver
p_sender <- df_message_status %>% distinct(sender, subject, recipient, .keep_all = TRUE) %>%
  group_by(sender)%>% count() %>% #to count the number of email send per email address
  ungroup() %>%
  #calculate the percentage for each sender
  mutate(perc = round(`n`/sum(`n`),3),
         labels = scales::percent(perc)) %>% 
  arrange(desc(n)) %>% head(10) 

p_rec <- df_message_status %>% distinct(sender, subject, recipient, .keep_all = TRUE)  %>% filter(rtype == "TO") %>%
  group_by(recipient)%>% count() %>% #to count the number of email received per email address
  ungroup() %>%
  #calculate the percentage for each sender
  mutate(perc = round(`n`/sum(`n`),3),
         labels = scales::percent(perc)) %>% 
  arrange(desc(n)) %>% head(10) 



#to study the email subject and content
#topics list 

topic_meeting <- c("message|origin|pleas|email|thank|attach|file|copi|inform|receiv|thank|all|time|meet|look|week|day|dont|vinc|talk")

topic_business_process <- c("enron|deal|agreement|chang|contract|corp|fax|houston|date|america|risk|analy|confidential|correction")

topic_core_business <- c("market|gas|price|power|company|energy|trade|busi|servic|manag")

topic_enron_event <- c("bankrup|SEC|MTM|fear|losing money|10-K|fears|investigation|phone|fax|document")

#for the analyse of email subject and content
email_subject_send <- df_message_status %>% distinct(date, year, month, sender, status_sender, subject, reference) %>%
  mutate(#count the number of email which contain at least one word in the list of each topic
    subject_meeting = if_else(str_detect(subject, topic_meeting), 1, 0),
    subject_business_process = if_else(str_detect(subject, topic_business_process), 1, 0),
    subject_core_business = if_else(str_detect(subject, topic_core_business), 1, 0),
    subject_enron_event = if_else(str_detect(subject, topic_enron_event), 1, 0),
    email_meeting = if_else(str_detect(reference,topic_meeting), 1, 0),
    email_business_process = if_else(str_detect(reference, topic_business_process), 1, 0),
    email_core_business = if_else(str_detect(reference, topic_core_business), 1, 0),
    email_enron_event = if_else(str_detect(reference, topic_enron_event), 1, 0),
    #to get the date in year/month
    year_month = as.Date(paste0(year,"-",month,"-01")))

#for analyzing the email content and subject over all the study period
email_analyze_send <- email_subject_send %>% group_by(year_month) %>%
  mutate(
    sum_subject_meeting = sum(subject_meeting),
    sum_subject_business_process = sum(subject_business_process),
    sum_subject_core_business = sum(subject_core_business),
    sum_subject_enron_event = sum(subject_enron_event),
    sum_email_business_process = sum(email_business_process, na.rm = TRUE),
    sum_email_core_business = sum(email_core_business, na.rm = TRUE),
    sum_email_meeting = sum(email_meeting, na.rm = TRUE),
    sum_email_enron_event = sum(email_enron_event, na.rm = TRUE)) %>% ungroup() %>%
  filter(((sum_subject_meeting != 0) | (sum_subject_business_process != 0) | (sum_subject_core_business != 0) | (sum_subject_enron_event != 0) | (sum_email_business_process != 0) | (sum_email_core_business != 0) | (sum_email_meeting != 0) | (sum_email_enron_event != 0)))%>%
  #keep one line per year and month
  select(subject, reference) %>% distinct()


#study the email send per worker status
status_email_subject_send <- email_subject_send %>%
  #we focus on the worker which their status are know
  filter(!is.na(status_sender)) %>%
  #compute the sum of each topics for each year studied
  group_by(year_month, status_sender) %>%
  mutate(
    sum_subject_meeting = sum(subject_meeting),
    sum_subject_business_process = sum(subject_business_process),
    sum_subject_core_business = sum(subject_core_business),
    sum_subject_enron_event = sum(subject_enron_event),
    #for the email we use na.rm = TRUE to allow the sum to be done
    sum_email_business_process = sum(email_business_process, na.rm = TRUE),
    sum_email_core_business = sum(email_core_business, na.rm = TRUE),
    sum_email_meeting = sum(email_meeting, na.rm = TRUE),
    sum_email_enron_event = sum(email_enron_event, na.rm = TRUE)) %>% ungroup() %>%
  #keep one line per year and month
  select(year_month, status_sender, sum_subject_meeting, sum_subject_business_process, sum_subject_core_business, sum_subject_enron_event, 
           sum_email_business_process,sum_email_core_business,sum_email_meeting,sum_email_enron_event) %>% distinct()


#pivot the data frame
status_email_subject_send <- status_email_subject_send %>%
  pivot_longer(
    cols = 3:length(status_email_subject_send),
    names_to = "topic_email",
    values_to = "value")

#study the email received
email_subject_rec <- df_message_status %>% distinct(date, year, month, recipient, status_recipient, subject, reference) %>%
  mutate(#count the number of email which contain at least one word in the list of each topic
    subject_meeting = if_else(str_detect(subject, topic_meeting), 1, 0),
    subject_business_process = if_else(str_detect(subject, topic_business_process), 1, 0),
    subject_core_business = if_else(str_detect(subject, topic_core_business), 1, 0),
    subject_enron_event = if_else(str_detect(subject, topic_enron_event), 1, 0),
    email_meeting = if_else(str_detect(reference,topic_meeting), 1, 0),
    email_business_process = if_else(str_detect(reference, topic_business_process), 1, 0),
    email_core_business = if_else(str_detect(reference, topic_core_business), 1, 0),
    email_enron_event = if_else(str_detect(reference, topic_enron_event), 1, 0),
    #to get the date in year/month
    year_month = as.Date(paste0(year,"-",month,"-01")))

#for analyzing the email content and subject over all the study period
email_rec_analyze <- email_subject_rec %>% group_by(year_month) %>%
  mutate(
    sum_subject_meeting = sum(subject_meeting),
    sum_subject_business_process = sum(subject_business_process),
    sum_subject_core_business = sum(subject_core_business),
    sum_subject_enron_event = sum(subject_enron_event),
    sum_email_business_process = sum(email_business_process, na.rm = TRUE),
    sum_email_core_business = sum(email_core_business, na.rm = TRUE),
    sum_email_meeting = sum(email_meeting, na.rm = TRUE),
    sum_email_enron_event = sum(email_enron_event, na.rm = TRUE)) %>% ungroup() %>%
  filter(((sum_subject_meeting != 0) | (sum_subject_business_process != 0) | (sum_subject_core_business != 0) | (sum_subject_enron_event != 0) | (sum_email_business_process != 0) | (sum_email_core_business != 0) | (sum_email_meeting != 0) | (sum_email_enron_event != 0)))%>%
  #keep one line per year and month
  select(subject, reference) %>% distinct()


#study the email received per status
status_email_subject_rec <- email_subject_rec %>%
  #we focus on the worker which their status are know
  filter(!is.na(status_recipient)) %>%
  #compute the sum of each topics for each year studied
  group_by(year_month, status_recipient) %>%
  mutate(
    sum_subject_meeting = sum(subject_meeting),
    sum_subject_business_process = sum(subject_business_process),
    sum_subject_core_business = sum(subject_core_business),
    sum_subject_enron_event = sum(subject_enron_event),
    #for the email we use na.rm = TRUE to allow the sum to be done
    sum_email_business_process = sum(email_business_process, na.rm = TRUE),
    sum_email_core_business = sum(email_core_business, na.rm = TRUE),
    sum_email_meeting = sum(email_meeting, na.rm = TRUE),
    sum_email_enron_event = sum(email_enron_event, na.rm = TRUE)) %>% ungroup() %>%
  #keep one line per year and month
  select(year_month, status_recipient, sum_subject_meeting, sum_subject_business_process, sum_subject_core_business, sum_subject_enron_event, 
           sum_email_business_process,sum_email_core_business,sum_email_meeting,sum_email_enron_event) %>% distinct()


#pivot the data frame
status_email_subject_rec <- status_email_subject_rec %>%
  pivot_longer(
    cols = 3:length(status_email_subject_rec),
    names_to = "topic_email",
    values_to = "value")




#for the analyze of the person imply in the Enron event
#email send:
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
      sender == "richard.causey@enron.com" ~ "Richard Causey", 
      sender == "greg.whalley@enron.com" ~ "Greg Whalley", 
      .default = sender))

person_of_interest_send_subject <- person_of_interest_send %>%
  #to compute the number of email sent in each topics by the person whose are directly involved in the Enron scandal
  group_by(year_month, email_label_sender) %>%
  mutate(
    sum_subject_meeting = sum(subject_meeting),
    sum_subject_business_process = sum(subject_business_process),
    sum_subject_core_business = sum(subject_core_business),
    sum_subject_enron_event = sum(subject_enron_event),
    #for the email we use na.rm = TRUE to allow the sum to be done
    sum_email_business_process = sum(email_business_process, na.rm = TRUE),
    sum_email_core_business = sum(email_core_business, na.rm = TRUE),
    sum_email_meeting = sum(email_meeting, na.rm = TRUE),
    sum_email_enron_event = sum(email_enron_event, na.rm = TRUE)) %>% ungroup() %>%
  #keep one line per year and month
  distinct(year_month, email_label_sender, sum_subject_meeting, sum_subject_business_process, sum_subject_core_business, sum_subject_enron_event, 
           sum_email_business_process,sum_email_core_business,sum_email_meeting,sum_email_enron_event)



#pivot the table
person_of_interest_send_subject <-person_of_interest_send_subject %>%
  pivot_longer(
    cols = 3:length(person_of_interest_send_subject),
    names_to = "topic_email",
    values_to = "value"
  )

#email received
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
                         "vkaminski@aol.com") ~ "Vincent Kaminski",
        recipient %in% c("jordan.mintz@enron.com","jordan_mintz@enron.com") ~ "Jordan Mintz",
        recipient == "sherron.watkins@enron.com" ~ "Sherron Watkins",
        recipient == "richard.causey@enron.com" ~ "Richard Causey", 
        recipient == "greg.whalley@enron.com" ~ "Greg Whalley", 
        .default = recipient)) 

person_of_interest_reciveid_subject <- person_of_interest_reciveid %>%
  #to compute the number of email sent in each topics by the person whose are directly involved in the Enron scandal
  group_by(year_month, email_label_recipient) %>%
  mutate(
    sum_subject_meeting = sum(subject_meeting),
    sum_subject_business_process = sum(subject_business_process),
    sum_subject_core_business = sum(subject_core_business),
    sum_subject_enron_event = sum(subject_enron_event),
    #for the email we use na.rm = TRUE to allow the sum to be done
    sum_email_business_process = sum(email_business_process, na.rm = TRUE),
    sum_email_core_business = sum(email_core_business, na.rm = TRUE),
    sum_email_meeting = sum(email_meeting, na.rm = TRUE),
    sum_email_enron_event = sum(email_enron_event, na.rm = TRUE)) %>% ungroup() %>%
  #keep one line per year and month
  distinct(year_month, email_label_recipient, sum_subject_meeting, sum_subject_business_process, sum_subject_core_business, sum_subject_enron_event, 
           sum_email_business_process,sum_email_core_business,sum_email_meeting,sum_email_enron_event)

#pivot the table
person_of_interest_reciveid_subject <-person_of_interest_reciveid_subject %>%
  pivot_longer(
    cols = 3:length(person_of_interest_reciveid_subject),
    names_to = "topic_email",
    values_to = "value"
  )


#function for the flux plot to enhance the app reactivity 
plot_flux <- function(data, year){
  #create the year_data plot
  year_data <- data %>% filter(year == year) %>%
    arrange(status_sender, status_recipient)
  
  #create the dataframe for usable by the package for the plot
  adjacencyData <- with(year_data, table(status_sender, status_recipient))
  
  #draw the flux diagram
  chordDiagram(adjacencyData, transparency = 0.5, grid.col = status_color)
}

#the base for each plot needed to draw the flux plot
data_by_year <- per_year %>% filter(year %in% 1999:2002) %>%
      group_by(status_sender, status_recipient) %>%
      mutate(sum = sum(number_exchange)) %>%
      ungroup() %>%
      distinct(status_sender, status_recipient, sum) %>%
      filter(status_sender != status_recipient) %>%
      arrange(status_sender, status_recipient)



#the color and label for the different visualisations
status_color <- c(
  "Employee" = "pink",
  "CEO - President" = "orange",
  "Trader" = "springgreen3",
  "Manger - Director" = "violetred4",
  "In House Lawyer" = "purple4")


#the list of category studied and their related color in each plot
topic_colors <- c("sum_subject_business_process" = "steelblue4",
                  "sum_subject_core_business" = "orchid",
                  "sum_subject_meeting" = "chocolate4",
                  "sum_subject_enron_event" = "yellowgreen",
                  "sum_email_business_process" = "cyan3",
                  "sum_email_core_business" = "plum4",
                  "sum_email_meeting" = "salmon",
                  "sum_email_enron_event" = "springgreen4")



#the list of category and their related label on the plot  
topic_label <- c("sum_subject_business_process" = "Business process email subject",
                 "sum_subject_core_business" = "Core Business email subject",
                 "sum_subject_meeting" = "Meeting email subject",
                 "sum_subject_enron_event" = "Enron Event email subject",
                 "sum_email_business_process" = "Business process email",
                 "sum_email_core_business" = "Core business email",
                 "sum_email_meeting" = "Meeting email",
                 "sum_email_enron_event" = "Enron's event email")

#the list of colors and label for each month as well as the enron worker statuss
month_label <- c("01" = "January","02" = "February","03" = "March","04" = "April","05" = "May","06" = "June","07" = "July","08" = "August",
                 "09" = "September","10" = "October","11" = "November","12" = "December")

month_color <- c("01" = "lightgreen","02" = "lightsalmon4","03" = "lightblue","04" = "greenyellow","05" = "cyan","06" = "darkgreen","07" = "lavender",
                 "08" = "plum","09" = "coral","10" = "honeydew4","11" = "hotpink","12" = "indianred")

status_list <- c("Employee", "CEO", "Manager", "Director", "Vice President", "Trader", "President", "Managing Director", "In House Lawyer")

#list of enron worker study in the analyse for their email exchange
enron_worker_rec <- c("Jeff Dasovich", "Jeffrey Skilling", "Timothy Belden","Lea Fastow","Andrew Fastow",
                  "Vincent Kaminski","Jordan Mintz","Sherron Watkins","Richard Causey", "Greg Whalley")

#list of word analyze in the email subject and content
word_list <- list("message","origin","pleas","email","thank","attach","file","copi","inform","receiv","thank","time","meet",
                  "look","week","dont","vinc","talk","enron","deal","agreement","chang","contract","corp","fax","houston","america",
                  "risk","analy","confidential","correction", "market","gas","price","power","company","energy","trade","busi","servic","manag",
                  "bankrup","SEC","MTM","fear", "investigation", "mark-to-market", "10-K", "losing money", "correction", "phone", "fax", "document")

#the theme for all the plot
theme_set(theme_light())


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
                menuItem("Email content analysis", tabName = "email", icon = icon("file-lines")),
                menuItem("Email exchange per status", tabName = "workerExchange", icon = icon("envelope")),
                menuItem("Email analyze per status", tabName = "status", icon = icon("business-time")),
                menuItem("The most active Enron worker", tabName = "active", icon = icon("sun")),
                menuItem("Potential fraud actor", tabName = "actor", icon = icon("circle-exclamation"))),
    
    #design the side bar for the email tab
    conditionalPanel(
      condition = "input.tabs == 'email'",
      dateRangeInput("date_range",
                     label = "Select the period: ",
                     start = min(email_subject_send$year_month),
                     end = max(email_subject_send$year_month),
                     format = "yyyy-mm",
                     startview = "year"),
      selectizeInput(
        "subject_choice", "Select the topic:",
        choices = c("Business process email subject","Core Business email subject","Meeting email subject",
                    "Enron Event email subject","Business process email","Core business email","Meeting email","Enron's event email"),
        selected = c("Business process email subject","Core Business email subject","Meeting email subject",
                     "Enron Event email subject","Business process email","Core business email","Meeting email","Enron's event email"),
        multiple = TRUE)),
    
    conditionalPanel(#sidebar for the status tab
      condition = "input.tabs == 'status'",
      #date range slicer
      dateRangeInput("date_range_status",
                     label = "Select the period: ",
                     start = min(email_subject_send$year_month),
                     end = max(email_subject_send$year_month),
                     format = "yyyy-mm",
                     startview = "year"),
      #status selector
      selectizeInput(
        "status_choice_status", "Select status:",
        choices = unique(status_email_subject_send$status_sender),
        selected = NULL),
      #topic selector
      selectizeInput(
        "subject_choice_status", "Select subject topic:",
        choices = c("Business process email subject","Core Business email subject","Meeting email subject",
                    "Enron Event email subject","Business process email","Core business email","Meeting email","Enron's event email"),
        selected = c("Business process email subject","Core Business email subject","Meeting email subject",
                     "Enron Event email subject","Business process email","Core business email","Meeting email","Enron's event email"),
        multiple = TRUE)
    ),
    conditionalPanel(#sidebar for the actor tab
      condition = "input.tabs == 'actor'",
      #date range slicer
      dateRangeInput("date_range_actor",
                     label = "Select the period: ",
                     start = min(person_of_interest_send_subject$year_month),
                     end = max(person_of_interest_send_subject$year_month),
                     format = "yyyy-mm",
                     startview = "year"),
      #actor selector for send
      selectizeInput(
        "worker_choice_send", "Select status sender:",
        choices = unique(person_of_interest_send_subject$email_label_sender),
        selected = NULL),
      #actor selector for received
      selectizeInput(
        "worker_choice_rec", "Select status recipient:",
        choices = unique(person_of_interest_reciveid_subject$email_label_recipient),
        selected = NULL),
      #topic selector
      selectizeInput(
        "subject_choice_actor", "Select subject topic:",
        choices = c("Business process email subject","Core Business email subject","Meeting email subject",
                    "Enron Event email subject","Business process email","Core business email","Meeting email","Enron's event email"),
        selected = c("Business process email subject","Core Business email subject","Meeting email subject",
                     "Enron Event email subject","Business process email","Core business email","Meeting email","Enron's event email"),
        multiple = TRUE)
    )),
  
  #the body of the dashboard
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(
        #name of the tab
        tabName = "dashboard",
        fluidRow(width = 12,
                 box(width = 12,
                     title = "Percentage of employee in the employee list with a know status",
                     switchInput(inputId = "show_na", value = FALSE, label = "show NA"),
                     plotOutput("NbWorker") 
                 )
        ),
        br(),
        fluidRow(width = 12,
                 box(width = 12,
                     title = "Key number",
                     fluidRow(width = 12,
                              valueBoxOutput("TotalEmail"),valueBoxOutput("PctgStatus"), valueBoxOutput("PctgGeneralAdd")),
                     fluidRow(width = 12,
                              valueBoxOutput("NbSend"),valueBoxOutput("NbRec"))
                 ))),
      
      #2nd tab content
      tabItem(tabName = "email",
              fluidRow(tabBox(width = 12,
                              tabPanel("Send",plotOutput("EmailSend")),
                              tabPanel("Received",plotOutput("Emailrec"))
              )),
              fluidRow(tabBox(width = 12,
                              tabPanel("Interactive study of the email send",
                                       fluidRow(column(width = 8,
                                                       plotOutput("EmailSubjectSend")),
                                                column(width = 4,
                                                       plotOutput("EmailWordSend")
                                                       
                                                ))),
                              tabPanel("Interactive study of the email received",
                                       fluidRow(column(width = 8,
                                                       plotOutput("EmailSubjectRec")),
                                                column(width = 4,
                                                       plotOutput("EmailWordRec"))))))
      ),
      
      
      # 3rd tab content
      tabItem(tabName = "workerExchange",
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
      
      
      #4th tab content
      tabItem(tabName = "status",
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
                       tabPanel("Send", 
                                fluidRow(
                                  column(width = 8,
                                         plotOutput("SubjectEmailSendStatus")),
                                  column(width = 4,
                                         plotOutput("WordcloudSendStatus")))),
                       tabPanel("Received", 
                                fluidRow(
                                  column(width = 8,
                                         plotOutput("SubjectEmailRecStatus")),
                                  column(width = 4,
                                         plotOutput("WordcloudRecStatus"))))))
      ),
      
      
      #5th tab content
      tabItem(tabName = "active",
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
      
      
      # 6th tab content
      tabItem(tabName = "actor",
              fluidRow(
                box(title = "Number of email send",
                    plotOutput("EnronWorkerSend")
                ),
                box(title = "Number of email received",
                    plotOutput("EnronWorkerRec")
                )),
              fluidRow(
                box(title = "Email send, subject and content analyze",
                    plotOutput("EnronWorkerSubjectSend")
                ),
                box(title = "Email received, subject and content analyze",
                    plotOutput("EnronWorkerSubjectRec")
                ))
      ))))


    
  

### Back-end server

server <- function(input, output){
  
  #the plot in the tab dashboard
  
  #the plot showing the percentage of worker for each status
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
      #ggtitle("Number of employee per status in Enron company")+
      labs(y = "Percentage (%)",x = "Employee status") +
      scale_fill_brewer(palette = "Set3", 
                        #to display the NA in grey on the graph
                        na.value = "grey50")+
      theme(legend.position = "none")
  })
  
  #the key number of the dataset display in value box
  output$NbSend <- renderValueBox({
    
    nbSender <- count(df_message_status %>% distinct(sender))
    
    valueBox(
      value = nbSender,
      subtitle = "Number of distinct sender",
      color = "light-blue")})
  
  output$NbRec <- renderValueBox({
    
    nbRecipient <- count(df_message_status %>% distinct(recipient))
    
    valueBox(
      value = nbRecipient,
      subtitle = "Number of distinct recipient",
      color = "light-blue")})
  
  
  output$TotalEmail <- renderValueBox({
    
    totalEmail <- count(df_message_status %>% filter(rtype == "TO") %>% distinct(sender, recipient, subject, reference))
    
    valueBox(
      value = totalEmail,
      subtitle = "Number of distinct email exchange",
      color = "light-blue")})
  
  output$PctgStatus <- renderValueBox({
    
    Emailcount <- count(df_message_status %>% filter(rtype == "TO") %>% distinct(sender, recipient, subject, reference))
    
    emailExchangeStatus <- count(df_message_status %>% distinct(sender, status_sender, recipient, status_recipient, subject, reference) 
                                 %>% filter(!is.na(status_sender)|!is.na(status_recipient)))
    
    pctg <-round((emailExchangeStatus/Emailcount)*100,2)
    
    valueBox(
      value = pctg,
      subtitle = "Percentage of email with a know worker status",
      color = "light-blue")})
  
  
  output$PctgGeneralAdd <- renderValueBox({
    
    Estimation_generalEmailAdd <- count(df_message_status %>% 
                                          #key word regularly used for general email address name and see in the sender or recipient variable
                                          filter(str_detect(sender,"^enron|^press|^office|^all|^announcement|^communications|affair|client|contact|secur|team|comit|^west|energy") | str_detect(recipient, "^enron|^press|^office|^all|^announcement|^communications|affair|client|contact|secur|team|comit|^west|energy")))
    valueBox(
      value = Estimation_generalEmailAdd,
      subtitle = "Estimation of the poucentage of general email address",
      color = "light-blue")})
  
  #email subject and content analysis, tab email
  
  #we create a reactive plot for use it in the plot of this part
  
  output$EmailSend <- renderPlot({
    #we create a reactive plot for construct a dataframe once which will kept in memory and not reload every time we interact with the plot
    Send <- reactive({
      email_subject_send %>% group_by(year_month) %>% mutate(nb_email_send = n()) %>% ungroup()
    })
    #to use the plot create by the reactive we must call it with () because it isn't a real table but an object of the environment
    ggplot(Send(),aes(year_month, nb_email_send)) +
      geom_line(size = 1)+
      scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months")+
      labs(title = "Number of email send in the study period",
           y = "Email number",
           x = "Study period")
  })
  
  output$Emailrec <- renderPlot({
    
    rec <- reactive({email_subject_rec %>% group_by(year_month) %>% mutate(nb_email_rec = n()) %>% ungroup()})
    
    ggplot(rec(),aes(year_month, nb_email_rec)) +
      geom_line(size = 1)+
      scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months")+
      labs(title = "Number of email received in the study period",
           y = "Email number",
           x = "Study period")
    
  })
  
  
  output$EmailSubjectSend <- renderPlot({
    
    email_analyze <- reactive({
      email_subject_send %>% group_by(year_month) %>%
        mutate(
          sum_subject_meeting = sum(subject_meeting),
          sum_subject_business_process = sum(subject_business_process),
          sum_subject_core_business = sum(subject_core_business),
          sum_subject_enron_event = sum(subject_enron_event),
          sum_email_business_process = sum(email_business_process, na.rm = TRUE),
          sum_email_core_business = sum(email_core_business, na.rm = TRUE),
          sum_email_meeting = sum(email_meeting, na.rm = TRUE),
          sum_email_enron_event = sum(email_enron_event, na.rm = TRUE)) %>% ungroup() %>%
        filter(((sum_subject_meeting != 0) | (sum_subject_business_process != 0) | (sum_subject_core_business != 0) | (sum_subject_enron_event != 0) | (sum_email_business_process != 0) | (sum_email_core_business != 0) | (sum_email_meeting != 0) | (sum_email_enron_event != 0)))%>%
        #keep one line per year and month
        select(year_month, subject, reference, sum_subject_meeting, sum_subject_business_process, sum_subject_core_business, sum_subject_enron_event,
               sum_email_business_process,sum_email_core_business,sum_email_meeting,sum_email_enron_event) %>% distinct() %>%
        #change the orientation of the data set
        pivot_longer(
          cols = 4:11,
          names_to = "topics",
          values_to = "value")
    })
    
    email_analyze()%>% filter(
      #allow the user to select the period of study display in the graph
      year_month >= input$date_range[1], #for year_month is greater than the min date in the range
      year_month <= input$date_range[2],
      #to allow the user select the topic on the graph
      topics %in% names(topic_label[topic_label %in% input$subject_choice])) %>%
      #scatter plot and trend line
      ggplot(aes(year_month,value, color=topics))+
      geom_line(size = 1)+
      #label, axis, and legend
      labs(color = "Email subject and text categories",
           title = "Email subject and text analyze over the study period",
           x = "Study period",
           y = "Number of email per topics") +
      #to display the year and month, every 3 months for a better reading
      scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months")+
      scale_color_manual(#to get only the customization for the email categories
        values = topic_colors,
        labels = topic_label)
    
  })
  
  output$EmailWordSend <- renderPlot({
    
    #initiate the list for storing the count for each words in text and subject
    email_words_freq <- c()
    subject_freq <- c()
    
    #here we transform the loop into a sapply for enhancing the app reactivity, the fonction is the same
    subject_freq <- sapply(word_list, function(i) sum(str_count(email_analyze_send$subject, i)))
    
    email_words_freq <- sapply(word_list, function(j) sum(!is.na(as.list(str_locate(email_analyze_send$reference, j)))))
    
    #for each status we make a total with the count from the subject and the text
    total_count <- subject_freq + email_words_freq
    
    #draw the wordcloud with the frequency of each word, only the top 10
    wordcloud(word_list, total_count, min.freq = 10 ,max.words= length(word_list),scale = c(3, 0.5) ,col=colorRampPalette(c("#cce5ff", "#3399ff", "#003366"))(length(total_count)), rot.per = 0.3)
    title(main = paste0("The top words seen in the email subject and text send"), col.main = "black", font.main = 2)
    
  })
  
  output$EmailSubjectRec <- renderPlot({
    
    email_analyze_rec <- reactive({email_subject_rec %>%
        group_by(year_month) %>%
        mutate(
          sum_subject_meeting = sum(subject_meeting),
          sum_subject_business_process = sum(subject_business_process),
          sum_subject_core_business = sum(subject_core_business),
          sum_subject_enron_event = sum(subject_enron_event),
          sum_email_business_process = sum(email_business_process, na.rm = TRUE),
          sum_email_core_business = sum(email_core_business, na.rm = TRUE),
          sum_email_meeting = sum(email_meeting, na.rm = TRUE),
          sum_email_enron_event = sum(email_enron_event, na.rm = TRUE)) %>% ungroup() %>%
        filter ((sum_subject_meeting != 0) | (sum_subject_business_process != 0) | (sum_subject_core_business != 0) | (sum_subject_enron_event != 0) | (sum_email_business_process != 0) | (sum_email_core_business != 0) | (sum_email_meeting != 0) | (sum_email_enron_event != 0)) %>%
        #keep one line per year and month
        select(year_month, subject, reference, sum_subject_meeting, sum_subject_business_process, sum_subject_core_business, sum_subject_enron_event,
               sum_email_business_process,sum_email_core_business,sum_email_meeting,sum_email_enron_event) %>% distinct() %>%
        #change the orientation of the data set
        pivot_longer(
          cols = 4:11,
          names_to = "topics",
          values_to = "value")})
    
    
    
    email_analyze_rec() %>%
      filter(
        year_month >= input$date_range[1], #for year_month is greater than the min date in the range
        year_month <= input$date_range[2],
        topics %in% names(topic_label[topic_label %in% input$subject_choice])) %>%
      #scatter plot and trend line
      ggplot(aes(year_month,value, color=topics))+
      geom_line(size = 1)+
      #label, axis, and legend
      labs(color = "Email subject categories",
           title = "Email subject analyze over the study period",
           x = "Study period",
           y = "Number of email per topics") +
      #to display the year and month, every 3 months for a better reading
      scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months")+
      scale_color_manual(#to get only the customization for the email categories
        values = topic_colors,
        labels = topic_label)
    
  })
  
  output$EmailWordRec <- renderPlot({
    
    #initiate the list for storing the count for each words in text and subject
    email_words_freq <- c()
    subject_freq <- c()
    
    subject_freq <- sapply(word_list, function(i) sum(str_count(email_rec_analyze$subject, i)))
    
    email_words_freq <- sapply(word_list, function(j) sum(!is.na(as.list(str_locate(email_rec_analyze$reference, j)))))
    
    #for each status we make a total with the count from the subject and the text
    total_count <- subject_freq + email_words_freq
    
    #draw the wordcloud with the frequency of each word, only the top 10
    wordcloud(word_list, total_count, min.freq = 10 ,max.words= length(word_list),scale = c(3, 0.5) ,col=colorRampPalette(c("#cce5ff", "#3399ff", "#003366"))(length(total_count)), rot.per = 0.3)
    title(main = paste0("The top words seen in the email subject and text received"), col.main = "black", font.main = 2)
    
  })
  
  
  #the plot and table WorkerExchange
  
  output$StatusSend <- renderPlot({
    #compute the number of email send per day per employee status
    violin_worker_send <- reactive({df_message_status %>% filter(!is.na(status_sender)) %>%
      group_by(date, status_sender) %>%
      summarise(email_count = n(), .groups = "drop")})
    
    #violin plot 
    ggplot(violin_worker_send(), aes(as.factor(status_sender), email_count, fill = as.factor(status_sender))) +
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
    #compute the number of email send per day per employee status
    violin_worker_rec <- reactive({df_message_status %>% filter(!is.na(status_recipient)) %>%
      group_by(date, status_recipient) %>%
      summarise(email_count = n(), .groups = "drop")})
    
    #violin plot 
    ggplot(violin_worker_rec(), aes(as.factor(status_recipient), email_count, fill = as.factor(status_recipient))) +
      geom_violin(trim = FALSE) +
      geom_boxplot(width = 0.1, outlier.shape = NA, color = "white")+
      ylim(c(0,250))+
      stat_compare_means(method = "anova", label.y = 250, size = 4)+
      labs(title = "Comparison of the number of email received in function of the enron's worker statuts",
           x = "Source",
           y = "Email count per day") +
      theme(legend.position = "none")
    
  })
  
  #display a table with the description for each worker of the email send
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
  
  #display a table with the description for each worker of the email received
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
  
  #the plot of the email exchange flux
  #create the plot for the flux plot
  
  #for each flux use the function to plot each
  output$flux1999 <- renderPlot({
    
    plot_flux(data_by_year, #the dataset become a function call each time it is read by the shiny parser 
              1999) #the year we study
  })
  
  output$flux2000 <- renderPlot({
    plot_flux(data_by_year, 2000)
    
  })
  
  output$flux2001 <- renderPlot({
    
    plot_flux(data_by_year, 2001)
    
  })
  
  output$flux2002 <- renderPlot({
    
    plot_flux(data_by_year, 2002)
  })
  
  
  #analyze the email for each status, tab status
  
  output$nbEmailSendStatus <- renderPlot({
    
    df_message_status %>% filter(status_sender == input$status_choice_status) %>%
      group_by(year,month)%>%
      count() %>%
      ggplot(aes(month, n, fill = month))+
      geom_bar(stat = "identity") +
      facet_grid(~year)+
      labs(title = paste("Email send per month for each year by the", input$status_choice_status),
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
    
    df_message_status %>% filter(status_recipient == input$status_choice_status) %>%
      group_by(year,month)%>%
      count() %>%
      ggplot(aes(month, n, fill = month))+
      geom_bar(stat = "identity") +
      facet_grid(~year)+
      labs(title = paste("Email received per month for each year by the", input$status_choice_status),
           y = "Email count per month",
           x = "Study period")+
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
        year_month >= input$date_range_status[1], #for year_month is greater than the min date in the range
        year_month <= input$date_range_status[2], #for year_month is smaller than the min date in the range
        #for selecting the status
        status_sender == input$status_choice_status, 
        #for selecting the topics
        topic_email %in% names(topic_label[topic_label %in% input$subject_choice_status]) 
      )%>%
      ggplot(aes(year_month,value, color = topic_email))+
      geom_line(size = 1) +
      scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months")+
      labs(color = "Email key words and topics",
           title = paste("Email send by", input$status_choice_status, ", content and subject analyze"),
           y = "Email count",
           x = "Study period")+
      scale_color_manual(values = topic_colors,
                         labels = topic_label)+
      theme(legend.text.position = "bottom")})
  
  
  output$WordcloudSendStatus <- renderPlot({
    
    filtered_email_send <- reactive({
      email_subject_send %>%
        #we focus on the worker which their status are know
        filter(status_sender == input$status_choice_status) %>%
        #compute the sum of each topics for each year studied
        group_by(year_month, status_sender) %>%
        mutate(
          sum_subject_meeting = sum(subject_meeting),
          sum_subject_business_process = sum(subject_business_process),
          sum_subject_core_business = sum(subject_core_business),
          sum_subject_enron_event = sum(subject_enron_event),
          #for the email we use na.rm = TRUE to allow the sum to be done
          sum_email_business_process = sum(email_business_process, na.rm = TRUE),
          sum_email_core_business = sum(email_core_business, na.rm = TRUE),
          sum_email_meeting = sum(email_meeting, na.rm = TRUE),
          sum_email_enron_event = sum(email_enron_event, na.rm = TRUE)) %>% ungroup() %>%
        filter((sum_subject_meeting != 0) | (sum_subject_business_process != 0) | (sum_subject_core_business != 0) | (sum_subject_enron_event != 0) | (sum_email_business_process != 0) | (sum_email_core_business != 0) | (sum_email_meeting != 0) | (sum_email_enron_event != 0)) %>%
        #keep one line per year and month
        distinct(status_sender, subject, reference)
    })
    
    df <- filtered_email_send() 
    
    #initiate the list for storing the count for each words in text and subject
    email_words_freq <- c()
    subject_freq <- c()
    
    subject_freq <- sapply(word_list, function(i) sum(str_count(df$subject, i)))
    
    email_words_freq <- sapply(word_list, function(j) sum(!is.na(as.list(str_locate(df$reference, j)))))
    
    
    #for each status we make a total with the count from the subject and the text
    total_count <- subject_freq + email_words_freq
    
    #draw the wordcloud with the frequency of each word, only the top 10
    wordcloud(word_list, total_count, min.freq = 10 ,max.words= 10,scale = c(3, 0.5) ,col=colorRampPalette(c("#cce5ff", "#3399ff", "#003366"))(length(total_count)), rot.per = 0.3)
    title(main = paste0("Top 10 words in the email send by ",input$status_choice_status), col.main = "black", font.main = 2)
    
  })
  
  output$WordcloudRecStatus <- renderPlot({
    
    filtered_email_rec <- reactive({
      email_subject_rec %>%
        #we focus on the worker which their status are know
        filter(status_recipient == input$status_choice_status) %>%
        #compute the sum of each topics for each year studied
        group_by(year_month, status_recipient) %>%
        mutate(
          sum_subject_meeting = sum(subject_meeting),
          sum_subject_business_process = sum(subject_business_process),
          sum_subject_core_business = sum(subject_core_business),
          sum_subject_enron_event = sum(subject_enron_event),
          #for the email we use na.rm = TRUE to allow the sum to be done
          sum_email_business_process = sum(email_business_process, na.rm = TRUE),
          sum_email_core_business = sum(email_core_business, na.rm = TRUE),
          sum_email_meeting = sum(email_meeting, na.rm = TRUE),
          sum_email_enron_event = sum(email_enron_event, na.rm = TRUE)) %>% ungroup() %>%
        filter((sum_subject_meeting != 0) | (sum_subject_business_process != 0) | (sum_subject_core_business != 0) | (sum_subject_enron_event != 0) | (sum_email_business_process != 0) | (sum_email_core_business != 0) | (sum_email_meeting != 0) | (sum_email_enron_event != 0)) %>%
        #keep one line per year and month
        distinct(status_recipient, subject, reference)
    })
    
    df <- filtered_email_rec() 
    
    #initiate the list for storing the count for each words in text and subject
    email_words_freq <- c()
    subject_freq <- c()
    
    subject_freq <- sapply(word_list, function(i) sum(str_count(df$subject, i)))
    
    email_words_freq <- sapply(word_list, function(j) sum(!is.na(as.list(str_locate(df$reference, j)))))
    
    
    #for each status we make a total with the count from the subject and the text
    total_count <- subject_freq + email_words_freq
    
    #draw the wordcloud with the frequency of each word, only the top 10
    #par(bg = "black")
    wordcloud(word_list, total_count, min.freq = 10 ,max.words= 10,scale = c(3, 0.5),col=colorRampPalette(c("#cce5ff", "#3399ff", "#003366"))(length(total_count)), rot.per = 0.3)
    title(main = paste0("Top 10 words in the email received by ",input$status_choice_status), col.main = "black", font.main = 2)
    
  })
  
  output$SubjectEmailRecStatus <- renderPlot({
    
    status_email_subject_rec %>%
      filter(
        year_month >= input$date_range_status[1], #for year_month is greater than the min date in the range
        year_month <= input$date_range_status[2], #for year_month is smaller than the min date in the range
        #for selecting the status
        status_recipient == input$status_choice_status, 
        #for selecting the topics
        topic_email %in% names(topic_label[topic_label %in% input$subject_choice_status]) 
      )%>%
      ggplot(aes(year_month,value, color = topic_email))+
      geom_line(size = 1) +
      scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months")+
      labs(color = "Email key words and topics",
           title = paste("Email received by", input$status_choice_status, ", content and subject analyze"),
           y = "Email count")+
      scale_color_manual(values = topic_colors,
                         labels = topic_label)+
      theme(legend.text.position = "bottom")
    
  })
  
  
  #the plot for the most active employee in the company, tab active
  
  output$Top10Send <- renderPlot({
      #bar chart
      ggplot(p_sender, aes(reorder(sender, perc, sum), perc, fill = sender)) +
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
      ggplot(p_rec, aes(reorder(recipient, perc, sum), perc, fill = recipient)) +
      geom_bar(stat="identity") +
      coord_flip() +
      #graph title and label
      geom_text(aes(label = labels), vjust = 0.5, size = 4) + #display the percentage for each category at the end of the corresponding bar
      scale_y_continuous(labels = scales::percent_format())+
      labs(title = "Top 10 Enron's employee email receiver",
           subtitle = "Only principal receiver")+
      xlab("Employee's email address")+
      ylab("Email received per recipient (%)") +
      scale_fill_brewer(palette = "Set3")+
      theme(legend.position = "none",
            plot.margin = margin(10, 10, 10, 20))
  })

  output$JeffStatusSendViz <- renderPlot({
    
    Jeff_status_send_f <- reactive({
      
      jeff_stat_send <- df_message_status %>% filter(sender == "jeff.dasovich@enron.com") %>%
        #we count the number of different email subject send per day
        group_by(date, subject) %>% 
        summarise(email_count = n(), .groups = "drop") %>%
        mutate(source = "Jeff Dasovich") %>% transform(source = as.factor(source))
      
      statuts_stat_send <- df_message_status %>% filter(status_sender == "Employee") %>% 
        #we count the number of different email subject send per day by each sender of status employee
        group_by(date, sender, subject) %>% 
        summarise(email_count = n(), .groups = "drop") %>%
        mutate(source = "Employee status") %>% transform(source = as.factor(source))
      
      
      Jeff_status_send <- bind_rows(jeff_stat_send, statuts_stat_send)
      
    })
    
    
    ggplot(Jeff_status_send_f(), aes(as.factor(source), email_count, fill = as.factor(source))) +
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
    
    Jeff_worker_send_f <- reactive({
      
      jeff_stat_send <- df_message_status %>% filter(sender == "jeff.dasovich@enron.com") %>%
        #we count the number of different email subject send per day
        group_by(date, subject) %>% 
        summarise(email_count = n(), .groups = "drop") %>%
        mutate(source = "Jeff Dasovich") %>% transform(source = as.factor(source))
      
      sender_stat <- df_message_status %>% 
        #we count the number of different email subject send per day by each sender
        group_by(date, sender, subject) %>% 
        summarise(email_count = n(), .groups = "drop") %>%
        mutate(source = "All sender") %>% select(-sender) %>% transform(source = as.factor(source))
      
      Jeff_worker_send <- bind_rows(jeff_stat_send, sender_stat)  

    })

    ggplot(Jeff_worker_send_f(), aes(as.factor(source), email_count, fill = as.factor(source))) +
      geom_violin(trim = FALSE) +
      geom_boxplot(width = 0.1, outlier.shape = NA, color = "white")+
      stat_compare_means(method = "t.test", label.y = 280)+
      coord_cartesian(ylim = c(0, 300))+
      labs(title = "Comparison of the email count between
       Jeff Dasovitch and all sender",
           x = "Source",
           y = "Email count per day") +
      scale_fill_manual(#set up the color for each resources
        values = c(
          "Jeff Dasovich" = "tomato2",
          "All sender" = "cyan"))+
      theme(legend.position = "none")

  })

  output$JeffSendStat <- renderTable({

    Jeff_stat_send_f <- reactive({
      
      jeff_stat_send <- df_message_status %>% filter(sender == "jeff.dasovich@enron.com") %>%
      #we count the number of different email subject send per day
      group_by(date, subject) %>% 
      summarise(email_count = n(), .groups = "drop") %>%
      mutate(source = "Jeff Dasovich") %>% transform(source = as.factor(source))
      
      sender_stat <- df_message_status %>% 
        #we count the number of different email subject send per day by each sender
        group_by(date, sender, subject) %>% 
        summarise(email_count = n(), .groups = "drop") %>%
        mutate(source = "All sender") %>% select(-sender) %>% transform(source = as.factor(source))
      
      statuts_stat_send <- df_message_status %>% filter(status_sender == "Employee") %>% 
        #we count the number of different email subject send per day by each sender of status employee
        group_by(date, sender, subject) %>% 
        summarise(email_count = n(), .groups = "drop") %>%
        mutate(source = "Employee status") %>% transform(source = as.factor(source))
      
      
      Jeff_status_send <- bind_rows(jeff_stat_send, statuts_stat_send)
      Jeff_worker_send <- bind_rows(jeff_stat_send, sender_stat)  
      Jeff_stat_send <- bind_rows(jeff_stat_send, sender_stat, statuts_stat_send)
      
      })
    
    Jeff_stat_send_f() %>% group_by(source)%>%
      summarise(
        mean = mean(email_count),
        sd = sd(email_count),
        min = min(email_count),
        Q1 = quantile(email_count, 0.25),
        Q3 = quantile(email_count, 0.75),
        max = max(email_count))
  })

  output$JeffRecViz <- renderPlot({
    
    Jeff_status_rec_f <- reactive({
      
      #statistics on the jeff dasovich email receive per day
      jeff_stat_rec <- df_message_status %>% filter(recipient == "jeff.dasovich@enron.com") %>%
        group_by(date) %>% 
        summarise(email_count = n(), .groups = "drop") %>%
        mutate(source = "Jeff Dasovich") %>% transform(source = as.factor(source))
      
      #statistics on the email send per day by the enron's worker who have an employee statuts
      statuts_stat_rec <- df_message_status %>% filter(status_recipient == "Employee") %>% group_by(date) %>% 
        summarise(email_count = n(), .groups = "drop") %>%
        mutate(source = "Employee status") %>% transform(source = as.factor(source))
      
      Jeff_status_rec <- bind_rows(jeff_stat_rec, statuts_stat_rec)
      
    })
    

    ggplot(Jeff_status_rec_f(), aes(as.factor(source), email_count, fill = as.factor(source))) +
      geom_violin(trim = FALSE) +
      geom_boxplot(width = 0.1, outlier.shape = NA, color = "white")+
      #compared statisticaly the 2 group to see if the difference is significant or not
      stat_compare_means(method = "t.test", label.y = 1700)+
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
    
    Jeff_worker_rec_f <- reactive({
      
      #statistics on the jeff dasovich email receive per day
      jeff_stat_rec <- df_message_status %>% filter(recipient == "jeff.dasovich@enron.com") %>%
        group_by(date) %>% 
        summarise(email_count = n(), .groups = "drop") %>%
        mutate(source = "Jeff Dasovich") %>% transform(source = as.factor(source))
      
      #statistics on the email send per day by all the recipient
      recipient_stat <- df_message_status %>% group_by(date, recipient) %>% 
        summarise(email_count = n(), .groups = "drop") %>%
        mutate(source = "All recipient") %>% select(-recipient) %>% transform(source = as.factor(source))
      
      Jeff_worker_rec <- bind_rows(jeff_stat_rec, recipient_stat)
      
    })

    ggplot(Jeff_worker_rec_f(), aes(as.factor(source), email_count, fill = as.factor(source))) +
      geom_violin(trim = FALSE) +
      geom_boxplot(width = 0.1, outlier.shape = NA, color = "white")+
      ylim(c(-10,350))+
      stat_compare_means(method = "t.test", label.y = 300)+
      labs(title = "Comparison of the email count between
       Jeff Dasovitch and all recipient",
           x = "Source",
           y = "Email count per day") +
      theme(legend.position = "none")+
      scale_fill_manual(#set up the color for each resources
        values = c(
          "Jeff Dasovich" = "tomato2",
          "All recipient" = "cyan"
        ))
  })

  output$JeffRecStat <- renderTable({
    
    Jeff_rec_stat_f <- reactive({
      
      #statistics on the jeff dasovich email receive per day
      jeff_stat_rec <- df_message_status %>% filter(recipient == "jeff.dasovich@enron.com") %>%
        group_by(date) %>% 
        summarise(email_count = n(), .groups = "drop") %>%
        mutate(source = "Jeff Dasovich") %>% transform(source = as.factor(source))
      
      #statistics on the email send per day by all the recipient
      recipient_stat <- df_message_status %>% group_by(date, recipient) %>% 
        summarise(email_count = n(), .groups = "drop") %>%
        mutate(source = "All recipient") %>% select(-recipient) %>% transform(source = as.factor(source))
      
      #statistics on the email send per day by the enron's worker who have an employee statuts
      statuts_stat_rec <- df_message_status %>% filter(status_recipient == "Employee") %>% group_by(date) %>% 
        summarise(email_count = n(), .groups = "drop") %>%
        mutate(source = "Employee status") %>% transform(source = as.factor(source))
      
      Jeff_status_rec <- bind_rows(jeff_stat_rec, statuts_stat_rec)
      Jeff_worker_rec <- bind_rows(jeff_stat_rec, recipient_stat)
      Jeff_rec_stat <- bind_rows(jeff_stat_rec, recipient_stat, statuts_stat_rec)
      
    })
    

    Jeff_rec_stat_f() %>% group_by(source) %>%
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



  #analyze the content and subject of email for specific enron worker
  output$EnronWorkerSend <- renderPlot({

    person_of_interest_send %>% filter(email_label_sender == input$worker_choice_send) %>%
      group_by(year,month) %>%
      count() %>% ggplot(aes(month, n, fill = month))+
      geom_bar(stat = "identity") +
      facet_grid(~year)+
      labs(title = input$worker_choice_send,
           y = "Email count per month")+
      scale_fill_manual(
        values = month_color,
        labels = month_label)+
      theme(legend.position = "bottom",
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.x = element_blank())

  })

  output$EnronWorkerRec <- renderPlot({

    person_of_interest_reciveid %>% filter(email_label_recipient == input$worker_choice_rec)%>%
      group_by(year,month) %>%
      count() %>% ggplot(aes(month, n, fill = month))+
      geom_bar(stat = "identity") +
      facet_grid(~year)+
      labs(title = input$worker_choice_rec,
           y = "Email count per month")+
      scale_fill_manual(
        values = month_color,
        labels = month_label)+
      theme(legend.position = "bottom",
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.x = element_blank())

  })

  output$EnronWorkerSubjectSend <- renderPlot({

    person_of_interest_send_subject %>% filter(
      year_month >= input$date_range_actor[1], #for year_month is greater than the min date in the range
      year_month <= input$date_range_actor[2], #for year_month is smaller than the min date in the range
      email_label_sender == input$worker_choice_send,
      topic_email %in% names(topic_label[topic_label %in% input$subject_choice_actor]) #for selecting the topics
      ) %>%
      ggplot(aes(year_month,value, color = topic_email))+
      geom_line(size = 1) +
      labs(color = "Email subject and content topics",
           title = input$worker_choice_send,
           y = "Number of email per topics",
           x = "Study period")+
      scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months")+
      scale_color_manual(values = topic_colors,
                         labels = topic_label)+
      theme(legend.text.position = "bottom")

  })

  output$EnronWorkerSubjectRec <- renderPlot({

    person_of_interest_reciveid_subject %>% filter(
      year_month >= input$date_range_actor[1], #for year_month is greater than the min date in the range
      year_month <= input$date_range_actor[2], #for year_month is smaller than the min date in the range
      email_label_recipient == input$worker_choice_rec,
      topic_email %in% names(topic_label[topic_label %in% input$subject_choice_actor]) #for selecting the topics
      )%>%
      ggplot(aes(year_month,value, color = topic_email))+
      geom_line(size = 1) +
      scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months")+
      labs(color = "Email subject and content topics",
           title = input$worker_choice_rec,
           y = "Number of email per topics",
           x = "Study period")+
      scale_color_manual(values = topic_colors,
                         labels = topic_label)+
      theme(legend.text.position = "bottom")

  })


  }

### the App

shinyApp(ui, server)
