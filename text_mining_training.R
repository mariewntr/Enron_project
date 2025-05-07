#Script test text mining sur le df_message_status


library(reshape)
library(tm)
library(wordcloud)


## isolate the variable which contain text
#isolate the different subject of email in the subject identify to be related to the Enron company events

df <- df_message_status %>% distinct(date, sender, status_sender, subject, reference) %>%
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
      year_month = as.Date(paste0(format(date, "%Y"),"-",format(date,"%m"),"-01"))) %>% group_by(year_month) %>%
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
  distinct(year_month, subject, reference, sum_topic_meeting, sum_topic_business_process, sum_topic_core_business, sum_topic_enron_event, sum_email_mark_to_market,
           sum_email_10K_report,
           sum_email_losing_money,
           sum_email_SEC_investigation,
           sum_email_fear_feeling,
           sum_email_correction,
           sum_email_bankruptcy) %>% 
  filter((sum_email_mark_to_market!=0)|(sum_email_10K_report != 0)|
           (sum_email_losing_money!=0)|(sum_email_SEC_investigation!=0)|(sum_email_fear_feeling!=0)|
           (sum_email_correction!=0)|(sum_email_bankruptcy!=0)) 

topic_meeting <- c("message|origin|pleas|email|thank|attach|file|copi|inform|receiv|thank|all|time|meet|look|week|day|dont|vinc|talk")

topic_business_process <- c("enron|deal|agreement|chang|contract|corp|fax|houston|date|america|risk|analy|confidential|correction")

topic_core_business <- c("market|gas|price|power|company|energy|trade|busi|servic|manag")

topic_enron_event <- c("bankrup|SEC|MTM|fear")

word_list <- list("message","origin","pleas","email","thank","attach","file","copi","inform","receiv","thank","time","meet","look","week","dont","vinc","talk",
  "enron","deal","agreement","chang","contract","corp","fax","houston","america","risk","analy","confidential","correction",
               "market","gas","price","power","company","energy","trade","busi","servic","manag",
               "bankrup","SEC","MTM","fear", "investigation")

word_count <- c()

for(i in seq_along(word_list)){
  
  search <- as.character(word_list[[i]])
  nb <- sum(str_count(df$subject, search))
  
  word_count <- c(word_count, nb)
  
}

par(bg = "black")

wordcloud(word_list, word_count, min.freq = 10 ,max.words=length(word_list), col=heat.colors(length(word_list), alpha = 0.9), rot.per = 0.3)





