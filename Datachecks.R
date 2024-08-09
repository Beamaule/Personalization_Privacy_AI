
#Preliminary Dataset Analysis - Pilot

#Project: AI Personalization and Privacy
#Author: Beatrice Maule

#Install packages
install.packages('gridExtra')
#Load packages
library("knitr")
library("magrittr")
library("dplyr")
library("haven")
library("car") 
library("here")
library('readr')
library('ggplot2')
library('stringr')
library('gridExtra')

#Pilot dataframe, substitute with actual data
data <- read.csv("~/Desktop/AI Personalization and Privacy/Personalization 3/Privacy_August 6, 2024_19.25.csv", comment.char="#") 


#################Code below for data cleaning purposes only####################

#Remove first rows of dataframe
data <- data[-c(2), ]

#Eliminate the tests & rename Duration and Age for clarity 
data_filtered <- data %>%
  mutate(
    Q125 = trimws(Q125),
    Q136 = trimws(Q136)) %>%
  filter(
    !(Q125 %in% c("test", "testkaushal", "another_test") | 
        Q136 %in% c("test", "AmandaTest", "fgdgh", "dasdasf", "chzh", "Amanda test"))
  ) %>%
  rename(completion_secs = `Duration..in.seconds.`,
    Age = Q58)

#Check how many finished the survey 
data %>%
  count(Finished)

data%>%
  count(Consent)

data%>%
  count(Progress)

#Delete Rows not finished
data_filtered <- data_filtered %>%
  filter(Finished == 1)

# Delete rows with no consent
data_filtered <- data_filtered %>%
  filter(Consent == 1)


#Survey completion time: mean, median, and standard deviation
data_filtered <- data_filtered %>%
  filter(!is.na(as.numeric(trimws(completion_secs))))

data_filtered <- data_filtered %>%
  mutate(completion_secs = as.numeric(trimws(completion_secs)))

avg_time <- mean(data_filtered$completion_secs, na.rm = TRUE)
print('Mean:')
print(avg_time)

# Calculate and print median
median_time <- median(data_filtered$completion_secs, na.rm = TRUE)
print('Median:')
print(median_time)

# Calculate and print standard deviation
sd_time <- sd(data_filtered$completion_secs, na.rm = TRUE)
print('Standard Deviation:')
print(sd_time)

#Check distribution of treatments and partecipants

#Check sample
sample_type <- ggplot(data_filtered, aes(x = sample_type, fill = sample_type)) +
  geom_bar() +
  theme(legend.position = "right")

print(sample_type)

#Check distribution of personalization type
pers_type <- ggplot(data_filtered, aes(x = personalization, fill = personalization)) +
  geom_bar() +
  theme(legend.position = "right")

print(pers_type)

#Check distribution of with/without cheap talk
talk_type <- ggplot(data_filtered, aes(x = cond_cheap_talk, fill = cond_cheap_talk)) +
  geom_bar() +
  theme(legend.position = "right")

print(talk_type)

grid.arrange(sample_type, pers_type, talk_type, nrow = 2, ncol = 2)

#Screening outside of Prolific

##############################################################################




###################### Code Begins Here ######################################


#How many got the correct definition of AI - open question


#Find number of correct
count_ai <- data_filtered %>%
  filter(!is.na(AI_Open) & str_detect(AI_Open, regex("artificial intelligence|AI",
                                   ignore_case = TRUE))) %>%
  nrow()

# Find number of incorrect
count_other <- data_filtered %>%
  filter(!(!is.na(AI_Open) & str_detect(AI_Open, regex("artificial intelligence|AI", 
                                    ignore_case = TRUE)))) %>%
  nrow()

print(paste("Count of rows containing 'artificial intelligence' or variations:",
            count_ai))
print(paste("Count of rows containing anything else:", count_other))


#####

#How many got the correct definition of AI - multiple choice
# 2 = correct
# 1 = incorrect

data_filtered %>%
  count(AI_Def)

#Plot correct and incorrect
# Name for clarity
data_filtered_1 <- data_filtered %>%
  mutate(
     AI_Label_1 = case_when(
     AI_Def == 2 ~ "Correct",
     AI_Def == 1 ~ "Incorrect"))

AI_correct <- ggplot(data_filtered_1, aes(x = AI_Label_1, fill = AI_Label_1)) +
  geom_bar() +
  scale_fill_manual(values = c("Correct" = "blue", "Incorrect" = "red")) +
  labs(title = "Number of Correct and Incorrect Answers",
       x = "Answer Type",
       y = "Count",
       fill = "Answer Type") +
  theme(legend.position = "none")

print(AI_correct)
#####

#How many got the correct definition of GenAI
# 2 = correct
# 1 = incorrect

data_filtered %>%
  count(GenAI_Def)

#Plot correct and incorrect

# Name for clarity
data_filtered_2 <- data_filtered %>%
  mutate(
    GenAI_Label_2 = case_when(
      GenAI_Def == 2 ~ "Correct",
      GenAI_Def == 1 ~ "Incorrect"))

GenAI_correct <- ggplot(data_filtered_2, aes(x = GenAI_Label_2, fill = GenAI_Label_2)) +
  geom_bar() +
  scale_fill_manual(values = c("Correct" = "blue", "Incorrect" = "red")) +
  labs(title = "Number of Correct and Incorrect Answers",
       x = "Answer Type",
       y = "Count",
       fill = "Answer Type") +
  theme(legend.position = "none")

print(GenAI_correct)
#####

#Pew questions analysis

#Customer Service
Pew_1_g <- ggplot(data_filtered, aes(x = Pew_1, fill = Pew_1)) +
  geom_bar() +
  labs(title = "Customer Service")+
  theme(legend.position = "none")

print(Pew_1_g)


#Music
Pew_2_g <- ggplot(data_filtered, aes(x = Pew_2, fill = Pew_2)) +
  geom_bar() +
  labs(title = "Music")+
  theme(legend.position = "none")

print(Pew_2_g)

#Email
Pew_3_g <- ggplot(data_filtered, aes(x = Pew_3, fill = Pew_3)) +
  geom_bar() +
  labs(title = "Emails")+
  theme(legend.position = "none")

print(Pew_3_g)

#Health Products
Pew_4_g <- ggplot(data_filtered, aes(x = Pew_4, fill = Pew_4)) +
  geom_bar() +
  labs(title = "Health Products")+
  theme(legend.position = "none")

print(Pew_4_g)

#Online Shopping
Pew_5_g <- ggplot(data_filtered, aes(x = Pew_5, fill = Pew_5)) +
  geom_bar() +
  labs(title = "Online Shopping")+
  theme(legend.position = "none")

print(Pew_5_g)

#Home devices
Pew_6_g <- ggplot(data_filtered, aes(x = Pew_6, fill = Pew_6)) +
  geom_bar() +
  labs(title = "Home Devices")+
  theme(legend.position = "none")

print(Pew_6_g)

####

#Frequencies

#Chat GPT
Chat_GPT <- ggplot(data_filtered, aes(x = X1_Frequency, fill = X1_Frequency )) +
  geom_bar() +
  labs(title = "Chat GPT")+
  theme(legend.position = "none")

print(Chat_GPT)

#CharacterAI
Char_AI <- ggplot(data_filtered, aes(x = X2_Frequency, fill = X2_Frequency )) +
  geom_bar() +
  labs(title = "Character.AI")+
  theme(legend.position = "none")

print(Char_AI)

#QuillBot
Quillbot <- ggplot(data_filtered, aes(x = X3_Frequency, fill = X3_Frequency )) +
  geom_bar() +
  labs(title = "QuillBot")+
  theme(legend.position = "none")

print(Quillbot)

#Midjourney
Midjourney <- ggplot(data_filtered, aes(x = X4_Frequency, fill = X4_Frequency )) +
  geom_bar() +
  labs(title = "Midjourney")+
  theme(legend.position = "none")

print(Midjourney)

#Gradescope
Gradescope <- ggplot(data_filtered, aes(x = X5_Frequency, fill = X5_Frequency )) +
  geom_bar() +
  labs(title = "Gradescope")+
  theme(legend.position = "none")

print(Gradescope)

#Lena
Lena <- ggplot(data_filtered, aes(x = X6_Frequency, fill = X6_Frequency )) +
  geom_bar() +
  labs(title = "Lena")+
  theme(legend.position = "none")

print(Lena)

#Side by side comparison 
grid.arrange(Chat_GPT, Char_AI, Quillbot, Midjourney, Gradescope, Lena, nrow = 2, ncol = 3)

#####

#Comprehension checks

Q33 <- ggplot(data_filtered, aes(x = Q33, fill = Q33 )) +
  geom_bar() +
  theme(legend.position = "none")
print(Q33)

Q34 <-  ggplot(data_filtered, aes(x = Q34, fill = Q34 )) +
  geom_bar() +
  theme(legend.position = "none")
print(Q34)

Q35 <- ggplot(data_filtered, aes(x = Q35, fill = Q35 )) +
  geom_bar() +
  theme(legend.position = "none")
print(Q35)

Q36 <- ggplot(data_filtered, aes(x = Q36, fill = Q36 )) +
  geom_bar() +
  theme(legend.position = "none")
print(Q36)

grid.arrange(Q33,Q34, Q35, Q36, nrow = 2, ncol = 2)

####

#Highest level of audio-willngness to share

highest_audio <- ggplot(data_filtered, aes(x = Q115, fill = Q115 )) +
  geom_bar() +
  labs(title = "Willingness to share-Highest Level of Audio ")+
  theme(legend.position = "none")

print(highest_audio)

#Lowest level of personalization-Willingness to recieve

lowest_pers <- ggplot(data_filtered, aes(x = Q116, fill = Q116 )) +
  geom_bar() +
  labs(title = "Willingness to Recieve-Lowest Level of Personalization ")+
  theme(legend.position = "none")

print(lowest_pers)

grid.arrange(highest_audio,lowest_pers, nrow = 1, ncol = 2)

###

#Code URLs 

replace_urls <- function(text, replacements) {
  for (url in names(replacements)) {
    text <- str_replace_all(text, fixed(url), replacements[url])
  }
  text
}

url_rep <- c(
  "https://chicagocdr.org/cdrwebexp/ai_personalization/actual_dashboard/black-dashboard-master14/examples/dashboard-1.html" = "1",
  "https://chicagocdr.org/cdrwebexp/ai_personalization/actual_dashboard/black-dashboard-master14/examples/dashboard-2.html" = "2",
  "https://chicagocdr.org/cdrwebexp/ai_personalization/actual_dashboard/black-dashboard-master14/examples/dashboard-3.html" = "3",
  "https://chicagocdr.org/cdrwebexp/ai_personalization/actual_dashboard/black-dashboard-master14/examples/dashboard-0.html" = "0"
)

data_filtered <- data_filtered %>%
  mutate(selected_url = replace_urls(selected_url, url_rep))

#Graph
selec_dash <-  ggplot(data_filtered, aes(x = selected_url, fill = selected_url )) +
  geom_bar() +
  theme(legend.position = "none")

print(selec_dash)
####

#Benefits in childhood education agreement - parents+teachers
benefits_child <- ggplot(data_filtered, aes(x = Q25, fill = Q25 )) +
  geom_bar() +
  theme(legend.position = "none")

print(benefits_child)

#Benfits daily - others
benefits_daily <- ggplot(data_filtered, aes(x = Q26, fill = Q26 )) +
  geom_bar() +
  theme(legend.position = "none")

print(benefits_daily)


##############################################################################

#Group : None

#Willingness to share demongraphic data

will_dem_o <- ggplot(data_filtered, aes(x = Q50, fill = Q50 )) +
  geom_bar() +
  labs(title = "Willingness to Share Own's Demographic Data ")+
  theme(legend.position = "none")

print(will_dem_o)

#Willingness to share attitude data

will_att_o<- ggplot(data_filtered, aes(x = Q51, fill = Q51 )) +
  geom_bar() +
  labs(title = "Willingness to Share Own's Attitude Data ")+
  theme(legend.position = "none")

print(will_act_o)

grid.arrange(will_dem_o,will_act_o, nrow = 1, ncol = 2)

#### 

#Group: Teachers

#Willingness to share student's demographic data

will_dem_t<- ggplot(data_filtered, aes(x = Q52, fill = Q52 )) +
  geom_bar() +
  labs(title = "Willingness to Share Student's Demographic Data ")+
  theme(legend.position = "none")

print(will_dem_t)

#Willingness to share in-class activity data
will_act_t<- ggplot(data_filtered, aes(x = Q53, fill = Q53 )) +
  geom_bar() +
  labs(title = "Willingness to Share Student's In-class Activity Data ")+
  theme(legend.position = "none")

print(will_act_t)
grid.arrange(will_dem_t,will_act_t, nrow = 1, ncol = 2)

####

#Group: Parents

#Willingness to share child's demographic data
will_dem_p<- ggplot(data_filtered, aes(x = Q130, fill = Q130 )) +
  geom_bar() +
  labs(title = "Willingness to Share Child's Demographic Data ")+
  theme(legend.position = "none")

print(will_dem_p)

#Willingness to share child's activity data
will_act_p<- ggplot(data_filtered, aes(x = Q131, fill = Q131 )) +
  geom_bar() +
  labs(title = "Willingness to Share Child's Activity Data ")+
  theme(legend.position = "none")

print(will_act_p)

grid.arrange(will_dem_p,will_act_p, nrow = 1, ncol = 2)

####


##########################Demographics#######################################


#Group: Teachers

#Age
data_filtered$Age_Group <- cut(as.numeric(data_filtered$Q144), 
                               breaks = seq(0, 100, by = 10), 
                               right = FALSE, 
                               labels = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90-99"))

age_t <- ggplot(data_filtered, aes(x = Age_Group, fill = Age_Group)) +
  geom_bar() +
  theme(legend.position = "none")

print(age_t)

age_t <- ggplot(data_filtered, aes(x = Age_Group, fill = Age_Group)) +
  geom_bar() +
  labs(title = "Age Distribution of Teachers", x = "Age Group", y = "Count") +
  theme(legend.position = "none")

print(age_t)

#Gender
gender_t <- ggplot(data_filtered, aes(x = Q68, fill = Q68)) +
  geom_bar() +
  theme(legend.position = "none")

print(gender_t)

#Marital Status
marstat_t <- ggplot(data_filtered, aes(x = Q69, fill = Q69)) +
  geom_bar() +
  theme(legend.position = "none")

print(marstat_t)

#Education Level
ed_t <- ggplot(data_filtered, aes(x = Q70, fill = Q70)) +
  geom_bar() +
  theme(legend.position = "none")

print(ed_t)

#Primary Language
lang_t <- ggplot(data_filtered, aes(x = Q71, fill = Q71)) +
  geom_bar() +
  theme(legend.position = "none")

print(lang_t)

#Hispanic or Latino
hisp_t <- ggplot(data_filtered, aes(x = Q72, fill = Q72)) +
  geom_bar() +
  theme(legend.position = "none")

print(hisp_t)

#Race
race_t <- ggplot(data_filtered, aes(x = Q73, fill = Q73)) +
  geom_bar() +
  theme(legend.position = "none")

print(race_t)

#Full-time, part-time
job_t <- ggplot(data_filtered, aes(x = Q74, fill = Q74)) +
  geom_bar() +
  theme(legend.position = "none")

print(job_t)

#Student
student_t <- ggplot(data_filtered, aes(x = Q75, fill = Q75)) +
  geom_bar() +
  theme(legend.position = "none")

print(student_t)

  #Type
stu_type_t <- ggplot(data_filtered, aes(x = Q76, fill = Q76)) +
  geom_bar() +
  theme(legend.position = "none")

print(stu_type_t)

#Time in profession
 time_job_t<-  ggplot(data_filtered, aes(x = Q79, fill = Q79)) +
   geom_bar() +
   theme(legend.position = "none")
 
 print(time_job_t)

#Role
 role_t <- ggplot(data_filtered, aes(x = Q80, fill = Q80)) +
   geom_bar() +
   theme(legend.position = "none")
 print(role_t)
 
 min_na <- data_filtered %>%
   filter(Q80_4_TEXT != "") %>%
   pull(Q80_4_TEXT)
 print(min_na)

#Monthly Household Income
 inc_t <- ggplot(data_filtered, aes(x = Q132, fill = Q132)) +
  geom_bar() +
  theme(legend.position = "none")
print(inc_t)
 
#Household size
hsize_t <- ggplot(data_filtered, aes(x = Q81, fill = Q81)) +
  geom_bar() +
  theme(legend.position = "none")
print(hsize_t) 

#Number of children
nchild_t <- ggplot(data_filtered, aes(x = Q85, fill = Q85)) +
  geom_bar() +
  theme(legend.position = "none")
print(nchild_t) 

#Number own children
nchild_own_t <- ggplot(data_filtered, aes(x = Q89, fill = Q89)) +
  geom_bar() +
  theme(legend.position = "none")
print(nchild_own_t) 

grid.arrange(nchild_t,nchild_own_t, nrow = 1, ncol = 2)
###

#Group: Parents

#Age
data_filtered$Age_Group_p <- cut(as.numeric(data_filtered$Q143), 
                               breaks = seq(0, 100, by = 10), 
                               right = FALSE, 
                               labels = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90-99"))



age_p <- ggplot(data_filtered, aes(x = Age_Group_p, fill = Age_Group_p)) +
  geom_bar() +
  labs(x = "Age Group", y = "Count") +
  theme(legend.position = "none")

print(age_p)

#Gender
gender_p <- ggplot(data_filtered, aes(x = Q91, fill = Q91)) +
  geom_bar() +
  theme(legend.position = "none")

print(gender_p)

#Marital Status
marstat_p <- ggplot(data_filtered, aes(x = Q92, fill = Q92)) +
  geom_bar() +
  theme(legend.position = "none")

print(marstat_p)

#Education Level
ed_p <- ggplot(data_filtered, aes(x = Q93, fill = Q93)) +
  geom_bar() +
  theme(legend.position = "none")

print(ed_p)

#Primary Language
lang_p <- ggplot(data_filtered, aes(x = Q94, fill = Q94)) +
  geom_bar() +
  theme(legend.position = "none")

print(lang_p)

#Hispanic or Latino
hisp_p <- ggplot(data_filtered, aes(x = Q95, fill = Q95)) +
  geom_bar() +
  theme(legend.position = "none")

print(hisp_p)

#Race
race_p <- ggplot(data_filtered, aes(x = Q96, fill = Q96)) +
  geom_bar() +
  theme(legend.position = "none")

print(race_p)

#Employed
emp_p <- ggplot(data_filtered, aes(x = Q112, fill = Q112)) +
  geom_bar() +
  theme(legend.position = "none")

print(emp_p)

#Full-time, part-time
job_p <- ggplot(data_filtered, aes(x = Q97, fill = Q97)) +
  geom_bar() +
  theme(legend.position = "none")

print(job_p)

#Type of work
type_work_p <- ggplot(data_filtered, aes(x = type_of_work, fill = type_of_work))+
  geom_bar()+
  theme(legend.position = 'none')
 
print(type_work_p)

#Social Worker
social_work_p <- ggplot(data_filtered, aes(x = Q137, fill = Q137))+
  geom_bar()+
  theme(legend.position = 'none')

print(social_work_p)

#type
social_work_type_p <- ggplot(data_filtered, aes(x = Q138, fill = Q138)) +
  geom_bar() +
  theme(legend.position = "none")
 print(social_work_type_p)
 
#Early Childhood/Childcare professionals
 ed_group_p <- ggplot(data_filtered, aes(x = Q139, fill = Q139)) +
   geom_bar() +
   theme(legend.position = "none")
 print(ed_group_p)
 
#Educational Instructor and Library (to postsecondary)
 eil_group_p <- ggplot(data_filtered, aes(x = Q140, fill = Q140)) +
   geom_bar() +
   theme(legend.position = "none")
 print(eil_group_p)
 
#Healthcare
health_p <- ggplot(data_filtered, aes(x = Q141, fill = Q141)) +
  geom_bar() +
  theme(legend.position = "none")
print(health_p)

  #Groups
  health_group_p <- ggplot(data_filtered, aes(x = Q142, fill = Q142)) +
  geom_bar() +
  theme(legend.position = "none")
  print(health_group_p)

#Student
student_p <- ggplot(data_filtered, aes(x = Q98, fill = Q98)) +
    geom_bar() +
    theme(legend.position = "none")
  
print(student_p)
  
#Type
stu_type_p <- ggplot(data_filtered, aes(x = Q99, fill = Q99)) +
    geom_bar() +
    theme(legend.position = "none")
  
print(stu_type_p)

#Monthly Household Income
inc_p <- ggplot(data_filtered, aes(x = Q133, fill = Q133)) +
  geom_bar() +
  theme(legend.position = "none")
print(inc_p)

#Household size
hsize_p <- ggplot(data_filtered, aes(x = Q104, fill = Q104)) +
  geom_bar() +
  theme(legend.position = "none")
print(hsize_p) 

#Number of children
nchild_p <- ggplot(data_filtered, aes(x = Q107, fill = Q107)) +
  geom_bar() +
  theme(legend.position = "none")
print(nchild_p) 

############################################################################

#Signup
signup <- ggplot(data_filtered, aes(x = Q119, fill = Q119)) +
  geom_bar() +
  theme(legend.position = "none")
print(signup) 

####################Qualitative Answers####################################

#Positives and Negatives of AI in Education - Parents/Teachers
procon_comm<- data_filtered %>%
  filter(Q27 != "") %>%
  pull(Q27)
print(procon_comm)

#Positives and Negatives of AI in Daily Life - Others
procon_comm_o<- data_filtered %>%
  filter(Q43 != "") %>%
  pull(Q43)
print(procon_comm_o)

#Knowledge of Privacy Terms and Conditions
privacy_termscon<- data_filtered %>%
  filter(Q47 != "") %>%
  pull(Q47)
print(privacy_termscon)

#Data Agreed to Share When Agreeing to Provacy Terms and Conditions
share_termscon<- data_filtered %>%
  filter(Q48 != "") %>%
  pull(Q48)
print(share_termscon)







#Comments
comments<- data_filtered %>%
  filter(Q120 != "") %>%
  pull(Q120)
print(comments)


 