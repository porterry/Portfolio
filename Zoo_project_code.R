library(tidyverse)
library(readxl)

#setting working directory
setwd("C:/Users/Ryan/Desktop/Data Science 485/Zoo")

zoo_data <- read_xlsx("Zoo_Data.xlsx", sheet = 1)

#exploratory graphs
zoo_data %>%
  filter(Exhibit == "Post") %>%
  mutate(Time_spent = Time_spent/60) %>%
  ggplot(aes(Animal,Time_spent)) +
  geom_bar(stat = "identity") #time spent per post animal

zoo_data %>%
  mutate(Time_spent = Time_spent/60) %>%
  ggplot(aes(Age, Time_spent)) +
  geom_bar(stat = "identity")

zoo_data %>%
  mutate(Time_spent = Time_spent/60) %>%
  ggplot(aes(Animal,Time_spent)) +
  geom_bar(stat = "identity")

zoo_data %>% 
  ggplot(aes(Time_spent)) + 
  geom_density(fill='blue', alpha=.5)

zoo_data %>% 
  ggplot(aes(Time_spent,fill=Age)) + 
  geom_density(alpha=.5)

zoo_data %>% 
  ggplot(aes(Exhibit,`2total`)) +
  geom_bar(stat = "identity")

# reshaping the data to make graphs easier
library(reshape2)

subset_1 <- melt(zoo_data[,c('Exhibit','1total','2total','3total','4total','5total')],id.vars = 1)

ggplot(subset_1,aes(x = Exhibit,y = value)) + 
  geom_bar(aes(fill = variable),stat = "identity") +
  ggtitle("Empathy") +
  scale_fill_discrete(name = "Emotion/Behavior", labels = c("Understands Needs", "Take Perspective", 
                                                            "Compassionate Concern","Positive Behavior","Wants to Help"))

subset_2 <- melt(zoo_data[,c('Exhibit','6total','7total','8total','9total','10total')],id.vars = 1)

ggplot(subset_2,aes(x = Exhibit,y = value)) + 
  geom_bar(aes(fill = variable),stat = "identity") +
  ggtitle("Related Emotions") +
  scale_fill_discrete(name = "Emotion/Behavior", labels = c("Curiousity","Appreciation/Respect","Recognizes Agency",
                                 "Direct Action","Caregiver Supports Behavior"))
# grouping variables by name
totals = zoo_data %>% summarise('Understands Needs'=sum(`1total`), 'Take Perspective'=sum(`2total`), 
                                'Compassionate Concern'=sum(`3total`), 
                                'Positive Behavior'=sum(`4total`),'Wants to Help'=sum(`5total`), 
                                'Curiousity'=sum(`6total`), 
                                'Appreciation/Respect'=sum(`7total`), 'Recognizes Agency'=sum(`8total`),
                                'Direct Action'=sum(`9total`), 
                                'Caregiver Supports Behavior'=sum(`10total`))
# bar graph
totals_bar = gather(totals, variable, count)

totals_bar$variable = factor(totals_bar$variable, levels=c('Understands Needs',
                                                           'Take Perspective',
                                                           'Compassionate Concern',
                                                           'Positive Behavior',
                                                           'Wants to Help',
                                                           'Curiousity',
                                                           'Appreciation/Respect',
                                                           'Recognizes Agency',
                                                           'Direct Action',
                                                           'Caregiver Supports Behavior')) #setting levels for legend in the graph

totals_bar %>% ggplot(aes(variable, count, fill=variable)) + 
  geom_bar(stat = 'identity',show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 45, hjust=1)) + 
  scale_color_brewer(palette="Dark2") +
  xlab('Emotion or Behavior') + ylab('Count') + 
  ggtitle('Count of Behaviors Observed') #graph used at the beginning of results section


zoo_data = zoo_data %>% mutate(total = `1total`+`2total`+`3total`+`4total`+`5total`+
                                 `6total`+`7total`+`8total`+`9total`+`10total`,
                               nulls_total = `1_nulls`+`2_nulls`+`3_nulls`+`4_nulls`+`5_nulls`+
                                 `6_nulls`+`7_nulls`+`8_nulls`+`9_nulls`+`10_nulls`)

zoo_data %>% 
  ggplot(aes(Age, total)) + 
  geom_boxplot()


gsums <- zoo_data %>%
  group_by(Animal) %>%
  summarise(Avg_Time = mean(Time_spent),
            total_1 = sum(`1total`), nulls_1 = sum(`1_nulls`),
            total_2 = sum(`2total`), nulls_2 = sum(`2_nulls`),
            total_3 = sum(`3total`), nulls_3 = sum(`3_nulls`),
            total_4 = sum(`4total`), nulls_4 = sum(`4_nulls`),
            total_5 = sum(`5total`), nulls_5 = sum(`5_nulls`),
            total_6 = sum(`6total`), nulls_6 = sum(`6_nulls`),
            total_7 = sum(`7total`), nulls_7 = sum(`7_nulls`),
            total_8 = sum(`8total`), nulls_8 = sum(`8_nulls`),
            total_9 = sum(`9total`), nulls_9 = sum(`9_nulls`),
            total_10 = sum(`10total`), nulls_10 = sum(`10_nulls`)) #table of totals and nulls by animal

sums <- zoo_data %>%
  group_by(Exhibit) %>%
  summarise(Avg_Time = mean(Time_spent),
            total_1 = sum(`1total`), nulls_1 = sum(`1_nulls`),
            total_2 = sum(`2total`), nulls_2 = sum(`2_nulls`),
            total_3 = sum(`3total`), nulls_3 = sum(`3_nulls`),
            total_4 = sum(`4total`), nulls_4 = sum(`4_nulls`),
            total_5 = sum(`5total`), nulls_5 = sum(`5_nulls`),
            total_6 = sum(`6total`), nulls_6 = sum(`6_nulls`),
            total_7 = sum(`7total`), nulls_7 = sum(`7_nulls`),
            total_8 = sum(`8total`), nulls_8 = sum(`8_nulls`),
            total_9 = sum(`9total`), nulls_9 = sum(`9_nulls`),
            total_10 = sum(`10total`), nulls_10 = sum(`10_nulls`)) #table of totals and nulls by pre and post

#Patas(pre) and Baboon(post)
#Spider(pre) and Vervet(post)

#Chi Squared
cont_table <- zoo_data %>% 
  filter(Animal == "Patas" | Animal == "Baboon") %>%
  group_by(Animal) %>%
  summarise(total = sum(`1total`+`2total`+`3total`+`4total`+`5total`+
                          `6total`+`7total`+`8total`+`9total`+`10total`), 
            nulls = sum(`1_nulls`+`2_nulls`+`3_nulls`+`4_nulls`+`5_nulls`+
                          `6_nulls`+`7_nulls`+`8_nulls`+`9_nulls`+`10_nulls`))

cont_table %>% select(-Animal) %>% chisq.test() #pvalue is .0001544

cont_table2 <- zoo_data %>% 
  filter(Animal == "Spider" | Animal == "Vervet") %>%
  group_by(Animal) %>%
  summarise(total = sum(`1total`+`2total`+`3total`+`4total`+`5total`+
                          `6total`+`7total`+`8total`+`9total`+`10total`), 
            nulls = sum(`1_nulls`+`2_nulls`+`3_nulls`+`4_nulls`+`5_nulls`+
                          `6_nulls`+`7_nulls`+`8_nulls`+`9_nulls`+`10_nulls`))

cont_table2 %>% select(-Animal) %>% chisq.test() #pvalue is 1.899*10^-10

cont_table_1 <- zoo_data %>% 
  select(Exhibit, `1total`, `1_nulls`) %>%
  group_by(Exhibit) %>%
  summarise(total_1 = sum(`1total`), nulls_1 = sum(`1_nulls`)) #contingency table of Understands Needs

cont_table_1 %>% select(-Exhibit) %>% chisq.test() #pvalue is .87

cont_table_2 <- zoo_data %>% 
  select(Exhibit, `2total`, `2_nulls`) %>%
  group_by(Exhibit) %>%
  summarise(total_2 = sum(`2total`), nulls_2 = sum(`2_nulls`)) #contingency table of Take Perspective

cont_table_2 %>% select(-Exhibit) %>% chisq.test() #pvalue is .00125

cont_table_3 <- zoo_data %>% 
  select(Exhibit, `3total`, `3_nulls`) %>%
  group_by(Exhibit) %>%
  summarise(total_3 = sum(`3total`), nulls_3 = sum(`3_nulls`)) #contingency table of Compassionate Concern

cont_table_3 %>% select(-Exhibit) %>% chisq.test() #pvalue is 1
cont_table_3 %>% select(-Exhibit) %>% fisher.test() #pvalue is 1

cont_table_4 <- zoo_data %>% 
  select(Exhibit, `4total`, `4_nulls`) %>%
  group_by(Exhibit) %>%
  summarise(total_4 = sum(`4total`), nulls_4 = sum(`4_nulls`)) #contingency table of Positive Behavior

cont_table_4 %>% select(-Exhibit) %>% chisq.test() #pvalue is .13
cont_table_4 %>% select(-Exhibit) %>% fisher.test() #pvalue is .06165

cont_table_5 <- zoo_data %>% 
  select(Exhibit, `5total`, `5_nulls`) %>%
  group_by(Exhibit) %>%
  summarise(total_5 = sum(`5total`), nulls_5 = sum(`5_nulls`)) #contingency table of Wants to Help

cont_table_5 %>% select(-Exhibit) %>% chisq.test() #pvalue is N/A

cont_table_6 <- zoo_data %>% 
  select(Exhibit, `6total`, `6_nulls`) %>%
  group_by(Exhibit) %>%
  summarise(total_6 = sum(`6total`), nulls_6 = sum(`6_nulls`)) #contingency table of Curiousity

cont_table_6 %>% select(-Exhibit) %>% chisq.test() #pvalue is .0000003286

cont_table_7 <- zoo_data %>% 
  select(Exhibit, `7total`, `7_nulls`) %>%
  group_by(Exhibit) %>%
  summarise(total_7 = sum(`7total`), nulls_7 = sum(`7_nulls`)) #contingency table of Appreciation/Respect

cont_table_7 %>% select(-Exhibit) %>% chisq.test() #pvalue is .34

cont_table_8 <- zoo_data %>% 
  select(Exhibit, `8total`, `8_nulls`) %>%
  group_by(Exhibit) %>%
  summarise(total_8 = sum(`8total`), nulls_8 = sum(`8_nulls`)) #contingency table of Recognizes Agency

cont_table_8 %>% select(-Exhibit) %>% chisq.test() #pvalue is .15

cont_table_9 <- zoo_data %>% 
  select(Exhibit, `9total`, `9_nulls`) %>%
  group_by(Exhibit) %>%
  summarise(total_9 = sum(`9total`), nulls_9 = sum(`9_nulls`)) #contingency table of Direct Action

cont_table_9 %>% select(-Exhibit) %>% chisq.test() #pvalue is N/A

cont_table_10 <- zoo_data %>% 
  select(Exhibit, `10total`, `10_nulls`) %>%
  group_by(Exhibit) %>%
  summarise(total_10 = sum(`10total`), nulls_10 = sum(`10_nulls`)) #contingency table of Caregiver Supports Behavior

cont_table_10 %>% select(-Exhibit) %>% chisq.test() #pvalue is .1778


#Empathy (1-5 variables)
#Related Emotions (6-10 variables)

cont_emp <- zoo_data %>%
  group_by(Exhibit) %>%
  summarise(Empathy = sum(Empathy), Nulls = sum(Empathy_Null)) #contingency table of Empathy

cont_emp %>% select(-Exhibit) %>% chisq.test() #pvalue is .05918

cont_emo <- zoo_data %>%
  group_by(Exhibit) %>%
  summarise(Related_Emotions = sum(Related_Emotions), Nulls = sum(Related_Emotions_Null)) #contingency table of Related Emotions

cont_emo %>% select(-Exhibit) %>% chisq.test() #pvalue is .04017
cont_emo %>% select(-Exhibit) %>% fisher.test() # fishers exact test for relation emotions since low frequencies
