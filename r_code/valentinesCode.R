library(tidyverse)

# https://www.kaggle.com/code/rohithmahadevan/relationship-analysis
relationships = 
  read.csv("~/Documents/math133/datasets/relationshipsKaggle.csv") %>%
  rename(gender=Gender,age=Age,loyalty=loyality) %>%
  dplyr::filter(status!="") %>%
  mutate(status=recode(status,
                       "In a relationship"="relationship",
                       "Single"="single"),
         status01=case_when(
           status=="single" ~ 0,
           status=="relationship" ~ 1)
         ) %>%
  select(-matches("X"))

glimpse(relationships)

ggplot(relationships,
       aes(x=age,y=love_age,color=status))+
  geom_point(shape=1)+
  scale_color_manual(values =c("#0072B2","#E69F00"))

set.seed(18)
n=nrow(relationships)
s=sample(n,round(.7*n,0))
train_df=relationships[s,]
test_df=relationships[-s,]


library(class)
status_knn=knn(train_df[,c("age","love_age")],
               test_df[,c("age","love_age")],
               cl=train_df$status,
               k=5)

er5=sum(status_knn!=test_df$status)/length(test_df$status)

null_er=sum(test_df$status=="relationship")/length(test_df$status)

fits=data.frame(K=1:40,er=NA)
for (i in 1:40){
  status_knn=knn(train_df[,c("age","love_age")],
                 test_df[,c("age","love_age")],
                 cl=train_df$status,
                 k=5)
  fits$er[i]=sum(status_knn!=test_df$status)/length(test_df$status) 
}
fits %>% ggplot(aes(x=K,y=er))+geom_line()

