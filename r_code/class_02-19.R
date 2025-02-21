library(tidyverse)

womenShots <- read.csv("~/Documents/math133/datasets/womenShots.csv") %>%
  select(-c("X", 'start.time', 'end.time', 'category')) %>%
  filter(Hand!="Unknown")%>%
  mutate(Hand=fct_relevel(Hand, 'Right'))


glimpse(womenShots)

goal_glm1 = glm(Goal=="Goal"~abs(xj)+yj, data=womenShots, family="binomial")
summary(goal_glm1)

womenShots$Prediction1=ifelse(
  predict(goal_glm1, womenShots, type="response") > 0.5, "Goal", "Not Goal")

# Accuracy
sum(womenShots$Prediction1==womenShots$Goal)/length(womenShots$Goal)

table(womenShots$Tactic)


# update
goal_glm2 = update(goal_glm1, .~.+Hand)
summary(goal_glm2)