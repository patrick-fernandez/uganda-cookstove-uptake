#Get summary statistics
get_summary_stats(data, type = "mean_sd")
#Number of observation for each group
table(data$GROUP)
#Check for randomization
#Show summary for each treatment group and control group
summaryRandom <- datatbl %>% group_by(GROUP) %>%
  get_summary_stats(., type = "mean_sd")
#Transform to long format
#Put all variables in the same column except for GROUP variable
datatbl.long <- datatbl %>% 
  pivot_longer(-GROUP, names_to = "variables", values_to = "value")
#Perform t-test, compare treatment to control only, add column for SD
stat.test <- datatbl.long %>%
  group_by(variables) %>%
  t_test(value ~ GROUP, detailed = TRUE) %>%
  adjust_pvalue(method = "BH") %>%
  add_significance()%>% 
  filter(group1 == "No Message") %>%
  mutate(sd = estimate/statistic) %>%
  mutate(estimate = estimate*-1) %>%
  select("variables", "group1", "group2", "estimate", "sd", "p.adj", 
         "p.adj.signif")

#Check for Joint Significance for each group
summary(lm(joint ~. -GROUP -MAXBID -MAXBIDNOVEL, 
           data = data %>% mutate(joint = ifelse(GROUP == "No Message", 1, 0))))
summary(lm(joint ~. -GROUP -MAXBID -MAXBIDNOVEL, 
           data = data %>% mutate(joint = ifelse(GROUP == "Improves Health", 1, 0))))
summary(lm(joint ~. -GROUP -MAXBID -MAXBIDNOVEL, 
           data = data %>% mutate(joint = ifelse(GROUP == "Saves Time and Money", 1, 0))))
summary(lm(joint ~. -GROUP -MAXBID -MAXBIDNOVEL, 
           data = data %>% mutate(joint = ifelse(GROUP == "Time, Money and Health", 1, 0))))
