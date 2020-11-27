library(dplyr)
library(rstatix)
library(tidyverse)
data <- read.csv("Marketing_messages_and_stoves.csv")
#data cleaning - remove all entries not assigned to neither treatment 
#nor control groups
data <- data[data$GROUP!="", ]
#add and compute for wealth
data$TV <- as.numeric(as.factor(data$TV) %>% factor(c("No","Yes"))) -1
data$TV[is.na(data$TV)] <- 0
data$RADIO <- as.numeric(as.factor(data$RADIO) %>% factor(c("No","Yes"))) -1
data$RADIO[is.na(data$RADIO)] <- 0
data$HOWMANYCOWS <- as.numeric(as.factor(data$HOWMANYCOWS) %>%
                                 factor(c("I don't know", "0-5 cows", 
                                          "6-10 cows", "11-15 cows", "16 or more")))
data$HOWMANYCOWS[is.na(data$HOWMANYCOWS)] <- 0
data$HOWMANYCOWS[data$HOWMANYCOWS == 1] <- 0
data$HOWMANYCOWS[data$HOWMANYCOWS == 2] <- 2.5
data$HOWMANYCOWS[data$HOWMANYCOWS == 3] <- 8
data$HOWMANYCOWS[data$HOWMANYCOWS == 4] <- 13
data$HOWMANYCOWS[data$HOWMANYCOWS == 5] <- 16
data$VEHICLE1 <- as.numeric(as.factor(data$VEHICLE1) %>% 
                              factor(c("None", "Bicycle", "Motorcycle", "Car")))
data$VEHICLE1[is.na(data$VEHICLE1)] <- 0
data$VEHICLE1[data$VEHICLE1 == 1] <- 0
data$VEHICLE1[data$VEHICLE1 == 2] <- 69.7
data$VEHICLE1[data$VEHICLE1 == 3] <- 783.78
data$VEHICLE1[data$VEHICLE1 == 4] <- 4509.61
data$HH_PHONES[is.na(data$HH_PHONES)] <- 0
data <- data %>% mutate(WEALTH = (VEHICLE1 + (TV*134.27) + (RADIO*13.83) + 
                                    (HOWMANYCOWS*252.23) + (HH_PHONES*34.26)))
#create dummy for husband as decision maker
data <- data %>% mutate(HUSBANDDECISION = ifelse(DECISIONS2 == "Husband", 1, 0))
#remove unneccessary columns
varlist= c("AGE", "GENDER", "MARRIED", "WIFECOOK", "JOINTDECISION",
           "HUSBANDDECISION", "WEALTH", "TIME_EMPLOYED", "TSF_PRIMARY", 
           "GATHERWOOD_DUMMY", "WOOD", "BUYWOOD_DUMMY")
data <- select(data, c(all_of(varlist), "GROUP", "MAXBID", "MAXBIDNOVEL"))
#Set factor for treatment and control groups
data$GROUP <- as.factor(data$GROUP) %>% factor(c("No Message","Improves Health",
                                                 "Saves Time and Money", "Time, Money and Health"))
#covert chr to int dummies
data$GENDER <- as.integer(as.factor(data$GENDER) %>% factor(c("Female","Male"))) -1
data$WOODLASTMONTH1 <- as.integer(as.factor(data$WOODLASTMONTH1) %>% factor(c("No","Yes"))) -1
#convert UGX to USD
data$MAXBID <- data$MAXBID*0.00027
data$MAXBIDNOVEL <- data$MAXBIDNOVEL*0.00027
datatbl <- as_tibble(data) #convert to data to tibble
options(scipen = 999) #turn off scientific notation

