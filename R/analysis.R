library(stargazer)
#regression on messages only
a <- lm(MAXBID ~ GROUP, data = data)
#regression on messages and demographics
b <- lm(MAXBID ~ GROUP + AGE + GENDER + MARRIED + WIFECOOK + 
             JOINTDECISION + HUSBANDDECISION, 
           data = data)
#regression on messages and wealth
c <- lm(MAXBID ~ GROUP + WEALTH + TIME_EMPLOYED, data = data)
#regression on messages and stone use
d <- lm(MAXBID ~ GROUP + TSF_PRIMARY + GATHERWOOD_DUMMY + 
             BUYWOOD_DUMMY + WOOD, data = data)
#regression on full model
e <- lm(MAXBID ~ GROUP + AGE + GENDER + MARRIED + WIFECOOK + JOINTDECISION + 
             HUSBANDDECISION + WEALTH + TIME_EMPLOYED + TSF_PRIMARY + 
             GATHERWOOD_DUMMY + BUYWOOD_DUMMY + WOOD, 
           data = data)
stargazer(a, b, c, d, e, type="html", out="MaxBidTraditionalStats.html") #export to html #note:copy to excel then word

#regression on messages only, with time payment scheme
a <- lm(MAXBIDNOVEL ~ GROUP, data = data)
#regression on messages and demographics, with time payment scheme
b <- lm(MAXBIDNOVEL ~ GROUP + AGE + GENDER + MARRIED + WIFECOOK + 
             JOINTDECISION + HUSBANDDECISION, 
           data = data)
#regression on messages and wealth
c <- lm(MAXBIDNOVEL ~ GROUP + WEALTH + TIME_EMPLOYED, data = data)
#regression on messages and stone use
d <- lm(MAXBIDNOVEL ~ GROUP + TSF_PRIMARY + GATHERWOOD_DUMMY + 
          BUYWOOD_DUMMY + WOOD, data = data)
#regression on full model
e <- lm(MAXBIDNOVEL ~ GROUP + AGE + GENDER + MARRIED + WIFECOOK + JOINTDECISION + 
             + HUSBANDDECISION + WEALTH + TIME_EMPLOYED + TSF_PRIMARY + 
          GATHERWOOD_DUMMY + BUYWOOD_DUMMY + WOOD, 
           data = data)
stargazer(a, b, c, d, e, type="html", out="MaxBidNovelStats.html") #export to html #note:copy to excel then word
#ttest of traditional payment and time payment scheme
t.test(data$MAXBID, data$MAXBIDNOVEL)
mean(data$MAXBID, na.rm=TRUE)
sd(data$MAXBID, na.rm=TRUE)
mean(data$MAXBIDNOVEL, na.rm=TRUE)
sd(data$MAXBIDNOVEL, na.rm=TRUE)