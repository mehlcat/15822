# Demographics

#Living group
print("Living Group of Respondents")
xtable(table(demographics$living_group))

ggplot(demographics, aes(x=living_group)) + 
  geom_histogram(aes(y=..count../sum(..count..), fill=factor(living_group)) + xlab("Living Group of Respondents") + ylab("Percentage") + 
  theme(axis.text.x=element_text(angle=50, size=10, vjust=0.5)) +
  scale_colour_brewer(palette=3)

#Major
print("Major of Respondents")
table(demographics$major)
ggplot(demographics, aes(x=major)) + 
  geom_histogram(aes(y=..count../sum(..count..)), binwidth=1, fill="white", colour="black") + xlab("Major of Respondents") + ylab("Percentage of Respondents")

#Gender
print("Gender of Respondents")
table(demographics$gender)

#Year
print("Graduation Year of Respondents")
table(as.factor(demographics$year))

#International
print("International")
table(as.factor(as.logical(demographics$international)))

#Office Hours
print("Goes to Office Hours")
table(as.factor(as.logical(demographics$goes_to_office_hours)))

#GECD
print("Has visited the GECD")
table(as.factor(as.logical(demographics$visited_GECD)))
print("Knows where the GECD is")
table(as.factor(as.logical(demographics$knows_where_GECD_is)))
print("Knew what GECD stood for before taking this survey")
table(as.factor(as.logical(demographics$knew_what_GECD_stood_for)))
print("Certainty of Post-graduate Plans")

print("Estimate of how general population would rate the GECD on a scale from 1-100")
summary(demographics$estimate_how_general_MIT_pop_would_rate_GECD)
ggplot(demographics, aes(x=estimate_how_general_MIT_pop_would_rate_GECD)) +
  geom_histogram(aes(y=..count../sum(..count..)), binwidth=10, fill="white", colour="black") + xlab("Estimate of how general population would rate the GECD on a scale from 1-100") + ylab("Percentage of Respondents")


#Comparison of certainty of Post-grad plans
summary(demographics$certainty_of_postgrad_plans)
summary(demographics$estimate_of_certainty_of_friends_postgrad_plans)

ggplot(demographics, aes(x=1, y=certainty_of_postgrad_plans)) + 
  geom_boxplot(width=.01) +  scale_x_continuous(breaks=NULL) + 
  xlab("") + ylab("Reported Certainty in % Form") +
  ggtitle("Respondents' Reported \n Certainty of Their \n Post-gradudate Plans")

ggplot(demographics, aes(x=1, y=estimate_of_certainty_of_friends_postgrad_plans)) + 
  geom_boxplot(width=.01) + scale_x_continuous(breaks=NULL) + 
  ylab("Estimate of Certainty in % Form") +
  xlab("") + ggtitle("Estimated \n Certainty of their Peer's \n Postgraduate Plans")


#Comparison of Deterrence, 

dyou <- deterrence_for_you
dyou["who"] <- rep("respondent", nrow(deterrence_for_you))
dfriends <- deterrence_for_friends
dfriends["who"] <- rep("friends", nrow(deterrence_for_friends))

deterrence_total = rbind(dyou, dfriends)
deterrence_total = melt(deterrence_total, id=c("ID", "who"))
deterrence_total$variable <- as.factor(deterrence_total$variable)


levels(deterrence_total$variable) = c("Bad Reputation", "No Services I Want", "Intimidated", 
                                      "Unfriendly Staff", "Don't know what it offers", 
                                      "Don't know what to expect", "Don't trust", "Takes an effort", "Anxious", "Not Applicable")

ggplot(deterrence_total, aes(x=who, y=value/39, fill=as.factor(who))) + 
  geom_bar(stat="identity")  +
  facet_wrap(~ variable, nrow=2) +
  ggtitle("% of Respondents who said the following might deter them \n (or their friends) from going to the GECD") +
  ylab("Percent/100") + xlab("")  +
  theme(axis.ticks = element_blank(), axis.text.x = element_blank()) 

#Comparison of importance v quality

qqual <- rank_quality_of_service
qqual["rating"] <- rep("quality", 39)
qimp <- rank_importance_of_service
qimp["rating"] <- rep("importance",  39)

q_total = rbind(qqual, qimp)
q_total = melt(q_total, id=c("ID", "rating"))
q_total$variable <- as.factor(q_total$variable)

for (cmd in levels(q_total$variable)){

q_total$value[q_total$rating == "quality" & q_total$variable== cmd] =  q_total$value[q_total$rating == "quality" & q_total$variable== cmd] / length(na.omit(q_total$value[q_total$rating == "quality" & q_total$variable== cmd]))
q_total$value[q_total$rating == "importance" & q_total$variable== cmd] =  q_total$value[q_total$rating == "importance" & q_total$variable== cmd] / length(na.omit(q_total$value[q_total$rating == "importance" & q_total$variable== cmd]))

}

levels(q_total$variable) = c("Help w Grad School", "Help w Finding Employment", "Premed Advising", 
                                               "Drop in Hours", "Resume Critique", "Mock Interviews", "Career Tests", 
                                               "Career Workshops", "Help w Going Abroad", "Networking Opportunities")

ggplot(q_total, aes(x=rating, y=value, fill=as.factor(rating))) + 
  geom_bar(stat="identity")  +
  facet_wrap(~ variable, nrow=2) +
  ggtitle("How Respondents Rated the Importance and Quality of GECD Services") +
  ylab("Rank from 0-100") + xlab("")  +
  theme(axis.ticks = element_blank(), axis.text.x = element_blank()) 
