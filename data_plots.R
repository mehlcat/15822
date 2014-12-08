#15.822 Marking Research Final project
#Giant computing mess


#Importing libraries and data, sinking output to /dev/null

{ sink("/dev/null");
  
  demographics <- read.csv("~/Desktop/Term/F14/15.822/data_analysis/data/surveydata_input - demographics.csv");
  attach(demographics);
  
  text <- read.csv("~/Desktop/Term/F14/15.822/data_analysis/data/surveydata_input - text.csv", stringsAsFactors=FALSE);
  attach(text);
  
  deterrence_for_friends <- read.csv("~/Desktop/Term/F14/15.822/data_analysis/data/surveydata_input - deterrence_for_friends.csv");
  attach(deterrence_for_friends);
  
  deterrence_for_you <- read.csv("~/Desktop/Term/F14/15.822/data_analysis/data/surveydata_input - deterrence_for_you.csv");
  attach(deterrence_for_you);
  
  how_heard_about_GECD <- read.csv("~/Desktop/Term/F14/15.822/data_analysis/data/surveydata_input - how_heard_about_GECD.csv");
  attach(how_heard_about_GECD);
  
  rank_importance_of_service <- read.csv("~/Desktop/Term/F14/15.822/data_analysis/data/surveydata_input - rank_importance_of_service.csv");
  attach(rank_importance_of_service);
  
  didnt_know_about <- read.csv("~/Desktop/Term/F14/15.822/data_analysis/data/surveydata_input - didnt_know_about.csv");
  attach(didnt_know_about);
  
  rank_quality_of_service <- read.csv("~/Desktop/Term/F14/15.822/data_analysis/data/surveydata_input - rank_quality_of_service.csv");
  attach(rank_quality_of_service);
  
  postgrad_plans <- read.csv("~/Desktop/Term/F14/15.822/data_analysis/data/surveydata_input - postgrad_plans.csv");
  attach(postgrad_plans);
  
  source_of_advice <- read.csv("~/Desktop/Term/F14/15.822/data_analysis/data/surveydata_input - source_of_advice.csv");
  attach(source_of_advice);
  
  library("ggplot2", lib.loc="/usr/lib/R/site-library");
  library("knitr", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.0")
  library("xtable", lib.loc="/usr/lib/R/site-library")
  library("reshape2", lib.loc="/usr/lib/R/site-library")
  
  sink(); }

# Demographics

#Living group
print("Living Group of Respondents")
xtable(table(demographics$living_group))
      
ggplot(demographics, aes(x=living_group)) + 
    geom_histogram(aes(y=..count../sum(..count..), fill=factor(living_group))) + xlab("Living Group of Respondents") + ylab("Percentage") + 
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


#Comparison of Deterrence, advice, 

dyou <- deterrence_for_you
dyou["who"] <- rep("respondent", nrow(deterrence_for_you))
dfriends <- deterrence_for_friends
dfriends["who"] <- rep("friends", nrow(deterrence_for_friends))

deterrence_total = rbind(dyou, dfriends)
deterrence_total = melt(deterrence_total, id=c("id, who"))
deterrence_total$variable <- as.factor(deterrence_total$variable)

levels(deterrence_total$variable) = c("Bad Reputation", "No Services I Want", "Intimidated", 
"Unfriendly Staff", "Don't know what it offers", 
"Don't know what to expect", "Don't trust", "Takes an effort", "Anxious", "Not Applicable")

ggplot(deterrence_total, aes(x=who, y=value/39, fill=as.factor(who))) + 
  geom_bar(stat="identity")  +
  facet_wrap(~ variable, nrow=2) +
  ggtitle("% of Respondents who said the following might deter them \n (or their friends) from going to the GECD") +
  ylab("Percent") + xlab("")  +
  theme(axis.ticks = element_blank(), axis.text.x = element_blank()) 

#checking data again
t(lapply(deterrence_for_you[names(deterrence_for_you)], sum))

#Heirarchical Clustering of demographics

demogmat = 
  merge(merge(merge(merge(
    demographics, postgrad_plans, by="ID", all=TRUE), 
    source_of_advice,  by="ID", all=TRUE), 
    deterrence_for_you,  by="ID", all=TRUE), 
    rank_importance_of_service,  by="ID", all=TRUE)

demogmat$visited_GECD <- NULL

demogmat$X <- NULL
demogmat$ID <- NULL

demogmat$don.t_trust_to_be_helpful <- NULL
demogmat$unfriendly_staff <- NULL
demogmat$intimidated <- NULL
demogmat$bad_reputation <- NULL

clustermat = demogmat

clustermat[names(clustermat[!(names(clustermat) %in% c("major", "gender", "living_group"))])] <- scale(na.pass(clustermat[!(names(clustermat) %in% c("major", "gender", "living_group"))]))
head(clustermat)

#Cluster dendrogram

d <- dist(na.omit(clustermat), method = "euclidean") 
fit <- hclust(d, method="ward")  
plot(fit)

groups <- cutree(fit, k=3)
rect.hclust(fit, k=3, border="red")

#Data analysis on clustering

clustered_IDs = data.frame(as.numeric(row.names(na.omit(clustermat))), rep(0, length(row.names(na.omit(clustermat)))), stringsAsFactors = FALSE)
colnames(clustered_IDs) = c("Ids", "group")

clustered_IDs[which(groups==1), "group"] <- 1
clustered_IDs[which(groups==2), "group"] <- 2
clustered_IDs[which(groups==3), "group"] <- 3

clustered_IDs = rbind(clustered_IDs, c(34, NA)) 
clustered_IDs = rbind(clustered_IDs, c(1, NA)) 
clustered_IDs = rbind(clustered_IDs, c(36, NA)) 

clustered_IDs = clustered_IDs[sort.list(as.numeric(clustered_IDs$Ids)), ]

post_clustered = demographics
post_clustered["group"] = clustered_IDs["group"]

post_clustered = na.omit(post_clustered)
total = length(post_clustered$ID)
ngroup1 = length(post_clustered$group[post_clustered$group == 1])
ngroup2 = length(post_clustered$group[post_clustered$group == 2])
ngroup3 = length(post_clustered$group[post_clustered$group == 3])

melt_and_merge <- function(d_name) {
  melted_merged = merge(post_clustered[, c("ID", "group")], d_name, by="ID", all=TRUE)
  melted_merged = melt(melted_merged, id=c("ID", "group"))
  melted_merged$variable <- as.factor(melted_merged$variable)
  
  melted_merged$value[melted_merged$ID == 1] = melted_merged$value[melted_merged$ID == 1]/ngroup1
  melted_merged$value[melted_merged$ID == 2] = melted_merged$value[melted_merged$ID == 2]/ngroup1
  melted_merged$value[melted_merged$ID == 3] = melted_merged$value[melted_merged$ID == 3]/ngroup1
  
  return(melted_merged)
}

ggplot(post_clustered, aes(x=year, y=..density.., fill=as.factor(group))) +
  geom_density(alpha=.2) 

ggplot(post_clustered, aes(x=certainty_of_postgrad_plans, y=..density.., fill=as.factor(group))) +
  geom_density(alpha=.2) 

ggplot(post_clustered, aes(x=estimate_of_certainty_of_friends_postgrad_plans, y=..density.., fill=as.factor(group))) + 
  geom_density(alpha=.2) 

ggplot(post_clustered, aes(x=estimate_how_general_MIT_pop_would_rate_GECD, y=..density.., fill=as.factor(group))) + 
  geom_density(alpha=.2) 

ggplot(post_clustered, aes(x=as.logical((visited_GECD)), fill=as.factor(group))) +
  geom_histogram( alpha=.2, position="dodge")

ggplot(post_clustered, aes(x=as.logical((goes_to_office_hours)), fill=as.factor(group))) +
  geom_histogram(alpha=.2, position="dodge")

ggplot(post_clustered, aes(x=(gender), fill=as.factor(group))) +
    geom_histogram(alpha=.2, position="dodge")

ggplot(post_clustered, aes(x=as.logical((knows_where_GECD_is_located)), fill=as.factor(group))) +
     geom_histogram(alpha=.2, position="dodge")

grouped_merged = melt_and_merge(deterrence_for_you)


levels(grouped_merged$variable) = c("Bad Reputation", "No Services I Want", "Intimidated", 
                                      "Unfriendly Staff", "Don't know what it offers", 
                                      "Don't know what to expect", "Don't trust", "Takes an effort", "Anxious", "Not Applicable")

ggplot(grouped_merged, aes(x=group, y=value/length(grouped_merged$ID), fill=as.factor(group))) + 
  geom_bar(stat="identity")  +
  facet_wrap(~ variable, nrow=2) +
  ggtitle("% of Respondents who said the following might deter them \n (or their friends) from going to the GECD") +
  ylab("Percent") + xlab("")  +
  theme(axis.ticks = element_blank(), axis.text.x = element_blank()) 

grouped_merged = na.omit(merge(post_clustered[, c("ID", "group")], rank_importance_of_service, by="ID", all=TRUE))
grouped_merged = melt(grouped_merged, id=c("ID", "group"))
grouped_merged$variable <- as.factor(grouped_merged$variable)



grouped_merged = na.omit(merge(post_clustered[, c("ID", "group")], didnt_know_about, by="ID", all=TRUE))
grouped_merged = melt(grouped_merged, id=c("ID", "group"))
grouped_merged$variable <- as.factor(grouped_merged$variable)


grouped_merged = na.omit(merge(post_clustered[, c("ID", "group")], source_of_advice, by="ID", all=TRUE))
grouped_merged = melt(grouped_merged, id=c("ID", "group"))
grouped_merged$variable <- as.factor(grouped_merged$variable)

