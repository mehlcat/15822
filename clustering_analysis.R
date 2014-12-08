#15.822 Marking Research Final project
#Giant computing mess


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



#Some of the entries weren't clustered due to NAs, have the dataframe reflect that
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

#Divide stats into groups

post_clustered = na.omit(post_clustered)
total = length(post_clustered$ID)
ngroup1 = length(post_clustered$group[post_clustered$group == 1])
ngroup2 = length(post_clustered$group[post_clustered$group == 2])
ngroup3 = length(post_clustered$group[post_clustered$group == 3])

melt_and_merge <- function(d_name) {
  melted_merged = merge(post_clustered[, c("ID", "group")], d_name, by="ID", all=TRUE)
  melted_merged = melt(melted_merged, id=c("ID", "group"))
  melted_merged$variable <- as.factor(melted_merged$variable)
  
  melted_merged = na.omit(melted_merged)
  melted_merged$value[melted_merged$group == 1] = melted_merged$value[melted_merged$group == 1]/ngroup1
  melted_merged$value[melted_merged$group == 2] = melted_merged$value[melted_merged$group == 2]/ngroup2
  melted_merged$value[melted_merged$group == 3] = melted_merged$value[melted_merged$group == 3]/ngroup3
  
  return(melted_merged)
}

melt_and_merge_with_NAs <- function(d_name) {
  melted_merged = merge(post_clustered[, c("ID", "group")], d_name, by="ID", all=TRUE)
  melted_merged = melt(melted_merged, id=c("ID", "group"))
  melted_merged$variable <- as.factor(melted_merged$variable)
  
  melted_merged = na.omit(melted_merged)
  
  for (cmd in levels(melted_merged$variable)){
  melted_merged$value[melted_merged$group == 1 & melted_merged$variable== cmd] = melted_merged$value[melted_merged$group == 1 & melted_merged$variable== cmd]/length(na.omit(melted_merged$value[melted_merged$group ==1 & melted_merged$variable== cmd]))
  melted_merged$value[melted_merged$group == 2 & melted_merged$variable== cmd] = melted_merged$value[melted_merged$group == 2 & melted_merged$variable== cmd]/length(na.omit(melted_merged$value[melted_merged$group ==2 & melted_merged$variable== cmd]))
  melted_merged$value[melted_merged$group == 3 & melted_merged$variable== cmd] = melted_merged$value[melted_merged$group == 3 & melted_merged$variable== cmd]/length(na.omit(melted_merged$value[melted_merged$group ==3 & melted_merged$variable== cmd]))
}
  
  return(melted_merged)
}


plot_basics_density <- function(col_name, title) {
  print(col_name)
  ggplot(post_clustered, aes_string(x=col_name, y="..density..", fill="as.factor(group)")) +
    geom_density(alpha=.2) + ggtitle(title)
  
}

plot_basics_density("year", "Density Curve for Graduation Year of Respondents, Clustered by Group")
plot_basics_density("certainty_of_postgrad_plans", "% Certainty of Postgrad Plans of Respondents, Clustered by Group")
plot_basics_density("estimate_of_certainty_of_friends_postgrad_plans", "Estimated % Certainty of Peers' Postgrad Plans, Clustered by Group")
plot_basics_density("estimate_how_general_MIT_pop_would_rate_GECD", "How Respondents Estimate People will Rate the GECD \n on a scale from 0-100, Clustered by Group")

plot_basics_logical <- function(col_name, title){
  
  ggplot(post_clustered, aes_string(x=paste("as.logical(",col_name,")"), fill="as.factor(group)")) +
    geom_histogram(alpha = .8, position="dodge") + ggtitle(title) 
    
}

summary(post_clustered$goes_to_office_hours[post_clustered$group == 1])

plot_basics_logical("visited_GECD", "Those who visited the GECD, Clustered by Group")
plot_basics_logical("goes_to_office_hours", "Goes to Office Hours, Clustered by Group")

ggplot(post_clustered, aes(x=gender, fill=as.factor(group))) +
  geom_histogram(alpha=.8, position="dodge") + ggtitle("Gender, Clustered by Group") 

plot_basics_logical("knows_where_GECD_is_located", "Knows where GECD is located, Clustered by Group")

plot_merged <- function(df, clevel, title) {

levels(df$variable)  = clevel
 ggplot(df, aes_string(x="group", y="value", fill="as.factor(group)")) + 
  geom_bar(stat="identity")  +
  facet_wrap(~ variable, nrow=2) +
  ggtitle(title) +
  ylab("Rating") + xlab("")  +
  theme(axis.ticks = element_blank(), axis.text.x = element_blank())
# scale_y_continuous(labels=function(x) format(100*x,digits = 2))

}

clevel = c("Bad Reputation", "No Services I Want", "Intimidated", 
                                    "Unfriendly Staff", "Don't know what it offers", 
                                    "Don't know what to expect", "Don't trust", "Takes an effort", "Anxious", "Not Applicable")

plot_merged(melt_and_merge(deterrence_for_you), clevel, "% of Respondents who said the following might deter them \n (or their friends) from going to the GECD")

clevel = c("Help w Grad School", "Help w Finding Employment", "Premed Advising", 
          "Drop in Hours", "Resume Critique", "Mock Interviews", "Career Tests", 
          "Career Workshops", "Help w Going Abroad", "Networking Opportunities")
plot_merged(melt_and_merge(rank_importance_of_service), clevel, "Average rating of importance of the following services, ranked from 0-100")

plot_merged(melt_and_merge_with_NAs(rank_quality_of_service), clevel, "Average rating of quality of the following services, ranked from 0-100")

plot_merged(melt_and_merge(didnt_know_about), clevel, "% of Respondents who didn't know about each service")

clevel = c("FSILG Alumni", "Other Alumni or Upperclassmen", "Academic Advisor", "Personal Friends", "MIT Professional Clubs")

plot_merged(melt_and_merge(source_of_advice), clevel, "% of respondents who get their career advice from the following resources")

names(postgrad_plans)= c("ID", "Industry", "Research", "Grad School", "Medical Field", "Start ups", "Other", "Unsure")

clevel = c("Industry", "Research", "Grad School", "Medical Field", "Start ups", "Other", "Unsure")

plot_merged(melt_and_merge(postgrad_plans), clevel, "% of Respondents who have the following postgrad plans")


