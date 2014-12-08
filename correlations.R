
#covariances

demogmat =  
  merge(merge(merge(merge(
    demographics, postgrad_plans, by="ID", all=TRUE), 
    source_of_advice,  by="ID", all=TRUE), 
    deterrence_for_you,  by="ID", all=TRUE), 
    rank_importance_of_service,  by="ID", all=TRUE)

  
demogmat = na.omit(demogmat)

demogmat$X <- NULL
demogmat$ID <- NULL

demogmat$don.t_trust_to_be_helpful <- NULL
demogmat$unfriendly_staff <- NULL
demogmat$intimidated <- NULL
demogmat$Medical_Field <- NULL
demogmat$bad_reputation <- NULL
demogmat$gender <- NULL
demogmat$major <- NULL
demogmat$living_group <- NULL

demogmat = scale(demogmat)

cormat = cor(demogmat)

write.table(cormat, file = "~/Desktop/Term/F14/15.822/data_analysis/cormat.txt", sep=",")