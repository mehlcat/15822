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

sink(); }

