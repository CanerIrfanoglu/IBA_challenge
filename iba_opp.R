library(dplyr)
library(Hmisc) 
library(ggplot2)
library(GGally)
library(DMwR)

source("/Users/Caner/Desktop/SMU_Class/Data_Mining_5580/ass1_kmeans/mining_A1_classwork.R")

################################# READING DATA #################################################### 
###### 1 - CHUNKS ########

####### This part will be used if I decide to read the lines in chunks #####

# con <- file("/Users/Caner/Desktop/projects/iba/opportunity_iba_challenge.csv", "r")
# opp_chunk <- list()
# for (i in 1:ceiling(813635/50000)){
#     opp_chunk(i) <- readLines(con,50000)
# }
# close(con)

####################################################################################################
###### 2 - SINGLE FILE  ########
opp_data <- read.csv("/Users/Caner/Desktop/projects/iba/iBA_ass1/opportunity_iba_challenge.csv",
                     na.strings=c(""," ","NA"),
                     stringsAsFactors=FALSE)
# Takes around 30 sec to read | 813,634 vs 23

################################## NOTES ###########################################################
# Duration is in weeks!
# Opportunity ID is not unique. INVESTIGATE!


################################## EXPLORATORY #####################################################

counts_status <- opp_data %>%
    group_by(status) %>% 
    summarise(n= n()) %>%
    arrange(n)
# Counts in status | Levels: open    draft   removed

unique_opportunities <- length(unique(opp_data$opportunity_id))
# 637,324 / 813,634 unique

opp_description <- describe(opp_data)
# Descriptive statistics 

opp_NA <-sapply(opp_data, function(y) sum(length(which(is.na(y))))) 
# NA counts 



####################### GETTING UNIQUE OPPORTUNUTIES ##############################################

#### TRYING TO GET ONES WITH VALID experience_start_date & experience_end_dates

#opp_data_counts <- sqldf("select *, count(opportunity_id) as counts from opp_data group by opportunity_id")
# get the counts for opportunity_ids

#opp_data_join <- sqldf("select * from opp_data o inner join opp_data_counts oc on o.opportunity_id = oc.opportunity_id")
# join with original dataset

#opp_data_join <- opp_data_join[,names(opp_data_join) %nin% "opportunity_id..23"]
# drop extra opportunity_id..23

#opp_data_duplicates <- opp_data_join[opp_data_join$counts > 1,]
# filter out the duplicate rows | 37477 UNIQUE !

#opp_data_no_NA <- opp_data_duplicates[!is.na(opp_data_duplicates$experience_start_date ),] # 29684 unique
# only non NA in experience_start_date

# NOT MUCH DIFFERENCE THAN GROUP BY WILL BE COMPLETED LATER IF NEEDED 

################################################################################################


opp_data_unique <- sqldf("select * from opp_data group by opportunity_id")
# Getting only unique opportunities 

#opp_data_unique <- opp_data %>% group_by(opportunity_id) %>% summarise(n = n())
# dplyr way filtering out duplicates

##############################FROM VINAY ##########################################################
#Finding the recency of the date of creation
#opp_data$recent <- floor(difftime(Sys.Date(), as.Date(as.character(opp_data$created_at), format="%Y-%m-%d %H:%M:%S"), units = "weeks"))

#finding the duration from the given earliest start and latest end dates
#opp_data$duration_calc <- floor(difftime(opp_dat$latest_end_date, opp_dat$earliest_start_date, units = "weeks"))
####################################################################################################

############################## PREPARING FOR KMEANS#################################################

uniq_desciption <- describe(opp_data_unique)
# Hmisc Describe 


opp_data <- opp_data[ , -which(names(opp_data) %in% c("X"))]
# Dropped the column X 

columns_to_convert_dt <- c("created_at","applications_close_date","earliest_start_date",
                           "latest_end_date","matched_or_rejected_at","experience_start_date",
                           "experience_end_date")
for (col in columns_to_convert_dt){
    opp_data[,col] <- as.POSIXct(opp_data[,c(col)], format="%Y-%m-%d %H:%M:%S")
}
# Converted date columns to POSIXct




opp_data_kmeans <- opp_data_unique %>% filter(programme_id == 'Global Volunteer') %>%
    select(opportunity_id, favourites_count,
           opportunity_applications_count, created_at, openings, programme_id) %>%
           mutate(recency = as.integer(round(difftime(max(created_at), 
           created_at , units = "weeks"),0)))

############################## VISUALIZE VARIABLES #################################################
ggpairs_product <- ggpairs(opp_data_kmeans[,which(names(opp_data_kmeans) %nin% c("opportunity_id","programme_id", "created_at"))], 
                           upper = list(continious = ggally_points),
                           lower = list(continious = points),
                           title = "Opportunity data pairs")
####################################################################################################

opp_data_kmeans <- opp_data_kmeans[opp_data_kmeans$openings <100,]
#Filter out openings > 100

opp_data_kmeans <- opp_data_kmeans[opp_data_kmeans$opportunity_applications_count < 1000,]
#Filter out application_count > 1000

#opp_data_kmeans <- opp_data_kmeans[opp_data_kmeans$recency < 950,]
#Filter out recency > 100




scaled_ranges <- sapply(as.data.frame((scale(opp_data_kmeans[,
                which(names(opp_data_kmeans) %nin% c("opportunity_id","programme_id", "created_at"))]))), range)
# View min-max values for each column

opp_data_scaled <- scale(opp_data_kmeans[,which(names(opp_data_kmeans) %nin% c("opportunity_id","programme_id", "created_at"))])
#scaled all the columns except for item_id

set.seed(123)
# set seed before running non-deterministic algorithm


elbow_plot <- plot(withinSSrange(opp_data_scaled,1,10,100))
# elbow plot for number of cluster selection

opp_4_clusters <- kmeans(opp_data_scaled, 4, 100)
# kmeans output for 4 clusters

opp_centers_4 <- unscale(opp_4_clusters$centers, opp_data_scaled)
# dataframe of centers for each cluster & column


opt_w_cluster_numbers_4 <- cbind(opp_data_kmeans, opp_4_clusters$cluster)
# appended corresponding cluster to each product

names(opt_w_cluster_numbers_4)[8] <- "CLUSTER"
# renamed cluster column properly

plot(opt_w_cluster_numbers_4[,names(opt_w_cluster_numbers_4) %nin% c("opportunity_id","programme_id", 
                                                "created_at")], col = opt_w_cluster_numbers_4$CLUSTER)
# plotting the final correlations highlighted by 4 clusters















                           





