library(dplyr)

library(Hmisc) 
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
opp_app_data <- read.csv("/Users/Caner/Desktop/projects/iba/opportunity_applications_iba_challenge.csv",
                         na.strings=c(""," ","NA"),
                         stringsAsFactors=FALSE)
# Takes around 90 sec to read | 4,678,888 vs 10

################################## NOTES ###########################################################
# opportunity_id's can be used for joining 
# approved (56116, 0.012),open(1659160, 0.355)(276705, 0.059), rejected (1266656, 0.271), withdrawn (1346569,0.288)

################################## EXPLORATORY #####################################################

unique_opp_app <-  length(unique(opp_app_data$opportunity_id))
# 334,285 / 4,678,887 unique

opp_app_description <- describe(opp_app_data)
# Descriptive statistics 

opp_app_NA <-sapply(opp_data, function(y) sum(length(which(is.na(y))))) 

unq_opp_app <- unique(opp_app_data$opportunity_id)
# 334,285 opportunities. 284,047 matches w opp_app 

counts_status <- opp_app_data %>%
    group_by(status) %>% 
    summarise(n= n()) %>%
    arrange(n)
# counts of each status category

################################## CLEANING ########################################################

opp_app_data <- opp_app_data[ , -which(names(opp_app_data) %in% c("X"))]
# Dropped the column X 

columns_to_convert_dt <- c("experience_start_date","experience_end_date","matched_or_rejected_at")
for (col in columns_to_convert_dt){
    opp_app_data[,col] <- as.POSIXct(opp_app_data[,c(col)], format="%Y-%m-%d %H:%M:%S")
}
# Converted date columns to POSIXct

which(opp_app_data$opportunity_id)[1] %in% unique(opp_data$opportunity_id))

