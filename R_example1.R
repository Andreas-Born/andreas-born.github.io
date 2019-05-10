library(dplyr)
library(tidyr)
library(DataCombine) #to create lags






setwd("C:/Users/anbor/Dropbox/Studium/Master Thesis/Politicians/Political Promising Project/Experiment/Data/")
rm(list=ls())
data <-read.csv("Session 14/data_session14.csv", header = TRUE, sep = ",",row.names=NULL)
#renaming variables
colnames(data)[colnames(data)=="participant._round_number"] <- "max_round"
colnames(data)[colnames(data)=="participant.subsession_id"] <- "id_subsession"
colnames(data)[colnames(data)=="subsession.round_number"] <- "round"
colnames(data)[colnames(data)=="participant.id_in_session"] <- "id_in_session"

colnames(data)[colnames(data)=="player.promise"] <- "promise"
colnames(data)[colnames(data)=="player.contribution"] <- "give"
colnames(data)[colnames(data)=="player.dict_contribution"] <- "dict_give"
colnames(data)[colnames(data)=="player.is_receiver"] <- "dict_receiver"
colnames(data)[colnames(data)=="player.choice"] <- "choice"
colnames(data)[colnames(data)=="player.treatment"] <- "treatment"
colnames(data)[colnames(data)=="player.id_in_group"] <- "id_in_group"


colnames(data)[colnames(data)=="group.subsession_id"] <- "id_group"
colnames(data)[colnames(data)=="group.promise1"] <- "group_prom1"
colnames(data)[colnames(data)=="group.promise2"] <- "group_prom2"
colnames(data)[colnames(data)=="group.promise3"] <- "group_prom3"

colnames(data)[colnames(data)=="group.contribution1"] <- "group_give1"
colnames(data)[colnames(data)=="group.contribution2"] <- "group_give2"
colnames(data)[colnames(data)=="group.contribution3"] <- "group_give3"

colnames(data)[colnames(data)=="group.choice"] <- "group_choice"
colnames(data)[colnames(data)=="subsession.receiver"] <- "receiver_ingroup"
colnames(data)[colnames(data)=="session.code"] <- "session_name"


# collecting variables to be deleted
a<-c(grep("participant", colnames(data)), grep("player", colnames(data)), grep("group\\.", colnames(data)), grep("session\\.", colnames(data)))
# reducing dataframe by variables not needed
data <- data[,-a]
rm(a)




###################### preparing variables for analysis ##############################

#generating a session id variable that starts with 1 for the first session
data$id_session <- as.numeric(factor(data$session_name, levels=unique(data$session_name)))
data$id_session <- factor(data$id_session)



###matching data pattern from pilot to other sessions###
#the pilot saved data from the promise game and the dictator game in a parallel round whereas in the other sessinos that
#was saved as two separate rounds

#drop the first period except for the pilot
data <- data[ (data$round>1 | data$id_session == 1), ]



#in session 1 we want to move the promise game data by one round back if participant played the dictator game first
data$helper <- c(data$id_session == 1 & data$round == 1  & is.na(data$give))
data$helper[data$id_session == 1 & !data$round == 1] <- data$helper[data$id_session == 1 & data$round == 1]

for (i in c(1:10)){
  data$give[data$id_session == 1 & data$round == i  & data$helper] <- data$give[data$id_session == 1 & data$round == i+1  & data$helper]
  data$promise[data$id_session == 1 & data$round == i  & data$helper] <- data$promise[data$id_session == 1 & data$round == i+1  & data$helper]
  data$choice[data$id_session == 1 & data$round == i  & data$helper] <- data$choice[data$id_session == 1 & data$round == i+1  & data$helper]
  data$id_in_group[data$id_session == 1 & data$round == i  & data$helper] <- data$id_in_group[data$id_session == 1 & data$round == i+1  & data$helper]

  data$group_give1[data$id_session == 1 & data$round == i  & data$helper] <- data$group_give1[data$id_session == 1 & data$round == i+1  & data$helper]
  data$group_give2[data$id_session == 1 & data$round == i  & data$helper] <- data$group_give2[data$id_session == 1 & data$round == i+1  & data$helper]
  data$group_give3[data$id_session == 1 & data$round == i  & data$helper] <- data$group_give3[data$id_session == 1 & data$round == i+1  & data$helper]
  data$group_choice[data$id_session == 1 & data$round == i  & data$helper] <- data$group_choice[data$id_session == 1 & data$round == i+1  & data$helper]

  data$group_promise1[data$id_session == 1 & data$round == i  & data$helper] <- data$group_promise1[data$id_session == 1 & data$round == i+1  & data$helper]
  data$group_promise2[data$id_session == 1 & data$round == i  & data$helper] <- data$group_promise2[data$id_session == 1 & data$round == i+1  & data$helper]
  data$group_promise3[data$id_session == 1 & data$round == i  & data$helper] <- data$group_promise3[data$id_session == 1 & data$round == i+1  & data$helper]
}

# for the same participants we want to move the dictator game data to the last round
data$dict_give[data$id_session == 1 & data$round == 11  & is.na(data$dict_give)]<- data$dict_give[data$id_session == 1 & data$round == 1  & !is.na(data$dict_give)]
data$give[data$id_session == 1 & data$round == 11] <- NA
data$promise[data$id_session == 1 & data$round == 11] <- NA
data$dict_give[data$id_session == 1 & !data$round == 11] <- NA

#backdating the round by 1
data$round[!data$id_session == 1] <- data$round[!data$id_session == 1]-1

################################### End of rematching pilot to other sessions ##################################################################


#One session was restarted, such that there are empty entries - delete those
data <- data[ !is.na(data$promise) & data$round<11 | !is.na(data$dict_give) & data$round == 11,]



#regenerate the session_id now after empty session is gone
data$id_session <- as.numeric(factor(data$session_name, levels=unique(data$session_name)))
data$id_session <- factor(data$id_session)


#create an indiviual specific id
data$id <- as.integer(data$id_in_session) + max(as.integer(data$id_in_session)) * (as.integer(data$id_session)-1)
#data$id <- factor(data$id, levels = c(1:length(unique(factor(data$id))))) causes problems
d <- select(data, -c("dict_give","choice","dict_receiver"))

#create selected vs not selected
data$prom_selected <- ifelse(data$choice == 1, data$group_prom1, ifelse(data$choice == 2, data$group_prom2, data$group_prom3))
data$give_selected <- ifelse(data$choice == 1, data$group_give1, ifelse(data$choice == 2, data$group_give2, data$group_give3))
data$gived_selected <- ifelse(data$choice == 1, data$group_give1, ifelse(data$choice == 2, data$group_give2, data$group_give3))

data$prom_nselected <- ifelse(data$choice <= 2 & data$id_in_group <= 2 , data$group_prom3, ifelse(data$choice >= 2 & data$id_in_group >= 2, data$group_prom1, data$group_prom2))
data$give_nselected <- ifelse(data$choice <= 2 & data$id_in_group <= 2, data$group_give3, ifelse(data$choice >= 2 & data$id_in_group >= 2, data$group_give1, data$group_give2))


# generate a promise breaking variable
data$broken_prom_selected <- ifelse(data$prom_selected > data$give_selected, 1, 0)
data$broken_prom_nselected <- ifelse(data$prom_nselected > data$give_nselected, 1, 0)

data$broken_prom <- ifelse(data$promise > data$give, 1, 0)
data$reiceiver <- ifelse(data$receiver_ingroup == data$id_in_group, 1, 0)
data$role <- ifelse(data$id_in_group == data$group_choice, "Selected Sender", ifelse(data$reiceiver  == 1, "Receiver", "Not selected Sender"))

#### creating lagged variables ####

data <- slide(data, Var = 'broken_prom_selected', TimeVar = 'round', GroupVar = 'id', NewVar = 'broken_prom_selected_l', slideBy = -1)
data <- slide(data, Var = 'broken_prom', TimeVar = 'round', GroupVar = 'id', NewVar = 'broken_prom_l', slideBy = -1)
data <- slide(data, Var = 'role', TimeVar = 'round', GroupVar = 'id', NewVar = 'role_l', slideBy = -1)
data <- slide(data, Var = 'promise', TimeVar = 'round', GroupVar = 'id', NewVar = 'promise_l', slideBy = -1)
data <- slide(data, Var = 'give', TimeVar = 'round', GroupVar = 'id', NewVar = 'give_l', slideBy = -1)
data <- slide(data, Var = 'prom_selected', TimeVar = 'round', GroupVar = 'id', NewVar = 'prom_selected_l', slideBy = -1)
data <- slide(data, Var = 'prom_nselected', TimeVar = 'round', GroupVar = 'id', NewVar = 'prom_nselected_l', slideBy = -1)
data <- slide(data, Var = 'give_selected', TimeVar = 'round', GroupVar = 'id', NewVar = 'give_selected_l', slideBy = -1)

data$promise_l_sqrt <- data$promise_l * data$promise_l
data$hprom_selected_l <- ifelse(data$prom_selected_l > data$prom_nselected_l,1,0)
data$diff_prom_selected_l <- data$prom_selected_l - data$promise_l
data$promise_sqrt <- data$promise * data$promise
data$receiver_l <- ifelse(data$role_l == "Receiver", 1, 0)
data$diff_promise <- data$promise - data$promise_l
label(data$promise) <- "Promise"
label(data$diff_promise) <- "$\\Delta$ Promise t/t-1"
data$diff_promise_sqrt <- (data$promise - data$promise_l)^2
data$diff_give <- data$give - data$give_l
label(data$diff_give) <- '$\\Delta$ points given  t/t-1'
label(data$give) <- 'Points given'



##### Reshaping the data in a wide format for some parts of the analysis ######

d_wide <- reshape(select(data, -c("dict_give","choice","dict_receiver")) , timevar = "round", idvar = c("id", "treatment"), direction = "wide",  v.names = c("give", "promise", "receiver_ingroup", "id_group", "id_in_group", "group_prom1", "group_prom2", "group_prom3", "group_give1", "group_give2", "group_give3", "group_choice", "prom_selected", "prom_nselected", "give_selected", "give_nselected") )
d_wide$give_d <- data[c(data$round == 11),]$dict_give
d_wide$mean_give <- (d_wide$give.1 +d_wide$give.2 + d_wide$give.3 + d_wide$give.4 + d_wide$give.5 + d_wide$give.6 + d_wide$give.7 + d_wide$give.8 + d_wide$give.9 + d_wide$give.10)/10





###################################   Create variables by Session ##################################################

## Finding the promise that has the most other promsises in a +/- 5 environment ##

#generate a special session_round_id omitting the round 11's
d <- mutate(data[data$round < 11,], session_round_id = (as.numeric(id_session)-1)*10+round)
data$session_round_id[data$round < 11] <- d$session_round_id

#determine the 'modal' promise of each session

#create empty matrices
A = matrix(data = NA, ncol = 100, nrow = lengths(unique(select(d, session_round_id))))
A2 = matrix(data = NA, ncol = 100, nrow = lengths(unique(select(d, session_round_id))))
B = matrix(data = NA, nrow = lengths(unique(select(d, session_round_id))),ncol = 1)
C = matrix(data = NA, nrow = lengths(unique(select(d, session_round_id))),ncol = 1)
D = matrix(data = NA, nrow = lengths(unique(select(d, session_round_id))),ncol = 1)
E = matrix(data = NA, nrow = nrow(A), ncol = 100)

#loop through all possible promises in all rounds in all sessions (session_round_id)
system.time(
for(i in 1:lengths(unique(select(d, session_round_id)))){
  d_help <- subset(d, session_round_id == i) #only look at data from one round
  for (j in 1:100){
    #count how many promises are close (to see later how many promises are in an x environment around the mode)
    d_help2 <- subset(d_help, promise >= j - 5 & promise <= j + 5)
    A[i,j] <- length(d_help2$promise)
    #count how many promises are close to determine the mode
    A2[i,j] <- length(subset(d_help2, (promise >= j - 5 & promise <= j + 5))$promise)
    }
})
# B contains the lowest and C the highest mode
B <- as.vector(max.col(A2,ties.method = "first"))
C <- as.vector(max.col(A2,ties.method = "last"))
# D contains the count of promises around the mode, find the maximum with the apply method
D <- cbind(apply(A2,1, max))


# E will contain all modes  
for(i in 1:nrow(A2))
{
  help <- which(A2[i,] == D[i])
  E[i,1:length(help)]<-help
}

# Find the means of the modes
F <- rowMeans(E, na.rm = TRUE)

# code to find the number of promises around the mean of the mode (!= around the mode): D <- cbind(A[cbind(1:nrow(A), round(F))],D1)


#merge together, then merge into data
modes = data.frame(B,C,F,D)
modes$session_round_id <- unique(d$session_round_id)
data <- full_join(data, modes, by = "session_round_id")
rm(A, B,C,D,E,F)
rm(d, help, i,j)


# mean and standard deviation of promises by session and round
data %<>% 
  group_by(session_round_id) %>%
  summarise(mean_prom=mean(promise), sd_prom=sd(promise)) %>%
  full_join(data, ., by = "session_round_id") 

# mean and standard deviation of givings by session and round
data %<>% 
  group_by(session_round_id) %>%
  summarise(mean_give=mean(give), sd_give=sd(give),
            mean_give_selected=mean(give_selected), mean_give_nselected = mean(give_nselected) ) %>%
  full_join(data, ., by = "session_round_id") 

#adding mean and standard deviation to the modes dataframe also  
data %>% 
  group_by(session_round_id) %>%
  summarise(mean_prom=mean(promise), sd_prom=sd(promise)) %>%
  left_join(modes, ., by = "session_round_id") %>% {.} ->
  modes

data %>% 
  group_by(session_round_id) %>%
  summarise(mean_give=mean(give), sd_give=sd(give)) %>%
  left_join(modes, ., by = "session_round_id") %>% {.} ->
  modes
#modes <- modes[complete.cases(modes),]

modes <- left_join(modes, aggregate(select(data, session_round_id, round), by = list(data$session_round_id), FUN = "mean"), by = "session_round_id")
modes$session_id <- modes$session_round_id - modes$round
colnames(data)[colnames(data)=="B"] <- "modal_prom_min"
colnames(data)[colnames(data)=="C"] <- "modal_prom_max"
colnames(data)[colnames(data)=="F"] <- "modal_prom_mean"
colnames(data)[colnames(data)=="D"] <- "num_prom_at_mode"

colnames(modes)[colnames(modes)=="B"] <- "modal_prom_min"
colnames(modes)[colnames(modes)=="C"] <- "modal_prom_max"
colnames(modes)[colnames(modes)=="F"] <- "modal_prom_mean"
colnames(modes)[colnames(modes)=="D"] <- "num_prom_at_mode"

### Generating an adjusted promise, such that modal promise is 50 ###

data %>%
  mutate(adj1 = 50 - modal_prom_mean, adj2 = 50 - modal_prom_min, adj3 = 50 - modal_prom_max) %>% {.} ->
  data
data_adj <- data.frame(prom_adj1 = data[c(data$round < 11),]$promise + data[c(data$round < 11),]$adj1,
                       prom_adj2 = data[c(data$round < 11),]$promise + data[c(data$round < 11),]$adj2,
                       prom_adj3 = data[c(data$round < 11),]$promise + data[c(data$round < 11),]$adj3,
                       prom_selected_adj1 = data[c(data$round < 11),]$prom_selected + data[c(data$round < 11),]$adj1,
                       prom_selected_adj2 = data[c(data$round < 11),]$prom_selected + data[c(data$round < 11),]$adj2,
                       prom_selected_adj3 = data[c(data$round < 11),]$prom_selected + data[c(data$round < 11),]$adj3,
                       
                       prom_nselected_adj1 = data[c(data$round < 11),]$prom_nselected + data[c(data$round < 11),]$adj1,
                       prom_nselected_adj2 = data[c(data$round < 11),]$prom_nselected + data[c(data$round < 11),]$adj2,
                       prom_nselected_adj3 = data[c(data$round < 11),]$prom_nselected + data[c(data$round < 11),]$adj3,
                       
                       round = data[c(data$round < 11),]$round
                       )  



#### Data frame for dicator promise giving comparison #### 

# #generate session_round_id that does not omit round 11 (the dictator round) and adds another one
# data$session_round_id2 <- (as.numeric(data$id_session)-1)*11+data$round
# # generate new data frame which includes giving and dictator giving as mean per 'session_round'
# data %>% 
#   group_by(session_round_id2) %>% 
#   #summarise(prom_give = ifelse(!is.na(give),mean(give), mean(dict_give))) %>% 
#   summarise(prom_give = mean(give), dict_give = mean(dict_give)) %>% 
#   as.vector()%>% {.} ->
#   A
# give_data <- data.frame(A)
# rm(A)
# 
# #merge both variables to one stream
# give_data$give <- ifelse(!is.na(give_data$prom_give),give_data$prom_give, give_data$dict_give)
# give_data <- left_join(give_data, aggregate(select(data, session_round_id2, round), by = list(data$session_round_id2), FUN = "mean"), by = "session_round_id2")
# give_data <- select(give_data,"session_round_id2","give", "round")


####### aggregating data to one observation per round ######
d_agg = aggregate(modes$modal_prom_min, by = list(modes$round), FUN = "mean")
colnames(d_agg)[colnames(d_agg)=="x"] <- "modal_prom_min"
d_agg = full_join(d_agg, aggregate(modes$modal_prom_max, by = list(modes$round), FUN = "mean"), by = "Group.1")
colnames(d_agg)[colnames(d_agg)=="x"] <- "modal_prom_max"
d_agg <- full_join(d_agg, aggregate(modes$modal_prom_mean , by = list(modes$round), FUN = "mean"), by = "Group.1")
colnames(d_agg)[colnames(d_agg)=="x"] <- "modal_prom_mean"
d_agg <- full_join(d_agg, aggregate(modes$num_prom_at_mode, by = list(modes$round), FUN = sum ), by = "Group.1")
colnames(d_agg)[colnames(d_agg)=="x"] <- "num_prom_at_mode"
d_agg <- full_join(d_agg, aggregate(modes$mean_prom , by = list(modes$round), FUN = "mean"), by = "Group.1")
colnames(d_agg)[colnames(d_agg)=="x"] <- "mean_prom"
d_agg <- full_join(d_agg, aggregate(modes$sd_prom , by = list(modes$round), FUN = "mean"), by = "Group.1")
colnames(d_agg)[colnames(d_agg)=="x"] <- "sd_prom"
d_agg <- full_join(d_agg, aggregate(modes$mean_give , by = list(modes$round), FUN = "mean"), by = "Group.1")
colnames(d_agg)[colnames(d_agg)=="x"] <- "mean_give"
d_agg$mean_give_selected <- aggregate(data[data$round<11,]$give_selected, by = list(data[data$round<11,]$round), mean, simplify = TRUE)$x
d_agg$mean_give_nselected <- aggregate(data[data$round<11,]$give_nselected, by = list(data[data$round<11,]$round), mean, simplify = TRUE)$x

d_agg <- full_join(d_agg, aggregate(modes$mean_give , by = list(modes$round), FUN = "mean"), by = "Group.1")
colnames(d_agg)[colnames(d_agg)=="x"] <- "sd"
colnames(d_agg)[colnames(d_agg)=="Group.1"] <- "round"


d_agg$mean_break <- aggregate(data[data$round<11,]$broken_prom, by = list(data[data$round<11,]$round), mean, simplify = TRUE)$x
d_agg$mean_break_selected <- aggregate(data[data$round<11,]$broken_prom_selected, by = list(data[data$round<11,]$round), mean, simplify = TRUE)$x
d_agg$mean_break_nselected <- aggregate(data[data$round<11,]$broken_prom_nselected, by = list(data[data$round<11,]$round), mean, simplify = TRUE)$x


############## data for comparing givings by treatment ##############################
select(data, "treatment", "give", "round") %>% 
  filter(!is.na(give)) %>% 
  group_by(treatment, round) %>% 
  summarise(mean_give = mean(give)) %>% 
  as.vector()%>% {.} ->
  B

select(data, "treatment", "dict_give") %>% 
  filter(!is.na(dict_give)) %>% 
  group_by(treatment) %>% 
  #summarise(prom_give = ifelse(!is.na(give),mean(give), mean(dict_give))) %>% 
  summarise(round = mean(treatment), mean_give = mean(dict_give)) %>% 
  as.vector()%>% {.} ->
  A
A$round <- ifelse(A$round == 1, 11, 0)

d_agg2 = data.frame(mean_give = c(B$mean_give, A$mean_give, d_agg$mean_give, A$mean_give), 
                    round = c(B$round, A$round, d_agg$round, A$round),
                    treatment = factor(c(B$treatment, A$treatment, rep(3, nrow(d_agg)+2)), labels = c("dictator game at end", "dictator game at start", "both")))
rm(A, B)





save(d_wide, data, data_adj, modes, d_agg, d_agg2, file="C:\\Users\\anbor\\Dropbox\\Studium\\Master Thesis\\Politicians\\Political Promising Project\\Experiment\\Data\\Analysis\\AnalysisR\\prepared_data.Rdata")
write.csv(data, "data.csv")
unlink("prepared_data.RData")


