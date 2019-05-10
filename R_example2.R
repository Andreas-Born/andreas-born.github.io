library(dplyr)
library(Cairo)

library(tidyr)
library(data.table)
library(psych)  # descriptives
library(ggplot2) #graphs
library(ggExtra) #histograms  one the axes of scatter plots
library(plm) #panel data models
# packages to display linear regressions in a nice way
#library(sjPlot)     
#library(sjmisc)
#library(sjstats)
#library(sjlabelled)
library(multiwayvcov)  # allows multiway clustering of standard errors
library(lmtest)  # coefficient tests
library(Hmisc)   # labels in data.frames (and other things)
library(stargazer)  #for latex tables
library(sandwich) #robust standard errors
library(lmtest) #to run a coefftest after adjusting standard errors

setwd("C:/Users/anbor/Dropbox/Studium/Master Thesis/Politicians/Political Promising Project/Experiment/Data/")
rm(list=ls())
load(file = "C:\\Users\\anbor\\Dropbox\\Studium\\Master Thesis\\Politicians\\Political Promising Project\\Experiment\\Data\\Analysis\\AnalysisR\\prepared_data.Rdata", envir = parent.frame(), verbose = FALSE)

############################################## ############################################## ########################
############################################## Analysis ##############################################################
############################################## ############################################## ########################

describeBy(data[c(data$round <11),c("give", "promise", "round")], "round")


# test condition   (pay attention to mean of give in round 1 vs. round 9)
for (i in 1:10){
  print(i)
  print(summary(data[data$round == i,c("give", "promise")]))
}





###########################     COMPARING SELECTED TO NONE-SELECTED PROMISES / GIVING   ##########################################


#promises

describeBy(data[data$round<11,c("prom_selected", "prom_nselected", "round")], "round")
summary(data[data$round==11,]$dict_give)


t.test(data[c(data$round == 1),]$prom_selected, data[c(data$round == 1),]$prom_nselected, paired = TRUE)
t.test(data[c(data$round == 9),]$prom_selected, data[c(data$round == 9),]$prom_nselected, paired = TRUE)

# test for all periods
t.test(data[c(data$round < 11),]$prom_selected, data[c(data$round < 11),]$prom_nselected, paired = TRUE)
for (i in  c(1:10)){
  print(i)
  a = t.test(data[c(data$round == i),]$prom_selected, data[c(data$round == i),]$prom_nselected, paired = TRUE) #test that the mean of promises is the same
  c = ks.test(data[c(data$round == i),]$prom_selected, data[c(data$round == i),]$prom_nselected, paired = TRUE)  #test that the distribution of promises is the same
  d = ks.test(data[c(data$round == i),]$give_selected, data[c(data$round == i),]$give_nselected, paired = TRUE)  #test that the distribution of promises is the same
  b = t.test(c(data[c(data$round == i),]$broken_prom_selected, data[c(data$round == i),]$broken_prom_nselected),var.equal = FALSE)
  print(c)
}

############## TEST PROMISES/GIVING BROKEN VS NOT BROKEN   ####################################

for (i in  c(1:10)){
  print(i)
  a = t.test(data[which(data$round == i & data$broken_prom == 1),]$promise, data[which(data$round == i & data$broken_prom == 0),]$promise, paired = FALSE) #test that the mean of promises is the same
  c = ks.test(data[c(data$round == i)&c(data$broken_prom == 1),]$promise, data[c(data$round == i)&c(data$broken_prom == 0),]$promise, paired = FALSE) #test that the distribution of promises is the same
  d = ks.test(data[c(data$round == i)&c(data$broken_prom == 1),]$give, data[c(data$round == i)&c(data$broken_prom == 0),]$give, paired = FALSE)  #test that the distribution of promises is the same
  b = t.test(c(data[c(data$round == i),]$broken_prom_selected, data[c(data$round == i),]$broken_prom_nselected),var.equal = FALSE)
  print(a)
  print(c)
}




test_results = data.table(data[c(data$round<11),])[,t.test(prom_selected, prom_nselected)$p.value , by = round]$V1
linearMod1 <- lm(give_d ~ diff_give_1, data=d_wide)


################ TABLE WITH GIVINGS PER ROUND BY SELECTION ##############################

d_agg$dff_stat <- data.table(data[c(data$round<11),])[,t.test(give_selected, give_nselected)$statistic, by = round]$V1
d_agg$dff_pvalue <- data.table(data[c(data$round<11),])[,t.test(give_selected, give_nselected)$p.value , by = round]$V1
d_agg$mean_give_selected <- aggregate(data[data$round<11,]$give_selected, by = list(data[data$round<11,]$round), mean, simplify = TRUE)$x
d_agg$mean_give_nselected <- aggregate(data[data$round<11,]$give_nselected, by = list(data[data$round<11,]$round), mean, simplify = TRUE)$x

stargazer(
  select(
          d_agg, 
          c(round, mean_give,mean_give_selected, mean_give_nselected, dff_stat, dff_pvalue)), 
  summary = FALSE, 
  rownames = FALSE, 
  column.sep.width = "3pt", 
  covariate.labels = c("\\textbf{Round}", 
                       "\\multirow{2}{1.5 cm}{\\textbf{senders}\\vspace{-0.1cm} \\begin{center}all \\end{center}}", 
                       "\\multirow{2}{2 cm}{\\\\ \\vspace{-0.15cm} \\begin{center}selected \\end{center}}", 
                       "\\multirow{2}{2.2 cm}{\\\\ not-selected}", "\\multirow{2}{2 cm}{\\textbf{difference} t-statistic}", 
                       "\\multirow{2}{2 cm}{\\\\ p-value}\\\\ &"),
  title = "Amount given by round",
  label = 'tab:given_byround',
  out.header = FALSE, 
  style = "qje",
  notes.align = "c",
  notes.append = FALSE,
  notes.label = "",
  notes = '\\parbox[t]{13cm}{\\textit{\\textbf{Notes:} The table displays the amount senders give in the promise game by round. The different columns represent all senders or only those who got selected or did not. The final two columns display the test statistic and p-value of a two sided t-test.}}',
  out="C:/Users/anbor/Dropbox/Studium/Master Thesis/Politicians/Political Promising Project/Working Versions/FiguresTables/table_giving.tex")



## compute the share of promises in a certain range per round ##
data[(data$round<11),] %>%
  mutate(centered = promise >30 & promise<70) %>%
  count(round, centered) %>%
  
  {inner_join(filter(.,centered == TRUE),filter(.,centered == FALSE), by = "round", suffix = c(".in",".out"))} %>% 
  select(c("n.in","n.out","round")) %>% 
  transmute(share = n.in / (n.in + n.out))



describeBy(data[data$round<11,c("give_selected", "give_nselected", "round")], "round")
describeBy(data[data$round<11,c("broken_prom", "give", "promise", "round")], "round")


 
############################### main test of hypothesis 1 ############################################

t.test(data[c(data$round == 1),]$give_selected, data[data$round == 1,]$give_nselected, paired = TRUE)
t.test(data[c(data$round == 9),]$give_selected, data[data$round == 9,]$give_nselected, paired = TRUE)

# test for all periods
t.test(data[data$round < 11 & data$round > 0,]$give_selected, data[data$round < 11 & data$round > 0,]$give_nselected, paired = TRUE)
for (i in  c(1:10)){
  print(i)
  print(t.test(data[c(data$round == i),]$give_selected, data[c(data$round == i),]$give_nselected, paired = TRUE))
}
rm(i)


######################## HISTGRAM + DENSITY PLOTS SELECTED VS NONE-SELECTED ######################## 
d = data.frame(promise = c(data[c(data$round == 10),]$prom_selected, data[c(data$round == 10),]$prom_nselected), 
               type=rep(c("selected", "not selected"), c(length(data[c(data$round == 2),]$prom_selected), length(data[c(data$round == 2),]$prom_nselected))))

ggplot(data=d, aes(x=promise)) + 
  geom_histogram(aes(y = (..count..)/sum(..count..)), data = subset(d, type == "selected"), binwidth = 5,fill = "black", col="black", alpha = 0.5 ) + 
  geom_histogram(aes(y = (..count..)/sum(..count..)), data = subset(d, type == "not selected"), binwidth = 5, fill = "grey", col="darkred", alpha = 0.5) +
  scale_y_continuous(labels=scales::percent) +
  scale_x_continuous(name = "promises", breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  ylab("relative frequencies") 
  # geom_density(colour="green", data = subset(d, type == "selected")) +
  # geom_density(colour="blue", data = subset(d, type == "not selected"))


###  Density Plots for Period 1 and 9 ###

density_prom_1 <- ggplot(d) +
  geom_density(aes(x=promise, colour=type, fill=type), bw=4, kernel = "gaussian", alpha=0.5) +
  scale_color_manual(values=c("darkred", "black")) +
  scale_fill_manual(values=c("grey", "black"))
rm(d)

d = data.frame(promise = c(data[c(data$round == 9),]$prom_selected, data[c(data$round == 9),]$prom_nselected), 
               type=rep(c("selected", "not selected"), c(length(data[c(data$round == 2),]$prom_selected), length(data[c(data$round == 2),]$prom_nselected))))
density_prom_9 <- ggplot(d) +
  geom_density(aes(x=promise, colour=type, fill=type), bw=4, kernel = "gaussian", alpha=0.5) +
  scale_color_manual(values=c("darkred", "black")) +
  scale_fill_manual(values=c("grey", "black")) 

ggplot(data=d, aes(x=promise)) + 
  geom_histogram(aes(y = (..count..)/sum(..count..)), data = subset(d, type == "selected"), binwidth = 5,fill = "black", col="black", alpha = 0.5 ) + 
  geom_histogram(aes(y = (..count..)/sum(..count..)), data = subset(d, type == "not selected"), binwidth = 5, fill = "grey", col="darkred", alpha = 0.5) +
  scale_y_continuous(labels=scales::percent) +
  scale_x_continuous(name = "promises", breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100)) +
  ylab("relative frequencies") 
rm(d)



#######  Plots for Promise density over all 10 rounds   (Histograms and density plots) #####
d = data.frame(promise = c(data[c(data$round < 11),]$prom_selected, data[c(data$round < 11),]$prom_nselected), 
               Promisor=rep(c("Selected", "Non-selected"), c(length(data[c(data$round < 11),]$prom_selected), length(data[c(data$round < 11),]$prom_nselected))),
               give = c(data[c(data$round < 11),]$give_selected,data[c(data$round < 11),]$give_nselected),
               avg_give_s = rep(data[c(data$round < 11),]$mean_give_selected,2),
               avg_give_ns = rep(data[c(data$round < 11),]$mean_give_nselected,2),
               round = rep(data[c(data$round < 11),]$round,2))



#Histogram of promises by selection status for all rounds
ggplot(d, aes(x=promise)) +
  geom_histogram(aes(y = (..count..)/sum(..count..) * 10, fill="Selected"), data = subset(d, Promisor == "Selected"), color = "black", binwidth = 10, alpha = 0.5 ) + 
  geom_histogram(aes(y = (..count..)/sum(..count..) * 10, fill="Non-selected"), data = subset(d, Promisor == "Non-selected"), color = "grey", binwidth = 10, alpha = 0.5) +
  scale_y_continuous(labels=scales::percent, breaks = c(.1, .2, .3, .40, .50), limits = c(0, .5)) +
  scale_colour_manual(values = c("black", "grey"), aesthetics = "colour") +
  scale_fill_manual("Legend", values = c("grey", "black")) +
  ylab("Relative Frequencies")  +
  xlab("Promise")  +
  theme(legend.position = "bottom", panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  facet_wrap(~round, ncol = 5)

d_mean <- data.frame(round = c(1:10), s = aggregate(d$avg_give_s, by = list(d$round), FUN = "mean"), ns = aggregate(d$avg_give_ns, by = list(d$round), FUN = "mean"))

#Density Plot of promises by selection status for all rounds
ggplot(d) +
  geom_density(aes(x=promise, colour=promisor, fill=promisor), bw=4, kernel = "gaussian", alpha=0.5) +
  scale_color_manual(values=c("darkred", "black")) +
  scale_fill_manual(values=c("grey", "black")) +
  scale_y_continuous(sec.axis = sec_axis(~.*1000, name = "average giving"), limits = c(0,0.04))+ 
  scale_x_continuous(breaks = c(0, 25, 50, 75, 100)) + 
  geom_hline(aes(yintercept = s.x/1000), color = "black", d_mean) +
  geom_hline(aes(yintercept = ns.x/1000), color = "darkred", d_mean) +
  labs(ylab("density"), xlab("promise")) +
  theme(legend.position = "bottom") +
  facet_wrap(~round, ncol = 5)

#Density Plot of promises without means
ggplot(d) +
  geom_density(aes(x=promise, colour=Promisor, fill=Promisor), bw=4, kernel = "gaussian", alpha=0.5) +
  scale_color_manual(values=c("darkred", "#999999")) +
  scale_fill_manual(values=c("light grey", "black")) +
  scale_y_continuous(limits = c(0,0.04)) + 
  scale_x_continuous(breaks = c(0, 25, 50, 75, 100)) + 
  labs(ylab("Density"), xlab("Promise")) +
  xlab("Promise") +
  theme(legend.position = "bottom", panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  facet_wrap(~round, ncol = 5)
ggsave(filename = "DensityPromises10Rounds_nomeans.jpeg", dpi = 300,  path = "C:/Users/anbor/Dropbox/Studium/Master Thesis/Politicians/Political Promising Project/Working Versions/FiguresTables/")
# Density Plot with adjusted promises to modus

d_temp = data.frame(adjusted_promise = c(data_adj[c(data_adj$round < 11),]$prom_selected_adj1, data_adj[c(data_adj$round < 11),]$prom_nselected_adj1), 
                    round = rep(data_adj[c(data_adj$round < 11),]$round,2),
                    Legend = rep(c("Selected", "Non-selected"), c(length(data_adj[c(data$round < 11),]$prom_selected_adj1), length(data_adj[c(data$round < 11),]$prom_nselected_adj1))))
label(d_temp["adjusted_promise"])  <- "adjusted promise"

ggplot(d_temp) +
  geom_density(aes(x=adjusted_promise, group=Legend, color=Legend, fill = Legend), bw=4, kernel = "gaussian", alpha=0.5) +
  scale_color_manual(values=c("darkred", "#999999")) +
  scale_fill_manual(values=c("light grey", "black")) +
  scale_x_continuous(breaks = c(0, 25, 50, 75, 100)) + 
  ylab("Density") +
  theme(legend.position = "bottom") +
  xlab("Adjusted Promise") +
  theme(legend.position = "bottom", panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  facet_wrap(~round, ncol = 5)
ggsave(filename = "DensityAdjPromises10Rounds.jpeg", dpi = 300,  path = "C:/Users/anbor/Dropbox/Studium/Master Thesis/Politicians/Political Promising Project/Working Versions/FiguresTables/")

ggplot(d_temp, aes(x=adjusted_promise)) +
  geom_histogram(aes(y = (..count..)/sum(..count..) * 10, fill="Selected"), data = subset(d_temp, Legend == "Selected"), color = "black", binwidth = 10, alpha = 0.5 ) + 
  geom_histogram(aes(y = (..count..)/sum(..count..) * 10, fill="Non-selected"), data = subset(d_temp, Legend == "Non-selected"), color = "grey", binwidth = 10, alpha = 0.5) +
  # geom_histogram(aes(y = (..count..)/sum(..count..) * 10), data = subset(d_temp, Legend == "Non-selected"), binwidth = 10, fill = "grey", col="darkred", alpha = 0.5) +
  scale_colour_manual(values = c("black", "grey"), aesthetics = "colour") +
  scale_fill_manual("Legend", values = c("grey", "black")) +
  scale_y_continuous(labels=scales::percent, breaks = c(0.1, 0.2, 0.3, 0.40, 0.50)) +
  scale_x_continuous(breaks = c(0, 25, 50, 75, 100), limits = c(0, 100)) + 
  ylab("Relative Frequencies")  +
  xlab("Adjusted Promise") +
  theme(legend.position = "bottom", panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  facet_wrap(~round, ncol = 5)
ggsave(filename = "DensityPromises10Rounds_nomeans.jpeg", dpi = 300,  path = "C:/Users/anbor/Dropbox/Studium/Master Thesis/Politicians/Political Promising Project/Working Versions/FiguresTables/")

rm(d_temp)



########################### Hypothesis 2: distribution of giving   #########################
#histogram
ggplot(d, aes(x=give)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), data = subset(d, Promisor == "Selected"), binwidth = 5, fill = "black", col="black", alpha = 0.5 ) + 
  geom_histogram(aes(y = (..count..)/sum(..count..)), data = subset(d, Promisor == "Non-selected"), binwidth = 5, fill = "grey", col="darkred", alpha = 0.5) +
  scale_color_manual(values=c("darkred", "#999999")) +
  scale_fill_manual(values=c("grey", "black")) +
  scale_y_continuous(limits = c(0,0.04)) + 
  # scale_x_continuous(breaks = c(0, 10, 20, 30, 40)) + 
  labs(ylab("density"), xlab("giving")) +
  theme(legend.position = "bottom",panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  facet_wrap(~round, ncol = 5)

#density plot
ggplot(d, aes(x=give)) +
  geom_density(aes(x=give, colour=Promisor, fill = Promisor), bw=4, kernel = "gaussian", alpha=0.5) +
  scale_color_manual(values=c("darkred", "#999999")) +
  scale_fill_manual(values=c("light grey", "black")) +
  scale_y_continuous(limits = c(0,0.04), name = 'Density') + 
  scale_x_continuous(breaks = c(0, 25, 50, 75, 100), name = "Amount Given") + 
  theme(legend.position = "bottom", panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  facet_wrap(~round, ncol = 5)
rm(d, d_mean)
ggsave(filename = "DensityGive10Rounds.jpeg", dpi = 300,  path = "C:/Users/anbor/Dropbox/Studium/Master Thesis/Politicians/Political Promising Project/Working Versions/FiguresTables/")


#####################  bar plot selected vs non selected giving #################################
# calculate confidence intervals 
d = data.frame(promise = c(data[c(data$round < 11),]$prom_selected, data[c(data$round < 11),]$prom_nselected), 
               Promisor=rep(c("Selected", "Non-selected"), c(length(data[c(data$round < 11),]$prom_selected), length(data[c(data$round < 11),]$prom_nselected))),
               give = c(data[c(data$round < 11),]$give_selected,data[c(data$round < 11),]$give_nselected),
               avg_give_s = rep(data[c(data$round < 11),]$mean_give_selected,2),
               avg_give_ns = rep(data[c(data$round < 11),]$mean_give_nselected,2),
               round = rep(data[c(data$round < 11),]$round,2))


cint_l <- as.data.frame(data.table(data[c(data$round<11),])[,t.test(give_selected, give_nselected)$conf.int[1], by = round])
cint_h <- as.data.frame(data.table(data[c(data$round<11),])[,t.test(give_selected, give_nselected)$conf.int[2], by = round])
names(cint_h)[names(cint_h)=="V1"] <- "cint_h"
names(cint_l)[names(cint_l)=="V1"] <- "cint_l"
give <- as.data.frame(data.table(data[c(data$round<11),])[,t.test(give_selected, give_nselected)$conf.int[2], by = round])

# merge the standard error on d
d = left_join(d, as.data.frame(cint_l), by = c("round"))
d = left_join(d, as.data.frame(cint_h), by = c("round"))
d$lower = d$give - (d$cint_l+d$cint_h)/2
d$upper = d$give + (d$cint_l+d$cint_h)/2
se <- function(y) sd(y)/sqrt(length(y))

bar_give_select_error_bar <- ggplot(d, aes(x = round, y = give, fill = Promisor)) +
    geom_bar(stat = "summary", fun.y = "mean", position = "dodge")+
    geom_errorbar(position = position_dodge2(width = 0.5, padding = 0.5), 
                  stat = "summary", fun.data = function(y)c(ymin=mean(y)-1.2*se(y),ymax=mean(y)+1.2*se(y)), color = "black") +
    theme(plot.title = element_text(size=10),panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    scale_x_continuous(name = "Round", breaks = c(1,2,3,4,5,6,7,8,9,10)) +
    scale_y_discrete(breaks = c(0,10,20,30,40), limits = c(0:50)) +
    scale_fill_grey(start=0.5, end=0.3) +
    labs(y = "Avg Giving", x = "Round")
ggsave(filename = "bar_give_select_error_bar.jpeg", device = "jpeg", path = 'C:/Users/anbor/Dropbox/Studium/Master Thesis/Politicians/Political Promising Project/Working Versions/FiguresTables/'
       , width= 6, height = 3, dpi = 300)

bar_give_select <- ggplot(d, aes(x = round, y = give, fill = Promisor)) +
  geom_bar(stat = "summary", fun.y = "mean", position = "dodge")+
  theme(plot.title = element_text(size=10),panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_x_continuous(name = "Round", breaks = c(1,2,3,4,5,6,7,8,9,10)) +
  scale_y_discrete(breaks = c(0,10,20,30,40,50), limits = c(0:50)) +
  scale_fill_grey(start=0.5, end=0.3) +
  labs(y = "Avg Giving", x = "Round")
ggsave(filename = "bar_give_select.jpeg", device = "jpeg", path = 'C:/Users/anbor/Dropbox/Studium/Master Thesis/Politicians/Political Promising Project/Working Versions/FiguresTables/')


################# END OF PLOTS SELECTED VS NONE-SELECTED ###############################


### density of promising and giving

d_temp = data.frame(promise = c(data[c(data$round < 11),]$promise, data[c(data$round < 11),]$give), 
               round = rep(data[c(data$round < 11),]$round,2),
               Legend = rep(c("Promise", "Giving"), c(length(data[c(data$round < 11),]$promise), length(data[c(data$round < 11),]$give))))
ggplot(d_temp) +
  geom_density(aes(x=promise, group=Legend, color=Legend, linetype = Legend), bw=4, kernel = "gaussian", alpha=0.5) +
  scale_color_manual(values=c("darkred", "black")) +
  scale_fill_manual(values=c("darkred", "grey")) +
  scale_linetype_manual(values=c("solid", "dotted"))+
  labs(ylab("Density"), xlab("Promise/Giving")) +
  scale_x_continuous(name = "Giving/Promise")+
  theme(plot.title = element_text(size=10),panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  facet_wrap(~round, ncol = 5)
rm(d_temp)



####################################### Hypothesis 1: Plot average of modes and share around mode #####################################################################

plot_prom_mode_share  <-ggplot(d_agg) +
  geom_bar(aes(x=round,y=modal_prom_mean ),stat="identity", fill="grey", colour="black")+
  geom_line(aes(x=round,y=num_prom_at_mode/153*100), colour="red") +
  geom_text(aes(label=round(d_agg$num_prom_at_mode/153,2), x=round, y=1.1*round(num_prom_at_mode/153*max(modal_prom_mean)*2)), colour="black")+
  geom_text(aes(label=round(d_agg$modal_prom_mean,digits =1), x=round, y=5),colour="white")+
  scale_y_continuous(sec.axis = sec_axis(~./100, name = "Share Promises around Mode"), limits = c(0,100))+
  scale_x_continuous(breaks = c(0:10))+ 
  theme(plot.title = element_text(size=10),panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(y = "Average Modal Promise", x = "Round")
#line plot of the mean per session 
plot_prom_mode_share

file <- tempfile("C:\\Users\\anbor\\Dropbox\\Studium\\Master Thesis\\Politicians\\Political Promising Project\\Experiment\\Data\\Analysis\\Analysis\\Graphs\\modalprom_share_at_mode.pdf")
ggsave("C:\\Users\\anbor\\Dropbox\\Studium\\Master Thesis\\Politicians\\Political Promising Project\\Experiment\\Data\\Analysis\\Graphs\\modalprom_share_at_mode.pdf", device = "pdf")
unlink(file)






######################################## main test of hypothesis 3 ###########################################

t.test(d_wide$mean_give, d_wide$give_d, paired = TRUE)
ks.test(d_wide$mean_give, d_wide$give_d, paired = TRUE)
describeBy(data[data$round<11,c("give", "round")], "round")


# compare giving in period 1 to dictator giving - both treatments
t.test(d_wide$give.1, d_wide$give_d, paired = TRUE)
ks.test(d_wide$give.1, d_wide$give_d, paired = TRUE)



# compare giving in period 1 to dictator giving - treatment dictator game first
summary(d_wide[d_wide$treatment == 2,]$give.1)
summary(d_wide[d_wide$treatment == 2,]$give_d)
t.test(d_wide[d_wide$treatment == 2,]$give.1, d_wide[d_wide$treatment == 2,]$give_d, paired = TRUE)

# compare giving in period 1 to dictator giving - treatment dictator game second
summary(d_wide[d_wide$treatment == 1,]$give.1)
summary(d_wide[d_wide$treatment == 1,]$give_d)
t.test(d_wide[d_wide$treatment == 1,]$give.1, d_wide[d_wide$treatment == 1,]$give_d, paired = TRUE)
t.test(d_wide[d_wide$treatment == 2,]$give.1, d_wide[d_wide$treatment == 2,]$give_d, paired = TRUE)

t.test(d_wide[d_wide$treatment == 2,]$give.1, d_wide[d_wide$treatment == 2,]$give_d, paired = TRUE)
t.test(d_wide[d_wide$treatment == 1,]$give.1, d_wide[d_wide$treatment == 1,]$give_d, paired = TRUE)
t.test(d_wide[d_wide$treatment == 1,]$give.1, d_wide[d_wide$treatment == 2,]$give_d, var.equal = FALSE)
t.test(d_wide[d_wide$treatment == 2,]$give.1, d_wide[d_wide$treatment == 1,]$give_d, var.equal = FALSE)






################################   HYPOTHESIS 3: Test give in promise game vs. dictator game ################################


# # take relevant columns from dataframe and bring into wide format (merge all rounds on one id) 
# d = data.frame(give = data[c(data$round < 11), ]$give, 
#                id = data[c(data$round < 11), ]$id, 
#                round = data[c(data$round < 11), ]$round,
#                treatment = data[c(data$round < 11), ]$treatment)
# d_wide <- reshape(d,timevar = "round", idvar = c("id", "treatment"), direction = "wide",  v.names = c("give") )
# d_wide$give_d <- data[c(data$round == 11),]$dict_give
# d_wide$mean_give <- (d_wide$give.1 +d_wide$give.2 + d_wide$give.3 + d_wide$give.4 + d_wide$give.5 + d_wide$give.6 + d_wide$give.7 + d_wide$give.8 + d_wide$give.9 + d_wide$give.10)/10


######  graph giving by round 
d_help = data.frame(mean_give = c(d_agg2[d_agg2$treatment == "dictator game at start" & d_agg2$round <= 0, ]$mean_give, 
                                  d_agg2[d_agg2$treatment == "both" & d_agg2$round >= 1 & d_agg2$round <=10, ]$mean_give, 
                                  d_agg2[d_agg2$treatment == "dictator game at end" & d_agg2$round == 11, ]$mean_give), 
                    round = c(0:11),
                    Game = c('Dictator Game', rep('Promise Game',  10), 'Dictator Game'))

d_help2 = data.frame(mean_give = c(d_agg2[d_agg2$treatment == "dictator game at start" & d_agg2$round <= 0, ]$mean_give,
                                   d_agg2[d_agg2$treatment == "dictator game at start" & d_agg2$round == 1, ]$mean_give,
                                   d_agg2[d_agg2$treatment == "dictator game at end" & d_agg2$round == 1, ]$mean_give, 
                                   d_agg2[d_agg2$treatment == "dictator game at end" & d_agg2$round == 11, ]$mean_give),
                    game_type = c('Dictator Game','Promise Game','Promise Game', 'Dictator Game' ),
                    treatment = c('T1 Dictator Game', 'T1 Promise Game','T2 Promise Game', 'T2 Dictator Game'),
                    position = c(1,2,3,4),
                    three_bars = c('Dictator Game (beginning)', 'Promise Game','Promise Game', 'Dictator Game (end)'))


###$################### USED IN PAPER #################################
ggplot(d_help, aes(x = round, y = mean_give, fill = Game)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_discrete(name = "Round", limits =c(0:11) ) +
  scale_fill_grey(start=0.5, end=0.3) + 
  #scale_fill_brewer(palette = "Paired") +
  # scale_fill_manual(values = c("#E69F00", "#56B4E9")) +
  scale_y_continuous(name = "Mean Giving") +
  theme(panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank(),
         panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  ggsave("C:/Users/anbor/Dropbox/Studium/Master Thesis/Politicians/Political Promising Project/Working Versions/FiguresTables/bar_giving_byround.jpeg", width= 6, height = 3, dpi = 300)
###$################### $$$$$$$$$$ #################################

# 
# ggplot(d_agg2[d_agg2$treatment != "both", ], aes(x = round, y = mean_give, fill = treatment)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   scale_x_discrete(name = "round", limits =c(1:10) ) +
#   scale_fill_manual(values = c("black", "dark grey")) +
#   scale_y_continuous(name = "Mean giving")


############################### BARGRAPH GIVING BY ROUND WITHOUT DGAME ########################################
ggplot(d_agg, aes(x = round, y = mean_give)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_discrete(name = "Round", limits =c(1:10) ) +
  theme(plot.title = element_text(size=10),panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Set1") +
  scale_y_continuous(name = "Mean giving")

############################### BARGRAPH DGAME VS 1.ROUND PGAME (by treatment) ########################################
d_help2$treatment = with(d_help2, reorder(treatment, position, mean))
ggplot(d_help2, aes(x = treatment, y = mean_give, fill = game_type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  scale_x_discrete(name = "Game" ) +
  scale_fill_grey(start=0.5, end=0.3) + 
  labs(fill="Color") +
  theme(plot.title = element_text(size=10),panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  #geom_errorbar(position = position_dodge2(width = 0.5, padding = 0.5), 
                #stat = "summary", fun.data = function(y)c(ymin=mean(y)-1.2*se(y),ymax=mean(y)+1.2*se(y))) +
  scale_y_continuous(name = "Mean giving")

############################### BARGRAPH 3 BARS: DGAME VS 1.ROUND PGAME  ########################################
d_help2$three_bars = with(d_help2, reorder(three_bars, position, min))

ggplot(d_help2, aes(x = three_bars, y = mean_give, fill = game_type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  scale_x_discrete(name = "Game" ) +
  scale_fill_grey(start=0.5, end=0.3) + 
  labs(fill="Color") +
  theme(plot.title = element_text(size=10),panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  #geom_errorbar(position = position_dodge2(width = 0.5, padding = 0.5), 
  #stat = "summary", fun.data = function(y)c(ymin=mean(y)-1.2*se(y),ymax=mean(y)+1.2*se(y))) +
  scale_y_continuous(name = "Mean giving")


################################## Comparing Just dictator game to Promise game ################################## 
ggplot(d_help2, aes(x = game_type, y = mean_give, fill = game_type)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.5) +
  scale_x_discrete(name = "Game" ) +
  scale_fill_grey(start=0.5, end=0.3) + 
  labs(fill="Color") +
  theme(plot.title = element_text(size=10),panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  #geom_errorbar(position = position_dodge2(width = 0.5, padding = 0.5), 
  #stat = "summary", fun.data = function(y)c(ymin=mean(y)-1.2*se(y),ymax=mean(y)+1.2*se(y))) +
  scale_y_continuous(name = "Mean giving")


############################### BARGRAPH GIVING BY ROUND INCL DGAME ########################################
d_help = data.frame(mean_give = c(d_agg$mean_give_selected, d_agg$mean_give_nselected), 
                    round = c(d_agg$round, d_agg$round),
                    selected = factor(c(rep(1, nrow(d_agg)), rep(2,nrow(d_agg))), labels = c("selected", "not selected")))


ggplot(d_help, aes(x = round, y = mean_give, fill = selected)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_x_discrete(name = "round", limits =c(1:10) ) +
    scale_fill_manual(values = c("black", "dark grey")) +
    theme(plot.title = element_text(size=10),panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    scale_y_continuous(name = "Mean giving")

rm(d_help)

################ TABLE TESTS FOR HYPOTHESIS 3 ##############################


d_temp = data.frame(
  round = c("all",
            "1", 
            "1"),  
  treatment = c('all', 
                'all',
                '1'),
  promise_give = c(mean(data[data$round<11,]$give), 
                   mean(data[data$round == 1,]$give), 
                   mean(data[data$round == 1 & data$treatment == 2,]$give)),
  dictator_give = c(mean(data[data$round==11,]$dict_give),
                    mean(data[data$round==11,]$dict_give),
                    mean(data[data$round==11 & data$treatment == 2,]$dict_give)),
  ttest = c(t.test(d_wide$mean_give, d_wide$give_d, paired = TRUE)$statistic,
            t.test(d_wide$give.1, d_wide$give_d, paired = TRUE)$statistic,
            t.test(d_wide[d_wide$treatment == 2,]$give.1, d_wide[d_wide$treatment == 2,]$give_d, paired = TRUE)$statistic),
  ttest_p = c(t.test(d_wide$mean_give, d_wide$give_d, paired = TRUE)$p.value,
              t.test(d_wide$give.1, d_wide$give_d, paired = TRUE)$p.value,
              t.test(d_wide[d_wide$treatment == 2,]$give.1, d_wide[d_wide$treatment == 2,]$give_d, paired = TRUE)$p.value)
)

stargazer(
  d_temp,
  summary = FALSE, 
  rownames = FALSE, 
  column.sep.width = "2pt", 
  covariate.labels = c("\\multirow{2}{1.4cm}{\\\\ \\vspace{-0.15cm}\\begin{center}Round\\end{center}}",
                       "\\multirow{2}{1.9cm}{\\\\ \\vspace{-0.15cm}\\begin{center}Treatment\\end{center}}", 
                       "\\multirow{2}{3 cm}{\\textbf{Mean sending}\\vspace{-0.1cm} \\begin{center}Promise Game \\end{center}}", 
                       "\\multirow{2}{2.8 cm}{\\\\ \\vspace{-0.15cm} \\begin{center}Dictator Game \\end{center}}", 
                       "\\multirow{2}{2.2 cm}{\\textbf{Difference} \\\\ t-statistic}",  
                       "\\multirow{2}{1.5 cm}{\\\\ p-value}\\\\ &"),
  title = "Comparison promise and dictator game givings",
  label = 'tab:give_by_game',
  out.header = FALSE, 
  notes.append = FALSE,
  notes.align = "c",
  notes = "\\parbox[t]{\\textwidth}{\\textit{\\textbf{Notes:} Givings in promise and dictator game. Treatment 1 plays the dictator game first. Paired and two-sided t-test. }}",
  style = "qje", 
  out="C:/Users/anbor/Dropbox/Studium/Master Thesis/Politicians/Political Promising Project/Working Versions/FiguresTables/table_hyp3.tex")




#### Tables change of promise on giving/change of giving for Hyp 3 ####

#regression on Giving 
reg1 <- lm(give ~ promise_l + diff_promise, data = data[data$round>1,])
coeftest(reg1, vcov = cluster.vcov(reg1, data[data$round>1,]$id))
reg2 <- lm(give ~ promise_l + diff_promise + diff_promise_sqrt, data = data)
coeftest(reg2, vcov = cluster.vcov(reg2, data$id))
reg3 <- lm(diff_give ~ promise_l + diff_promise + factor(data$round), data = data)
coeftest(reg3, vcov = cluster.vcov(reg3, data[data$round>1,]$id))
reg4 <- lm(diff_give ~ promise_l + diff_promise + diff_promise_sqrt + factor(data$round), data = data)
coeftest(reg4, vcov = cluster.vcov(reg4, data$id))
reg1_se <- sqrt(diag(cluster.vcov(reg1, data[data$round>1,]$id)))
reg2_se <- sqrt(diag(cluster.vcov(reg2, data$id)))
reg3_se <- sqrt(diag(cluster.vcov(reg3, data$id)))
reg4_se <- sqrt(diag(cluster.vcov(reg4, data$id)))


stargazer(  
  reg1, reg2, reg3,
  se=list(reg1_se, reg2_se, reg3_se, reg4_se),
  p.auto = TRUE,
  title = "Regression of changes in giving on changes in promises.",
  dep.var.labels = c('$\\Delta_{t/t-1}$ Giving'),
  covariate.labels = c('$\\Delta_{t/t-1}$ Promise'),
  rownames = FALSE,
  model.names = FALSE,
  column.separate = c(4),
  omit.stat=c("adj.rsq", "f", "ser"),
  star.char = c("*", "**", "***"),
  star.cutoffs = c(0.05, 0.01, 0.001),
  notes.align = "c",
  notes = "\\parbox[t]{\\textwidth}{\\textit{\\textbf{Notes:} Regression of change of giving between round $t$ and $t-1$ change of promising Individual and round fixed effects. Robust standard errors in parenthesis. * $p<0.05$; ** $p<0.01$; *** $p<0.001$}}", 
  notes.append = FALSE,
  notes.label = "",
  label = 'tab:dprom_give',
  table.placement = 'h',
  style = "qje",
  add.lines=list(c("Individual FE",  "", "X", "X"), c("Time FE", "", "", "X")),
  out="C:/Users/anbor/Dropbox/Studium/Master Thesis/Politicians/Political Promising Project/Working Versions/FiguresTables/table_diffprom_giving_FE.tex")




#################Regression on change in giving  TABLE 4  IN PAPER  #####################################

reg1 <- lm(diff_give ~ diff_promise , data = data[data$round>1,])
# coeftest(reg1, vcov = vcovHC(reg1, type="HC1"))
coeftest(reg1, vcov = cluster.vcov(reg1, data[data$round>1,]$id))

# reg2 <- plm(diff_give ~  diff_promise, data = data, index=c("round"), model = "within")
reg2 <- lm(diff_give ~  0 + diff_promise + factor(data$round), data = data)

# coeftest(reg2, vcov = vcovHC(reg2, type="HC1"))
coeftest(reg2, vcov = cluster.vcov(reg2, data$id))

# reg3 <- plm(diff_give ~ diff_promise+ diff_promise_sqrt, data=data, index=c("round"), model = "within")
reg3 <- lm(diff_give ~ 0 + diff_promise+ diff_promise_sqrt + factor(data$round), data=data)
coeftest(reg3, vcov = vcovHC(reg3, type="HC1"))
coeftest(reg3, vcov = cluster.vcov(reg3, data$id))

reg3 <- lm(diff_give ~ 0 + diff_promise+ diff_promise_sqrt + factor(data$round) + factor(data$id), data=data)
# coeftest(reg4, vcov = vcovHC(reg4, type="HC1"))
coeftest(reg3, vcov = cluster.vcov(reg3, data$id))

# Robust (not clustered) standard errors
# reg1_se <- sqrt(diag(vcovHC(reg1, type="HC1")))
# reg2_se <- sqrt(diag(vcovHC(reg2, type="HC1")))
# reg3_se <- sqrt(diag(vcovHC(reg3, type="HC1")))

#Robust clustered standard errors
reg1_se <- sqrt(diag(cluster.vcov(reg1, data[data$round>1,]$id)))
reg2_se <- sqrt(diag(cluster.vcov(reg2, data$id)))
reg3_se <- sqrt(diag(cluster.vcov(reg3, data$id)))

stargazer(  
  reg1, reg2, reg3, 
  se=list(reg1_se, reg2_se, reg3_se),
  p.auto = TRUE,
  title = "Regression of change in giving on change in promising",
  dep.var.labels = c('$\\Delta_{t/t-1}$ Giving'),
  covariate.labels = c('$\\Delta_{t/t-1}$ Promise', 'Sqrt $(\\Delta_{t/t-1}$ Promise)'),
  column.separate = c(4),
  omit.stat=c("adj.rsq", "f", "ser"),
  omit=c("round", "id"), 
  star.char = c("*", "**", "***"),
  star.cutoffs = c(0.05, 0.01, 0.001),
  model.names = FALSE,
  notes.align = "c",
  notes = "\\parbox[t]{10cm}{\\textit{\\textbf{Notes:} Regression of difference of giving in round $t$ to $t-1$ on difference of promise. Round and individual fixed effects. Clustered standard errors (individual level) in parenthesis.\\\\ * $p<0.05$; ** $p<0.01$; *** $p<0.001$.}}", 
  notes.append = FALSE,
  notes.label = "",
  label = 'tab:dprom_give',
  style = "qje",
  rownames = FALSE,
  table.placement = 'h',
  add.lines=list(c("Individual FE", "", "", "X"), c("Round FE", "", "X", "X") ),
  out="C:/Users/anbor/Dropbox/Studium/Master Thesis/Politicians/Political Promising Project/Working Versions/FiguresTables/table_diffprom_give.tex")


############################# MOdifications 

#add variable for initial promise
data %>% 
  filter(round ==1) %>% 
  select(id, promise) %>% {.} ->
  id_filter
colnames(id_filter)[colnames(id_filter)=="promise"] <- "initial_promise"
data <- left_join(data, id_filter, by='id')

# add dummy for initial promise broken
data %>% 
  filter(dict_give>=33 & !is.na(dict_give)) %>% 
  select(id) %>% {.} ->
  id_filter
data$high_dict_giver <- as.numeric(data$id %in% id_filter$id)

# add dictator_givings for initial promise broken
data %>% 
  filter(round ==1 & broken_prom == 1) %>% 
  select(id) %>% {.} ->
  id_filter
data$initial_prom_broken <- as.numeric(data$id %in% id_filter$id)
# data$both <- as.numeric(data$initial_prom_broken == 1 & data$high_dict_giver == 1)


reg1 <- lm(diff_give ~ diff_promise, data = data[data$round>1,])
# reg1 <- lm(diff_give ~ diff_promise + high_dict_giver, data = data[data$round>1,])
coeftest(reg1, vcov = cluster.vcov(reg1, data[data$round>1,]$id))

reg2 <- lm(diff_give ~  0 + diff_promise + factor(data[data$round>1,]$round), data = data[data$round>1,])
coeftest(reg2, vcov = cluster.vcov(reg2, data[data$round>1,]$id))

reg3 <- lm(diff_give ~ 0 + diff_promise+ diff_promise_sqrt +  factor(data[data$round>1,]$round) + factor(data[data$round>1,]$id), data=data[data$round>1,])
coeftest(reg3, vcov = cluster.vcov(reg3, data[data$round>1,]$id))

reg4 <- lm(diff_give ~ 0 + diff_promise   + initial_prom_broken  + initial_prom_broken * diff_promise + factor(data[data$round>1,]$round), data=data[data$round>1,])
# coeftest(reg4, vcov = vcovHC(reg4, type="HC1"))
coeftest(reg4, vcov = cluster.vcov(reg4, data$id))



#Robust clustered standard errors
reg1_se <- sqrt(diag(cluster.vcov(reg1, data[data$round>2,]$id)))
reg2_se <- sqrt(diag(cluster.vcov(reg2, data[data$round>2,]$id)))
reg3_se <- sqrt(diag(cluster.vcov(reg3, data[data$round>2,]$id)))
reg4_se <- sqrt(diag(cluster.vcov(reg4, data[data$round>2,]$id)))

stargazer(  
  reg3, reg4,
  se=list(reg3_se, reg4_se),
  p.auto = TRUE,
  title = "Regression of change in giving on change in promising",
  dep.var.labels = c('$\\Delta_{t/t-1}$ Giving'),
  covariate.labels = c('$\\Delta_{t/t-1}$ Promise', '$(\\Delta_{t/t-1}$ Promise) sqrt', 'Broke initial promise', '$\\Delta_{t/t-1}$ Promise * Broke initial prom.'),
  column.separate = c(4),
  omit.stat=c("adj.rsq", "f", "ser"),
  omit=c("round", "id"), 
  star.char = c("*", "**", "***"),
  star.cutoffs = c(0.05, 0.01, 0.001),
  model.names = FALSE,
  notes.align = "c",
  notes = "\\parbox[t]{12cm}{\\textit{\\textbf{Notes:} Regression of difference of giving in round $t$ to $t-1$ on difference of promise. Round and individual fixed effects. Clustered standard errors (individual level) in parenthesis. * $p<0.05$; ** $p<0.01$; *** $p<0.001$.}}", 
  notes.append = FALSE,
  notes.label = "",
  label = 'tab:dprom_give_interactions',
  style = "qje",
  rownames = FALSE,
  table.placement = 'h',
  add.lines=list(c("Individual FE", "", "", "X", ""), c("Round FE", "", "X", "X", "X") ),
  out="C:/Users/anbor/Dropbox/Studium/Master Thesis/Politicians/Political Promising Project/Working Versions/FiguresTables/table_diffprom_giving_interaction.tex")

# additional tests relating to table 
#using hausman test to show that FE are needed (over RE)
reg2.1 <- plm(give ~ promise_l + diff_promise+ diff_promise_sqrt, data=data, index=c("id", "round"), model="random")
coeftest(reg2.1, vcov = vcovHC(reg2.1, type="HC1"))
phtest(reg2, reg2.1)
###################### End table difference in promise on giving ##########################


#####################  Same table for subset of data ##############################

#first filter data to IDs that fulfill certain criteria
data %>% 
  filter(promise >= 0 & promise <= 100 & round == 1 & broken_prom == 0) %>% 
  select(id) %>% {.} ->
  id_filter

data_filtered1 <- filter(data, id %in% id_filter$id)

data %>% 
  filter(promise >= 0 & promise <= 100 & round == 1 & broken_prom == 1) %>% 
  select(id) %>% {.} ->
  id_filter

data_filtered2 <- filter(data, id %in% id_filter$id)

#now run same regressions on this data
################   #Regression on change in giving  (subset of data)
# reg1 <- lm(diff_give ~ diff_promise, data = data_filtered[data_filtered$round>1,])
# coeftest(reg1, vcov = cluster.vcov(reg1, data_filtered[data_filtered$round>1,]$id))
# 
# reg2 <- lm(diff_give ~  0 + diff_promise + factor(data_filtered$round), data = data_filtered)
# coeftest(reg2, vcov = cluster.vcov(reg2, data_filtered$id))
keepers <- lm(diff_give ~ 0 + diff_promise+ diff_promise_sqrt + factor(data_filtered1$round)+ factor(data_filtered1$id), data=data_filtered1)
coeftest(keepers, vcov = cluster.vcov(keepers, data_filtered1$id))

breakers <- lm(diff_give ~ 0 + diff_promise+ diff_promise_sqrt + factor(data_filtered2$round)+ factor(data_filtered2$id), data=data_filtered2)
coeftest(breakers, vcov = cluster.vcov(breakers, data_filtered2$id))

reg4 <- lm(diff_give ~ 0 + diff_promise+ diff_promise_sqrt + factor(data_filtered$round) + factor(data_filtered$id), data=data_filtered)
coeftest(reg4, vcov = cluster.vcov(reg4, data_filtered$id))

#Robust clustered standard errors
reg1_se <- sqrt(diag(cluster.vcov(reg1, data_filtered1$id)))
reg2_se <- sqrt(diag(cluster.vcov(reg2, data_filtered2$id)))
# reg3_se <- sqrt(diag(cluster.vcov(reg3, data_filtered$id)))
# reg4_se <- sqrt(diag(cluster.vcov(reg4, data_filtered$id)))


stargazer(  
  keepers, breakers,
  se=list(reg1_se, reg2_se),
  p.auto = TRUE,
  title = "Regression of change in giving on change in promising by fulfillment of promise in round 1",
  dep.var.labels = c('$\\Delta_{t/t-1}$ Giving'),
  covariate.labels = c('$\\Delta_{t/t-1}$ Promise', '$(\\Delta_{t/t-1}$ Promise) sqrt'),
  column.separate = c(4),
  omit.stat=c("adj.rsq", "f", "ser"),
  omit=c("round", "id"), 
  star.char = c("*", "**", "***"),
  star.cutoffs = c(0.05, 0.01, 0.001),
  model.names = FALSE,
  object.names = TRUE,
  model.numbers=FALSE,
  column.labels = c("fulfillment of promise in round 1."),
  notes.align = "c",
  notes = "\\parbox[t]{10cm}{\\textit{\\textbf{Notes:} Regression of difference of giving in round $t$ to $t-1$ on difference of promise. Regression (1) uses participants that keep their promise in round 1. Regression (2) uses participants that break their promise in round 1. Round and individual fixed effects. Clustered standard errors (individual level) in parenthesis.\\\\ * $p<0.05$; ** $p<0.01$; *** $p<0.001$.}}", 
  notes.append = FALSE,
  notes.label = "",
  label = 'tab:dprom_give_appendix',
  style = "qje",
  rownames = FALSE,
  table.placement = 'h',
  add.lines=list(c("Individual FE", "X", "X"), c("Round FE", "X", "X") ),
  out="C:/Users/anbor/Dropbox/Studium/Master Thesis/Politicians/Political Promising Project/Working Versions/FiguresTables/table_diffprom_giving_appendix.tex")


#################################### END REGRESSIONS DIFF PROMISE DIFF GIVE#########################################


############################### SCATTER PLOTS PROMISE AND GIVING #####################################
#create dataframe with filtered data
data %>% 
  filter(promise >= 0 & promise <= 100 & round == 1 & broken_prom == 0) %>% 
  select(id) %>% {.} ->
  id_filter
data_filtered1 <- filter(data, id %in% id_filter$id)

  data %>% 
    filter(promise >= 0 & promise <= 100 & round == 1 & broken_prom == 1) %>% 
    select(id) %>% {.} ->
    id_filter
  data_filtered2 <- filter(data, id %in% id_filter$id)
  
  # create variable for initially broken promise
  data %>% 
    filter(round ==1 & broken_prom == 1) %>% 
    select(id) %>% {.} ->
    id_filter
  data$initial_prom_broken <- as.numeric(data$id %in% id_filter$id)

labels <- c('0' = "Keeper", '1' = "Breaker")
ggplot(data = data[data$round <4,], aes(x=promise, y=give)) +
  # geom_smooth(method='lm',formula=y~poly(x, 2)) +
  #geom_smooth(method='lm',formula=y~poly(x, 1)) +
  # geom_smooth(span = 0.6,method = 'loess', formula=y~x) +
  geom_point(size=2, shape=23) +
  theme(plot.title = element_text(size=11)) + 
   facet_wrap(  ~ round) +
  # facet_plot( round ~ , scales="free", labeller=labeller(initial_prom_broken = labels)) +
  theme(legend.position = "bottom", panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) 

ggsave(filename="C:/Users/anbor/Dropbox/Studium/Master Thesis/Politicians/Political Promising Project/Working Versions/FiguresTables/scatter_prom_give_both.pdf", plot=scatter_prom_give_both)
ggplot(data = data[data$round == 10,], aes(x=promise, y=give)) +
  # geom_smooth(method='lm',formula=y~poly(x, 2)) +
  #geom_smooth(method='lm',formula=y~poly(x, 1)) +
  # geom_smooth(span = 0.6,method = 'loess', formula=y~x) +
  theme(plot.title = element_text(size=11)) + 
  # facet_wrap(  ~ round) +
  #facet_grid( round ~ initial_prom_broken, scales="free", labeller=labeller(initial_prom_broken = labels)) +
  theme(legend.position = "bottom", panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  geom_point(size=2, shape=23)


scatter_prom_give_both <- ggplot(data = data[data$round<11,], aes(x=promise, y=give)) +
  # geom_smooth(method='lm',formula=y~poly(x, 2)) +
  geom_smooth(method='lm',formula=y~poly(x, 1)) +
  # geom_smooth(span = 0.6,method = 'loess', formula=y~x) +
  theme(plot.title = element_text(size=11)) + 
  # facet_wrap(  ~ round) +
  facet_grid( round ~ initial_prom_broken, scales="free", labeller=labeller(initial_prom_broken = labels)) +
  theme(legend.position = "bottom", panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  geom_point(size=2, shape=23)
ggsave(filename="C:/Users/anbor/Dropbox/Studium/Master Thesis/Politicians/Political Promising Project/Working Versions/FiguresTables/scatter_prom_give_both.pdf", plot=scatter_prom_give_both)

# scatter_diff_prom_give_breakers <- ggplot(data = data_filtered2, aes(x=diff_promise, y=diff_give)) +
#   geom_smooth(method='lm',formula=y~x) +
#   theme(plot.title = element_text(size=11)) + 
#   xlab('Difference Promise t / t-1') +
#   ylab('Difference Giving t / t-1') +
#   theme(legend.position = "bottom", panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "black")) +
#   geom_point(size=2, shape=23)


scatter_diff_prom_give_both <- ggplot(data = data[data$round>1 & data$round<11,], aes(x=diff_promise, y=diff_give)) +
  geom_smooth(method='lm',formula=y~x) +
  theme(plot.title = element_text(size=11)) + 
  xlab('Difference Promise t / t-1') +
  ylab('Difference Giving t / t-1') +
  theme(legend.position = "bottom", panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  facet_grid( round ~ initial_prom_broken, scales="free", labeller=labeller(initial_prom_broken = labels)) +
  geom_point(size=2, shape=23)
ggsave(filename="C:/Users/anbor/Dropbox/Studium/Master Thesis/Politicians/Political Promising Project/Working Versions/FiguresTables/scatter_diff_prom_give_both.pdf", plot=scatter_diff_prom_give_both)


#scatter of differences without type
scatter_diff_prom_give <- ggplot(data = data[data$round>1 & data$round<11,], aes(x=diff_promise, y=diff_give)) +
  geom_smooth(method='lm',formula=y~x) +
  theme(plot.title = element_text(size=11)) + 
  xlab('Difference Promise t / t-1') +
  ylab('Difference Giving t / t-1') +
  theme(legend.position = "bottom", panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  facet_grid( round ~ ., scales="free", labeller=labeller(initial_prom_broken = labels)) +
  geom_point(size=2, shape=23)

ggplot(data = data[data$round<11,], aes(x=promise, y=give)) +
  # geom_smooth(method='lm',formula=y~poly(x, 2)) +
  geom_smooth(method='lm',formula=y~poly(x, 1)) +
  # geom_smooth(span = 0.6,method = 'loess', formula=y~x) +
  theme(plot.title = element_text(size=11)) + 
  # facet_wrap(  ~ round) +
  facet_grid( round ~ ., scales="free", labeller=labeller(initial_prom_broken = labels)) +
  theme(legend.position = "bottom", panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  geom_point(size=2, shape=23)


######################      Extension: Additional tests regarding selection      ##########################





# test for all periods
t.test(data[c(data$round < 11),]$broken_prom_selected, data[c(data$round < 11),]$broken_prom_nselected, paired = TRUE)

counts_selected = select(data.frame(num.rows = 2), -'num.rows')
counts_nselected = select(data.frame(num.rows = 2), -'num.rows')
test_results = vector(length = 10)


#running a test of proportions for each round of the experiment
for (i in  c(1:10)){
  #a = t.test(data[c(data$round == i),]$prom_selected, data[c(data$round == i),]$prom_nselected, paired = TRUE) #test that the mean of promises is the same
  #c = ks.test(data[c(data$round == i),]$prom_selected, data[c(data$round == i),]$prom_nselected, paired = TRUE)  #test that the distribution of promises is the same
  data[data$round == i,] %>% 
    count(broken_prom_selected) %>% 
    select('n') %>%
    t() %>% {.} ->
    counts_selected

  data[data$round == i,] %>% 
    count(broken_prom_nselected) %>% 
    select('n') %>%
    t() %>% {.} ->
    counts_nselected

  test_results[i] <- prop.test(rbind(counts_selected, counts_nselected))$p.value
}
d_agg$diff_promb <- test_results
#creating table with results
stargazer(
  select(
    d_agg, 
    c(round, mean_break,mean_break_selected, mean_break_nselected, diff_promb)), 
  summary = FALSE, 
  rownames = FALSE, 
  column.sep.width = "3pt", 
  covariate.labels = c("\\textbf{Round}", 
                       "\\multirow{2}{1.5 cm}{\\textbf{senders}\\vspace{-0.1cm} \\begin{center}all \\end{center}}", 
                       "\\multirow{2}{2 cm}{\\\\ \\vspace{-0.15cm} \\begin{center}selected \\end{center}}", 
                       "\\multirow{2}{2.2 cm}{\\\\ not-selected}", "\\multirow{2}{2 cm}{Chi-2 test p-value}\\\\ &"), 
  title = "Share of broken promises by selection status and round",
  label = 'tab:break_prom_byround',
  notes.align = "c",
  notes = '\\parbox[t]{10cm}{\\textit{\\textbf{Notes:} The table displays the share of senders who break their promise by round of the promise game. The last column displays the p-value of a test of proportions comparing the share of broken promises by selected and not-selected senders.}}',
  out.header = FALSE, 
  style = "qje",
  out="C:/Users/anbor/Dropbox/Studium/Master Thesis/Politicians/Political Promising Project/Working Versions/FiguresTables/table_broken_promise.tex")

#graph promie-breaking by round 
bar_prombreak_byround <- ggplot(d_agg, aes(x = round, y = mean_break)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_discrete(name = "Round", limits =c(0:10) ) +
  scale_fill_brewer(palette = "Set1") +
  theme(plot.title = element_text(size=10),panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_y_continuous(name = "Share of Promises Broken")

ggsave(filename = "bar_brokenprom.jpg", device = "jpeg", path = 'C:/Users/anbor/Dropbox/Studium/Master Thesis/Politicians/Political Promising Project/Working Versions/FiguresTables/')



### Testing same for selection of generous dictators ###

for (i in  c(1:10)){
  #a = t.test(data[c(data$round == i),]$prom_selected, data[c(data$round == i),]$prom_nselected, paired = TRUE) #test that the mean of promises is the same
  #c = ks.test(data[c(data$round == i),]$prom_selected, data[c(data$round == i),]$prom_nselected, paired = TRUE)  #test that the distribution of promises is the same
  data[data$round == i,] %>% 
    count(broken_prom_selected) %>% 
    select('n') %>%
    t() %>% {.} ->
    counts_selected
  
  data[data$round == i,] %>% 
    count(broken_prom_nselected) %>% 
    select('n') %>%
    t() %>% {.} ->
    counts_nselected
  
  test_results[i] <- prop.test(rbind(counts_selected, counts_nselected))$p.value
}
#creating table with results
stargazer(
  select(
    d_agg, 
    c(round, mean_break,mean_break_selected, mean_break_nselected, dff_pvalue)), 
  summary = FALSE, 
  rownames = FALSE, 
  column.sep.width = "3pt", 
  covariate.labels = c("\\textbf{Round}", 
                       "\\multirow{2}{1.5 cm}{\\textbf{senders}\\vspace{-0.1cm} \\begin{center}all \\end{center}}", 
                       "\\multirow{2}{2 cm}{\\\\ \\vspace{-0.15cm} \\begin{center}selected \\end{center}}", 
                       "\\multirow{2}{2.2 cm}{\\\\ not-selected}", "\\multirow{2}{2 cm}{Chi-2 test p-value}\\\\ &"), 
  title = "Share of broken promises by selection status and round",
  label = 'tab:break_prom_byround',
  out.header = FALSE, 
  style = "qje",
  out="C:/Users/anbor/Dropbox/Studium/Master Thesis/Politicians/Political Promising Project/Working Versions/FiguresTables/table_give_dic.tex")


########### Heterogeneity test: Do low or high dictator givers increase giving more? ##############


d_wide$diff_give_1 <- d_wide$give.1 - d_wide$give_d
d_wide$diff_give_2 <- d_wide$give.2 - d_wide$give_d
d_wide$diff_give_3 <- d_wide$give.3 - d_wide$give_d
d_wide$diff_give_4 <- d_wide$give.4 - d_wide$give_d
d_wide$diff_give_5 <- d_wide$give.5 - d_wide$give_d
d_wide$diff_give_6 <- d_wide$give.6 - d_wide$give_d
d_wide$diff_give_7 <- d_wide$give.7 - d_wide$give_d
d_wide$diff_give_8 <- d_wide$give.8 - d_wide$give_d
d_wide$diff_give_9 <- d_wide$give.9 - d_wide$give_d
d_wide$diff_give_10 <- d_wide$give.10 - d_wide$give_d

d_wide$diff_give_avg <- (d_wide$give.1 + d_wide$give.2 + d_wide$give.3 + d_wide$give.4 + d_wide$give.5 + d_wide$give.6 + d_wide$give.7 + d_wide$give.8 + d_wide$give.9 + d_wide$give.10)/10 - d_wide$give_d
summary(d_wide$diff_give_avg )
ggplot(d_wide,aes(y = diff_give_avg, x = give_d)) +
  geom_point(shape = 21) +
  # geom_smooth(method = "loess") +
  geom_smooth(method='lm',formula=y~x)



## regression
linearMod1 <- lm(diff_give_1 ~ give_d, data=d_wide)
linearMod2 <- lm(diff_give_2 ~ give_d, data=d_wide)
linearMod3 <- lm(diff_give_3 ~ give_d, data=d_wide)
linearMod4 <- lm(diff_give_4 ~ give_d, data=d_wide)
linearMod5 <- lm(diff_give_5 ~ give_d, data=d_wide)
linearMod6 <- lm(diff_give_6 ~ give_d, data=d_wide)
linearMod7 <- lm(diff_give_7 ~ give_d, data=d_wide)
linearMod8 <- lm(diff_give_8 ~ give_d, data=d_wide)
linearMod9 <- lm(diff_give_9 ~ give_d, data=d_wide)
linearMod10 <- lm(give.10 ~ give_d, data=d_wide)
linearModavg <- lm(diff_give_avg ~ give_d, data=d_wide)

d_wide$low_give_d_dummy <- ifelse(d_wide$give_d <30, 1, 0)

coeftest(linearMod1, vcov = vcovHC(linearMod1, type="HC1"))


linearModavg_robust <- sqrt(diag(vcovHC(linearModavg, type="HC1")))
linearMod9_robust <- sqrt(diag(vcovHC(linearMod9, type="HC1")))
linearMod1_robust <- sqrt(diag(vcovHC(linearMod1, type="HC1")))

stargazer(  
  linearModavg, 
  se=list(linearModavg_robust),
  p.auto = TRUE,
  title = "Regression difference in giving on dictator game giving",
  rownames = FALSE,
  omit.stat=c("adj.rsq", "f", "ser"),
  label = 'tab:diff_give',
  table.placement = 'h',
  style = "qje",
  out="C:/Users/anbor/Dropbox/Studium/Master Thesis/Politicians/Political Promising Project/Working Versions/FiguresTables/table_diff_giving.tex")




stargazer(  
  linearModavg, 
  se=list(linearModavg_robust),
  p.auto = TRUE,
  title = "Regression difference in giving on dictator game giving",
  dep.var.labels = c('Avg. giving Promise - giving Dictator game'),
  covariate.labels = c('Giving Dictator game'),
  rownames = FALSE,
  model.names = FALSE,  omit.stat=c("adj.rsq", "f", "ser"),
  star.char = c("*", "**", "***"),
  star.cutoffs = c(0.05, 0.01, 0.001),
  notes.align = "c",
  notes = "\\parbox[t]{10cm}{\\textit{\\textbf{Notes:} Regression of difference in giving between promise and dictator game on giving in the dictator game. Robust standard errors in parenthesis.\\\\ * $p<0.05$; ** $p<0.01$; *** $p<0.001$.}}", 
  notes.append = FALSE,
  notes.label = "",
  label = 'tab:diff_give',
  table.placement = 'h',
  style = "qje",
  out="C:/Users/anbor/Dropbox/Studium/Master Thesis/Politicians/Political Promising Project/Working Versions/FiguresTables/table_diff_giving.tex")




dep<- colnames(d_wide[,c(grep("diff_give", colnames(d_wide)))])
indep1<-c(rep("~give_d",10))  # list of first unique independent variables 
myvar<-cbind(dep,indep1)

out <- data.frame(NULL)              
for (i in 1:10){
  print(paste("This is", i, "regression", "with dependent var",gsub("~","",myvar[i,1])))
  out[[i]] <- lm(as.formula(paste(myvar[i,1],paste(myvar[i,2],collapse="+"))),d_wide)
}
names(out) <- c("y.variable", "intercept", "coef.x")


print(linearMod)
summary(linearMod)


################## EXTENSION: COMPARISON OF TREATMENT GROUPS #####################################
#(1 = dictator game second, 2 = dictator game first)

summary(data[c(data$round == 11),]$dict_give)
describeBy(data[c(data$round <11),c("give", "promise", "treatment")], "treatment")
t.test(give~treatment, data = filter(data, round > 0 & round < 11))
t.test(promise~treatment, data = data)
t.test(dict_give~treatment, data = data[c(data$round == 11),])

for (i in c(1:10)) {
  print(i)
  print(describeBy(data[c(data$round == i),c("give", "promise", "treatment")], "treatment"))
}
t.test(promise~treatment, data = data[c(data$round == 1),c("give", "promise", "treatment")])
t.test(give~treatment, data = data[c(data$round == 9),c("give", "promise", "treatment")])



###################### EXTENSION: SCATTERPLOTS PROMISES ON GIVINGS ##############################
# 
# attach(data)
# scatter_give_promall <- ggplot(data[c(round<11),], aes(x=promise, y=give)) +
#   geom_point(size=2, shape=1, position = "jitter") +
#   geom_smooth(method = loess) +
#   # geom_rug(col="black",alpha=.2) +
#   scale_x_continuous(promise, breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100), limits =c(0, 100) ) +
#   scale_y_continuous(give, breaks = c(0, 50, 100), limits =c(-10, 105) ) +
#   theme(plot.title = element_text(size=10),panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "black")) +
#   facet_wrap(~round, ncol = 5)
# detach(data)
# attach(data)
# scatter_give_prom1 <- ggplot(data[c(round == 1),], aes(x=promise, y=give)) +
#   geom_point(size=2, shape=1, position = "jitter") +
#   geom_smooth(method = loess, se = FALSE) +
#   #geom_rug(col="black",alpha=.2) +
#   scale_x_continuous(promise, breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100), limits =c(0, 100) ) +
#   scale_y_continuous(give, breaks = c(0, 50, 100) ) 
#   theme(plot.title = element_text(size=10),panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank(),
#       panel.background = element_blank(), axis.line = element_line(colour = "black")) +
# detach(data)
#   
# 
# attach(data)
# scatter_give_prom9 <- ggplot(data[c(data$round == 9),], aes(x=promise, y=give)) +
#   geom_point(size=2, shape=1, position = "jitter") +
#   geom_smooth(method = loess, se= FALSE) +
#   # geom_rug(col="black",alpha=.2) +
#   scale_x_continuous(name = "promises", breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100), limits =c(0, 100) ) +
#   scale_y_continuous(name = "givings", breaks = c(0, 50, 100) )  +
#   theme(plot.title = element_text(size=10),panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank(),
#         panel.background = element_blank(), axis.line = element_line(colour = "black")) 
# detach(data)

ggMarginal(scatter_give_prom1, type = "histogram")
ggMarginal(scatter_give_prom1)
ggMarginal(scatter_give_prom9, type = "histogram")
ggMarginal(scatter_give_prom9)
scatter_give_prom9

scatter_give_promall
# hist_top <- ggplot()+geom_histogram(aes(rnorm(100)))
# empty <- ggplot()+geom_point(aes(1,1), colour="white")+
#   theme(axis.ticks=element_blank(), 
#         panel.background=element_blank(), 
#         axis.text.x=element_blank(), axis.text.y=element_blank(),           
#         axis.title.x=element_blank(), axis.title.y=element_blank())
# 
# scatter <- ggplot()+geom_point(aes(rnorm(100), rnorm(100)))
# hist_right <- ggplot()+geom_histogram(aes(rnorm(100)))+coord_flip()
# grid.arrange(hist_top, empty, scatter, hist_right, ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4))




  
############################ Graphs for different modes by round  ############################



## Graph, modal and mean promise by round
d_agg_long = data.frame(prom_stat = c(d_agg$modal_prom_min, d_agg$modal_prom_max, d_agg$mean_prom, d_agg$modal_prom_mean), 
               statistic=rep(c("Lowest Mode", "Highest Mode", "Average", "Average Mode"), c(length(d_agg$modal_prom_min), length(d_agg$modal_prom_max), length(d_agg$mean_prom), length(d_agg$modal_prom_mean))),
               give_mean = rep(d_agg$mean_give, 4),
               round = rep(d_agg$round,4))
linegraph_modalavg_prom <- ggplot(d_agg_long) +
  geom_line(aes(x=round,y=prom_stat, colour = statistic), d_agg_long) +
  ylab("Promise") +
  theme(legend.position = "bottom") +
  theme(plot.title = element_text(size=10),panel.grid.major = element_line(colour = "grey"), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_x_continuous(limits = c(1,10), breaks = c(1:10),name = "Round") 

linegraph_modalavg_prom
rm(d_agg_long)








############################ Extention: Reason to decrease giving  ############################


#create initial promise broken indicator
data %>% 
  filter(round ==1 & broken_prom == 1) %>% 
  select(id) %>% {.} ->
  id_filter
data$initial_prom_broken <- as.numeric(data$id %in% id_filter$id)



## determine what promise depends on 

regression <- lm(promise ~ 0 + promise_l + prom_selected_l+ broken_prom_selected_l + prom_nselected_l + factor(data$round) + prom_selected_l * factor(data$role_l), data = data)
coeftest(regression, vcov = cluster.vcov(regression, data$id))

reg_se <- sqrt(diag(cluster.vcov(regression, data$id)))


stargazer(  
  regression, 
  se=list(reg_se),
  p.auto = TRUE,
  title = "Regression of promise in $t$ on promise and promise-breaking in $t-1$.",
  covariate.labels = c('Promise $t-1$', 'Promise $t-1$, selected Sender',  'Selected Sender Broke Promise', 'Promise $t-1$, non-selected Sender', 'Previous Role: Receiver', 'Previous Role: Selected Sender', 'Promise selected Sender * Receiver', 'Promise selected Sender * selected Sender'),
  rownames = FALSE,
  model.names = FALSE,  
  omit.stat=c("adj.rsq", "f", "ser"),
  omit=c("round", "id"), 
  star.char = c("*", "**", "***"),
  star.cutoffs = c(0.05, 0.01, 0.001),
  notes.align = "c",
  notes = "\\parbox[t]{10cm}{\\textit{\\textbf{Notes:} Regression of promise in $t$ on promise and promise-breaking in $t-1$. Clustered standard errors on individual level in parenthesis. * $p<0.05$; ** $p<0.01$; *** $p<0.001$.}}", 
  notes.append = FALSE,
  notes.label = "",
  no.space=TRUE,
  label = 'tab:reg_prom',
  add.lines=list(c("Round FE", "X") ),
  table.placement = 'h',
  style = "qje",
  out="C:/Users/anbor/Dropbox/Studium/Master Thesis/Politicians/Political Promising Project/Working Versions/FiguresTables/table_reg_promise.tex")







regression <- plm(promise ~ promise_l + prom_selected_l * broken_prom_selected_l + prom_nselected_l, data = data, model = "within", effect = "twoways", index = c("id","round"))
summary(regression)
Mysum <- summary(regression)
coef(Mysum)[1:5]

### broken promise
regression <- lm(broken_prom ~ initial_prom_broken +  promise * initial_prom_broken + promise_sqrt * initial_prom_broken + factor(data$round), data = data)
vcov_id<- cluster.vcov(regression, data$id)
vcov_round<- cluster.vcov(regression, data$round)
vcov_both <- cluster.vcov(regression, cbind(data$id, data$round))
coeftest(regression, cluster.vcov(regression, data$id))


regression <- plm(broken_prom ~ broken_prom_l +  broken_prom_selected_l + factor(role_l) + promise + promise_sqrt , data = data, model = "within", effect = "twoways", index = c("id","round"))
regression <- lm(give ~ 0 + give_l + promise+ promise_l + factor(data$round) + factor(data$id), data = data)
coeftest(regression, cluster.vcov(regression, data$id))
summary(regression)



describeBy(data[data$round<11,c("promise", "give", "round", "initial_prom_broken")], c("round", "initial_prom_broken"))


# givings 
reg1 <- lm(give ~ give_l + give_selected_l  + factor(data$round), data = data)
reg2 <- lm(give ~  give_l + give_selected_l * factor(role_l)  + factor(role_l)  +   factor(data$round), data = data)
reg1_se <- sqrt(diag(cluster.vcov(reg1, data$id)))
reg2_se <- sqrt(diag(cluster.vcov(reg2, data$id)))
coeftest(reg1, cluster.vcov(reg1, data$id))
coeftest(reg2, cluster.vcov(reg2, data$id))

stargazer(  
  reg1, reg2,
  se=list(reg1_se, reg2_se),
  p.auto = TRUE,
  title = "Regression of giving in $t$ on giving and role in $t-1$.",
  covariate.labels = c('Giving $t-1$', 'Giving $t-1$ Select Sender', 'Previous Role: Receiver', 'Previous Role: Selected Sender', 'Giving Selected Sender * Receiver', 'Giving Selected Sender * Selected Sender'),
  rownames = FALSE,
  model.names = FALSE,  
  omit.stat=c("adj.rsq", "f", "ser"),
  omit=c("round", "id"), 
  star.char = c("*", "**", "***"),
  star.cutoffs = c(0.05, 0.01, 0.001),
  notes.align = "c",
  notes = "\\parbox[t]{10cm}{\\textit{\\textbf{Notes:} Regression of giving in round $t$ on giving in previous round. Individual and round fixed effects. Clustered standard errors on individual level in parenthesis. * $p<0.05$; ** $p<0.01$; *** $p<0.001$.}}", 
  notes.append = FALSE,
  notes.label = "",
  no.space=TRUE,
  label = 'tab:reg_give',
  add.lines=list(c("Round FE", "X", "X"), c("Individual FE", "X", "X") ),
  table.placement = 'h',
  style = "qje",
  out="C:/Users/anbor/Dropbox/Studium/Master Thesis/Politicians/Political Promising Project/Working Versions/FiguresTables/table_reg_giving.tex")


############## Regression of giving on broken promise ######################
reg1 <- lm(give ~ give_l + give_selected_l + broken_prom_l  + broken_prom_selected_l  + factor(data$round), data = data)
coeftest(reg1, cluster.vcov(reg1, data$id))
##################### broken promise of previous receiver does not correlate with giving ####################################


## Cost of promise breaking
d_wide2 = data.frame(give_d = d_wide$give_d,
                      give_1 = d_wide$give.1,
                      prom_1 = d_wide$promise.1
                      )
d_wide2$dic_smaller <- ifelse(d_wide2$give_d < d_wide2$give_1, 1, 0)
d_wide2$dic_larger <- ifelse(d_wide2$give_d > d_wide2$give_1, 1, 0)
d_wide2$dic_equal <- ifelse(d_wide2$give_d == d_wide2$give_1, 1, 0)

d_wide2$give_between <- ifelse(  (d_wide2$prom_1 > d_wide2$give_1) & (d_wide2$give_1 > d_wide2$give_d), 1, 0)

sum(d_wide2$give_between == 1)
sum(d_wide2$dic_smaller==1)
sum(d_wide2$dic_larger==1)
sum(d_wide2$dic_equal==1)

