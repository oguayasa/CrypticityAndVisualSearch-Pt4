# val_LDAResultsGlmm.R
# 
# TODO CHANGE SUMMARY 
# 
# Summary:
# Do an ANOVA type analysis with GLMMs to determine how search difficulty and 
# search type affects search behavior LD1 scores. Do ICC and correlation 
# analysis to determine how consistently subjects performed within and across 
# difficulty treatments respectively. Use model comparisons to determine if 
# included search behavior helps to explain search performance outcomes.
#
# Requires: LDA behavior data files, listed libraries
#
# Outputs: Results of Glmms, model comparisons, R squared, ICCs, and Spearman's
# correlation analysis. Also output figures detailing Glmm and correlation
# results.
# 
# Author: Olivia Guayasamin
# Date: 8/16/2017
# 
# ----- Initializing steps -----

# read and work with data files
library(plyr)
library(reshape2)   

# glmm analysis
library(lme4)
library(lmerTest)
library(MuMIn)
library(pscl)

# additional stats functions
library(psych)
library(car)  
library(MASS)

# format figures and output
library(ggplot2)
library(grid)
library(gridExtra)  
library(broom)
library(knitr) 

# quick function for getting legend from a ggplot
get_legend <- function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

#set working directory
getwd()
setwd("~/R/val_LDAResultsGlmm")  #TODO CHANGE 

# Save all console output to file
sink("val_LDAResultsGlmm.doc", append = FALSE)  # initialize output sink


# ----- Import and format data -----
# 
# import data files containing LD1 scores
lda.Data.Long <- read.csv("val_LDA_Data_Long.csv")

# convert need variables to factors
lda.Data.Long$PartID <- factor(lda.Data.Long$PartID)  
lda.Data.Long$TreatmentOrder <- factor(lda.Data.Long$TreatmentOrder)  
lda.Data.Long$BlockOrder <- factor(lda.Data.Long$BlockOrder)  


# ----- Visualize data distributions -----
# 
# add pos and neg labels to data set
pVn <- as.data.frame(matrix(nrow = nrow(lda.Data.Long), ncol = 1))

# subset data by search type and put into a in a list
for (i in 1:nrow(lda.Data.Long)){
  # figure out type
  if ((lda.Data.Long$search.Class[i] == 'Fp') || (lda.Data.Long$search.Class[i] == 'Tp')){
    pVn[i, 1] <- 'Positive'
  } else {
    pVn[i, 1] <- 'Negative'
  }
}

# change name of new col and append to existing data set
colnames(pVn) <- 'EndType'
lda.Data.Long <- cbind(lda.Data.Long, pVn)
  
# save search type histogram as jpeg image
jpeg(filename = "ldaResults_hist1.jpg", units = "in", height = 6, width = 10, res  = 300)  
ggplot(data = lda.Data.Long, aes(x = LD1, fill = EndType)) + 
  # histogram
  geom_histogram(alpha = 0.4, position = "identity", breaks = seq(-5, 8, 0.5)) +  
  # mean lines
  labs(x = "LD1 Score", y = "Count") + # axes labels and plot titles
  ggtitle("LD1 Scores by Pos. or Neg. Search Outcome", 
          subtitle = "Pos. = Tp & Fp | Neg. = Tn & Fn") +
  theme(plot.title = element_text(hjust = 0.5, size = 16), 
        plot.subtitle = element_text(hjust = 0.5, size = 12))
dev.off()

# separat by endtype
lda.Pos <- subset(lda.Data.Long, EndType == 'Positive')
lda.Neg <- subset(lda.Data.Long, EndType == 'Negative')
posNeg.list <- list(lda.Pos, lda.Neg)
names(posNeg.list) <- c('Positives', 'Negatives')

# visualize data using hists and normal qq plots
fig.Title <- sprintf("ldaResults_qqPlots.jpg")
jpeg(filename <- fig.Title, units = "in", height = 6, width = 6, res = 300)
par(mfcol = c(2, 2)) # format figure window
# iterate throughDifferent Variables
for (i in 1:2){
  # for each variable plot a histogram and qqplot 
  cur.Data <- posNeg.list[[i]]$LD1
  thisPlot.Title <- sprintf("%s", names(posNeg.list[i]))  # format and add title
  hist(cur.Data, main = thisPlot.Title)  # histogram
  qqnorm(cur.Data); qqline(cur.Data)  # qqplot with normal line
}
dev.off()  # close and save figures


# ----- Glmm data for pos and neg search outcomes -----

# run separate glmms for pos and neg dataframes in list
for (i in 1:2){
  # pull out
  cur.Data <- posNeg.list[[i]]
  
  # so that we use the right functions
  require(lmerTest)
  
  # create model
  lda.Anova.Glmm <- glmer(LD1 ~ search.Class*ExpVer + (1|BlockOrder) + (1|PartID),
                          data = cur.Data, family = gaussian(link = identity),
                          na.action = na.exclude, 
                          REML = FALSE)
  
  # compute coeff results
  coefs <- data.frame(coef(summary(lda.Anova.Glmm)))
  # use normal distribution to approximate p-value
  coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
  
  # show output
  print(sprintf("Results for Glmm examing LD1 Scores"))
  print(summary(lda.Anova.Glmm))
  print(coefs)  # full summary, with p values for fixed effects
  print(confint(lda.Anova.Glmm, method = 'boot', level = 0.99, oldNames = FALSE))  # confidence intervals for everything
  print(lsmeansLT(lda.Anova.Glmm, test.effs = "search.Class:ExpVer", alpha = 0.99))  # get estimates for all levels of fixed effect
  print(lsmeansLT(lda.Anova.Glmm, test.effs = "search.Class", alpha = 0.99))
  print(lsmeansLT(lda.Anova.Glmm, test.effs = "ExpVer", alpha = 0.99))
  print(rand(lda.Anova.Glmm))  # p values for random effects and slopes, likelihood ratio test on random effects of a linear mixed effects model
  
  # print conditional r squared and marginal r squared
  cur.Glmm.Rsquareds <- r.squaredGLMM(lda.Anova.Glmm)
  print(cur.Glmm.Rsquareds)
  
  # for each variable plot residuals
  fig.Title <- sprintf("ldaResults_glmmResids_%s.jpg", names(posNeg.list[i])) # file name
  resid.Title <- sprintf("%s_LD1Glmm_Resids", names(posNeg.list[i])) # plot title
  jpeg(filename <- fig.Title, units = "in", height = 6, width = 6, res = 300)
  qqnorm(resid(lda.Anova.Glmm), main = resid.Title) ; qqline(resid(lda.Anova.Glmm))
  dev.off()  # close and save figures
}

# ----- Compare full Tp LD1 scores across dificulty treatments -----
# this is to examine the full effect because of how NAs are treated in Glmm,
# just lost a lot of Tp data due to spotty Fp Data

# separate fp search rows from the rest
tp.Data <- subset(lda.Data.Long, lda.Data.Long$search.Class == 'Tp')
# divide fp search rows by difficulty treatment
tp.Easy <- subset(tp.Data, tp.Data$ExpVer == 'most')
tp.Hard <- subset(tp.Data, tp.Data$ExpVer == 'least')

# analysis on FP with wilcox tests
tp.Test <- wilcox.test(tp.Easy$LD1, tp.Hard$LD1, 
                       paired = TRUE, conf.int = TRUE, 
                       na.action = na.exclude)
sprintf("Median value of Tp LD1 Scores in Easy Treatment %f", 
        median(tp.Easy$LD1, na.rm = TRUE))
sprintf("Median value of Tp LD1 Scores in Hard Treatment %f", 
        median(tp.Hard$LD1, na.rm = TRUE))
print(kable(tidy(tp.Test), 
            caption = "Tp LD1 Scores, Easy vs. Hard Treatment",
            digits = 2))


# ----- Reformat data for plotting and further analyses -----

# create string with search type names
search.Names <- cbind('Fp', 'Tp', 'Fn', 'Tn')

# init lists
outcomes.List <- list()

# subset data by search type and put into a in a list
for (i in 1:ncol(search.Names)){
  outcomes.List[[i]] <- subset(lda.Data.Long[c(1, 2, 4, 6)], 
                               lda.Data.Long$search.Class == search.Names[i])
  }
# change names
names(outcomes.List) <- search.Names


# ----- Correlation Analysis Across Treatments and ICC Within Treatments -----

# empty list
dat.4Corr <- list() 

# iterate through data frames in list to format data for ICC and correlation
# analysis
for (i in 2:length(outcomes.List)) {
  # select data for ICC analysis
  cur.Corr <- outcomes.List[[i]]
  
  # current variable data
  cur.Most.Corr <- dcast(subset(cur.Corr[, c(1, 3, 4)], 
                                cur.Corr[, 2] == 'most'),
                                PartID ~ BlockOrder)
  cur.Least.Corr <- dcast(subset(cur.Corr[, c(1, 3, 4)], 
                                 cur.Corr[, 2] == 'least'), 
                                 PartID ~ BlockOrder)
  # take average of column values and convert to data frame
  cur.Data.Sum <- as.data.frame(cbind(cur.Most.Corr[1],
                                      rowMeans(cur.Most.Corr[2:5]), 
                                      rowMeans(cur.Least.Corr[2:5])))
  colnames(cur.Data.Sum) <- cbind('PartID', 'Most', 'Least')
  # add to list
  dat.4Corr[[i]] <- cur.Data.Sum
  
  # determine subject behavioral consistency within treatments
  # ICC analysis
  most.ICC <- ICC(na.omit(cur.Most.Corr[, 2:5]), alpha = 0.01)
  least.ICC <- ICC(na.omit(cur.Least.Corr[, 2:5]), alpha = 0.01)
  
  # temporary data frames for outputting results to file
  t.Easy = t(as.data.frame(c(Treatment = "Easy",
                             rbind(subset(most.ICC$results, 
                                          most.ICC$results$type == 'ICC2'),
                                   subset(most.ICC$results,
                                          most.ICC$results$type == 'ICC2k')))))
  t.Hard = t(as.data.frame(c(Treatment = "Hard",
                             rbind(subset(least.ICC$results, 
                                          least.ICC$results$type == 'ICC2'),
                                   subset(least.ICC$results,
                                          least.ICC$results$type == 'ICC2k')))))
  
  # combine into one data frame
  cur.ICC.Output <- as.data.frame(cbind(t.Easy, t.Hard))
  
  # filename, TODO kable this output later
  sprintf("%s ICC Results", search.Names[i])
  # print
  print(cur.ICC.Output)
}

# change names of list slots to variable names
names(dat.4Corr) <- search.Names

# calculate correlations for data from each experimental group
for (i in 2:length(dat.4Corr)){
  # run spearmans correlation
  corr.res <- cor.test(dat.4Corr[[i]][, 2], dat.4Corr[[i]][, 3],
                       method = "spearman", na.action = na.exclude)  
  # print with nice format
  print(sprintf("Spearman's correlation results for %s", names(dat.4Corr[i])))
  corr.4Print <- cbind(names(dat.4Corr[[i]][2]), 
                       names(dat.4Corr[[i]][3]),
                       corr.res$estimate, 
                       corr.res$p.value)
  # change names
  colnames(corr.4Print) <- cbind("Var1", "Var2", "Corr-Value", "P-Value")
  # put results into list
  # print for immediate output
  print(corr.4Print)
}


# ----- Boxplots to compare treatments -----
# use boxplots and histograms to show subject change in behavior as a result
# of difficulty treatment

# empty list
melted.Plot.Data <- list()
melted.Hist.Data <- list()

# go through data and reshape for plotting
for (i in 2:length(dat.4Corr)){
  
  # prep data frame for boxplot 1
  names(dat.4Corr[[i]]) <- cbind("SubjectID", "Easy", "Hard")
  melted.Plot.Data[[i]] <- melt(dat.4Corr[[i]], id = "SubjectID", 
                                value.name = "LD1Score",
                                variable.name = "SearchDifficulty")
  # prep data frame for histogram 1
  melted.Hist.Data[[i]] <- dat.4Corr[[i]][3] - dat.4Corr[[i]][2]
  colnames(melted.Hist.Data[[i]]) <- "Change"
}

# empty plot list
plot.List <- list()

# iterate throught each of the three search types and plot
for (i in 2:length(melted.Plot.Data)){
  plot1 <- ggplot(melted.Plot.Data[[i]], aes(x = SearchDifficulty,
                                             y = LD1Score)) + 
    geom_line(aes(group = SubjectID, colour = SubjectID), size = 1) +
    geom_point(aes(group = SubjectID, colour = SubjectID), size = 1.5) + 
    geom_boxplot(aes(fill = SearchDifficulty), width = 0.6, alpha = 0.6, lwd = 1,
                 notch = TRUE, outlier.shape = NA) +
    labs(title = "Effects of Difficulty Treatment on",  # change labels and titles
         subtitle = sprintf("%s Search LD1 Score", search.Names[i]),  
         y = "LD1 Score") +
    theme_minimal() + 
    theme(plot.title = element_text(hjust = 0.5, size = 12), 
          plot.subtitle = element_text(hjust = 0.5, size = 10),
          legend.position = "none") +
    scale_fill_manual(values = c("#072bab", "#0ab2f4"))  # boxplot fill

  # add to list
  plot.List[[i - 1]] <- plot1  
    
  # save search type histogram as jpeg image
  hist1 <- ggplot(melted.Hist.Data[[i]], aes(x = Change)) + 
    # histogram
    geom_histogram(alpha = 0.7, position = "identity", bins = 10, 
                   fill = "#0072BB") +
    # with normal dist on top
    geom_vline(xintercept = 0, linetype = "dashed", color = 'blue') +
    labs(title = expression(paste("Subject ", Delta, " Across Treatments")), 
         subtitle = sprintf("%s Search LD1 Score", search.Names[i]),
         x = expression(paste(Delta ["H - E"])), 
         y = "Count") + # axes labels and plot titles
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 12), 
          plot.subtitle = element_text(hjust = 0.5, size = 10),
          legend.position = "none")
  
  # add to list
  plot.List[[i + 2]] <- hist1  
}

# plot scatterplots
fig.Title <- sprintf("ldaResults_BoxPlots.jpg") # title
figs <- marrangeGrob(plot.List, nrow=2, ncol=3)  # arrange plots
# wrtie to file
jpeg(filename <- fig.Title, units = "in", height = 6, width = 9, res = 300)
figs
dev.off()  # close and save


# ----- Scatterplots to view correlations -----
plot.List = list()  # list for holding plots

# plot scatterplots to visualize relationships 
for (i in 2:length(dat.4Corr)){
  # format data for plotting
  corr1Plot <- dat.4Corr[[i]]
  # scatterplot
  corrPlot <- ggplot(corr1Plot, aes(x = Easy, y = Hard)) +
    geom_point(shape = 1, size = 2, na.rm = TRUE) +
    geom_smooth(method = lm, se = TRUE) +
    labs(title = sprintf("%s LD1 Search Scores", search.Names[i], 
                         subtitle = "Across Treatments", 
                         x = sprintf("Easy"),
                         y = sprintf("Hard"))) +  # change text
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 14),
          plot.subtitle = element_text(hjust = 0.5, size = 12)) 

 plot.List[[i-1]] <- corrPlot
}

# plot scatterplots
fig.Title <- sprintf("ldaResultsCorrPlots.jpg") # title
figs  <- marrangeGrob(plot.List, nrow=1, ncol=3)  # arrange plots
# wrtie to file
jpeg(filename <- fig.Title, units = "in", height = 3, width = 9, res = 300)
figs
dev.off()  # close and save

# ----- Clean Up -----
# End Sink
sink()
closeAllConnections() # make sure sink saves

# Detach libraries
detach(package:reshape2)
detach(package:plyr)

detach(package:lmerTest)
detach(package:lme4)
detach(package:MuMIn)
detach(package:pscl)
detach(package:car)
detach(package:MASS)

detach(package:ggplot2)
detach(package:gridExtra)
detach(package:grid)
detach(package:knitr)

