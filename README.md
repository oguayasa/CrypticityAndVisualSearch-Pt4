Examining behavioral search 'profiles' during a visual search task
==================================================================

Olivia Guayasamin 9/10/2017

Introduction
------------

This code demonstrate initial analyses to determine if peoples' behavioral "profiles" during a [visual search task](https://github.com/oguayasa/CrypticityAndVisualSearch-Pt1) vary as a function of task difficulty and search outcome. [Previously](https://github.com/oguayasa/CrypticityAndVisualSearch-Pt3), we assigned a single behavioral "profile" score to every search that each subject completed. These scores were estimated using a model generated from LDA analysis, an approach we took to reduce the dimensionality of our data sets that contained various gaze-trajectory and behavioral measurements.

This data set comes from a study where we asked subjects to complete several rounds of a repeated visual search task where their goal was to find as many target stimuli as possible (randomly dispersed inside a large "cloud" of similar looking distractors) within a limited amount of time. Each subject completed two blocks of trials, with one block containing only Easy versions of the task and the other containing only Hard versions. The order in which these versions were presented was counterbalanced across subjects. Subject gaze positions were recorded throughout the study using Tobii eye-trackers.Task difficulty was manipulated by varying the [crypticity of targets](https://github.com/oguayasa/CrypticityAndVisualSearch-Pt2) present in each trial of our search task, and search outcomes were determined by classying each search as: True Positives **TP** were searches where the target was successfully identified, True Negatives **TN** describe searches where a distractor was correctly left alone, False Positives **FP** were searches where a distractor was incorrectly identifed as a target, and False Negatives **FN** describe searches where a target went unidentified.

This analysis is meant to answer questions like, *"Are distinct search outcomes described by different patterns of eye movement behaviors?"*, or *"How do subjects differ in their behavior profiles during successful and unsuccessful searches?"* and *"Are these behavioral differences influenced by search difficulty?"*. The data file is organized by subject, search outcome, crypticity treatment, and the order in which the treatment was presented. The effects of task difficulty and search outcome on behavior profile scores was determined with [GLMMs](http://www.theanalysisfactor.com/advantages-of-repeated-measures-anova-as-a-mixed-model/) containing treatment and search outcome as fixed effects, and individual subject and sub-block order as random effects. Subject consistency within and across crypticity treatments was estimated using Intraclass Correlation Coefficients ([ICC](http://www.theanalysisfactor.com/the-intraclass-correlation-coefficient-in-mixed-models/)) and correlation analysis with [Spearman's Rho](https://www.r-bloggers.com/spearmans-rho/) respectiviely. Summaries, comparisons, and relationships are visualized with tables, boxplots, and scatterplots.

GLMMs were conducted using the [lme4](https://cran.r-project.org/web/packages/lme4/lme4.pdf) package and further analyzed using functions from the [car](https://cran.r-project.org/web/packages/car/car.pdf), [lmerTest](https://cran.r-project.org/web/packages/lmerTest/lmerTest.pdf), [MuMIn](https://cran.r-project.org/web/packages/MuMIn/MuMIn.pdf), and [pscl](https://cran.r-project.org/web/packages/pscl/pscl.pdf) packages. ICC values were estimated using the [psych](https://cran.r-project.org/web/packages/psych/psych.pdf) package, and the *χ*<sup>2</sup> and correlation analyses were performed using the [MASS](https://cran.r-project.org/web/packages/MASS/MASS.pdf) package.

**Summary:** Use GLMMS to determine how search behaviors differ across crypticity treatments and search outcomes. Run ICC and correlation analysis to determine how consistently subjects performed within and across difficulty treatments respectively. **Requires:** Data file, mentioned R libraries.**Outputs:** GLMM, ICC, and correlation analysis. Boxplot and scatterplot figures visualizing the data as jpeg files.

Initializing Steps
==================

Load libraries from CRAN
------------------------

``` r
# read and work with data files
library(reshape2)   
library(plyr)

# glmm analysis
library(lme4)
library(lmerTest)
library(MuMIn)
library(pscl)

# stats functions
library(psych)
library(car)  
library(MASS)

# format figures and test output
library(ggplot2)
library(grid)
library(gridExtra)  
library(knitr) 
library(broom)
```

Import and format data files
----------------------------

After importing our data files, we need to make sure that all data rows containing numbers representing levels are converted to factors. Otherwise, some of our later reshaping and analyses methods may get confused and throw an error.

``` r
# import data files containing LD1 scores
lda.Data.Long <- read.csv("val_LDA_Data_Long.csv")

# convert variables to factors
lda.Data.Long$PartID <- factor(lda.Data.Long$PartID)  
lda.Data.Long$TreatmentOrder <- factor(lda.Data.Long$TreatmentOrder)  
lda.Data.Long$BlockOrder <- factor(lda.Data.Long$BlockOrder)  
```

Using GLMMs to Determine Treatment Effects
==========================================

To determine how search profiles differ according to difficulty treatment (Easy vs. Hard) and search outcomes, we will conduct repeated measures analyses using Generalized Linear Mixed Models. There are several reasons why GLMMs can be a better alternative to Repeated Measures ANOVAs. We won't spend time discussing that here, but if you are interested in knowing more, the Analysis Factor blog gives a nice overview [here](http://www.theanalysisfactor.com/advantages-of-repeated-measures-anova-as-a-mixed-model/).

Check data distributions prior to analysis
------------------------------------------

This is so that we know how to properly specify our GLMMs in our code. If the data are clearly bimodal, or do not appear continuous, we will need to take this into account when specifying our GLMMs, or else our results will be irrelavant. Here, we first will visualize data distributions using histograms, and then compare with normal distributions using qq-plots.

When we first generated our search profile scores in a previous [tutorial](https://github.com/oguayasa/CrypticityAndVisualSearch-Pt3) and plotted histograms of profile scores by search outcome (TP, TN, FN, FP), it did appear as though the data were bimodal. Specifically, it looked as though there were two distributions. One was comprised of the positive search outcome (TP, FN), and the other contained the negative search outcomes (TN, FN). However, at the time we did not plot according to whether the search outcome was positive or negative, so we will do so here. As a reminder, a *positive* search outcome is one where the participant clicked on a stimuli believing it to a be a task target, while a *negative* search outcome is one where the participant never clicked on the stimuli at all. 

### Check to see if data is bi-modal

``` r
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
```

![](https://github.com/oguayasa/CrypticityAndVisualSearch-Pt4/blob/master/imgs/ldaResults_hist1.jpg)

**Figure 1:** Histogram of search profile scores colored according to whether search outcome was positive or negative. 

Clearly, it looks as though we are dealing with two separate distributions of profile scores. Here, I am not going to both with testing to determine if this difference is "statistically" significant for two reasons: 1) from visual examination these distributions are barely overlapping, with clearly different central values and 2) conducting an unneeded statistical test would raise the risk of Type 1 error (false positive) for all of the analyses in this set of tutorials. The benefits would be minimal (telling us what we already know), and the cost would be that some (or more) of our results may be invalid due to chance. 

### Check the normality of each distribution

Now that we know we are working with two distributions, the best course of action is to run two GLMMs, with each examining the search profile scores from positive and negative search outcomes respectively. Before we run these GLMMs, however, it is important that we determine whether the shape of these distributions approximately normal. Determining the shape of a distribution is vital when running a GLMMs, because different distribution shapes each come with their own set of instrucitons that we must use to inform the GLMM. To check if our distributions are approximately normal, we will visualize our data with a qq-plot. 

``` r
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
```
![](https://github.com/oguayasa/CrypticityAndVisualSearch-Pt4/blob/master/imgs/ldaResults_qqPlots.jpg)

**Figure 2:** Plotting search profile scores (dots) on top of a simuliated normal line.

Nice! It looks like our data points largely fall on top of the diagonal line, indicated that our distributions are indeed normal. We can now proceed to actually running our GLMMs. 

Specificy our GLMMs
-------------------

While the code demonstrates how to run a GLMM within a loop, we will only show the detailed output for the GLMM conducted on negative search outcomes. For more information on how to specify and run GLMMs in R, I highly recommend [this](http://ase.tufts.edu/gsc/gradresources/guidetomixedmodelsinr/mixed%20model%20guide.html) thorough tutorial. 

``` r
# run separate glmms for pos and neg dataframes in list
for (i in 1:2){
  # extract current data frame
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
}
```

(An easy way to check if the model was specified correctly is to take a look at the distrition of errors (residuals). If you would liek to verify this model for yourself,  you can find the residual plots [here](https://github.com/oguayasa/CrypticityAndVisualSearch-Pt4/blob/master/imgs).

Examine one GLMM results
------------------------

Using the GLMM for the negative search outcomes, let's look at how all of our fixed and random effects played out. 


| term                       |    estimate|  std.error|    statistic| group      |
|:---------------------------|-----------:|----------:|------------:|:-----------|
| (Intercept)                |  -1.5416051|  0.1125732|  -13.6942427| fixed      |
| search.ClassTn             |  -0.0334213|  0.0841119|   -0.3973427| fixed      |
| ExpVermost                 |   0.8266510|  0.0850628|    9.7181242| fixed      |

| .rownames                  |      X0.5..|     X99.5..|
|:---------------------------|-----------:|-----------:|
| sd\_(Intercept)|PartID     |   0.3680675|   0.7505657|
| sd\_(Intercept)|BlockOrder |   0.0000000|   0.1118345|
| sigma                      |   0.6587191|   0.7776333|
| (Intercept)                |  -1.8388186|  -1.2099219|
| search.ClassTn             |  -0.2390326|   0.2064321|
| ExpVermost                 |   0.6156785|   1.0490708|
| search.ClassTn:ExpVermost  |  -1.2759810|  -0.5974316|

Above, we have two lists of the estimated effect sizes of our fixed and randome effects. The first table lists the fixed effect estimates and some stats, while the second table lists the upper and lower confidencce intervals of all fixed and randome effects. 


|                              |  Estimate|  Standard Error|    DF|  t-value|  Lower CI|  Upper CI|  p-value|
|------------------------------|---------:|---------------:|-----:|--------:|---------:|---------:|--------:|
| search.Class:ExpVer Fn least |   -1.5416|          0.1126|  53.4|   -13.69|   -1.7674|   -1.3159|        0|
| search.Class:ExpVer Tn least |   -1.5750|          0.1126|  53.4|   -13.99|   -1.8008|   -1.3493|        0|
| search.Class:ExpVer Fn most  |   -0.7150|          0.1133|  54.8|    -6.31|   -0.9420|   -0.4879|        0|
| search.Class:ExpVer Tn most  |   -1.6878|          0.1126|  53.4|   -14.99|   -1.9135|   -1.4620|        0|

|                 |  Estimate|  Standard Error|    DF|  t-value|  Lower CI|  Upper CI|  p-value|
|-----------------|---------:|---------------:|-----:|--------:|---------:|---------:|--------:|
| search.Class Fn |   -1.1283|          0.1046|  39.9|   -10.79|   -1.3397|   -0.9168|        0|
| search.Class Tn |   -1.6314|          0.1044|  39.6|   -15.62|   -1.8425|   -1.4203|        0|

|              |  Estimate|  Standard Error|    DF|  t-value|  Lower CI|  Upper CI|  p-value|
|--------------|---------:|---------------:|-----:|--------:|---------:|---------:|--------:|
| ExpVer least |   -1.5583|          0.1044|  39.6|   -14.92|   -1.7694|   -1.3472|        0|
| ExpVer most  |   -1.2014|          0.1046|  39.9|   -11.48|   -1.4128|   -0.9899|        0|

Here, we are examining the estimates for each "level" or combinates of the fixed effects. The tables descend according to fixed effect interactions, search outcome effect, and task difficulty effect. They list the estimated mean of each level, and whether these estimated means are significnatly different from zero. However, by examining the lower and upper CI, one can also determine if the levels significantly differe from each other. 


|            |       Chi.sq|  Chi.DF|    p.value|
|------------|------------:|-------:|----------:|
| BlockOrder |    0.0760293|       1|  0.7827523|
| PartID     |  198.3603504|       1|  0.0000000|

| names |          x|
|:------|----------:|
| R2m   |  0.1471961|
| R2c   |  0.4775683|

These two tables represent ways of determine how important the random effects were. The first table describes whether a model with the effect is significantly better than a model without the first, and the second describes how much additional variance the model explains once the random effects are included. 

To make a long story short, while there is a significant interaciton between search outcome and task difficulty, it appears that this is driven by Fn searches that took place in the Easy version of the task. In these searches, the profile score is significantly higher than in all other combinations of fixed effects. Additional, there appears to be a difference in profile score due ot search outcome, with Fn scores being higher than Tn scores, but it is difficulty to determine how much this difference is driven by the interaction effect. Lastly, while BlockOrder was not a significant random effect (including it in the model did not explain a significant amount of variation) participant ID was. This indicates that each participant has search behavior profile scores that were consistent throughout all instances in the experiment described by the different combinations of fixed effects in this GLMM. While this strong suggests consistency of participant behavior, it does not tell us whether this consistency was within a particular combination of fixed levels (Were profile scores during Fn searches in the Easy treatment consistent across trials?), or across (Were profile scores during Fn treatments similar across Easy and Hard treatments?), or both. To determine this, we must conduct further analyses. 

### The Effects of Difficulty Treatment on LD1 Scores during Tp Searches

But before we start analyzing subject consistency. We need to address one issue in our data, primarily, the rarity of Fp searches. Throughout the study, participants rarely committed a False Positive mistake. This means two things: 1) the GLMM for positive searches left out a lot of data for Tp searches and 2) we don't have enough data to continue analyzing Fp searches. 

The first issue occurs because of how R handles missing data. In the GLMM above, I instructed R to drop rows that contained missing values. This means that only subjects who committed both Fp and Tp searches were included in the GLMM analysis. While these results are still informative, they neglect to include all of the data for subjects who only performed Tp searches and never committed an Fp error. To address this, we are going to analyze the full Tp data set below, using a simple paired test to examine the effects of task difficulty on search behavior profile scores during Tp searches.

``` r
# separate fp search rows from the rest
tp.Data <- subset(lda.Data.Long, lda.Data.Long$search.Class == 'Tp')
# divide fp search rows by difficulty treatment
tp.Easy <- subset(tp.Data, tp.Data$ExpVer == 'most')
tp.Hard <- subset(tp.Data, tp.Data$ExpVer == 'least')

# put median values into a table
tp.MedScores <- as.data.frame(rbind(median(tp.Easy$LD1, na.rm = TRUE), 
                                    median(tp.Hard$LD1, na.rm = TRUE)),
                              row.names = c('Easy', 'Hard'),
                              col.names = 'LD1 Score Medians')

# run wilcox test
tp.Test <- wilcox.test(tp.Easy$LD1, tp.Hard$LD1, 
                       paired = TRUE, conf.int = TRUE, 
                       na.action = na.exclude)
```

|      |    V1|
|------|-----:|
| Easy |  2.39|
| Hard |  2.17|

|  estimate|  statistic|  p.value|  conf.low|  conf.high|
|---------:|----------:|--------:|---------:|----------:|
|      0.27|       6923|        0|      0.09|       0.46|


Looking at the median values values and test results, there is both a small difference in profile score (~0.22) across the difficulty treatments and a significant p-value. These results indicate that there is small but significant effect of task difficulty on Tp behavior profile scores. The meaning of these score differences will be interpreted when we get to the visualizations of our results. 

The second issue arises because the rarity of Fp searches is a result of the fact that subjects rarely even committed one, much less more than one. This means that we don't have the data to examine subject consistency, and so, we will not include Fp searches in our following analyses of subject profile score consistency within and across treatments.

Determine within-treatment consistency of subject search profiles
-----------------------------------------------------------------

To analyze within difficulty treatment consistency we are going to use a statistical measure of reliability known as the intraclass correlation coefficient (ICC). There are many versions of the ICC equation, but the one that will be applied here is the ICC 2, because I am considering sub-block order (judge) as a random effect. This ICC measures subject rank consistency across samples, while controlling for the effects of varying group means. It is not an absolute measure of agreement. 

### Reformat data for analyses and plotting

Subset the data

``` r
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
```

Now that the data is in a suitable format (we converted from long to wide) we can determine within treatment consistency. 

### Conduct ICC analyses

``` r
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
  icc.Name <- sprintf("%s ICC Results", search.Names[i])
  # print
  print(kable(cur.ICC.Output, caption = icc.Name, results = 'asis', digits = 2))
}
```

**Consistency of search profile scores during Tp Searches**

|             | V1        | V2        | V3           | V4           |
|-------------|:----------|:----------|:-------------|:-------------|
| Treatment   | Easy      | Easy      | Hard         | Hard         |
| type        | ICC2      | ICC2k     | ICC2         | ICC2k        |
| ICC         | 0.6623663 | 0.8869694 | 0.2860182    | 0.6157369    |
| F           | 9.009525  | 9.009525  | 2.669457     | 2.669457     |
| df1         | 36        | 36        | 35           | 35           |
| df2         | 108       | 108       | 105          | 105          |
| p           | 0         | 0         | 6.307545e-05 | 6.307545e-05 |
| lower.bound | 0.4736108 | 0.7825586 | 0.08210186   | 0.26350475   |
| upper.bound | 0.8183363 | 0.9474202 | 0.5360318    | 0.8221046    |


**Consistency of search profile scores during Fn Searches**

|             | V1          | V2          | V3           | V4           |
|-------------|:------------|:------------|:-------------|:-------------|
| Treatment   | Easy        | Easy        | Hard         | Hard         |
| type        | ICC2        | ICC2k       | ICC2         | ICC2k        |
| ICC         | 0.1190205   | 0.3508182   | 0.5034608    | 0.8022057    |
| F           | 1.525015    | 1.525015    | 5.019074     | 5.019074     |
| df1         | 31          | 31          | 36           | 36           |
| df2         | 93          | 93          | 108          | 108          |
| p           | 0.06315376  | 0.06315376  | 3.843326e-11 | 3.843326e-11 |
| lower.bound | -0.06813046 | -0.34253253 | 0.2880977    | 0.6181382    |
| upper.bound | 0.3926140   | 0.7211065   | 0.7120399    | 0.9081795    |

**Consistency of search profile scores during Tn Searches**

|             | V1           | V2           | V3           | V4           |
|-------------|:-------------|:-------------|:-------------|:-------------|
| Treatment   | Easy         | Easy         | Hard         | Hard         |
| type        | ICC2         | ICC2k        | ICC2         | ICC2k        |
| ICC         | 0.5655578    | 0.8388970    | 0.6043178    | 0.8593357    |
| F           | 6.614153     | 6.614153     | 7.115602     | 7.115602     |
| df1         | 36           | 36           | 36           | 36           |
| df2         | 108          | 108          | 108          | 108          |
| p           | 1.021405e-14 | 1.021405e-14 | 8.881784e-16 | 8.881784e-16 |
| lower.bound | 0.3564142    | 0.6889752    | 0.4022159    | 0.7290985    |
| upper.bound | 0.7555809    | 0.9251796    | 0.7813344    | 0.9346096    |

Looking at the values for the "ICC", "p", "lower.bound", and "upper.bound" rows for all search outcomes, we see something interesting. Namely that each search outcome shows a different pattern of profile score consistency. During Tp searches subjects were more consistency in the Easy treatment, while the opposite was true for Fn searches, and there was not difference in consistency between the difficulty treatments during the Tn searches. 

Boxplots to visualize search outcome and treatment effects
----------------------------------------------------------

To visualize treatment effects and subject variation, we will use boxplots overlaid on top of individual lines representing subject averages across treatments. Below each boxplot figure, we will include a histogram showing the distribution of within subject differences across treatments. These figures will include data from all measures, so that we can discuss results from the measures that were not used as examples for the GLMM and ICC analyses.

![](https://github.com/oguayasa/CrypticityAndVisualSearch-Pt4/blob/master/imgs/ldaResults_BoxPlots.jpg)

**Figure 3** Boxplots showing the effects of difficulty treatment on search profile scores, organized by search outcome. 

These boxplots validate our GLMM and Tp results nicely. Specifically, we can see that the biggest difference between Easy and Hard treatments occures during Fn searches, and that while the difference in Tp searches is significant, it is small. 

Correlation Analysis Across Difficulty Treatments
=================================================

Using the data that we formatted in a previous section, we will examine subject consistency across task difficulty treatments for each search outcome. 

Determine across-treatment consistency of subject search profiles
-----------------------------------------------------------------

Then we will run correlate the subject averages across treatments using Spearman's *ρ*, a rank-based correlation statistic for use with non-parametric data distributions.

``` r
# change names of list slots to variable names
names(dat.4Corr) <- search.Names

# calculate correlations for data from each experimental group
for (i in 2:length(dat.4Corr)){
  # run spearmans correlation
  corr.res <- cor.test(dat.4Corr[[i]][, 2], dat.4Corr[[i]][, 3],
                       method = "spearman", na.action = na.exclude)  
  # print with nice format
  corr.4Print <- cbind(names(dat.4Corr[[i]][2]), 
                       names(dat.4Corr[[i]][3]),
                       corr.res$estimate, 
                       corr.res$p.value)
  # change names
  colnames(corr.4Print) <- cbind("Var1", "Var2", "Corr-Value", "P-Value")
  # create table title
  tab.Cap <- sprintf("Correlation Results for %s", search.Names[i])
  # print for immediate output
  print(kable(corr.4Print, results = 'asis', caption = tab.Cap, digits = 2))
}
```
** Tp Searches **

|     | Var1 | Var2  | Corr-Value        | P-Value              |
|-----|:-----|:------|:------------------|:---------------------|
| rho | Most | Least | 0.644787644787645 | 3.36533526597102e-05 |

** Fn Searches **

|     | Var1 | Var2  | Corr-Value        | P-Value              |
|-----|:-----|:------|:------------------|:---------------------|
| rho | Most | Least | 0.595674486803519 | 0.000416581254664065 |

** Tn Searches **

|     | Var1 | Var2  | Corr-Value        | P-Value             |
|-----|:-----|:------|:------------------|:--------------------|
| rho | Most | Least | 0.440493124703651 | 0.00681259815161805 |

Interestingly, all of the correlations are moderate to strong, positive, and significant. This indicates that the individuals who had higher (or lower) profile scores during the Easy treatment were also the individuals who had higher (or lower) scores during trials in the Hard treatment.

Scatterplots to view across-treatment relationships
---------------------------------------------------

Let's visualize these correlations using scatterplots overlaid with a line of best fit to validate our test results.

![](https://github.com/oguayasa/CrypticityAndVisualSearch-Pt4/blob/master/imgs/ldaResultsCorrPlots.jpg)

**Figure 4** Scatterplots organzied by search outcome overlaid with line of best fit representing the relationship between subject search profile scores during Easy and Hard treatments. 

Simply put, our correlation results are backed up by visualized these relationships. This may seem like a trivial statement, but it is always important to visualize your data to verify the results of statistical test. Visualizeing your data can let you know if your data meet the assumptions of the test, and therefore, if the results of the test are valid and meaningful. 

Conclusions
===========

One thing that I think is particularly interesting about this data set, is not that there were differences in profile scores between Negative and Positive search outcomes (given that positive searches included an additional behavior, an identifying mouse click, this result is not at all surprising), but that there were differences between True and False searches within the Negative and Positive outcomes. Despite the fact the decision outcome (click or not click) was the same, correct and incorrect decisions were preceeded by different patterns of search and attention. For example, Tn and Fn searches demonstrated both different average search profile scores and different responses to task difficulty. Even though their conscious action was the same in both cases, the cognitive processes that occured during each type of search was different. How do these different unconscious processes lead to the same conclusion? Why are these processes different, and can we describe how they are different? From a more human-centric and applied angle, we can ask, by knowing how these processes differ, is there any way we can learn to intervene and help individuals make correct decision more often? In science, results rarely lead to finished answers and a completed story, instead they show us new quesitons and problems that we never considered before. The questions I've listed are just a few examples of how we could use our current results to inspire further research. 
