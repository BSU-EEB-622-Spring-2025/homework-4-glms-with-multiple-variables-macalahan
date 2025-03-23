## Homework 4 Submission Calahan## 

# Load packages
library(MASS)
library(marginaleffects)
library(performance)
library(ggplot2)
library(pROC)

# read in csv
mistletoe <- read.csv("mistletoes.csv")
# look at first 6 lines:
head(mistletoe)

## Question 1:

## 1a) Fit a glm assessing evidence for the following hypothesis: Seedling density is increased beneath trees experiencing mistletoe infection. Describe the rationale you used in selecting a glm structure and probability distribution. Calculate model fit using MAE.

# Based on the data, we can determine that a poisson or negative binomial is appropriate, before testing for overdispersion. Both of these distributions would work because the number of seedlings in this dataset are discrete, and positively constrained, and we do not have a defined number of possible seedlings. We learned that in poisson, we use lambda, or the rate of success, which is the number of successes * number of trials, which describes the number of occurrences per unit of time or space. In this case, the space is the space under trees surveyed for seedlings. Negative binomial is used instead of poisson when count data is overdispersed, which is common in ecology, so I had to test for that first.

##ASW: right! A Poisson distribution only has one parameter, which describes the mean rate and the "spread" of the distribution... so it assumes the mean and the variance are equal. A negative binomial will estimate that variance separately, which is a better choice if the variance is greater or smaller than the mean. If we apply a poisson when over/under dispersion is present, we will violate the assumptions of this distribution, which will make our p-values inappropriate to interpret!


## The poisson distribution assumes the mean = the variance, it is important to check that the data meet that assumption:
mean(mistletoe$Seedlings)  #160.6591
var(mistletoe$Seedlings) #113237.6

# Since variance > mean, the data is overdispersed, and the poisson distribution would not be appropriate. Proceed with negative binomial:
mismod.nbin <- glm.nb(Seedlings~Treatment, data = mistletoe)
# summarize model results
summary(mismod.nbin)
coef(mismod.nbin)
summary(mismod.nbin)
confint(mismod.nbin)

# MAE using performance package
performance::mae(mismod.nbin) # 145.841

## 1b) Use visual (e.g. a marginal effects plot) and written (e.g. effect sizes on scale of response) approaches to interpret the results of your model.

# Use marginal effects package to plot predictions, with treatment as categorical
plot_predictions(mismod.nbin, condition="Treatment")

# Estimates using link function "exp" for negative binomial. This converts to 0-infinity:

# Effect of parasitized
# Parasitized (intercept in summary table)
exp(5.7308) # converted to 308.2

# Effect of nonparasitized
# Not parasitized (intercept) + Treatmentunparasitized as seen in summary table
exp(5.7308+-3.1575) # converted to 13.1

## Based on your fitted model results and model fit, write 3-4 sentences to discuss the following biological conclusions. Does mistletoe infection alter seedling density? How much does seedling recruitment differ beneath parasitized and unparasitized trees? Explain which elements of your glm results informed your conclusions and annotate the steps you needed to take to interpret your parameters. Explain which elements of your glm results informed your conclusions and annotate the steps you needed to take to interpret your parameters.

# Results support the hypothesis that mistletoe parasitizing trees shifts community composition in forests and influences seedling dispersal underneath trees. The model showed that treatment had a statistically significant effect on seedling density (p-value<0.05). To look at this in biological terms, the number of seedlings counted under parasitized trees averaged 308, while the mean of seedlings underneath unparasitized trees averaged only 13, a difference of 295 seedlings between treatment groups. Mean absolute error was high, calculated at 145.841, so predicted veviated from observed values significantly, indicating that there are other variables that are causing variation that are not included in the model. 

## ASW: lovely work! Great interpretation, and i agree that this is moderate fit -- considering that the values range from 0 to the 2000s (but this is a simpler model than what they fit in the real paper!)


## 1c) During the course of this study, 2012 was an atypically rainy year, compared to 2011. Fit an additional glm that quantifies how the effect of mistletoe differs between the two years in this study. Write ~2 new sentences that summarize the results of the new model and their biological implications.

# first convert year as factor
mistletoe$Year<-as.factor(mistletoe$Year)

# Rerun negative binomial + year
mismod.nbin2 <- glm.nb(Seedlings~Treatment*Year, data = mistletoe)

# Summarize new model
summary(mismod.nbin2)

# Plot again with year, now years differentiated by color
plot_predictions(mismod.nbin2, condition = c("Treatment", "Year")) 

# Predict seedling estimates for treatment groups per year
predictions(mismod.nbin2, newdata=data.frame(Treatment=c("parasitized", "unparasitized","unparasitized","parasitized"), Year=c("2011","2012","2011","2012")))

# Calculate new MAE:
performance::mae(mismod.nbin2) # 140.0407

# This model included the effects of treatment (parasitized and unparasitized) and year as well as treatment:year as an interaction term. The state of the year 2012 as well as the interaction term was found to be just marginally significant (p-value = 0.0717, 0.0599), so we are not seeing that year has a significant effect on seedling numbers. MAE has decreased only slightly to 140.0407, meaning that it is estimated that predictions from the model are still off from real observed values by an average of ~140 seedlings. This is considerable when we consider the number of seedlings ranged from 13 to 308, so we are still missing explanation for variation in the model.

## ASW: number of seedlings ranges from 0 to >2500
hist(mistletoe$Seedlings)

## ASW: what does the interaction indicate?

## 29/30

## Question 2:

# read in csv
treemortality <- read.csv("treemortality.csv")
# look at first 6 lines
head(treemortality)

## 2a) Fit a glm (using a probability distribution of your choice) that reflects the following research question (including thinning as your only predictor and mortality as your response): Do forest thinning treatments reduce the probability of tree mortality? Interpret the results of the glm by writing 2-4 sentences about the biological significance of the effect of thinning, including descriptions of the sizes of the effect on the scale of the response variable, evidence for/against the hypothesis, and a metric of model fit.

# I will use a binomial distribution glm because the data is binary, ex. thinning (yes = 1, no = 0) and (survival = 1, mortality =0).
treemod.bi <- glm(mortality~thinning, 
              data=treemortality, family="binomial"(link="logit")) # link function is logit

# summarize model
summary(treemod.bi)
coef(treemod.bi)
summary(treemod.bi)
confint(treemod.bi)

# Probability of tree mortality when treatment is set at 0
plogis(coef(treemod.bi)["(Intercept)"]) # = 0.7297297

# Probability of mortality of thinned trees 
plogis(0.9933+-1.8559) # intercept plus slope (effect of being thinned)
# = 0.2967964

# Plot using marginal effects package 
plot_predictions(treemod.bi, condition="thinning") +
  ylab("Probability of Tree Mortality") + # Add label axes
  xlab("Thinning Experiment") + # Add label axes
  theme_bw()

# Model fit in binomial glms:
## ROC for model fit from the pROC package
test_prob <- predict(treemod.bi, type = "response")
test_roc <- roc(treemortality$mortality # survival data
                ~ test_prob, plot = TRUE,  # Test probabilities
                print.auc = TRUE)

test_roc #area under curve 0.7096

### Interpret the results of the glm by writing 2-4 sentences about the biological significance of the effect of thinning, including descriptions of the sizes of the effect on the scale of the response variable, evidence for/against the hypothesis, and a metric of model fit.

# Our model revealed that thinning treatment had a significant effect (p-value < 0.05) on tree mortality. Using the binomial link function to transform the intercept to a biologically-relevant value, we could see that when treatment is set to zero, base tree mortality from a fire was expected to be 73%, opposed to 30% with the effect of thinning. Area under the curve (0.710) was relatively high in this ecological application, indicating that predictions of the model were better than what would have been predicted by chance (0.5).

## 2b) The researchers explicitly considered the potential for confounding relationships related to tree size in their design and randomized their post-fire sampling by tree size. Given this information, do the researchers need to incorporate tree size into their glm to accurately estimate the effect of thinning? Why or why not?

# If they had not randomly sampled, they could include tree size as another effect in their model to check for significance. However, since it was mentioned in the project description that the researchers resampled the 1000 trees in a randomized fashion by tree size to ensure that small and large stems were recorded equally, so I do not think that the researchers would need to worry about cofounding variables in this case. 

## ASW: excellent! You could include it for other reasons, but it's not needed to accurately estimate the effect of thinning!

## 2c) Refit the model from 2a to include the necessary variables to minimize bias in our estimation of the “thinning” variable, based on the reviewer’s proposed DAG (above). Does the effect of “thinning” change? If so, describe the degree of change and why the two models may differ in their conclusions. If needed, modify your model interpretation from 2a.

# New model, thinning, slope, and road distance included:
treemod.bi2 <- glm(mortality~thinning + roaddist + slope, 
                 data=treemortality, family="binomial"(link="logit")) 

# View summary table
summary(treemod.bi2)

# Save intercept and slopes of variables
int<-coef(treemod.bi2)["(Intercept)"]
slopeT<-coef(treemod.bi2)["thinning"] # save slope of thinning treatment
slopeR<-coef(treemod.bi2)["roaddist"] # save slope of road distance
slopeS<-coef(treemod.bi2)["slope"] # save slope of measured slope

# Use link functions for interpretation:
# Interpret base tree mortality (without thinning treatment): hold thinning at zero, slope of measured slope and road distance at mean:
plogis(int + (slopeT*0)+(slopeR*mean(treemortality$roaddist))+slopeS*mean(treemortality$slope)) # plogis as link function
# 0.528606, Our new model predicts tree mortality in a wildfire without any type of tree thinning treatment to be 52.8%.

# Interpret thinning treatment on tree mortality: hold thinning treatment at 1, slope of slope and road distance at mean:
plogis(int + (slopeT*1)+(slopeR*mean(treemortality$roaddist))+slopeS*mean(treemortality$slope))
# 0.3096572, Our new model predicts tree mortality in a wildfire with thinning to be about 31%.

# Plot by condition of thinning (but model also accounts for raod distance and slope)
plot_predictions(treemod.bi2, condition="thinning") + # specify slope to plot - thinning
  ylab("Predicted Tree Mortality") + # Add label axes
  xlab("Thinning Treatment") + # Add label axes
  theme_bw()

# Make predictions 
predictions(treemod.bi2, newdata=data.frame(thinning=c("thinned", "unthinned"), roaddist=mean(treemortality$roaddist), slope=mean(treemortality$slope)))

# ROC for new model
test_prob2 <- predict(treemod.bi2, type = "response")
test_roc2 <- roc(treemortality$mortality 
                ~ test_prob2, plot = TRUE,  
                print.auc = TRUE)
# AUC = 0.959

# The effect of thinning changed just slightly. Once transformed, we can see that the predicted percentage of tree mortality with the thinned treatment slightly increased from 30 to 31%. The more dramatic change was seen in the predicted morality of trees without thinning treatment, which decreased from 73% in the first model to 53% in the new model that included the other variables. This means that measured slope and road distance variables have significant effects on tree mortality ( also statistically significant in model), and are responsible for variation in the model that was originally not explained in model 1. We can also see that the model was a better fit as the AUC increased to 95.9%, which is excellent in ecological models, and gives us great confidence that results are not due to random chance alone.

## ASW: the effect of thinning is the difference between the unthinned and thinned treatments, so the effect here has changed pretty profoundly. In the original model, there's a >40% decrease in prob of mortality between unthinned and thinned areaws, but in this model, that shifts to only being ~20%. 

## ASW: AUC is not telling us about the results being due to random chance. it's describing the model's ability to classify "0s" and "1s" correctly.  AUC of 1.0 indicates perfect discrimination, while 0.5 indicates that the model isn't classifying values any better than a random guess would (because if we randomly guess about 0s and 1s, we should be right half the time).

## ASW: why do the two models differ in their conclusions?
## 16/20

## ASW: 45/50 Nice work!!