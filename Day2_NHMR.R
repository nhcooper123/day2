# -----------------------------------------------
# Getting started with Stats
#-----------------------------------------------
# Load libraries
library(ggplot2)
library(dplyr)
library(gridExtra)
library(ggfortify)

#-----------------------------------------------
# t-test practical
#-----------------------------------------------

# read the data and explore
tt <- read.csv("GardenOzone.csv")
str(tt)
tbl_df(tt)

# Make a helpful plot linked to the question about two means
ggplot(tt, aes(x = Ozone)) + 	
  geom_histogram(bins = 10) + 
  facet_wrap(~Garden.location, ncol = 1)

# check the means and some assumptions about variance
summarise(group_by(tt, Garden.location),
	meanOzone = mean(Ozone),
	varOzone = var(Ozone))

# Model Formulae syntax for t-test with a data frame
# note default is welch and unequal variance is OK
t.test(Ozone ~ Garden.location, data = tt, var.equal = FALSE)

#--------------------------------------------------
# chi-square (Hadleyverse version)
#--------------------------------------------------

## read in the data
dd <- read.csv("ladybirds.csv")
glimpse(dd)
str(dd)

## get the number of each colour in each habitat type
lb <- summarise(group_by(dd, Habitat, colour), 
	          obs = sum(number))
 
lb # LOOK AT IT

# make a plot
ggplot(lb, aes(x = colour, y = obs, fill = colour)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Habitat) +
  scale_fill_manual(values = c("black", "red"))

# the contingency table
lb_matrix <- xtabs(obs ~ Habitat + colour, data = lb)

# the test
chisq.test(lb_matrix)

#----------------------------------------------------
# general linear models - ANOVA - daphniagrowth.csv
#----------------------------------------------------

## read in the data
dd <- read.csv("Daphniagrowth.csv")

## look at the structure of the data
str(dd)
tbl_df(dd)

# box plot
ggplot(dd, aes(x = parasite, y = growth.rate))+
	geom_boxplot()+
	theme(axis.text.x = element_text(angle = 90, vjust = 1))

# Calculate the Grand Mean
grMean<-mean(dd$growth.rate)

# summarise the data by parasite
growth.stats<-summarise(group_by(dd, parasite),
	meanGR = mean(growth.rate),
	seGR  = sd(growth.rate)/sqrt(n()),
	lwr = meanGR - seGR,
	upr = meanGR + seGR)

# create the limits aesthetics for error bars
limits <- aes(ymin = lwr, ymax = upr)

# construct a fancy picture 
# note that we use the growth stats first,
# then add the raw data....

ggplot(growth.stats, aes(x = parasite, y = meanGR)) + 
	# first add the points from the growth stats
	geom_point(colour = 'red', size = 8, alpha = 0.5) +
	# now add the error bars from the limits
	geom_errorbar(limits, width = 0.1, col = 'red', alpha = 0.8) +
	# now add the raw data from the original dataframe
	geom_point(data = dd, aes(x = parasite, y = growth.rate), 
		size = 3, colour = 'darkgreen', alpha = 0.3) +
	# now add the mean line we calcuated
	geom_hline(yintercept = grMean, linetype = 'dashed', colour = 'darkgreen') +
	theme(axis.text.x = element_text(angle = 90, vjust = 1))+
	coord_flip()

# BUILD THE MODEL
mod2 <- lm(growth.rate ~ parasite, data = dd)

## model diagnostics
par(mfrow=c(2, 2), mar=c(5,4,4,2))
plot(mod2)

## model interpretation
anova(mod2) ## the parasite treatment explains a large amount of variation in daphnia growth rate

summary(mod2) ## Shows the effects of the parasites on growth rate, relative to the control (Intercept)

# --------------------------------------------------
# RELEVELING when reference is not alphabetical
# --------------------------------------------------

# if you want to change the reference level, use relevel
# otherwise it takes the first alphabetically and numerically
# first check the levels
levels(dd$parasite) 

# use mutate and relevel to change the levels
dd <- mutate(dd, parasite = relevel(parasite, 
							 ref = "control"))

# check the levels again to see if they have changed
levels(dd$parasite)

## --------------------- EXTRAS ----------------------------------------------

# How to fit a Tukey or other post-hoc comparison using the multcomp library
mod2.aov <- aov(mod2) # repackage the model for a Tukey Test
tukey.out <- TukeyHSD(mod2.aov) # generate tukey test
tukey.out # table
# plot, setting margins and 1 x 1 grid and rotated axis labels
par(mfrow = c(1,1), mar = c(5,20,4,2))
plot(tukey.out, las = 2)

# --------------
library(multcomp)
tukey.par<-glht(mod2, linfct=mcp(parasite = "Tukey"))
summary(tukey.par)
par(mfrow = c(1,1), mar = c(5,20,4,2))
plot(tukey.par)

# RESET PLOT MARGINS
par(mar = c(5,4,4,2))

## Set up orthogonal (planned) contrasts: rather than ALL pairwise....
## some specific examples
c1 <- c(-1, 1/3, 1/3, 1/3) # control vs overall parasite mean
c2 <- c(0, -1, 1/2, 1/2)   # parasite 1 vs 2 & 3
c3 <- c(0, 0, -1/2, 1/2)   # parasite 2 vs 3

# combine the hypotheses
mat <- cbind(c1, c2, c3)

# allocate them to the treatment
contrasts(dd$parasite) <- mat

# run the model returning THESE hypotheses
mmcheck <- lm(growth.rate ~ parasite, data = dd)
summary(mmcheck)

# now set up a contrast to compare copntrol to mean of all 3 parasites
levels(dd$parasite)
contrastmatrix <- cbind(c(0, 1, 1, 1))
contrasts(dd$parasite, how.many = 1) <- contrastmatrix

# rerun
mod2 <- lm(growth.rate ~ parasite, data = dd)
summary(mod2)

#----------------------------------------------------
# general linear models - regression
#----------------------------------------------------

# Find the room, get the file
dd<-read.csv("plant.growth.rate.csv")
str(dd)

# PLOT YOUR DATA
ggplot(dd, aes(x = soil.moisture.content, y = plant.growth.rate)) + 
	geom_point(col = 'cornflowerblue', size = 3) + 
	labs(x = expression(paste("Soil Moisture ", bar(x)/mu)), 
		y = "Growth Rate (mm/week)") +
	theme_bw() +
	theme(axis.title=element_text(size=rel(1.5)))

# make a model
mod1 <- lm(plant.growth.rate ~ soil.moisture.content, data=dd)
names(mod1)

# diagnostic plots
par(mfrow = c(2,2)) # make grid of two rows and two colums
plot(mod1)
par(mfrow = c(1,1))

# OR diagnostics with ggfortify
autoplot(mod1, smooth.colour = NA)

# Look at the results
anova(mod1)
summary(mod1)

# ONLY with a single variable regression
ggplot(dd, aes(x = soil.moisture.content, y = plant.growth.rate)) + 
	geom_point(col = 'cornflowerblue', size = 3)+
	geom_smooth(method = 'lm')

#  lets make some new values of soil moisture 
# so we can generate predictions from the model
# note to self: you must use the explanatory variable NAME
# that's the one on the x-axis...
newX <- expand.grid(soil.moisture.content = 
	                seq(from = 0.2, to = 2, length = 100))
newX

# lets get the prediction (line) and the confidence 
# bands using predict()
newY <- predict(mod1, newdata = newX, interval = 'confidence')
newY

# housekeeping (brush the two data frames together)
addThese <- data.frame(newX, newY)
addThese

# make sure names match: change fit to plant.growth.rate
addThese <- rename(addThese, plant.growth.rate = fit)
addThese

# layers of fun
ggplot(dd, aes(x = soil.moisture.content, y = plant.growth.rate)) + 
	geom_point(size = 5, col = 'cornflowerblue')+
	geom_smooth(aes(ymin = lwr, ymax = upr), data = addThese, 
		stat = 'identity')


#----------------------------------------------------
# general linear models - two way ANOVA - growth.csv
#----------------------------------------------------
rm(list=ls())

dd <- read.csv("growth.csv")

# alter reference level NOW to control
dd <- mutate(dd, supplement = relevel(supplement, ref = 'control'))

# raw data
p1 <- ggplot(dd, aes(x = supplement, y = gain, colour = diet))+geom_point()

# the interaction plot via ggplot step 1
cows<-summarise(group_by(dd, diet, supplement),
	growth = mean(gain))

# Interaction Plot via ggplot step 2
# note group argument to make sure lines connect what you want connected

p2 <- ggplot(cows, 
	aes(x = supplement, y = growth, group = diet, colour = diet))+
	geom_point()+
	geom_line()

# both on the same picture (needs gridExtra)
grid.arrange(p1, p2, ncol = 2)

# BASE functions
with(dd, interaction.plot( supplement, diet, gain))

## and the model will have the control treatment as the intercept.
## Note that we use * to fit the main effects and the interaction term

mm <- lm(gain ~ supplement*diet, data = dd)
# mm <- lm(gain ~ supplement + diet + supplemenet:diet, data = dd)

## model diagnostics
par(mfrow=c(2, 2))
plot(mm)

## There is little evidence of an interaction, but strong main effects
anova(mm)

## Again, no clear interactions terms (there should not be,
## given the lack of a significant interaction term by F-test) 
summary(mm)
## The table looks rather scary, but is explained in the powerpoint presentation.

#----------------------------------------------------
# general linear models - ANCOVA
#----------------------------------------------------

# Get the data
rm(list=ls())
limp<-read.csv("limpet.csv")

str(limp)

# note use of scale_colour_manual
ggplot(limp, aes(x = DENSITY, y = EGGS, colour = SEASON)) + 
	geom_point(size = 5) + 
	scale_colour_manual(values = c('yellowgreen','salmon3'))+
	theme_bw()

# make the model
fit2 <- lm(EGGS~DENSITY*SEASON,data=limp) # linear model with interacti

# check the assumptions
par(mfrow=c(2,2))
plot(fit2)

# examine the results
anova(fit2) # ANOVA tables with sequential sums of squares
summary(fit2) # coefficients

#----------------------------------------------------#----------------------------------------------------
# STEP 1
# construct a data frame of where you want predicted values 
# to be "located" on your graph
# play around with this to see what happens when you 
# fix DENSITY to its mean, etc....
#----------------------------------------------------
newX<-expand.grid(
	DENSITY=seq(from = 8, to = 50, length = 10),
	SEASON = c("spring", "summer"))
newX

# Or you can use levels rather than typing these out
newX<-expand.grid(
	DENSITY=seq(from = 8, to = 50, length = 10),
	SEASON = levels(limp$SEASON))

# STEP 2
# get the predictions at the values and levels 
# specified above
newY<-predict(fit2, newdata=newX, interval = "confidence")
newY

# Step 3
# Housekeeping
PlotThis<-data.frame(newX, newY)
PlotThis <- rename(PlotThis, EGGS = fit)
PlotThis

# Step 4 + extras
# Now use geom_smooth() to make a lovely visualisation
# of your raw data and the fitted values from the model
# and the 95% CI band around them
ggplot(limp, aes(x = DENSITY, y = EGGS, colour = SEASON)) +
	geom_point(size = 5) +
	geom_smooth(data = PlotThis,
		aes(ymin = lwr, ymax = upr, fill = SEASON), 
		stat = 'identity') +

	# If you want to set custom colours
	# do so for the raw data (colour) and the bands (fill)
	scale_colour_manual(values = c('darkgreen','hotpink'))+
	scale_fill_manual(values = c('darkgreen','hotpink'))+
	
	# adding some annotation to a graph
	annotate("text", x = 40, y = 2.88, label = "Hello")+
	
	# using maths expressions on the x axis
	# see ?plotmath
	xlab(expression(paste(bar(x),phi)))+
	theme_bw()
#-----------------------------------------------
# Getting specific predictions
#-----------------------------------------------

newX<-expand.grid(
	DENSITY=40,
	SEASON = 'spring')
# get the predictions at the values and levels 
# specified above
newY<-predict(fit2, newdata=newX, interval = "confidence")
# housekeeping
data.frame(newX, newY)

