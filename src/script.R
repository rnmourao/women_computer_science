# file: script.R
# authors: Guilherme N. Ramos (gnramos@unb.br)
#          Roberto N. Mourão
#
# Script for analysis of data for the Meninas na Computação Project in
# the University of Brasília (http://meninas.cic.unb.br)

# Dependencies
library(agricolae)
library(car)
library(ggplot2)
library(gridExtra)
library(multcomp)
library(readxl)
library(stringr)

# Pretty printing
options(digits=2, device='pdf', max.print=99999)
cat('\n')

# Cleaning up workspace
rm(list=ls())

#### Set working directory
# setwd('/home/f8676628/Documentos/women_computer_science/src/')
plot.dir <- '../dexa/img/'

# Get raw data
poll.answers <- read_excel('../data/raw.xlsx', sheet='answers', na='')

# Cleanup data
poll.answers$Q1 <- NULL
poll.answers$Q2 <- NULL

cat('Number of respondents: ', nrow(poll.answers), '\n\n')

# Girls only
poll.answers <- subset(poll.answers, poll.answers$Gender == 'F')
poll.answers$Gender <- NULL

# Remove 2011's questionnaires
poll.answers <- subset(poll.answers, poll.answers$Year != 2011)

# Only Middle and High Schools
poll.answers <- subset(poll.answers, 
                       poll.answers$Educational.Stage != 'College' & 
                         poll.answers$Educational.Stage != 'Adult Education Program')

# Processing
poll.answers <- as.data.frame(lapply(poll.answers, as.factor))

cat('Female respondents: ', nrow(poll.answers), '\n\n')

# Results per year #############################################################
respondents.per.year <- aggregate(x=list(Total=poll.answers$Year),
                                  by=list(Year=poll.answers$Year),
                                  FUN=length)

p <- ggplot(data=respondents.per.year, aes(x='', y=Total, fill=Year)) +
     geom_bar(width=1, stat='identity') +
     coord_polar(theta='y', start=0) +
     scale_x_discrete() +
     xlab('') + ylab('') + labs(fill='') + # + labs(fill='Stage')
     # ggtitle('Respondents per Year') +
ggsave(paste0(plot.dir, 'RespondentsPerYear.pdf'), width=7, height=2)


# Results per educational stage ################################################
educational.stage <- aggregate(x=list(total=poll.answers$Educational.Stage),
                               by=list(Year=poll.answers$Year, Educational.Stage=poll.answers$Educational.Stage),
                               FUN=length)
by.year <- aggregate(x=list(Total=poll.answers$Educational.Stage),
                     by=list(Year=poll.answers$Year),
                     FUN=length)

educational.stage <- merge(educational.stage, by.year)
educational.stage$Percentage <- (educational.stage$total * 100) / educational.stage$Total

p <- ggplot(data=educational.stage, aes(x='', y=Percentage, fill=Educational.Stage)) +
     geom_bar(width=1, stat='identity') +
     facet_grid(facets=. ~ Year) +
     coord_polar(theta='y', start=0) +
     scale_x_discrete() +
     xlab('') + ylab('') + labs(fill='') + # + labs(fill='Stage')
     # ggtitle('Respondents per Educational Stage') +
     scale_fill_discrete(breaks=c('Middle School', 'High School (10th Grade)',
                                  'High School (11th Grade)', 'High School (12th Grade)'))
ggsave(paste0(plot.dir, 'EducationalStage.pdf'), width=7, height=2)

# Results per interest in undergraduate field of study #########################
field.of.interest <- aggregate(x=list(NumRespondents=poll.answers$Field.Interest),
                               by=list(Year=poll.answers$Year, FieldOfInterest=poll.answers$Field.Interest),
                               FUN=length)
by.year <- aggregate(x=list(TotalRespondents=poll.answers$Field.Interest),
                     by=list(Year=poll.answers$Year),
                     FUN=length)

field.of.interest <- merge(field.of.interest, by.year)
field.of.interest$Percentage <- (field.of.interest$NumRespondents * 100) / field.of.interest$TotalRespondents

p <- ggplot(data=field.of.interest, aes(x='', y=Percentage, fill=FieldOfInterest)) +
     geom_bar(width=1, stat='identity') +
     facet_grid(facets=. ~ Year) +
     coord_polar(theta='y', start=0) +
     scale_x_discrete() +
     xlab('') + ylab('') + labs(fill='') + # + labs(fill='Field')
     # ggtitle('Respondents per Field of Interest')
ggsave(paste0(plot.dir, 'FieldOfInterest.pdf'), width=7, height=2)

cat('All respondents:\n')
aggregate(x=list(Percentage=field.of.interest$Percentage),
          by=list(FieldOfInterest=field.of.interest$FieldOfInterest),
          FUN=mean)
cat('\n')

# Results for interest in Computer Science #####################################
interest.in.CS <- aggregate(x=list(Quantidade=poll.answers$CS.Interest),
                            by=list(Year=poll.answers$Year, CS.Interest=poll.answers$CS.Interest),
                            FUN=length)
by.year <- aggregate(x=list(Total=poll.answers$CS.Interest),
                     by=list(Year=poll.answers$Year),
                     FUN=length)

interest.in.CS <- merge(interest.in.CS, by.year)
interest.in.CS$Percentage <- (interest.in.CS$Quantidade * 100) / interest.in.CS$Total

p <- ggplot(data=interest.in.CS, aes(x='', y=Percentage, fill=CS.Interest)) +
     geom_bar(width=1, stat='identity') +
     facet_grid(facets=. ~ Year) +
     coord_polar(theta='y', start=0) +
     scale_x_discrete() +
     xlab('') + ylab('') + labs(fill='') + # labs(fill='Year')
     # ggtitle('Respondents Interested in CS')
     scale_fill_discrete(breaks=c('Yes', 'No', 'Maybe'))
ggsave(paste0(plot.dir, 'InterestInCS.pdf'), width=7, height=2)


# Respondents By CS.Interest
aggregate(x=list(Quantity=poll.answers$CS.Interest), 
          by=list(CS.Interest=poll.answers$CS.Interest), 
          FUN=length)

# Variance analysis ############################################################

# For girls who intend to take a CS course
CS.choice <- data.frame(CS.Interest=levels(poll.answers$CS.Interest),
                        CS.Choice=0)
CS.choice$CS.Choice[CS.choice$CS.Interest == 'No'] <- -1
CS.choice$CS.Choice[CS.choice$CS.Interest == 'Maybe'] <- 0
CS.choice$CS.Choice[CS.choice$CS.Interest == 'Yes'] <- 1
poll.answers <- merge(CS.choice, poll.answers)
CS.choice.index <- match('CS.Choice', names(poll.answers))
CS.interest.index <- match('CS.Interest', names(poll.answers))
year.index <- match('Year', names(poll.answers))

# Grand Mean
mean(poll.answers$CS.Choice)

# a table to organize treatments by its means
treatments <- data.frame(Attribute=NULL, F.Value=NULL, Assumptions=NULL, Max.Mean=NULL)

## Attribute Selection using Factorial Analysis
# Removing not-significant attributes using a top-down approach
not.significant <- c(CS.interest.index)
temp <- poll.answers[, -not.significant]
gotcha <- FALSE
while(!gotcha | ncol(temp) <= 1) {
  # checking which treatments are significants
  model <- lm(formula='CS.Choice ~ . - 1', data=temp, contrasts=contr.sum)
  
  # Type II Anova analyzes treatments ignoring interacions
  fit <- Anova(model, type="II")
  
  # Creating a data frame for p-values
  values <- as.data.frame(fit)
  values$Attribute <- trimws(rownames(values))
  rownames(values) <- seq(1, nrow(values))
  values <- values[!is.na(values$`Pr(>F)`),]
  values <- values[order(values$`Pr(>F)`, decreasing=TRUE),]
  
  # remove attribute with greater p-value
  if (values$`Pr(>F)`[1] >= 0.05) {
    index <- match(values$Attribute[1], names(poll.answers))
    not.significant <- c(not.significant, index)
    temp <- poll.answers[, -not.significant]
  } else {
    break
  }
}

# turning back with CS.Interest for additional analysis
not.significant <- setdiff(not.significant, CS.interest.index)

# Analyze individual attributes
poll.answers <- poll.answers[, -not.significant]
CS.choice.index <- match('CS.Choice', names(poll.answers))
for (i in (CS.choice.index+1):ncol(poll.answers)) {  
  temp <- data.frame(Treatment=poll.answers[, i],
                     CS.Choice=poll.answers[, CS.choice.index],
                     CS.Interest=poll.answers[, CS.interest.index])
  
  attribute.name <- colnames(poll.answers)[i]
  names(temp)[1] <- attribute.name
  
  f <- as.formula(paste("CS.Choice ~", attribute.name, "- 1"))
  
  # unbalanced designs need a different library
  lst <- list(A='contr.sum')
  names(lst) <- c(attribute.name)
  model <- lm(formula=f, data=temp, contrasts=lst)
  fit <- Anova(model, type="III")
  fit.summary <- summary(model)  
  
  ## checking anova assumptions
  
  # Are residuals normal? Since normality assumption is not a big issue when treatments are fixed
  # effects, we will test it using a rough test of Skewness and Kurtosis instead usual 
  # normality tests, e.g., Shapiro-Wilk and Kolmogorov-Smirnov.
  # using rule of thumb proposed by Bulmer, 1979.  
  symmetric <- (abs(skewness(model$residuals)) < 0.5)
  # using rule of thumb proposed by Vieira, 2006.
  not.leptokurtic <- kurtosis(model$residuals) <= 0
  
  # Are samples independent each other? Yes, because each observation belongs to one student.
  independent <- TRUE
  
  # Are variances equal?
  # using rule of thumb proposed by Dean and Voss - Design and analysis of experiments, 1999.
  v <- aggregate(x=list(variance=temp$CS.Choice), by=list(Treatment=temp[, 1]), FUN=var)
  if (max(v$variance, na.rm = TRUE) / min(v$variance, na.rm = TRUE) < 3) {
    homoscedastic <- TRUE
  } else {
    homoscedastic <- FALSE
  }
  
  # Only an indicator to ease the attribute selection. 
  # It is still necessary to check the residuals' charts.
  if (symmetric & not.leptokurtic & independent & homoscedastic) {
    a <- 'Granted'
  } else {
    a <- 'Not Granted'
  }

  # tukey-kramer post hoc test
  o <- mcp(Treatment="Tukey")
  attributes(o) <- list(names=attribute.name, 
                        interaction_average=FALSE, 
                        covariate_average=FALSE, 
                        class="mcp")
  mcs <- summary(glht(model, linfct=o))
  tukey <- cld(mcs, level=0.05, decreasing=TRUE)
  tukey.matrix <- as.matrix(unlist(tukey$mcletters$Letters))
  tukey.df <- data.frame(trt=row.names(tukey.matrix), M=tukey.matrix[,1])
  Mean <- aggregate(x=list(means=temp$CS.Choice), by=list(trt=temp[, 1]), FUN=mean)
  SD <- aggregate(x=list(SD=temp$CS.Choice), by=list(trt=temp[, 1]), FUN=sd)
  tukey.df <- merge(tukey.df, Mean)
  tukey.df <- merge(tukey.df, SD)
  tukey.df <- tukey.df[, c(1, 3, 4, 2)]
  tukey.df <- tukey.df[order(tukey.df$M),]
  names(tukey.df) <- c(attribute.name, "Means", "Std.Dev", "Tukey")

  # find the greatest mean value for CS.Choice which has significance.
  # initialization: max.mean begins with lower possible value for mean of CS.Choice attribute
  max.mean <- -1
  for (j in 1:nrow(tukey.df)) {
    if (tukey.df$Means[j] > max.mean) {
      if (str_count(as.character(tukey.df$Tukey[j])) == 1 & 
          str_count(paste(tukey.df$Tukey, collapse=''), 
                    as.character(tukey.df$Tukey[j])) == 1) {
        max.mean <- tukey.df$Means[j]
        trt <- tukey.df[j, 1]
      }
    }
  }
  
  if (max.mean != -1) {
    df <-  data.frame(Attribute=attribute.name,
                      F.Value=fit$`F value`[1],
                      Assumptions=a,
                      Max.Mean=max.mean)
    treatments <- rbind(treatments, df)
  }
  
  ## Charts
  # barplot to show differences between treatments
  temp2 <- aggregate(x=list(Quantity=temp$CS.Interest),
                     by=list(CS.Interest=temp$CS.Interest, Treatment=temp[, 1]),
                     FUN=length)
  p <- ggplot(temp2, aes(fill=Treatment, y=Quantity, x=CS.Interest)) +
    geom_bar(position="dodge", stat="identity") +
    scale_fill_discrete(name=attribute.name)
  ggsave(paste0(plot.dir, 'plot.', attribute.name, '.pdf'), width=5, height=3)
  
  # Residuals charts
  pdf(paste0(plot.dir, 'conditions.', attribute.name, '.pdf'))
    # A warning here about the exclusion of the lack of fit chart. 
    # This chart is not essencial to perform residuals analysis.
    options(warn=-1)
      residualPlots(model, layout= c(2, 1))
    options(warn=0)
    qqPlot(model)
  ignore <- dev.off()
  
  # anova table
  pdf(paste0(plot.dir, 'anova.', attribute.name, '.pdf'), height=1, width=8)
    grid.table(fit)
  ignore <- dev.off()
  
  # height and width adjustments based on trial and error
  h <- 1.5 + 0.1 * length(unique(temp$Treatment))
  w <- 1.0 + 0.1 * sum(nchar(paste0(names(tukey.df))))
  pdf(paste0(plot.dir, 'tukey.', attribute.name, '.pdf'), height=h, width=w)
    grid.table(tukey.df, rows=NULL)
  ignore <- dev.off()
}

# Sort significant treatments by F value. 
treatments <- treatments[order(treatments$Max.Mean, decreasing=TRUE),]
pdf(paste0(plot.dir, 'significant.pdf'), height = 5, width = 6)
  grid.table(treatments, rows=NULL)
ignore <- dev.off()


################################ Test interactions #############################
# Select the three attributes which has treatments with higher mean.
interactions <- as.character(treatments$Attribute[1:3]) 

# create a table with the three selected attributes, CS.Interest and CS.Choice
temp <- poll.answers[, c("CS.Interest", "CS.Choice", interactions)]

# create combinations 
combinations <- t(combn(names(temp)[3:5], 2))
for (i in 1:nrow(combinations)) {
  temp2 <- temp[, c("CS.Interest", "CS.Choice", combinations[i, 1], combinations[i, 2])]
  interaction.name <- paste0(combinations[i, 1], '.x.', combinations[i, 2])
  ## construct a new model to test only the interaction
  A.index <- match(combinations[i, 1], names(temp2))
  B.index <- match(combinations[i, 2], names(temp2))
  temp2$I <- interaction(temp2[, A.index], temp2[, B.index], drop = TRUE, sep = ".x.")

  f <- as.formula(paste("CS.Choice ~",  combinations[i, 1], "*", combinations[i, 2], "- 1"))

  # unbalanced designs need a different library to analyze interactions  
  lst <- list(A="contr.sum", B="contr.sum")
  names(lst) <- c(combinations[i, 1], combinations[i, 2])
  model <- lm(formula=f, data=temp2, contrasts=lst)
  fit <- Anova(model, type="III")
  fit.summary <- summary(model)
  
  ## Charts
  # barplot to show differences between treatments
  temp3 <- aggregate(x=list(Quantity=temp2$CS.Interest),
                     by=list(CS.Interest=temp2$CS.Interest, Interaction=temp2$I),
                     FUN=length)
  p <- ggplot(temp3, aes(fill=Interaction, y=Quantity, x=CS.Interest)) +
    geom_bar(position="dodge", stat="identity") +
    scale_fill_discrete(name=interaction.name)
  ggsave(paste0(plot.dir, 'plot.', interaction.name, '.pdf'), width=8, height=5)
  
  # anova table main effects + interaction
  pdf(paste0(plot.dir, 'anova.complete.', interaction.name, '.pdf'), height=2, width=10)
    grid.table(fit)
  ignore <- dev.off()
  
  # interaction chart
  pdf(paste0(plot.dir, 'interactions.', interaction.name, '.pdf'))
    interaction.plot(x.factor=temp2[, A.index], 
                     trace.factor=temp2[, B.index], 
                     response=temp2$CS.Choice, 
                     type='b',
                     xlab=combinations[i, 1],
                     ylab="CS.Choice",
                     trace.label=combinations[i, 2]) 
  ignore <- dev.off()
  
  ##### Post hoc test for interactions
  # Creating a model to test only the interaction
  model <- lm(formula='CS.Choice ~ I - 1', data=temp2, contrasts=contr.sum)
  fit <- Anova(model, type="III")
  fit.summary <- summary(model)
  
  # tukey-kramer post hoc test
  mcs <- summary(glht(model, linfct = mcp(I="Tukey")))
  tukey <- cld(mcs, level=0.05, decreasing=TRUE)
  tukey.matrix <- as.matrix(unlist(tukey$mcletters$Letters))
  tukey.df <- data.frame(trt=row.names(tukey.matrix), M=tukey.matrix[,1])
  Mean <- aggregate(x=list(means=temp2$CS.Choice), by=list(trt=temp2$I), FUN=mean)
  SD <- aggregate(x=list(SD=temp2$CS.Choice), by=list(trt=temp2$I), FUN=sd)
  tukey.df <- merge(tukey.df, Mean)
  tukey.df <- merge(tukey.df, SD)
  tukey.df <- tukey.df[, c(1, 3, 4, 2)]
  tukey.df <- tukey.df[order(tukey.df$M),]
  names(tukey.df) <- c(interaction.name, "Means", "Std.Dev", "Tukey")
  
  ##  Charts
  # Residuals charts
  pdf(paste0(plot.dir, 'conditions.', interaction.name, '.pdf'))
    # A warning here about the exclusion of the lack of fit chart. 
    # This chart is not essencial to perform residuals analysis.
    options(warn=-1)
      residualPlots(model, layout= c(2, 1))
    options(warn=-1)
    qqPlot(model)
  ignore <- dev.off()  
  
  # anova table
  pdf(paste0(plot.dir, 'anova.', interaction.name, '.pdf'), height=2, width=10)
    grid.table(fit)
  ignore <- dev.off()
  
  # height and width adjustments based on trial and error
  h <- 2.5 + 0.1 * length(unique(temp2$I))
  w <- 1.0 + 0.1 * sum(nchar(paste0(names(tukey.df))))
  pdf(paste0(plot.dir, 'tukey.', interaction.name, '.pdf'), height=h, width=w)
    grid.table(tukey.df, rows=NULL)
  ignore <- dev.off()
}
