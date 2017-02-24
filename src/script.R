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
# setwd('/home/mourao/Documentos/women_computer_science/src/')
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
educational.stage <- aggregate(x=list(total=poll.answers$Educational_Stage),
                               by=list(Year=poll.answers$Year, Educational_Stage=poll.answers$Educational_Stage),
                               FUN=length)
by.year <- aggregate(x=list(Total=poll.answers$Educational_Stage),
                     by=list(Year=poll.answers$Year),
                     FUN=length)

educational.stage <- merge(educational.stage, by.year)
educational.stage$Percentage <- (educational.stage$total * 100) / educational.stage$Total

p <- ggplot(data=educational.stage, aes(x='', y=Percentage, fill=Educational_Stage)) +
     geom_bar(width=1, stat='identity') +
     facet_grid(facets=. ~ Year) +
     coord_polar(theta='y', start=0) +
     scale_x_discrete() +
     xlab('') + ylab('') + labs(fill='') + # + labs(fill='Stage')
     # ggtitle('Respondents per Educational Stage') +
     scale_fill_discrete(breaks=c('Middle School', 'High School (10th Grade)',
                                  'High School (11th Grade)', 'High School (12th Grade)',
                                  'Adult Education Program', 'College'))
ggsave(paste0(plot.dir, 'EducationalStage.pdf'), width=7, height=2)

# Results per interest in undergraduate field of study #########################
field.of.interest <- aggregate(x=list(NumRespondents=poll.answers$Field_Interest),
                               by=list(Year=poll.answers$Year, FieldOfInterest=poll.answers$Field_Interest),
                               FUN=length)
by.year <- aggregate(x=list(TotalRespondents=poll.answers$Field_Interest),
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
interest.in.CS <- aggregate(x=list(Quantidade=poll.answers$CS_Interest),
                            by=list(Year=poll.answers$Year, CS_Interest=poll.answers$CS_Interest),
                            FUN=length)
by.year <- aggregate(x=list(Total=poll.answers$CS_Interest),
                     by=list(Year=poll.answers$Year),
                     FUN=length)

interest.in.CS <- merge(interest.in.CS, by.year)
interest.in.CS$Percentage <- (interest.in.CS$Quantidade * 100) / interest.in.CS$Total

p <- ggplot(data=interest.in.CS, aes(x='', y=Percentage, fill=CS_Interest)) +
     geom_bar(width=1, stat='identity') +
     facet_grid(facets=. ~ Year) +
     coord_polar(theta='y', start=0) +
     scale_x_discrete() +
     xlab('') + ylab('') + labs(fill='') + # labs(fill='Year')
     # ggtitle('Respondents Interested in CS')
     scale_fill_discrete(breaks=c('Yes', 'No', 'Maybe'))
ggsave(paste0(plot.dir, 'InterestInCS.pdf'), width=7, height=2)


# Variance analysis ############################################################

# For girls who intend to take a CS course
CS.choice <- data.frame(CS_Interest=levels(poll.answers$CS_Interest),
                        CS_Choice=0)
CS.choice$CS_Choice[CS.choice$CS_Interest == 'No'] <- -1
CS.choice$CS_Choice[CS.choice$CS_Interest == 'Maybe'] <- 0
CS.choice$CS_Choice[CS.choice$CS_Interest == 'Yes'] <- 1
poll.answers <- merge(poll.answers, CS.choice)
CS.choice.index <- match('CS_Choice', names(poll.answers))
CS.response.index <- match('CS_Interest', names(poll.answers))

year.index <- match('Year', names(poll.answers))

treatments <- data.frame(treatment=NULL, f.value=NULL, max.mean=NULL, assumptions=NULL)

# Analyze individual attributes
for (i in year.index:(CS.choice.index-1)) {  # CS.choice.index is the last one
  temp <- data.frame(Treatment=poll.answers[, i],
                     CS_Choice=poll.answers[, CS.choice.index],
                     CS_Interest=poll.answers[, CS.response.index])
  temp <- temp[!is.na(temp$Treatment),]
  attribute.name <- colnames(poll.answers)[i]
  
  f <- as.formula("CS_Choice ~ Treatment - 1")
  
  # unbalanced designs need a different library to analyze interactions  
  model <- lm(formula=f, data=temp, contrasts=list(Treatment = contr.sum))
  fit <- Anova(model, type="III")
  fit.summary <- summary(model)  
  mcs <- summary(glht(model, linfct = mcp(Treatment="Tukey")))
  tukey <- cld(mcs, level=0.05, decreasing=TRUE)
  
  ## checking anova assumptions
  
  # Are residuals normal? Since normality assumption is not a big issue when treatments are fixed
  # effects, we will test it using a rough test of Skewness and Kurtosis instead usual 
  # normality tests, e.g., Shapiro-Wilk and Kolmogorov-Smirnov.
  # using rule of thumb proposed by Bulmer, 1979.  
  symmetric <- (abs(skewness(model$residuals)) < 0.5)
  # using rule of thumb proposed by Vieira, 2006.
  not_leptokurtic <- kurtosis(model$residuals) <= 0
  
  # Are samples independent each other? Yes, because each observation belongs to one student.
  independent <- TRUE
  
  # Are variances equal?
  # using rule of thumb proposed by Dean and Voss - Design and analysis of experiments, 1999.
  v <- aggregate(x=list(variance=temp$CS_Choice), by=list(Treatment=temp$Treatment), FUN=var)
  if (max(v$variance, na.rm = TRUE) / min(v$variance, na.rm = TRUE) < 3) {
    homoscedastic <- TRUE
  } else {
    homoscedastic <- FALSE
  }
  
  # Only an indicator to ease the attribute selection. 
  # It is still necessary to check the residuals' charts.
  if (symmetric & not_leptokurtic & independent & homoscedastic) {
    a <- 'Granted'
  } else {
    a <- 'Not Granted'
  }
  
  # barplot to show differences between treatments
  temp2 <- aggregate(x=list(Quantity=temp$CS_Interest),
                     by=list(CS_Interest=temp$CS_Interest, Treatment=temp$Treatment),
                     FUN=length)
  p <- ggplot(temp2, aes(fill=Treatment, y=Quantity, x=CS_Interest)) +
       geom_bar(position="dodge", stat="identity") +
       scale_fill_discrete(name='Treatment')
  ggsave(paste0(plot.dir, 'plot_', attribute.name, '.pdf'), width=5, height=3)

  # anova table
  pdf(paste0(plot.dir, 'anova_', attribute.name, '.pdf'), height=1, width=8)
    grid.table(fit)
  ignore <- dev.off()

  # Residuals charts
  pdf(paste0(plot.dir, 'conditions_', attribute.name, '.pdf'))
    residualPlots(model, layout= c(2, 1))
    qqPlot(model)
  ignore <- dev.off()

  # height and width adjustments based on trial and error
  h <- 1.5 + 0.1 * length(unique(temp$Treatment))
  w <- 1.5 + 0.1 * max(nchar(as.character(levels(temp$Treatment))))

  # tukey-kramer post hoc test
  pdf(paste0(plot.dir, 'tukey_', attribute.name, '.pdf'), height=h, width=w)
    tukey.matrix <- as.matrix(unlist(tukey$mcletters$Letters))
    tukey.df <- data.frame(trt=row.names(tukey.matrix), M=tukey.matrix[,1])
    Mean <- aggregate(x=list(means=temp$CS_Choice), by=list(trt=temp$Treatment), FUN=mean)
    tukey.df <- merge(tukey.df, Mean)
    tukey.df <- tukey.df[, c(1, 3, 2)]
    tukey.df <- tukey.df[order(tukey.df$M),]
    grid.table(tukey.df, rows=NULL)
  ignore <- dev.off()

  # find the greatest mean value for CS_Choice which has significance.
  # initialization: max.mean begins with lower possible value for mean of CS_Choice attribute
  max.mean <- -1
  for (j in 1:nrow(tukey.df)) {
    if (tukey.df$means[j] > max.mean) {
      if (str_count(paste(tukey.df$M, collapse=''), 
                    as.character(tukey.df$M[j])) == 1) {
        max.mean <- tukey.df$means[j]
      }
    }
  }

  df <-  data.frame(treatment=attribute.name,
                    f.value=fit$`F value`[1],
                    max.mean=max.mean,
                    assumptions=a)
  treatments <- rbind(treatments, df)
}

## Attribute Selection using Factorial Analysis

# Removing not-significant attributes using a top-down approach
not_significant <- c(CS.response.index, year.index)
temp <- poll.answers[, -not_significant]
gotcha <- FALSE
while(!gotcha | ncol(temp) <= 1) {
  fit <- aov(CS_Choice ~ ., data=temp)

  # Creating a data frame for p-values
  values <- as.data.frame(summary(fit)[[1]][4:5])
  names(values)[1] <- 'f'
  names(values)[2] <- 'p'
  values$attribute <- trimws(rownames(values))
  rownames(values) <- seq(1, nrow(values))
  values <- values[!is.na(values$p),]
  values <- values[order(values$p, decreasing=TRUE),]

  # remove attribute with greater p-value
  if (values$p[1] >= 0.05) {
    index <- match(values$attribute[1], names(poll.answers))
    not_significant <- c(not_significant, index)
    temp <- poll.answers[, -not_significant]
  } else {
    print(summary(fit))
    break
  }
}

### Test interactions
combinations <- t(combn(1:(ncol(temp) - 1), 2))
for (i in 1:nrow(combinations)) {
  CS.choice.index <- match('CS_Choice', names(temp))
  temp2 <- temp[, c(combinations[i, 1], combinations[i, 2], CS.choice.index)]
  trtA.name <- names(temp)[combinations[i, 1]]
  trtB.name <- names(temp)[combinations[i, 2]]
  names(temp2) <- c('A', 'B', 'CS_Choice')
  
  trt <- paste0(names(temp2)[1], '_x_', names(temp2)[2])
  
  f <- as.formula("CS_Choice ~ A * B")

  # unbalanced designs need a different library to analyze interactions  
  model <- lm(formula=f, data=temp2, contrasts=list(A = contr.sum, B = contr.sum))
  fit <- Anova(model, type="III")
  fit.summary <- summary(model)
  
  ## checking anova assumptions
  
  # Are residuals normal? Since normality assumption is not a big issue when treatments are fixed
  # effects, we will test it using a rough test of Skewness and Kurtosis instead usual 
  # normality tests, e.g., Shapiro-Wilk and Kolmogorov-Smirnov.
  # using rule of thumb proposed by Bulmer, 1979.  
  symmetric <- (abs(skewness(model$residuals)) < 0.5)
  # using rule of thumb proposed by Vieira, 2006.
  not_leptokurtic <- kurtosis(model$residuals) <= 0
  
  # Are samples independent each other? Yes, because each observation belongs to one student.
  independent <- TRUE
  
  # Are variances equal?
  # using rule of thumb proposed by Dean and Voss - Design and analysis of experiments, 1999.
  CS.choice.index <- match('CS_Choice', names(temp2))
  trtA.index <- match('A', names(temp2))
  trtB.index <- match('B', names(temp2))
  v <- aggregate(x=list(variance=temp2[, CS.choice.index]), 
                 by=list(A=temp2[, trtA.index], B=temp2[, trtB.index]), FUN=var)
  if (max(v$variance, na.rm = TRUE) / min(v$variance, na.rm = TRUE) < 3) {
    homoscedastic <- TRUE
  } else {
    homoscedastic <- FALSE
  }
  
  if (symmetric & not_leptokurtic & independent & homoscedastic) {
    a <- 'Granted'
  } else {
    a <- 'Not Granted'
  }
  
  # temp2$interact <- with(temp2, interaction(temp2$A, temp2$B))
  # cell <- lm('CS_Choice ~ interact - 1', data = temp2)
  mcs <- summary(glht(model, linfct = mcp(A="Tukey")))
  cld(mcs,
      level=0.05,
      decreasing=TRUE)
  
  # tukey <- HSD.test(model, trt)

  # width adjustments based on trial and error
  w <- 4 + 2.6 *(nchar(trt) + nchar(as.character(summary(fit)[[1]][['Pr(>F)']][1])))/ 20
  pdf(paste0(plot.dir, 'anova_', trt, '.pdf'), height=1, width=w)
    grid.table(anova(fit))
  ignore <- dev.off()

  pdf(paste0(plot.dir, 'conditions_', trt, '.pdf'))
    par(mfrow=c(2,2))
    plot(fit)
  ignore <- dev.off()  
  
  pdf(paste0(plot.dir, 'interactions_', trt, '.pdf'))
    interaction.plot(temp2[, 1], temp2[, 2], temp2[, 3], type='b')
  ignore <- dev.off()

  # height and width adjustments based on trial and error
  h <- 1.5 + 0.3 * length(levels(temp2[, 4]))
  w <- 1.5 + 0.1 * max(nchar(as.character(levels(temp2[, 4]))))
  pdf(paste0(plot.dir, 'tukey_', trt, '.pdf'), height=h, width=w)
    grid.table(HSD.test(fit, trt)$groups, rows=NULL)
  ignore <- dev.off()

  groups <- tukey$groups
  means <- tukey$means
  sum.means <- sum(means$r)
  
  # initialization: max.mean begins with lower possible value for mean of CS_Choice attribute
  max.mean <- -1
  for (j in 1:nrow(groups)) {
    if (groups$means[j] > max.mean) {
      if (str_count(paste(groups$M, collapse=''), as.character(groups$M[j])) == 1) {
        max.mean <- groups$means[j]
      }
    }
  }

  df <- data.frame(treatment=trt,
                   f.value=fit.summary[[1]][['F value']][[1]],
                   max.mean=max.mean,
                   assumptions=a)
  treatments <- rbind(treatments, df)
}

treatments <- treatments[order(treatments$max.mean, decreasing=TRUE),]
write.table(treatments, '../data/anova.csv', row.names=FALSE, dec=',', sep=';')
