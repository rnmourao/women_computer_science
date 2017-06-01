#      file: script.R
# authors: Guilherme N. Ramos (gnramos@unb.br)
#          Roberto N. Mourão
#
# Data analysis for the surveys of the Meninas na Computação Project in the
# University of Brasília (http://meninas.cic.unb.br)


# Setup ########################################################################
library(arules)
library(arulesViz)
library(ggplot2)
library(gridExtra)
library(methods)

options(digits=2, device='pdf', max.print=99999) # Pretty printing

# Generate charts
plot.dir <- './'  # this could be something like 'img/'
save.plot <- function(plot.name, plot.width, plot.height) {
  plot.file <- paste0(plot.dir, plot.name, '.pdf')
  cat('\nGenerating', plot.file)
  ggsave(plot.file, width=plot.width, height=plot.height)
}

################################################################################
# Enrollment in UnB CS majors ##################################################
################################################################################
cat('\n------------------------------------------------------')
cat('\n                  Enrollment in UnB                   ')
cat('\n------------------------------------------------------')

enrollments <- read.csv('Data.UnB.CIC.Enrollment.csv')

cat('\n                   Total number of enrollments: |', nrow(enrollments))
enrolled.in.Computer.Science <- sum(enrollments$Computer.Science.Males + enrollments$Computer.Science.Females, na.rm=TRUE)
enrolled.in.Licentiate <- sum(enrollments$Licentiate.Males + enrollments$Licentiate.Females, na.rm=TRUE)
enrolled.in.Computer.Engineering <- sum(enrollments$Computer.Engineering.Males + enrollments$Computer.Engineering.Females, na.rm=TRUE)
cat('\n                  Enrolled in Computer Science: |', enrolled.in.Computer.Science)
cat('\n                        Enrolled in Licentiate: |', enrolled.in.Licentiate, '\n')
cat('\n              Enrolled in Computer Engineering: |', enrolled.in.Computer.Engineering)

x <- enrollments$Year
variable <-c(rep('Bachelor in CS', length(x)),
             rep('Licentiate in Computing', length(x)),
             rep('Computer Engineer', length(x)))
value <-  c(Computer.Science=enrollments$Computer.Science.Females * 100.00 / (enrollments$Computer.Science.Males + enrollments$Computer.Science.Females),
            Licentiate=enrollments$Licentiate.Females * 100.00 / (enrollments$Licentiate.Males + enrollments$Licentiate.Females),
            Computer.Engineering=enrollments$Computer.Engineering.Females * 100.00 / (enrollments$Computer.Engineering.Males + enrollments$Computer.Engineering.Females))
ratios <- data.frame(x, variable, value)

old.warning.settings <- getOption('warn') # Ignore plot warnings for NA values
options(warn=-1)
p <- ggplot(ratios, aes(x=x, y=value, group=variable, colour=variable)) +
     geom_line(size=1.5) +
     ylab('Female Ratio %') +
     xlab('Years') + #labs(fill='') + #
     scale_x_continuous(breaks=seq(1987, 2017, by=2)) +
     theme(legend.title=element_blank(), legend.position='bottom') +
     scale_fill_discrete(breaks=c('Bachelor in CS',
                                  'Licentiate in Computing',
                                  'Computer Engineer'))

plot.width <- 7
plot.height <- 4
save.plot('CS.Enrollment.In.UnB', plot.width, plot.height)
options(warn=old.warning.settings)  # Reset warnings
cat('\n\n')

################################################################################
# Perception of CS majors survey ###############################################
################################################################################

poll.answers <- read.csv('Data.Survey.csv')

# Preprocessing ################################################################
poll.answers$Q1 <- NULL
poll.answers$Q2 <- NULL

cat('\n------------------------------------------------------')
cat('\n                     Survey Data                      ')
cat('\n------------------------------------------------------')
cat('\n                   Total number of respondents: |', nrow(poll.answers))

# Girls only
poll.answers <- subset(poll.answers, poll.answers$Gender == 'F')
poll.answers$Gender <- NULL
cat('\n                               Number of girls: |', nrow(poll.answers))

# Middle and High Schools only
poll.answers <- subset(poll.answers,
                       poll.answers$Educational.Stage == 'Middle School' |
                       poll.answers$Educational.Stage == 'High School (10th Grade)' |
                       poll.answers$Educational.Stage == 'High School (11th Grade)' |
                       poll.answers$Educational.Stage == 'High School (12th Grade)')
cat('\n                  Middle and High School girls: |', nrow(poll.answers))

# Remove NA from Would.Enroll.In.CS
poll.answers <- subset(poll.answers, poll.answers$Would.Enroll.In.CS != '')
cat('\n Girls who answered if they would enroll in CS: |', nrow(poll.answers))

# Define attributes as factors
poll.answers <- as.data.frame(lapply(poll.answers, as.factor))
for (i in 1:ncol(poll.answers))
  if (length(levels(poll.answers[, i])) == 3 &
      names(poll.answers)[i] != 'Year' &
      names(poll.answers)[i] != 'Field.Of.Interest')
    poll.answers[, i] <- factor(poll.answers[, i],
                                levels=c('No', 'Maybe', 'Yes'))

# Sort columns (Would.Enroll.In.CS is first)
poll.answers <- poll.answers[, c('Would.Enroll.In.CS',
                                 setdiff(names(poll.answers),
                                         'Would.Enroll.In.CS'))]

# End preprocessing data #######################################################

num <- nrow(poll.answers[poll.answers$Would.Enroll.In.CS=='Yes',])
cat('\n                  Girls who would enroll in CS: |', num)
cat('\n------------------------------------------------------\n\n')

# Generate charts ##############################################################
save.plot <- function(plot.name, plot.width, plot.height) {
  plot.file <- paste0(plot.dir, plot.name, '.pdf')
  cat('\nGenerating', plot.file)
  ggsave(plot.file, width=plot.width, height=plot.height)
}

# Respondents per year
plot.width <- 7
plot.height <- 2
respondents.per.year <- aggregate(x=list(Total=poll.answers$Year),
                                  by=list(Year=poll.answers$Year),
                                  FUN=length)

p <- ggplot(data=respondents.per.year, aes(x='', y=Total, fill=Year)) +
     geom_bar(width=1, stat='identity') +
     coord_polar(theta='y', start=0) +
     scale_x_discrete() +
     xlab('') + ylab('') + labs(fill='')

save.plot('Respondents.Per.Year', plot.width, plot.height)

# Educational stage
educational.stage <- aggregate(x=list(total=poll.answers$Educational.Stage),
                               by=list(Year=poll.answers$Year,
                                       Educational.Stage=poll.answers$Educational.Stage),
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
     xlab('') + ylab('') + labs(fill='') +
     scale_fill_discrete(breaks=c('Middle School',
                                  'High School (10th Grade)',
                                  'High School (11th Grade)',
                                  'High School (12th Grade)'))

save.plot('Educational.Stage', plot.width, plot.height)

# Interest in undergraduate field of study
field.of.interest <- aggregate(x=list(NumRespondents=poll.answers$Field.Of.Interest),
                               by=list(Year=poll.answers$Year,
                                       FieldOfInterest=poll.answers$Field.Of.Interest),
                               FUN=length)
by.year <- aggregate(x=list(TotalRespondents=poll.answers$Field.Of.Interest),
                     by=list(Year=poll.answers$Year),
                     FUN=length)
field.of.interest <- merge(field.of.interest, by.year)
field.of.interest$Percentage <- (field.of.interest$NumRespondents * 100) / field.of.interest$TotalRespondents

p <- ggplot(data=field.of.interest, aes(x='', y=Percentage, fill=FieldOfInterest)) +
     geom_bar(width=1, stat='identity') +
     facet_grid(facets=. ~ Year) +
     coord_polar(theta='y', start=0) +
     scale_x_discrete() +
     xlab('') + ylab('') + labs(fill='')

save.plot('Field.Of.Interest', plot.width, plot.height)

# Interest in Computer Science
would.enroll.in.CS <- aggregate(x=list(Quantidade=poll.answers$Would.Enroll.In.CS),
                                by=list(Year=poll.answers$Year,
                                        Would.Enroll.In.CS=poll.answers$Would.Enroll.In.CS),
                                FUN=length)
by.year <- aggregate(x=list(Total=poll.answers$Would.Enroll.In.CS),
                     by=list(Year=poll.answers$Year),
                     FUN=length)
would.enroll.in.CS <- merge(would.enroll.in.CS, by.year)
would.enroll.in.CS$Percentage <- (would.enroll.in.CS$Quantidade * 100) / would.enroll.in.CS$Total

p <- ggplot(data=would.enroll.in.CS, aes(x='', y=Percentage, fill=Would.Enroll.In.CS)) +
     geom_bar(width=1, stat='identity') +
     facet_grid(facets=. ~ Year) +
     coord_polar(theta='y', start=0) +
     scale_x_discrete() +
     xlab('') + ylab('') + labs(fill='') +
     scale_fill_discrete(breaks=c('Yes', 'No', 'Maybe'))

save.plot('Would.Enroll.In.CS', plot.width, plot.height)

# Would.Enroll.In.CS X others variables
plot.width <- 5
plot.height <- 3

for (i in 2:ncol(poll.answers)) {
  attribute.name <- names(poll.answers)[i]

  temp <- aggregate(x=list(Quantity=poll.answers$Would.Enroll.In.CS),
                    by=list(Would.Enroll.In.CS=poll.answers$Would.Enroll.In.CS,
                            Treatment=poll.answers[, i]),
                    FUN=length)

  p <- ggplot(temp, aes(fill=Treatment, y=Quantity, x=Would.Enroll.In.CS)) +
       geom_bar(position='dodge', stat='identity') +
       ggtitle(attribute.name) +
       theme(plot.title=element_text(hjust=0.5))

  if (attribute.name == 'Educational.Stage')
    p <- p + scale_fill_discrete(name='', breaks=c('Middle School',
                                                   'High School (10th Grade)',
                                                   'High School (11th Grade)',
                                                   'High School (12th Grade)'))
  else
    p <- p + scale_fill_discrete(name='')

  plot.name <- paste0('Would.Enroll.In.CSx', attribute.name)
  save.plot(plot.name, plot.width, plot.height)
}

# Association Rule Mining ######################################################
cat('\n\n------------------------------------------------------\n')
CS.rules <- apriori(data=poll.answers,
                    parameter=list(confidence=0.5, maxtime=300, maxlen=3),
                    appearance=list(rhs=list('Would.Enroll.In.CS=Yes',
                                             'Would.Enroll.In.CS=Maybe',
                                             'Would.Enroll.In.CS=No'),
                                    default='lhs'))

sorted.CS.rules <- sort(CS.rules, by='lift')
# inspect(sorted.CS.rules)

# pdf(paste0(plot.dir, 'apriori.scatterplot.pdf'))
# plot(sorted.CS.rules)
# ignore <- dev.off()

CS.rules.df <- cbind(as(sorted.CS.rules, 'data.frame'))

CS.rules.df$lhs <- substr(CS.rules.df$rules, 1,
                          regexpr(' =>', as.character(CS.rules.df$rules),
                                  fixed=TRUE))
CS.rules.df$lhs <- gsub(',', ',\n', CS.rules.df$lhs)
CS.rules.df$rhs <- substr(CS.rules.df$rules,
                          regexpr('=>', as.character(CS.rules.df$rules),
                                  fixed=TRUE) + 3,
                          nchar(as.character(CS.rules.df$rules)))
CS.rules.df$rules <- NULL
CS.rules.df <- CS.rules.df[, c(4, 5, 1, 2, 3)]
CS.rules.df <- subset(CS.rules.df, CS.rules.df$lift >= 1.5) # greater than 50%

plot.width <- 8
plot.height <- 5.25  # size set manually by visual inspection
pdf(paste0(plot.dir, 'apriori.rule.table.pdf'),
    width=plot.width, height=plot.height)
grid.table(head(CS.rules.df, 10), rows=NULL)  # Top 10
ignore <- dev.off()

# Frequency tables for attributes ##############################################
frequency.table <- function(...) {
  columns <- list(...)
  column.names <- names(list(...))
  freq.table <- table(A=columns[[1]], B=columns[[2]])
  Total <- margin.table(freq.table, 1)
  freq.table <- cbind(freq.table, Total)
  Total <- margin.table(freq.table, 2)
  freq.table <- rbind(freq.table, Total)
  freq.table <- cbind(matrix(ncol=1, nrow=nrow(freq.table)), freq.table)
  freq.table <- rbind(matrix(nrow=1, ncol=ncol(freq.table)), freq.table)
  rownames(freq.table)[1] <- column.names[1]
  colnames(freq.table)[1] <- column.names[2]
  freq.table <- as.data.frame(freq.table)
  freq.table[is.na(freq.table)] <- ' '
  freq.table
}

save.grid.plot <- function(attribute.name, data.table, theme, plot.width, plot.height) {
  plot.file <- paste0(plot.dir, 'frequency.table.', attribute.name, '.pdf')
  cat('\nGenerating', plot.file)
  pdf(plot.file, width=plot.width, height=plot.height)
  grid.table(data.table, theme=theme)
  ignore <- dev.off()
}

for (i in 2:ncol(poll.answers)) {
  attribute.name <- names(poll.answers)[i]

  ft <- frequency.table(a=poll.answers[, i],
                        Would.Enroll.In.CS=poll.answers$Would.Enroll.In.CS)
  rownames(ft)[1] <- attribute.name

  # Column font theme
  theme <- ttheme_minimal(rowhead=list(fg_params=list(fontface=c(rep('bold',
                                                                     nrow(ft))))))

  plot.width <- 1 * ncol(ft) + .1 * max(nchar(row.names(ft)))
  plot.height <- 1 + nrow(ft) * .2
  save.grid.plot(attribute.name, ft, theme, plot.width, plot.height)
}

cat('\n\n------------------------------------------------------')
cat('\n                         Done                         ')
cat('\n------------------------------------------------------\n\n')
