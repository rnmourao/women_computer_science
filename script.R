# file: script.R
# authors: Guilherme N. Ramos (gnramos@unb.br)
#          Roberto Mourão
#
# Script for analysis of data for the Meninas na Computação Project in
# the University of Brasília (http://meninas.cic.unb.br)

# Dependencies
for (pkg in c('agricolae', 'ggplot2', 'knitr', 'readxl'))
  if (!require(pkg, character.only=TRUE))
    ignore <- install.packages(pkg)

# Pretty printing
options('scipen'=100, 'digits'=2, device='pdf')
cat('\n')

poll.answers <- read_excel('raw.xlsx', sheet='unificado', na='')

# Cleanup data
poll.answers$Q1 <- NULL
poll.answers$Q2 <- NULL
poll.answers$Serie <- stringr::str_trim(poll.answers$Serie, side='both')

cat('Number of respondents: ', nrow(poll.answers), '\n\n')

# Girls only
poll.answers <- subset(poll.answers, poll.answers$Sexo == 'F')
poll.answers$Sexo <- NULL

# Translation
poll.answers$Serie[poll.answers$Serie=='Ensino Fundamental'] <- 'Middle School'
poll.answers$Serie[poll.answers$Serie=='Primeiro Ano'] <- 'High School (10th Grade)'
poll.answers$Serie[poll.answers$Serie=='Segundo Ano'] <- 'High School (11th Grade)'
poll.answers$Serie[poll.answers$Serie=='Terceiro Ano'] <- 'High School (12th Grade)'
poll.answers$Serie[poll.answers$Serie=='Supletivo'] <- 'Adult Education Program'
poll.answers$Serie[poll.answers$Serie=='Ensino Superior'] <- 'College'
poll.answers$Fara_Curso_Superior[poll.answers$Fara_Curso_Superior=='Biologicas e Saude'] <- 'Biology-Health Sciences'
poll.answers$Fara_Curso_Superior[poll.answers$Fara_Curso_Superior=='Humanas'] <- 'Human Sciences'
poll.answers$Fara_Curso_Superior[poll.answers$Fara_Curso_Superior=='Exatas'] <- 'Exact Sciences'
poll.answers$Fara_Computacao[poll.answers$Fara_Computacao=='Sim'] <- 'Yes'
poll.answers$Fara_Computacao[poll.answers$Fara_Computacao=='Nao'] <- 'No'
poll.answers$Fara_Computacao[poll.answers$Fara_Computacao=='Nao sei ainda'] <- 'Maybe'

# Processing
poll.answers <- as.data.frame(lapply(poll.answers, as.factor))

cat('Female respondents: ', nrow(poll.answers), '\n\n')

# Results per year #############################################################
respondents.per.year <- aggregate(x=list(Total=poll.answers$Ano),
                               by=list(Year=poll.answers$Ano),
                               FUN=length)

p <- ggplot(data=respondents.per.year, aes(x='', y=Total, fill=Year)) +
     geom_bar(width=1, stat='identity') +
     coord_polar(theta='y', start=0) +
     scale_x_discrete() +
     xlab('') + ylab('') + labs(fill='') + # + labs(fill='Stage')
     # ggtitle('Respondents per Year') +
ggsave('dexa/img/RespondentsPerYear.pdf', width=7, height=2)


# Results per educational stage ################################################
educational.stage <- aggregate(x=list(total=poll.answers$Serie),
                               by=list(Year=poll.answers$Ano, Serie=poll.answers$Serie),
                               FUN=length)
by.year <- aggregate(x=list(Total=poll.answers$Serie),
                     by=list(Year=poll.answers$Ano),
                     FUN=length)

educational.stage <- merge(educational.stage, by.year)
educational.stage$Percentage <- (educational.stage$total * 100) / educational.stage$Total

p <- ggplot(data=educational.stage, aes(x='', y=Percentage, fill=Serie)) +
     geom_bar(width=1, stat='identity') +
     facet_grid(facets=. ~ Year) +
     coord_polar(theta='y', start=0) +
     scale_x_discrete() +
     xlab('') + ylab('') + labs(fill='') + # + labs(fill='Stage')
     # ggtitle('Respondents per Educational Stage') +
     scale_fill_discrete(breaks=c('Middle School', 'High School (10th Grade)', 'High School (11th Grade)', 'High School (12th Grade)', 'Adult Education Program', 'College'))
ggsave('dexa/img/EducationalStage.pdf', width=7, height=2)


# Results per interest in undergraduate field of study #########################
field.of.interest <- aggregate(x=list(NumRespondents=poll.answers$Fara_Curso_Superior),
                               by=list(Year=poll.answers$Ano, FieldOfInterest=poll.answers$Fara_Curso_Superior),
                               FUN=length)
by.year <- aggregate(x=list(TotalRespondents=poll.answers$Fara_Curso_Superior),
                     by=list(Year=poll.answers$Ano),
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
ggsave('dexa/img/FieldOfInterest.pdf', width=7, height=2)

cat('All respondents:\n')
aggregate(x=list(Percentage=field.of.interest$Percentage),
          by=list(FieldOfInterest=field.of.interest$FieldOfInterest),
          FUN=mean)
cat('\n')

# Results for interest in Computer Science #####################################
interest.in.CS <- aggregate(x=list(Quantidade=poll.answers$Fara_Computacao),
                            by=list(Year=poll.answers$Ano, Fara_Computacao=poll.answers$Fara_Computacao),
                            FUN=length)
by.year <- aggregate(x=list(Total=poll.answers$Fara_Computacao),
                     by=list(Year=poll.answers$Ano),
                     FUN=length)

interest.in.CS <- merge(interest.in.CS, by.year)
interest.in.CS$Percentage <- (interest.in.CS$Quantidade * 100) / interest.in.CS$Total

p <- ggplot(data=interest.in.CS, aes(x='', y=Percentage, fill=Fara_Computacao)) +
     geom_bar(width=1, stat='identity') +
     facet_grid(facets=. ~ Year) +
     coord_polar(theta='y', start=0) +
     scale_x_discrete() +
     xlab('') + ylab('') + labs(fill='') + # labs(fill='Ano')
     # ggtitle('Respondents Interested in CS')
ggsave('dexa/img/InterestInCS.pdf', width=7, height=2)

# Variance analysis ############################################################

# For girls who chose to
CS.choice <- data.frame(Fara_Computacao=levels(poll.answers$Fara_Computacao),
                        CS_Choice=c(1, 2, 3))
poll.answers <- merge(poll.answers, CS.choice)
CS.choice.index <- match('CS_Choice', names(poll.answers))

year.index <- match('Ano', names(poll.answers))
temp <- data.frame(Tratamento=poll.answers[, year.index],
                   CS_Choice=poll.answers[, CS.choice.index])

fit <- aov(CS_Choice ~ Tratamento, data=temp)
knit_print(anova(fit))
cat('\n')

knit_print(HSD.test(fit, 'Tratamento'))

par(mfrow=c(2,2))
pdf('dexa/img/ANOVA.pdf')
plot(fit)
ignore <- dev.off()

# Other attributes
pdf('dexa/img/Variance.pdf')
for (i in (year.index + 1):(CS.choice.index-1)) {  # CS.choice.index is the last one
  temp <- data.frame(Tratamento=poll.answers[, i], CS_Choice=poll.answers[, CS.choice.index])

  nome <- colnames(poll.answers)[i]
  cat(paste('\nTratamento:', nome, '\n'))
  cat('\n')

  fit <- aov(CS_Choice ~ Tratamento, data=temp)
  knit_print(anova(fit))
  cat('\n')

  knit_print(HSD.test(fit, 'Tratamento'))

  # assign(nome, temp)

  par(mfrow=c(2,2))

  plot(fit)
}
ignore <- dev.off()
