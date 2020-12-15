## Many Researchers Project: team 84
# Anton Ohlsson Collentine & Erwin Gielens
# RQ1: is there an effect of religiosity on wellbeing?
# RQ2: is the effect of religiosity on wellbeing moderated by cultural norms?

#factors + indicators
# x: religiosity -- rel_1 to rel_9
# y: wellbeing -- generalized -- gen_1, gen_2
#                 physical -- phys_1 to phys_7
#                 psychological -- psych_1 to psych_6
#                 social -- soc_1, soc_2
# z: perceived cultural importance of relgion -- cnorm_1, cnorm_2
# controls: age, gender (3), education (7), ses, minority (2), gdp,
#           sample_type (4), compensation (5)

#k variables = 47
n=1000
df <- as.data.frame(matrix(NA, nrow=n, ncol=47))
colnames(df) <- c('rel_1', 'rel_2', 'rel_3', 'rel_4', 'rel_5', 'rel_6', 'rel_7', 'rel_8', 'rel_9', 
                  'gen_1', 'gen_2',
                  'phys_1', 'phys_2', 'phys_3', 'phys_4', 'phys_5', 'phys_6', 'phys_7', 
                  'psych_1', 'psych_2', 'psych_3', 'psych_4', 'psych_5', 'psych_6', 
                  'soc_1', 'soc_2', 
                  'cnorm_1', 'cnorm_2', 
                  'age', 'gender_d1', 'gender_d2', 
                  'education_d1', 'education_d2', 'education_d3', 'education_d4', 'education_d5', 'education_d6', 
                  'ses', 'minority', 'gdp', 
                  'sample_type_d1', 'sample_type_d2', 'sample_type_d3', 
                  'compensation_d1', 'compensation_d2', 'compensation_d3', 'compensation_d4')

for (i in 1:ncol(df)){ df[,i] <- sample(1:10, n, replace=T) }

library(lavaan)

#Confirmatory Factor Analysis
m_cfa <- 'rel =~ rel_1 + rel_2 + rel_3 + rel_4 + rel_5 + rel_6 + rel_7 + rel_8 + rel_9
            cnorm =~ cnorm_1 + cnorm_2
            wb_gen =~ gen_1 + gen_2
            wb_phys =~ phys_1 + phys_2 + phys_3 + phys_4 + phys_5 + phys_6 + phys_7
            wb_psych =~ psych_1 + psych_2 + psych_3 + psych_4 + psych_5 + psych_6
            wb_soc =~ soc_1 + soc_2
            wb =~ wb_gen + wb_phys + wb_psych + wb_soc
            
            #fix factor variance for identification purposes
            cnorm ~~ 1*cnorm
            wb_gen ~~ 1*wb_gen'

fit_cfa <- cfa(m_cfa, data = df)
summary(fit_cfa, fit.measures=T, rsquare=T, standardized = T)



#Strucural Equation Model
#Total effect
m_sem_total <- 'rel =~ rel_1 + rel_2 + rel_3 + rel_4 + rel_5 + rel_6 + rel_7 + rel_8 + rel_9

                wb_gen =~ gen_1 + gen_2
                wb_phys =~ phys_1 + phys_2 + phys_3 + phys_4 + phys_5 + phys_6 + phys_7
                wb_psych =~ psych_1 + psych_2 + psych_3 + psych_4 + psych_5 + psych_6
                wb_soc =~ soc_1 + soc_2
                wb =~ wb_gen + wb_phys + wb_psych + wb_soc
      
                cnorm =~ cnorm_1 + cnorm_2
      
                wb ~ rel'

fit_sem_total <- sem(m_sem_total, data=df)
summary(fit_sem_total, fit.measures=T, rsquare=T, standardized = T)

#Interaction effect
df <- data.frame(df, predict(fit_cfa))
df$rel_cnorm <- df$rel * df$cnorm

m_sem_int <- 'rel =~ rel_1 + rel_2 + rel_3 + rel_4 + rel_5 + rel_6 + rel_7 + rel_8 + rel_9

                wb_gen =~ gen_1 + gen_2
                wb_phys =~ phys_1 + phys_2 + phys_3 + phys_4 + phys_5 + phys_6 + phys_7
                wb_psych =~ psych_1 + psych_2 + psych_3 + psych_4 + psych_5 + psych_6
                wb_soc =~ soc_1 + soc_2
                wb =~ wb_gen + wb_phys + wb_psych + wb_soc
      
                cnorm =~ cnorm_1 + cnorm_2
      
                wb ~ rel + cnorm + rel_cnorm'

fit_sem_int <- sem(m_sem_int, data=df)
summary(fit_sem_int, fit.measures=T, rsquare=T, standardized = T)

