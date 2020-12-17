## Many Researchers Project: team 84
# Anton Olsson Collentine & Erwin Gielens
# RQ1: is there an effect of religiosity on wellbeing?
# RQ2: is the effect of religiosity on wellbeing moderated by cultural norms?

#factors + indicators
# x: religiosity -- rel_1 to rel_9
# y: wellbeing -- generalized -- gen_1, gen_2
#                 physical -- phys_1 to phys_7
#                 psychological -- psych_1 to psych_6
#                 social -- soc_1, soc_2
# z: perceived cultural importance of relgion -- cnorm_1, cnorm_2
# controls: age, gender (3), education (7), ses, gdp,
#           sample_type (4), country (24)


#########################
#### Data Simulation ####
#########################
#Simulated data to test out code
#see e.g., https://github.com/simsem/simsem/wiki/Example-2:-Getting-Started-%28lavaan-input%29

library(simsem)

m_sim <-  '
                rel =~ 0.9*rel_1 + 0.7*rel_2 + 0.8*rel_3 + 0.6*rel_4 + 0.6*rel_5 + 1.2*rel_6 + 0.75*rel_7 + 0.55*rel_8 + 0.3*rel_9

                wb_gen =~ 1.0*gen_1 + 0.9*gen_2
                wb_phys =~ 0.6*phys_1 + 0.8*phys_2 + 0.4*phys_3 + 1.1*phys_4 + 1.2*phys_5 + 0.86*phys_6 + 0.99*phys_7
                wb_psych =~ 0.6*psych_1 + 1.02*psych_2 + 0.9*psych_3 + 0.55*psych_4 + 0.83*psych_5 + 1.15*psych_6
                wb_soc =~ 0.5*soc_1 + 0.6*soc_2
                wb =~ 1.2*wb_gen + 0.6*wb_phys + 1.1*wb_psych + 0.5*wb_soc
      
                cnorm =~ 1.1*cnorm_1 + 0.9*cnorm_2
      
                wb ~ 0.6*rel + 
                     0.02*age + 0.15*gender_d1 + 0.3*gender_d2 + 
                     0.12*education + 
                     0.3*ses + 0.2*gdp +
                     0.02*sample_type_d1 + 0.01*sample_type_d2 + 0.001*sample_type_d3 
                
                #fix factor variance for identification purposes
                cnorm ~~ 1*cnorm
                wb_soc ~~ 1*wb_soc
                wb_gen ~~ 1*wb_gen
                ' 
n <- 24*400
dat_sim <- generate(m_sim, n)

#add country dummies
countries <- c('country_1','country_2','country_3','country_4','country_5','country_6','country_7','country_8',
               'country_9','country_10','country_11','country_12','country_13','country_14','country_15','country_16',
               'country_17','country_18','country_19','country_20','country_21','country_22','country_23', 'country_24')

country <- rep(countries, each = 400)
country <- factor(country)
country_dummies <- model.matrix(~country)
country_dummies <- country_dummies[,-1] #drop intercept column
colnames(country_dummies) <- countries[-24] #rename for clarity

dat_sim <- cbind(dat_sim, country_dummies) #add dummies to rest of dataframe


#########################
#### Model Estimation ###
#########################
library(lavaan)

#*******************
#Test main effect
#*******************




#Strucural Equation Model
#Direct effect
m_sem_direct <- '
                rel =~ rel_1 + rel_2 + rel_3 + rel_4 + rel_5 + rel_6 + rel_7 + rel_8 + rel_9

                wb_gen =~ gen_1 + gen_2
                wb_phys =~ phys_1 + phys_2 + phys_3 + phys_4 + phys_5 + phys_6 + phys_7
                wb_psych =~ psych_1 + psych_2 + psych_3 + psych_4 + psych_5 + psych_6
                wb_soc =~ soc_1 + soc_2
                wb =~ wb_gen + wb_phys + wb_psych + wb_soc
      
                wb ~ rel + 
                     age + gender_d1 + gender_d2 + 
                     education + 
                     ses + gdp +
                     sample_type_d1 + sample_type_d2 + sample_type_d3 ++
                     country_1 + country_2 + country_3 + country_4 + country_5 +
                     country_6 + country_7 + country_8 + country_9 + country_10 +
                     country_11 + country_12 + country_13 + country_14 + country_15 +
                     country_16 + country_17 + country_18 + country_19 + country_20 +
                     country_21 + country_22 + country_23
                
                #fix factor variance for identification purposes
                wb_soc ~~ 1*wb_soc
                wb_gen ~~ 1*wb_gen
                ' 

fit_sem_direct <- sem(m_sem_direct, data=dat_sim)
summary(fit_sem_direct, fit.measures=T, rsquare=T, standardized = T)

#***********************
#Test interaction effect
#***********************
#using this approach for the interaction: https://stackoverflow.com/questions/24399353/r-lavaan-coding-latent-variable-interactions
#see also ?semTools::indProd for some references

#First fit cfa to compute the latent interaction variable
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
            wb_soc ~~ 1*wb_soc
            wb_gen ~~ 1*wb_gen
'

fit_cfa <- cfa(m_cfa, data = dat_sim, orthogonal=T)
summary(fit_cfa, fit.measures=T, rsquare=T, standardized = T)

dat_sim2 <- data.frame(dat_sim, predict(fit_cfa))
dat_sim$rel_cnorm <- dat_sim2$rel * dat_sim2$cnorm #compute the latent interaction from the estimate latent variables

m_sem_int <- '
                rel =~ rel_1 + rel_2 + rel_3 + rel_4 + rel_5 + rel_6 + rel_7 + rel_8 + rel_9

                wb_gen =~ gen_1 + gen_2
                wb_phys =~ phys_1 + phys_2 + phys_3 + phys_4 + phys_5 + phys_6 + phys_7
                wb_psych =~ psych_1 + psych_2 + psych_3 + psych_4 + psych_5 + psych_6
                wb_soc =~ soc_1 + soc_2
                wb =~ wb_gen + wb_phys + wb_psych + wb_soc
      
                cnorm =~ cnorm_1 + cnorm_2
      
                wb ~ rel + cnorm + rel_cnorm + 
                     age + gender_d1 + gender_d2 + 
                     education +
                     ses  + gdp +
                     sample_type_d1 + sample_type_d2 + sample_type_d3 +
                     country_1 + country_2 + country_3 + country_4 + country_5 +
                     country_6 + country_7 + country_8 + country_9 + country_10 +
                     country_11 + country_12 + country_13 + country_14 + country_15 +
                     country_16 + country_17 + country_18 + country_19 + country_20 +
                     country_21 + country_22 + country_23
                     
                #fix factor variance for identification purposes
                cnorm ~~ 1*cnorm
                wb_soc ~~ 1*wb_soc
                wb_gen ~~ 1*wb_gen
              ' 


fit_sem_int <- sem(m_sem_int, data=dat_sim)
summary(fit_sem_int, fit.measures=T, rsquare=T, standardized = T)


