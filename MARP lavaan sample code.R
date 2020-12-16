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
# controls: age, gender (3), education (7), ses, gdp,
#           sample_type (4)

#k variables = 47 (EDIT: check)
n=5000
df <- as.data.frame(matrix(NA, nrow=n, ncol=38))
colnames(df) <- c('rel_1', 'rel_2', 'rel_3', 'rel_4', 'rel_5', 'rel_6', 'rel_7', 'rel_8', 'rel_9', 
                  'gen_1', 'gen_2',
                  'phys_1', 'phys_2', 'phys_3', 'phys_4', 'phys_5', 'phys_6', 'phys_7', 
                  'psych_1', 'psych_2', 'psych_3', 'psych_4', 'psych_5', 'psych_6', 
                  'soc_1', 'soc_2', 
                  'cnorm_1', 'cnorm_2', 
                  'age', 'gender_d1', 'gender_d2', 'education',  
                  'ses', 'gdp', 
                  'sample_type_d1', 'sample_type_d2', 'sample_type_d3',
                  'country')

for (i in 1:ncol(df)){ df[,i] <- sample(1:10, n, replace=T) }
df$country <- rep(1:10, each = n/10)
df$gdp <- rep(sample(10, 10, replace = TRUE), each = n/10) #gdp is a country level variable

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
            wb_soc ~~ 1*wb_soc
            wb_gen ~~ 1*wb_gen'

fit_cfa <- cfa(m_cfa, data = df)
summary(fit_cfa, fit.measures=T, rsquare=T, standardized = T)


#ANTON: Because this model includes cnorm (latent) but it is not in the regression
#it by default is set to covary with Religiosity. Does this make sense? Better to drop IMO+
#EDIT: ah, I see now you are using this approach for the interaction: https://stackoverflow.com/questions/24399353/r-lavaan-coding-latent-variable-interactions
#see also ?semTools::indProd for some references

#Strucural Equation Model
#Total effect
m_sem_direct <- '
                rel =~ rel_1 + rel_2 + rel_3 + rel_4 + rel_5 + rel_6 + rel_7 + rel_8 + rel_9

                wb_gen =~ gen_1 + gen_2
                wb_phys =~ phys_1 + phys_2 + phys_3 + phys_4 + phys_5 + phys_6 + phys_7
                wb_psych =~ psych_1 + psych_2 + psych_3 + psych_4 + psych_5 + psych_6
                wb_soc =~ soc_1 + soc_2
                wb =~ wb_gen + wb_phys + wb_psych + wb_soc
      
                cnorm =~ cnorm_1 + cnorm_2
      
                wb ~ rel + 
                     age + gender_d1 + gender_d2 + 
                     education + 
                     ses + gdp +
                     sample_type_d1 + sample_type_d2 + sample_type_d3 
                
                #fix factor variance for identification purposes
                cnorm ~~ 1*cnorm
                wb_soc ~~ 1*wb_soc
                wb_gen ~~ 1*wb_gen
                ' 

fit_sem_direct <- sem(m_sem_direct, data=df)
summary(fit_sem_direct, fit.measures=T, rsquare=T, standardized = T)

#Interaction effect
df <- data.frame(df, predict(fit_cfa))
df$rel_cnorm <- df$rel * df$cnorm

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
                     ses + minority + gdp +
                     sample_type_d1 + sample_type_d2 + sample_type_d3
                     
                #fix factor variance for identification purposes
                cnorm ~~ 1*cnorm
                wb_soc ~~ 1*wb_soc
                wb_gen ~~ 1*wb_gen
              ' 


fit_sem_int <- sem(m_sem_int, data=df)
summary(fit_sem_int, fit.measures=T, rsquare=T, standardized = T)

##NOTES ANTON

#Currently not multi-level

#Very good baseline model
# but a few extra covariates: minority, compensation

#****************************************************
##Multilevel SEM
#Not a lot of clear guidance.. Best: https://docplayer.net/123371066-Multilevel-structural-equation-modeling-with-lavaan.html
#p. 82 - 102

#Essentially, we fit the same model as before, but fit WB also at level 2 (= random intercept) with the level 2 predictor gdp

m_sem_multilevel <- '
                level: 1
                    rel =~ rel_1 + rel_2 + rel_3 + rel_4 + rel_5 + rel_6 + rel_7 + rel_8 + rel_9

                    wb_gen =~ gen_1 + gen_2
                    wb_phys =~ phys_1 + phys_2 + phys_3 + phys_4 + phys_5 + phys_6 + phys_7
                    wb_psych =~ psych_1 + psych_2 + psych_3 + psych_4 + psych_5 + psych_6
                    wb_soc =~ soc_1 + soc_2
                    wb =~ wb_gen + wb_phys + wb_psych + wb_soc
      
                    cnorm =~ cnorm_1 + cnorm_2
      
                    wb ~ rel + 
                        age + gender_d1 + gender_d2 + 
                        education + 
                        ses + 
                        sample_type_d1 + sample_type_d2 + sample_type_d3
                        
                    #fix factor variance for identification purposes
                    cnorm ~~ 1*cnorm
                    wb_soc ~~ 1*wb_soc
                    wb_gen ~~ 1*wb_gen
                           
              
                     
                level: 2
                    wb_gen =~ gen_1 + gen_2
                    wb_phys =~ phys_1 + phys_2 + phys_3 + phys_4 + phys_5 + phys_6 + phys_7
                    wb_psych =~ psych_1 + psych_2 + psych_3 + psych_4 + psych_5 + psych_6
                    wb_soc =~ soc_1 + soc_2
                    wb =~ wb_gen + wb_phys + wb_psych + wb_soc
                
                    #Level 2 covariates
                    wb ~ gdp
                    
                    #fix factor variance for identification purposes
                    cnorm ~~ 1*cnorm
                    wb_soc ~~ 1*wb_soc
                    wb_gen ~~ 1*wb_gen
                  
                ' 

fit_sem_multilevel <- sem(m_sem_multilevel,
                          data = df,
                          cluster = "country")
#hmm, did not converge, althought this may be because the data is just random
#possibly simulate more complex data?
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

fake_dat <- generate(m_sim, 5000)
fake_dat$country <- rep(1:10, each = n/10)
fake_dat$gdp <- rep(sample(10, 10, replace = TRUE), each = n/10) #gdp is a country level variable

fit_sem_multilevel <- sem(m_sem_multilevel,
                          data = fake_dat,
                          cluster = "country")

#Hmm, still not converging. Miss-specified the model somehow?
#EDIT: after some testing cannot converge with multiple latents on both levels it seems..
#drop the multilevel approach?
#Perhaps consider using an approach with clustered standard errors instead? 
#I think the lavaan.survey package does this
#Then just first fit the model and then the clustered model e.g.,
library(lavaan.survey)
country.as.cluster <- svydesign(ids=~country, probs~=1, data=df)
fit2.clustered <- lavaan.survey(fit, country.as.cluster, estimator="MLM")
