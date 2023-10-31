####################################################################
#  Generalized Linear Models
####################################################################

library(tidyverse)

## --------------------------------------------------------------------------------------------------------------------------------------------------
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir) 
mtpl_orig <- read.table('../data/PC_data.txt', 
                        header = TRUE,
                        stringsAsFactors = TRUE)
mtpl_orig <- as_tibble(mtpl_orig)


## --------------------------------------------------------------------------------------------------------------------------------------------------
mtpl_orig %>% slice(1:3) %>% select(-LONG, -LAT)

mtpl <- mtpl_orig %>% rename_all(tolower) %>% rename(expo = exp)
names(mtpl)


## --------------------------------------------------------------------------------------------------------------------------------------------------
dim(mtpl)

mtpl %>% summarize(emp_freq = sum(nclaims) / sum(expo))

mtpl %>% 
  group_by(sex) %>% 
  summarize(emp_freq = sum(nclaims) / sum(expo))

KULbg <- "#116E8A"
g <- ggplot(mtpl, aes(nclaims)) + theme_bw() + 
  geom_bar(aes(weight = expo), 
           col = KULbg, fill = KULbg) + 
  labs(y = "Abs freq (in exposure)") +
  ggtitle("MTPL - number of claims")
g


## Your Turn!
## --------------------------------------------------------------------------------------------------------------------------------------------------
ggplot(mtpl, aes(ageph)) + theme_bw() + 
  geom_histogram(binwidth = 2, alpha = .5,
           col = KULbg, fill = KULbg) + 
  labs(y = "Absolute frequency") +
  ggtitle("MTPL - age policyholder")

mtpl %>% group_by(ageph) %>% summarize(total_exposure = sum(expo),
                                       total_number_claims = sum(nclaims),
                                       total_observations=n())


freq_by_age <- mtpl %>% group_by(ageph) %>% summarize(emp_freq = sum(nclaims) / sum(expo))

ggplot(data = freq_by_age,
       aes(x = ageph, y = emp_freq)) + theme_bw() +
  geom_bar(stat = 'identity', alpha = .5,
           color = KULbg, fill = KULbg) +
  ggtitle('MTPL - empirical claim freq per
          age policyholder')


freq_by_bm <- mtpl %>% group_by(bm) %>% summarize(emp_freq = sum(nclaims) / sum(expo))

ggplot(data = freq_by_bm,
       aes(x = bm, y = emp_freq)) + theme_bw() +
  geom_bar(stat = 'identity', alpha = .5,
           color = KULbg, fill = KULbg) +
  ggtitle('MTPL - empirical claim freq per
          age policyholder')


## --------------------------------------------------------------------------------------------------------------------------------------------------
g_freq <- ggplot(mtpl, aes(nclaims)) + theme_bw() + 
  geom_bar(aes(weight = expo), col = KULbg, 
           fill = KULbg, alpha = .5) + 
  labs(y = "Abs freq (in exposure)") +
  ggtitle("MTPL - number of claims")
g_freq

g_sev <- ggplot(mtpl, aes(x = avg)) + theme_bw() +
  geom_histogram(bins = 30, boundary = 0, color = KULbg, fill = KULbg, alpha = .5) + 
  labs(x = "claim severity") +
  xlim(c(0, 20000))
g_sev


## --------------------------------------------------------------------------------------------------------------------------------------------------
freq_glm_1 <- glm(nclaims ~ sex, offset = log(expo), 
                  family = poisson(link = "log"), 
                  data = mtpl)

freq_glm_1 %>% broom::tidy() 

freq_glm_1 %>% broom::augment(type.predict = "response") %>% slice(1:2) %>% select(nclaims, sex, .fitted)

exp(coef(freq_glm_1)[1])
exp(coef(freq_glm_1)[1] + coef(freq_glm_1)[2])

freq_glm_1 %>% broom::glance() 


## Your Turn!
## --------------------------------------------------------------------------------------------------------------------------------------------------
# Q1
?predict.glm
# Q2
sex_driver <- data.frame(expo = c(1,1), sex = c("male","female"))
sex_driver$predict <- predict(freq_glm_1, newdata = sex_driver, type="response")

predict(freq_glm_1, newdata = sex_driver, type="link")
exp(predict(freq_glm_1, newdata = sex_driver, type="link"))

predict(freq_glm_1, newdata = sex_driver, type="terms")

# Q3
sev_glm_1 <- glm(avg ~ sex, 
                  family = Gamma(link = "log"), 
                  data = mtpl)

summary(sev_glm_1)
#sex is not a explanatory variable for the severity.

