library(dplyr)
#install.packages("skimr")
#install.packages("pillar")
#update.packages()
library(skimr)


loan_data_preprocessed_backup <- read.csv(file = 'loan_data_2007_2014_preprocessed.csv')
loan_data_preprocessed_backup
str(loan_data_preprocessed_backup)
loan_data_preprocessed = loan_data_preprocessed_backup
names(loan_data_preprocessed)

loan_data_preprocessed %>% head()
loan_data_preprocessed %>% tail()



loan_data_defaults = loan_data_preprocessed %>% filter(loan_status %in% c('Charged Off',
                                                     'Does not meet the credit policy. Status:Charged Off'))



dim(loan_data_defaults)

skim(loan_data_defaults) %>% arrange(desc(n_missing))

loan_data_defaults['mths_since_last_delinq'][is.na(loan_data_defaults['mths_since_last_delinq'])] <- 0
loan_data_defaults['mths_since_last_record'][is.na(loan_data_defaults['mths_since_last_record'])] <- 0

loan_data_defaults['recovery_rate'] <- loan_data_defaults['recoveries'] / loan_data_defaults['funded_amnt']

loan_data_defaults['recovery_rate'] %>% str()
loan_data_defaults['recovery_rate'] %>% summary()


loan_data_defaults$recovery_rate[loan_data_defaults$recovery_rate > 1] <- 1
loan_data_defaults$recovery_rate[loan_data_defaults$recovery_rate < 0] <- 0


loan_data_defaults['CCF'] = (loan_data_defaults['funded_amnt'] - loan_data_defaults['total_rec_prncp']) / loan_data_defaults['funded_amnt']
write.csv(loan_data_defaults,"loan_data_defaults_R.csv")
          


hist(loan_data_defaults$recovery_rate, breaks= 100, main="recovery rate")
hist(loan_data_defaults$recovery_rate, breaks= 50, main="recovery rate")
hist(loan_data_defaults$CCF, breaks= 100, main="recovery rate")


loan_data_defaults$recovery_rate[loan_data_defaults$recovery_rate > 1] <- 1
loan_data_defaults$recovery_rate[loan_data_defaults$recovery_rate < 0] <- 0




loan_data_defaults$recovery_rate_0_1 = loan_data_defaults$recovery_rate
loan_data_defaults$recovery_rate_0_1[loan_data_defaults$recovery_rate_0_1==0] <- 0.00001
loan_data_defaults$recovery_rate_0_1[loan_data_defaults$recovery_rate_0_1==1] <- 0.99999

loan_data_defaults$recovery_rate_0_1 %>% str()
loan_data_defaults$recovery_rate_0_1 %>% summary()

# LGD Model
#install.packages("recipes")
#install.packages("glue")
library(caret)
set.seed(168)
num_train <- createDataPartition(y= loan_data_defaults$recovery_rate_0_1 , p= 0.8 , list =FALSE)
train_data <- loan_data_defaults[num_train,]
test_data <- loan_data_defaults[-num_train,]



str(train_data)
str(test_data)

features_all = c(
  'recovery_rate_0_1',
  'grade.A',
  'grade.B',
  'grade.C',
  'grade.D',
  'grade.E',
  'grade.F',
  'grade.G',
  'home_ownership.MORTGAGE',
  'home_ownership.NONE',
  'home_ownership.OTHER',
  'home_ownership.OWN',
  'home_ownership.RENT',
#  'verification_status.Not Verified',
#  'verification_status.Source Verified',
#  'verification_status.Verified',
  'purpose.car',
  'purpose.credit_card',
  'purpose.debt_consolidation',
  'purpose.educational',
  'purpose.home_improvement',
  'purpose.house',
  'purpose.major_purchase',
  'purpose.medical',
  'purpose.moving',
  'purpose.other',
  'purpose.renewable_energy',
  'purpose.small_business',
  'purpose.vacation',
  'purpose.wedding',
  'initial_list_status.f',
  'initial_list_status.w',
  'term_int',
  'emp_length_int',
  'mths_since_issue_d',
  'mths_since_earliest_cr_line',
  'funded_amnt',
  'int_rate',
  'installment',
  'annual_inc',
  'dti',
  'delinq_2yrs',
  'inq_last_6mths',
  'mths_since_last_delinq',
  'mths_since_last_record',
  'open_acc',
  'pub_rec',
  'total_acc',
  'acc_now_delinq',
  'total_rev_hi_lim'
  )

features_reference_cat <- c('grade.G',
                          'home_ownership.RENT',
          #                'verification_status.Verified',
                          'purpose.credit_card',
                          'initial_list_status.f')

lgd_inputs_stage_1_train <-  train_data[,features_all]
lgd_inputs_stage_1_train <- lgd_inputs_stage_1_train[ , -which(names(lgd_inputs_stage_1_train) %in% features_reference_cat)]


#install.packages("betareg")
library(betareg)



betaMod <- betareg(recovery_rate_0_1 ~ ., data = lgd_inputs_stage_1_train)
summary (betaMod) 

#predict (betaMod, testData)






