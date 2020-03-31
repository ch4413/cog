library(tidyverse)
library(tidymodels)
library(caret)

####
set.seed(123)
aia <- read_csv("data/AIA_Churn_Modelling_Case_Study.csv") %>%
  select(-customerID) %>%
  mutate(gender = factor(gender),SeniorCitizen = factor(SeniorCitizen),
         Partner = factor(Partner), Dependents = factor(Dependents),gender = factor(gender),
         PhoneService = factor(PhoneService),MultipleLines = factor(MultipleLines),
         InternetService = factor(InternetService),OnlineSecurity = factor(OnlineSecurity),
         DeviceProtection = factor(DeviceProtection),TechSupport = factor(TechSupport),
         StreamingTV = factor(StreamingTV),StreamingMovies = factor(StreamingMovies),
         Contract = factor(Contract),PaperlessBilling = factor(PaperlessBilling),
         PaymentMethod = factor(PaymentMethod), Churn = factor(Churn)) %>%
  mutate(TotalCharges = if_else(is.na(TotalCharges), 0, TotalCharges)) %>%
  select(-gender)

### Split data
aia_split <- initial_split(aia, prop = 0.8, strata = "Churn")
aia_train <- training(aia_split)
aia_test <- testing(aia_split)

rec <- recipe(Churn ~ ., data = aia_train) %>%
  ## No imputation needed
  step_dummy(SeniorCitizen, Partner, Dependents, PhoneService, MultipleLines,
             InternetService, OnlineSecurity, OnlineBackup, DeviceProtection, TechSupport,
             StreamingTV, StreamingMovies, Contract, PaperlessBilling, PaymentMethod)

logreg <- logistic_reg(mode = 'classification')
logreg <- logreg %>%
  set_engine("glm")

glm_stat <- glm(formula = Churn ~ ., family = binomial, data = aia_train)

glm_sum <- summary(glm_stat)
data.frame(glm_sum$coefficients) %>%
  rownames_to_column(var = "Feature") %>%
  arrange(`Pr...z..`) %>%
  write_csv('/Users/chughes/Documents/DS/git/cog/data/feature_stats.csv')

fitted_model <- logreg %>%
  fit(Churn ~ ., data = aia_train) 
fitted_model$fit

predictions <- bind_cols(
  select(aia_test, Churn),
  predict(fitted_model, aia_test)
  )
confusion <- conf_mat(predictions, truth = Churn, estimate = .pred_class)

aia_folds <- vfold_cv(aia_train, v = 10, repeats = 5, strata = "Churn")
aia_indices <- rsample2caret(aia_folds)

ctrl_rsample_cv <- trainControl(index = aia_indices$index, classProbs = TRUE)

glm_model <- train(rec, data = aia_train,
                   method = "glm", trControl = ctrl_rsample_cv)
rpart_model <- train(rec, data = aia_train, method = "rpart",
                  trControl = ctrl_rsample_cv)
resamps <- resamples(list(RF = rpart_model,GLM = glm_model))

summary(resamps)
