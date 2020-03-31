library(tidyverse)
library(GGally)
library(corrplot)

aia <- readr::read_csv('data/AIA_Churn_Modelling_Case_Study.csv')
aia <- mutate(aia, Churn = factor(Churn))
levels(aia$Churn)
count(aia, Churn)
glimpse(aia)
sum(is.na(aia))
dim(aia)
names(aia)
aia %>%
  select(tenure, MonthlyCharges, TotalCharges, Churn) %>%
  gather(key="key", value="value", -Churn) %>%
  ggplot(aes(x = value, fill = Churn)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~key, scales = "free")

ggpairs(aia, columns = 2:6, mapping = aes(colour=Churn, alpha = 0.5))

aia %>%
  select(tenure, MonthlyCharges, TotalCharges) %>%
  drop_na() %>%
  cor() %>%
  corrplot(order = "hclust")

aia <- read_csv("data/AIA_Churn_Modelling_Case_Study.csv") %>%
  select(-customerID) %>%
  mutate(gender = factor(gender),SeniorCitizen = factor(SeniorCitizen),
         Partner = factor(Partner), 
         Dependents = factor(Dependents),gender = factor(gender),
         PhoneService = factor(PhoneService),MultipleLines = factor(MultipleLines),
         InternetService = factor(InternetService),OnlineSecurity = factor(OnlineSecurity),
         DeviceProtection = factor(DeviceProtection),TechSupport = factor(TechSupport),
         StreamingTV = factor(StreamingTV),StreamingMovies = factor(StreamingMovies),
         Contract = factor(Contract),PaperlessBilling = factor(PaperlessBilling),
         PaymentMethod = factor(PaymentMethod), Churn = factor(Churn)
         )

set.seed(123)
aia_split <- initial_split(aia, prop = 0.75, strata = "Churn")
aia_train <- training(aia_split)
aia_test <- testing(aia_split)

count(aia_train, Churn)
logreg <- logistic_reg()
logreg <- logreg %>%
  set_engine("glm")

fitted_model <- logreg %>%
  fit(Churn ~ ., data = aia_train)
fitted_model$fit

predict(fitted_model, aia_test)
predict(fitted_model, aia_test, type = "prob")

predictions <- bind_cols(
  select(aia_test, Churn),
  predict(fitted_model, aia_test)
  )

metrics(predictions, truth = Churn, estimate = .pred_class)
confusion <- conf_mat(predictions, truth = Churn, estimate = .pred_class)
confusion
summary(confusion)

### Run model wf
rec <- recipe(Churn ~ ., data = aia_train)
rec
dim(aia_train)
help.search("step.*impute")

rec_impute <- rec %>%
  step_meanimpute(all_numeric()) %>%
  step_knnimpute(all_nominal(), neighbors = 4) %>%
  prep(aia_train)
rec_impute

##################
# Read
aia <- read_csv("data/AIA_Churn_Modelling_Case_Study.csv") %>%
  select(-customerID) %>%
  mutate(gender = factor(gender),SeniorCitizen = factor(SeniorCitizen),
         Partner = factor(Partner), Dependents = factor(Dependents),gender = factor(gender),
         PhoneService = factor(PhoneService),MultipleLines = factor(MultipleLines),
         InternetService = factor(InternetService),OnlineSecurity = factor(OnlineSecurity),
         DeviceProtection = factor(DeviceProtection),TechSupport = factor(TechSupport),
         StreamingTV = factor(StreamingTV),StreamingMovies = factor(StreamingMovies),
         Contract = factor(Contract),PaperlessBilling = factor(PaperlessBilling),
         PaymentMethod = factor(PaymentMethod), Churn = factor(Churn))

# Split
set.seed(123)
aia_split <- initial_split(aia, prop = 0.75, strata = "Churn")
aia_train <- training(aia_split)
aia_test <- testing(aia_split)

# Preprocess
aia_rec <- recipe(Churn ~., data = aia_train) %>%
  step_meanimpute(all_numeric()) %>%
  step_knnimpute(all_nominal()) %>%
  step_dummy(gender, SeniorCitizen, Partner, Dependents, PhoneService, MultipleLines,
             InternetService, OnlineSecurity, OnlineBackup, DeviceProtection, TechSupport,
             StreamingTV, StreamingMovies, Contract, PaperlessBilling, PaymentMethod) %>%
  prep(aia_train)

# Fit (with preproc)
fitted_model <- logistic_reg() %>%
  set_engine("glm") %>%
  fit(Churn ~ ., data = bake(aia_rec, aia_train))

rf_spec <- rand_forest(mode = 'classification') %>%
  set_engine("randomForest")
fitted_model <- rf_spec %>%
  fit(Churn ~ ., data = bake(aia_rec, aia_train))
gb_spec <- boost_tree(mode = 'classification', learn_rate = 0.4, tree_depth = 5) %>%
  set_engine("xgboost")
fitted_model <- gb_spec %>%
  fit(Churn ~ ., data = bake(aia_rec, aia_train))
# Score (with preproc)
predictions <- bind_cols(
  select(aia_test, Churn),
  predict(fitted_model, bake(aia_rec, aia_test))
)

metrics(predictions, truth = Churn, estimate = .pred_class)
### No need for rare levels


### Caret
set.seed(123)
aia_split <- initial_split(aia, prop = 0.75, strata = "Churn")
aia_train <- training(aia_split)
aia_test <- testing(aia_split)

rec <- recipe(Churn ~ ., data = aia_train) %>%
  step_meanimpute(all_numeric()) %>%
  step_knnimpute(all_nominal()) %>%
  step_dummy(gender, SeniorCitizen, Partner, Dependents, PhoneService, MultipleLines,
             InternetService, OnlineSecurity, OnlineBackup, DeviceProtection, TechSupport,
             StreamingTV, StreamingMovies, Contract, PaperlessBilling, PaymentMethod)

recipe_model <- train(rec, data = aia_train,
                      method = "glm", trControl = ctrl)

recipe_pred <- predict(recipe_model, aia_test)

### resampling
ctrl_single <- trainControl(method = "none")
single_model <- train(rec, trControl = ctrl_single, method = "glm", data = aia_train)
ctrl_boot <- trainControl(method = "boot", number = 10)
boot_model <- train(rec, trControl = ctrl_boot,
                    method = "glm", data = aia_train)
boot_model
head(boot_model$resample)

ctrl_cv <- trainControl(method = "cv", number = 10)
ctrl_repcv <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
repcv_model <- train(rec, data = aia_train,method = "glm", trControl = ctrl_repcv)

resampleHist(repcv_model)
predictions <- predict(repcv_model, aia_test)
postResample(predictions, aia_test$Churn)


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
         PaymentMethod = factor(PaymentMethod), Churn = factor(Churn))
aia_split <- initial_split(aia, prop = 0.75, strata = "Churn")
aia_train <- training(aia_split)
aia_test <- testing(aia_split)

rec <- recipe(Churn ~ ., data = aia_train) %>%
  step_meanimpute(all_numeric()) %>%
  step_knnimpute(all_nominal()) %>%
  step_dummy(gender, SeniorCitizen, Partner, Dependents, PhoneService, MultipleLines,
             InternetService, OnlineSecurity, OnlineBackup, DeviceProtection, TechSupport,
             StreamingTV, StreamingMovies, Contract, PaperlessBilling, PaymentMethod)

aia_folds <- vfold_cv(aia_train, v = 10, repeats = 5, strata = "Churn")
aia_indices <- rsample2caret(aia_folds)

ctrl_rsample_cv <- trainControl(index = aia_indices$index, classProbs = TRUE)

glm_model <- train(rec, data = aia_train,
                   method = "glm", trControl = ctrl_rsample_cv)

rf_model <- train(rec, data = aia_train, method = "rf",
                  trControl = ctrl_rsample_cv)

resamps <- resamples(list(RF = rf_model,GLM = glm_model))
 
summary(resamps)
