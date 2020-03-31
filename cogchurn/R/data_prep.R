#' Get and Prep Customer churn
#' 
#' The function will: read the csv, remove metadata, convert characters to factors
#' and fill in `TotalCharges` in for the customers at the beginning of their tenure.
#' 
#' @param filepath A filepath to raw Customer Churn data.

#' @return The formatted and prepped dataset for modelling
#' @examples
#' getnclean('/valid/file/path')
getnclean <- function(data = "AIA_Churn_Modelling_Case_Study.csv") {
  read_csv(data) %>%
    # Remove metadata
    select(-customerID) %>%
    # Convert characters to factors
    mutate(gender = factor(gender),SeniorCitizen = factor(SeniorCitizen),
           Partner = factor(Partner), Dependents = factor(Dependents),gender = factor(gender),
           PhoneService = factor(PhoneService),MultipleLines = factor(MultipleLines),
           InternetService = factor(InternetService),OnlineSecurity = factor(OnlineSecurity),
           DeviceProtection = factor(DeviceProtection),TechSupport = factor(TechSupport),
           StreamingTV = factor(StreamingTV),StreamingMovies = factor(StreamingMovies),
           Contract = factor(Contract),PaperlessBilling = factor(PaperlessBilling),
           PaymentMethod = factor(PaymentMethod), Churn = factor(Churn)) %>%
    # Fill TotalCharges if tenure is 0
    mutate(TotalCharges = if_else(is.na(TotalCharges) & (tenure == 0), 0, TotalCharges))
}
