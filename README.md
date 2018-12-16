## Case Study

As a financial service company, credit scoring is a crucial indicator to decide which people eligible to get a credit or loan from it. By predicting the probability of a user tends to pay their loan or not it helps to determine the credit scoring for the user. 

### Variable Explanation

* SeriousDlqin2yrs:                     Person experienced 90 days past due delinquency or worse 
* RevolvingUtilizationOfUnsecuredLines:	Total balance on credit cards and personal lines of credit except real estate and no installment                                         debt like car loans divided by the sum of credit limits
* age:                                	 Age of borrower in years
* NumberOfTime30-59DaysPastDueNotWorse:	Number of times borrower has been 30-59 days past due but no worse in the last 2 years.
* DebtRatio:                          	 Monthly debt payments, alimony,living costs divided by monthy gross income
* MonthlyIncome:                      	 Monthly income
* NumberOfOpenCreditLinesAndLoans:    	 Number of Open loans (installment like car loan or mortgage) and Lines of credit (e.g. credit                                           cards)
* NumberOfTimes90DaysLate:            	 Number of times borrower has been 90 days or more past due.
* NumberRealEstateLoansOrLines:       	 Number of mortgage and real estate loans including home equity lines of credit
* NumberOfTime60-89DaysPastDueNotWorse:	Number of times borrower has been 60-89 days past due but no worse in the last 2 years.
* NumberOfDependents:                 	 Number of dependents in family excluding themselves (spouse, children etc.)

### Components

* Explore Data ('dplyr', 'naniar' package)
  * Data Balance
  * Outliers
  * Data Correlation
  * Missing Values

* Visualize Data ('ggplot2' package)
  * Debt Ratio data comparison
  * Age Histogram
  * Missing values from Monthly Income variable and its correlation to Number Of Dependents variable
  * Debt Ratio of people who did and did not disclose their salary

* Clean Data ('dplyr' package)
  * Because there are missing data, there are several options to handle it
    * Keep N/A values
    * Delete N/A rows
    * Impute N/A values

* Prediction
  * Split into train and test sets
  * Building the model
    * Naive Bayes ('e1071' package)
    * Random Forest ('randomForest' package)
  * Prediction
    * ROC Curve 
    * AUC
  * Variable Importance

  
### Future Improvements

Because of limited time and computational power, I haven't explore more about this data. There are several improvements that can be done.

* Feature Engineer
* Using 'mice' package to impute missing values
* Using other classification methods such as
 * SVM
 * XGBoost
 * Neural Network
  
### Tools

This project is completed using R, RStudio's tools, and other third party libraries.
