## Case Study

This project is completed using R, RStudio's tools, and other third party libraries.

### Variable Explanation

SeriousDlqin2yrs:                     Person experienced 90 days past due delinquency or worse 
RevolvingUtilizationOfUnsecuredLines:	Total balance on credit cards and personal lines of credit except real estate and no installment                                           debt like car loans divided by the sum of credit limits
age:                                	Age of borrower in years
NumberOfTime30-59DaysPastDueNotWorse:	Number of times borrower has been 30-59 days past due but no worse in the last 2 years.
DebtRatio:                          	Monthly debt payments, alimony,living costs divided by monthy gross income
MonthlyIncome:                      	Monthly income
NumberOfOpenCreditLinesAndLoans:    	Number of Open loans (installment like car loan or mortgage) and Lines of credit (e.g. credit                                               cards)
NumberOfTimes90DaysLate:            	Number of times borrower has been 90 days or more past due.
NumberRealEstateLoansOrLines:       	Number of mortgage and real estate loans including home equity lines of credit
NumberOfTime60-89DaysPastDueNotWorse:	Number of times borrower has been 60-89 days past due but no worse in the last 2 years.
NumberOfDependents:                 	Number of dependents in family excluding themselves (spouse, children etc.)

### Components

* Explore Data ('tidyverse', 'naniar' package)
  * Data Balance
  * Outliers
  * Data Correlation
  * Missing Values

* Visualize Data ('ggplot2' package)
  * Debt Ratio data comparison
  * Age Histogram
  * Missing values from Monthly Income variable and its correlation to Number Of Dependents variable
  * Debt Ratio of people who did and did not disclose their salary

* Clean Data ('tidyverse')
  * Because there are missing data, there are several options to handle it
    * Keep N/A values
    * Delete N/A rows
    * Impute N/A values

* Predict Data
  * Naive Bayes ('e1071' package)
  * Random Forest ('randomForest' package)
  
  
