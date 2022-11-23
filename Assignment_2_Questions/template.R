################################# 
# You can use this template to draft the script for your Assessment 2.
# More clarification and related resources can be found at
# https://d2l.deakin.edu.au/d2l/le/content/1191557/viewContent/6267637/View
#################################

#############################################################################################
# save your code as "name-code.R" (where “name” is replaced with your surname or first name).
#############################################################################################

##################################
#Question 1 - Understand the Data
##################################

the.data <- as.matrix(read.table("Forest_2022.txt"))  

set.seed(ID) # using your student ID number for reproducible sampling with the seed function

my.data <- the.data[sample(1:517,330),c(1:13)] 


# Use scatter plots and histograms to understand the relationship between each of the 
# variables X5, X6, X7, X8, X9, X10, X11, X12, and your variable of interest Y.

# Create 8 scatterplots function (for each X variable against the variable of interest Y) 



# Create 9 histograms for each X variable and Y



################################
#Question 2 - Transform the Data
################################


I <- c("define your variable index") # Choose any four X variables (from X5, X6, X7, X8, X9, X10, X11, X12) and Y

variables_to_transform <- my.data[,I]  # obtain a 330 by 5 matrix

# for each variable, you need to figure out a good data transformation method, 
# such as Polynomial, log and negation transformation. The k-S test and Skewness 
# calculation may be helpful to select the transformation method

# you need to manually check outliers and impute or remove them with reasonable judegement, before applying any transformation

p=2 # for example, using p=2 to transform the first variable. You should change p based on your distribution.
data.transformed[,1]=variables_to_transform[,1]^p 

# A Min-Max and/or Z-score transformation should then be used to adjust the scale of each variable

# minmax normalisation
minmax <- function(x){
  (x - min(x))/(max(x)-min(x))
}

# z-score standardisation and scaling to unit interval
unit.z <- function(x){
  0.15*((x-mean(x))/sd(x)) + 0.5
}

data.transformed[,1]=minmax(data.transformed[,1]) # for example,using min-max normalisation for the first varible.


# Save this transformed data to a text file
write.table(data.transformed, "name-transformed.txt")  # replace “name” with either your surname or first name.


##########################################
#Question 3 - Build models and investigate
##########################################

source("AggWaFit718.R")

data.transformed_copy <- as.matrix(read.table("name-transformed.txt"))  # import your saved data

# Get weights for Weighted Arithmetic Mean with fit.QAM() 



# Get weights for Power Mean p=0.5 and p=2 with fit.QAM()



# Get weights for Ordered Weighted Average with fit.OWA()



# Get weights for Choquet Integral with fit.choquet() - Optional




#######################################
#Question 4 - Use Model for Prediction
#######################################

# new_input has X5=96.1; X6=181.1; X7=671.2; X8=14.3; X9=20.7; X10=69; X11=4.9; X12=0.4

new_input_to_transform <- c("choose the same four X variables as in Q2")


# transforming the four variables in the same way as in question 2 



# applying the transformed variables to the best model selected from Q3 for Y prediction



# Reverse the transformation to convert back the predicted Y to the original scale and then round it to integer



# Compare your prediction with the measured value of Y, Y=0.0146.




#############################################################################################
# References 
# Following Harvard style: https://www.deakin.edu.au/students/studying/study-support/referencing/harvard
#############################################################################################


# You must cite all the datasets and packages you used for this assessment. 
# You will loose some marks for lack of or inappropriate citations/references.
# Your assignment will not be assessed if the code is missing, or the outputs of the code are inconsistent with the content of the slides.
