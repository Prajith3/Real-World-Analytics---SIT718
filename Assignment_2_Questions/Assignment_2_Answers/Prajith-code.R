# Task 1: Understand the data

original_data = as.matrix(read.table("Forest_2022.txt")) # Saving the data to a variable in a matrix form.
# original_data
set.seed(220156351) # To reproduce the random sample results

sample_data = original_data[sample(1:517,330),c(1:13)] #Taking 330 samples and assigning it to a var.
#sample_data
#summary(sample_data)

#Scatter plots of Variables from X5:X12 to Y and also finding their correlation

plot(sample_data[,5],sample_data[,13], main='Scatter of X5 and Y', col='blue')
cor(sample_data[,5],sample_data[,13])

plot(sample_data[,6],sample_data[,13], main='Scatter of X6 and Y', col='blue')
cor(sample_data[,6],sample_data[,13])

plot(sample_data[,7],sample_data[,13], main='Scatter of X7 and Y', col='blue')
cor(sample_data[,7],sample_data[,13])

plot(sample_data[,8],sample_data[,13], main='Scatter of X8 and Y', col='blue')
cor(sample_data[,8],sample_data[,13])

plot(sample_data[,9],sample_data[,13], main='Scatter of X9 and Y', col='blue')
cor(sample_data[,9],sample_data[,13])

plot(sample_data[,10],sample_data[,13], main='Scatter of X10 and Y', col='blue')
cor(sample_data[,10],sample_data[,13])

plot(sample_data[,11],sample_data[,13], main='Scatter of X11 and Y', col='blue')
cor(sample_data[,11],sample_data[,13])

plot(sample_data[,12],sample_data[,13], main='Scatter of X12 and Y', col='blue')
cor(sample_data[,12],sample_data[,13])

# Histograms of Variables X5:12 and Y and finding skewness of original data before transformation

hist(sample_data[,5],col='red')
skewness(sample_data[,5])

hist(sample_data[,6],col='red')
skewness(sample_data[,6])

hist(sample_data[,7],col='red')
skewness(sample_data[,7])

hist(sample_data[,8],col='red')
skewness(sample_data[,8])

hist(sample_data[,9],col='red')
skewness(sample_data[,9])

hist(sample_data[,10],col='red')
skewness(sample_data[,10])

hist(sample_data[,11],col='red')
skewness(sample_data[,11])

hist(sample_data[,12],col='red')
skewness(sample_data[,12])

hist(sample_data[,13],col='red')
skewness(sample_data[,13])

transformed_data = array(0,c(330,5))
transformed_data[,1] = sample_data[,6]
transformed_data[,2] = sample_data[,9]
transformed_data[,3] = sample_data[,10]
transformed_data[,4] = sample_data[,11]
transformed_data[,5] = sample_data[,13]

# Task 2: Transform the Data

# Here, I have chosen to work with the Variables :X6,X9, X10 and X11 and Y.

min(transformed_data[,1]);max(transformed_data[,1]) # 1.1,291.3
min(transformed_data[,2]);max(transformed_data[,2]) # 2.2,33.3
min(transformed_data[,3]);max(transformed_data[,3]) # 15,100
min(transformed_data[,4]);max(transformed_data[,4]) # 0.4,9.4
min(transformed_data[,5]);max(transformed_data[,5]) # 0.006287804,0.02100974

#Negation transformation for the variables X10 and X11 as they have negative correlation.
transformed_data[,3] = max(transformed_data[,3])+min(transformed_data[,3])-transformed_data[,3]
hist(transformed_data[,3],col='yellow')

transformed_data[,4] = max(transformed_data[,4])+min(transformed_data[,4])-transformed_data[,4]
hist(transformed_data[,4],col='yellow')

# Linear scaling and Polynomial Transformation on X6 to get the values under unit interval and treat the skewed values.
transformed_data[,1] = transformed_data[,1]^0.75 # P = 0.75
skewness(transformed_data[,1])
hist(transformed_data[,1], col='brown')

transformed_data[,1] = (transformed_data[,1]-min(transformed_data[,1]))/(max(transformed_data[,1])-min(transformed_data[,1]))
skewness(transformed_data[,1])
hist(transformed_data[,1], col='brown')

# Linear scaling on X9 to get values under the unit interval
transformed_data[,2] = (transformed_data[,2]-min(transformed_data[,2]))/(max(transformed_data[,2])-min(transformed_data[,2]))
skewness(transformed_data[,2])
hist(transformed_data[,2], col='pink')

# Linear scaling and Polynomial Transformation on X10 to get the values under unit interval and treat the skewed values.
transformed_data[,3] = transformed_data[,3]^3.34 # P = 3.34
skewness(transformed_data[,3])
hist(transformed_data[,3], col='grey')

transformed_data[,3] = (transformed_data[,3]-min(transformed_data[,3]))/(max(transformed_data[,3])-min(transformed_data[,3]))
skewness(transformed_data[,3])
hist(transformed_data[,3], col='grey')

# Linear scaling and Polynomial Transformation on X11 to get the values under unit interval and treat the skewed values.
transformed_data[,4] = transformed_data[,4]^1.25 # P=1.25
skewness(transformed_data[,4])
hist(transformed_data[,4], col='purple')

transformed_data[,4] = (transformed_data[,4]-min(transformed_data[,4]))/(max(transformed_data[,4])-min(transformed_data[,4]))
skewness(transformed_data[,4])
hist(transformed_data[,4], col='purple')

# Linear scaling and Polynomial Transformation on Y to get the values under unit interval and treat the skewed values.
transformed_data[,5] = sample_data[,13]^0.9 #P = 0.9
skewness(transformed_data[,5])
hist(transformed_data[,5], col='orange')

transformed_data[,5] = (transformed_data[,5]-min(transformed_data[,5]))/(max(transformed_data[,5])-min(transformed_data[,5]))
skewness(transformed_data[,5])
hist(transformed_data[,5], col='orange')

write.table(transformed_data,'prajith-transformed.txt')

# Task 3: Build Models
source('AggWaFit718.R')

transformed_data_copy = as.matrix(read.table('prajith-transformed.txt'))

#Get weights for Weighted Arithmetic Mean with fit.QAM() 

fit.QAM(transformed_data_copy[,c(1:4,5)])

# Get weights for Power Mean p=0.5 with fit.QAM()

fit.QAM(transformed_data_copy[,c(1:4,5)],output.1="PM05output1.txt",stats.1="PM05stats1.txt", g=PM05,g.inv = invPM05)

# Get weights for Power Mean p=2 with fit.QAM()

fit.QAM(transformed_data_copy[,c(1:4,5)],output.1="QMoutput1.txt",stats.1="QMstats1.txt",g=QM,g.inv = invQM)

# Get weights for Ordered Weighted Average with fit.OWA()

fit.OWA(transformed_data_copy[,c(1:4,5)],"OWAoutput1.txt","OWAstats1.txt")

# Get weights for Choquet Integral with fit.choquet()

fit.choquet(transformed_data_copy[,c(1:4,5)],output.1="Choutput1.txt",stats.1="Chstats1.txt",)

#Question 4 - Use Model for Prediction
# X6=181.1; X9=20.7; X10=69; X11=4.9

# Transformation of the above variables using the same as Task 2.
min_X6_value = 1.1
max_X6_value = 291.3

polynomial_X6_value= (181.1)^0.75
transformed_X6_value = (polynomial_X6_value-min_X6_value)/(max_X6_value-min_X6_value)
transformed_X6_value

min_X9_value = 2.2
max_X9_value = 33.3

transformed_X9_value = (20.7-min_X9_value)/(max_X9_value-min_X9_value)
transformed_X9_value

min_X10_value = 15
max_X10_value = 100

#negation_X10_value = min_X10_value+max_X10_value-69
polynomial_X10_value= (69)^3.34
transformed_X10_value = (polynomial_X10_value-min_X10_value)/(max_X10_value-min_X10_value)
transformed_X10_value

min_X11_value = 0.9
max_X11_value = 9.4

#negation_X11_value = min_X11_value+max_X11_value-4.9
polynomial_X11_value= (4.9)^1.25
transformed_X11_value = (polynomial_X11_value-min_X11_value)/(max_X11_value-min_X11_value)
transformed_X11_value

new_input_tranformation = c(transformed_X6_value,transformed_X9_value,transformed_X10_value,transformed_X11_value)
write.table(new_input_tranformation,'prajith-y-transformed.txt')

# Applying the transformed variables to the best model(Choquet) selected from Q3 for Y prediction

x = c(new_input_tranformation)

v = c(0.289099857,0.417931082,0.838822694,0.173473207,0.37834619,0.449375478,1,0,0.597899884,0.417931082,0.838822694,0.173473207,1,0.449375478,1)

val = choquet(x,v)
val # value of the weights using Choquet

# Reverse Transformation of the Y variable.

min_Y_value = 0.006287804
max_Y_value = 0.02100974

reverse_minmax_scale = (val*(max_Y_value-min_Y_value))+min_Y_value
reverse_minmax_scale
reverse_poly_trans = reverse_minmax_scale^-0.9
predicted_val_y = reverse_poly_trans
predicted_val_y

#citation("lpSolve")

# Comparing the values of Y

"Predicted value of Y = 0.03486013"
"Measured value of  Y = 0.0146"

# References:

"'UCI Machine Learning Repository: Forest Fires Data Set'. Archive.ics.uci.edu. N.p., 2017,http://archive.ics.uci.edu/ml/datasets/forest+fires."

"Berkelaar M, others (2020). _lpSolve: Interface to 'Lp_solve' v. 5.5 to Solve Linear/Integer Programs_. R
  package version 5.6.15, <https://CRAN.R-project.org/package=lpSolve>."

"Inversing Power Functions: http://wmueller.com/precalculus/newfunc/invpwr.html"

"Simon James(2016) An Introduction to Data Analysis using Aggregation Functions in R,Springer, Deakin University Library, Melbourne"










