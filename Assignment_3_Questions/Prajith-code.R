median(2,5,9,9,9,18,23,26,42,76)
# Solution for Question No: 1

Cheese_Factory_Model <- make.lp(0, 2) # Decision Variables with A and B

lp.control(Cheese_Factory_Model, sense= "minimize")

set.objfn(Cheese_Factory_Model, c(5,8)) #  min z = 5A + 8B

#set.objfn(Cheese_Factory_Model, c(0,8)) #  The cost of A can be kept from 0 AUD to 7.9 AUD while having the same optimal points

#set.objfn(Cheese_Factory_Model, c(7.9,8)) #  min z = 5A + 8B

add.constraint(Cheese_Factory_Model, c(60-45,40-45), ">=", 0) # 60A + 40B >= 45(A+B)

add.constraint(Cheese_Factory_Model, c(40-50,70-50), ">=", 0) # 40A + 70B >= 50(A+B)

add.constraint(Cheese_Factory_Model, c(30-60,80-60), "<=", 0) # 30A + 80B <= 60(A+B)

add.constraint(Cheese_Factory_Model, c(1,1), ">=", 60) # A + B >= 60

set.bounds(Cheese_Factory_Model, lower = c(0,0), columns = c(1, 2)) # A , B >= 0

set.bounds(Cheese_Factory_Model, upper = c(Inf,Inf), columns = c(1, 2))

RowNames <- c("Cow", "Goat" , "Sheep", "Constraint 1")

ColNames <- c("A", "B")

dimnames(Cheese_Factory_Model) <- list(RowNames, ColNames)

Cheese_Factory_Model

solve(Cheese_Factory_Model) 

get.objective(Cheese_Factory_Model)

get.variables(Cheese_Factory_Model)

get.constraints(Cheese_Factory_Model) 


# Solution for Question No: 2

Cereal_Model <- make.lp(19, 12)

lp.control(Cereal_Model, sense= "maximize")

set.objfn(Cereal_Model, c(2396,1897.2,3397,2376,1877.2,3377,2416,1917.2,3417,2296,1797.2,3297))

#Min Demand constraint
set.row(Cereal_Model, 1, c(1,1,1,1), indices = c(1,4,7,10))
set.row(Cereal_Model, 2, c(1,1,1,1), indices = c(2,5,8,11))
set.row(Cereal_Model, 3, c(1,1,1,1), indices = c(3,6,9,12))

#Max availability constraint
set.row(Cereal_Model, 4, c(1,1,1), indices = c(1,2,3))
set.row(Cereal_Model, 5, c(1,1,1), indices = c(4,5,6))
set.row(Cereal_Model, 6, c(1,1,1), indices = c(7,8,9))
set.row(Cereal_Model, 7, c(1,1,1), indices = c(10,11,12))

#Proportion constraint
set.row(Cereal_Model, 8,  c(0.2,-0.8,-0.8,-0.8),     indices = c(1,4,7,10))
set.row(Cereal_Model, 9,  c(-0.1,0.9,-0.1,-0.1),     indices = c(1,4,7,10))
set.row(Cereal_Model, 10, c(-0.05,-0.05,0.95,-0.05), indices = c(1,4,7,10))
set.row(Cereal_Model, 11, c(-0.05,-0.05,-0.05,0.95), indices = c(1,4,7,10))
set.row(Cereal_Model, 12, c(0.35,-0.65,-0.65,-0.65), indices = c(2,5,8,11))
set.row(Cereal_Model, 13, c(-0.2,0.8,-0.2,-0.2),     indices = c(2,5,8,11))
set.row(Cereal_Model, 14, c(-0.05,-0.05,0.95,-0.05), indices = c(2,5,8,11))
set.row(Cereal_Model, 15, c(-0.1,-0.1,-0.1,0.9),     indices = c(2,5,8,11))
set.row(Cereal_Model, 16, c(0.5,-0.5,-0.5,-0.5),     indices = c(3,6,9,12))
set.row(Cereal_Model, 17, c(-0.1,0.9,-0.1,-0.1),     indices = c(3,6,9,12))
set.row(Cereal_Model, 18, c(-0.1,-0.1,0.9,-0.1),     indices = c(3,6,9,12))
set.row(Cereal_Model, 19, c(-0.3,-0.3,-0.3,0.7),     indices = c(3,6,9,12))

set.rhs(Cereal_Model, c(1,0.7,0.75,10,5,2,2,0,0,0,0,0,0,0,0,0,0,0,0 ))

set.constr.type(Cereal_Model, c(">=",	">=",	">=","<=","<=","<=","<=","=","=","=","=","=","=","=","=","=","=","=","="))

set.type(Cereal_Model, c(1:12),"real")

set.bounds(Cereal_Model, lower = rep(0, 12), upper = rep(Inf, 12))

solve(Cereal_Model)

objvalue<-get.objective(Cereal_Model)
objvalue
solution<-get.variables(Cereal_Model)
solution

# Solution for Question No: 3

Bidding_Model <- make.lp(0, 6) #x1 x2 x3 x4 x5 v

lp.control(Bidding_Model, sense= "maximize") #  

set.objfn(Bidding_Model, c(0, 0, 0, 0, 0, 1)) 

add.constraint(Bidding_Model, c(-1, -1, -1, -1, 1, 1), "<=", 0)

add.constraint(Bidding_Model, c(1, -1, -1, -1, 1, 1), "<=", 0)

add.constraint(Bidding_Model, c(1, 1, -1, -1, 1, 1), "<=", 0)

add.constraint(Bidding_Model, c(1, 1, 1, -1, 1, 1), "<=", 0)

add.constraint(Bidding_Model, c(1, 1, 1, 1, -1, 1), "<=", 0)

add.constraint(Bidding_Model, c(1,1,1,1,1,0), "=", 1)

set.bounds(Bidding_Model, lower = c(0, 0, 0, 0, 0, -Inf))

RowNames <- c("Row1", "Row2", "Row3", "Row4", "Row5","Row6")

ColNames <- c("x1", "x2", "x3", "x4", "x5","v")

dimnames(Bidding_Model) <- list(RowNames, ColNames)

solve(Bidding_Model) 

get.objective(Bidding_Model)

get.variables(Bidding_Model)

get.constraints(Bidding_Model)


Bidding_Model 

# Player II's game #

Bidding_Model <- make.lp(0, 6) #y1 y2 y3 y4 y5 v

lp.control(Bidding_Model, sense= "minimize") #  

set.objfn(Bidding_Model, c(0, 0, 0, 0, 0, 1)) 

add.constraint(Bidding_Model, c(-1, 1, 1, 1, 1, 1), ">=", 0)

add.constraint(Bidding_Model, c(-1, -1, 1, 1, 1, 1), ">=", 0)

add.constraint(Bidding_Model, c(-1, -1, -1, 1, 1, 1), ">=", 0)

add.constraint(Bidding_Model, c(-1, -1, -1, -1, 1, 1), ">=", 0)

add.constraint(Bidding_Model, c(1, 1, 1, 1, -1, 1), ">=", 0)

add.constraint(Bidding_Model, c(1,1,1,1,1,0), "=", 1)

set.bounds(Bidding_Model, lower = c(0, 0, 0, 0, 0, -Inf))

RowNames <- c("Row1", "Row2", "Row3", "Row4", "Row5","Row6")

ColNames <- c("y1", "y2", "y3", "y4", "y5","v")

dimnames(Bidding_Model) <- list(RowNames, ColNames)

solve(Bidding_Model) 

get.objective(Bidding_Model)

get.variables(Bidding_Model)

get.constraints(Bidding_Model)

Bidding_Model 

#Reference:

# "Wayne L.Winston, Operations Research: Applications and Algorithms, Deakin University Library, Melbourne”
# "Hamdy A. Taha, Operations Research: An Introduction, Deakin University Library, Melbourne”
