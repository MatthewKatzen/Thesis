library(lpSolve)

constr_A <- matrix(data = 
                       c(1,1,1,
                         2/3,1/3,0,
                         1/3,-1/3,0,
                         1/3,-1/3,0,
                         1,0,0,
                         0,1,0,
                         0,0,1), 
                   nrow = 7, byrow = TRUE)
constr_B <- matrix(data = c(1500, 400, 100, -100, 700, 700, 700))

constr_equal <- c("=", "<=", "<=", ">=", "<=", "<=", "<=")

objective_coeffs <- c(80, 100, 200)

output <- lp(direction="min",
   objective.in = objective_coeffs,
   const.mat = constr_A,
   const.dir = constr_equal,
   const.rhs = constr_B)

output$solution

#add firm z at location i

constr_total <- matrix(data = 
                           c(1,1,1,1,"=",1500,
                             2/3,1/3,0,2/3,"<=",400,
                             1/3,-1/3,0,1/3,"<=",100,
                             1/3,-1/3,0,1/3,">=",-100,
                             1,0,0,0,"<=",600,
                             0,1,0,0,"<=",600,
                             0,0,1,0,"<=",600,
                             0,0,0,1,"<=",200),
                       nrow = 8, byrow = TRUE)
constr_A <- constr_total[,1:4]
constr_equal <- constr_total[,5]
constr_B <- constr_total[,6]

objective_coeffs <- c(80, 100, 200, 70)

output1 <- lp(direction="min",
             objective.in = objective_coeffs,
             const.mat = constr_A,
             const.dir = constr_equal,
             const.rhs = constr_B)
output1$solution
output1$solution * c(80, 100, 200, 80) #profits
sum(output1$solution * objective_coeffs)#market cost
sum(output1$solution * c(80, 100, 200, 80)) #market payout


#add firm z at location j
constr_total <- matrix(data = 
                           c(1,1,1,1,"=",1500,
                             2/3,1/3,0,1/3,"<=",400,
                             1/3,-1/3,0,-1/3,"<=",100,
                             1/3,-1/3,0,-1/3,">=",-100,
                             1,0,0,0,"<=",600,
                             0,1,0,0,"<=",600,
                             0,0,1,0,"<=",600,
                             0,0,0,1,"<=",200),
                       nrow = 8, byrow = TRUE)
constr_A <- constr_total[,1:4]
constr_equal <- constr_total[,5]
constr_B <- constr_total[,6]

objective_coeffs <- c(80, 100, 200, 70)

output2 <- lp(direction="min",
             objective.in = objective_coeffs,
             const.mat = constr_A,
             const.dir = constr_equal,
             const.rhs = constr_B)
output2$solution
output2$solution * c(80, 100, 200, 100) #profits
sum(output2$solution * objective_coeffs)#market cost
sum(output2$solution * c(80, 100, 200, 100)) #market payout


#add firm z at location k
constr_total <- matrix(data = 
                           c(1,1,1,1,"=",1500,
                             2/3,1/3,0,0,"<=",400,
                             1/3,-1/3,0,0,"<=",100,
                             1/3,-1/3,0,0,">=",-100,
                             1,0,0,0,"<=",2000,
                             0,1,0,0,"<=",2000,
                             0,0,1,0,"<=",2000,
                             0,0,0,1,"<=",0),
                       nrow = 8, byrow = TRUE)
constr_A <- constr_total[,1:4]
constr_equal <- constr_total[,5]
constr_B <- constr_total[,6]

objective_coeffs <- c(80, 100, 110, 70)

output3 <- lp(direction="min",
              objective.in = objective_coeffs,
              const.mat = constr_A,
              const.dir = constr_equal,
              const.rhs = constr_B)
output3$solution
output3$solution * c(80, 100, 110, 110) #profits
sum(output3$solution * objective_coeffs)#market cost
sum(output3$solution * c(80, 100, 110, 110)) #market payout



constr_total <- matrix(data = 
                           c(1,1,1,1"=",1500,
                             2/3,1/3,0,0,"<=",400,
                             1/3,-1/3,0,0,"<=",100,
                             1/3,-1/3,0,0,">=",-100,
                             0,0,2/3,1/3,"<=",700,
                             0,0,1/3,-1/3,"<=",100,
                             0,0,1/3,-1/3,">=",-100,
                             1,0,0,0,"<=",2000,
                             0,1,0,0,"<=",2000,
                             0,0,1,0,"<=",2000,
                             0,0,0,1,"<=",2000),
                       nrow = 11, byrow = TRUE)
constr_A <- constr_total[,1:4]
constr_equal <- constr_total[,5]
constr_B <- constr_total[,6]

objective_coeffs <- c(60, 70, 80, 90)

output3 <- lp(direction="min",
              objective.in = objective_coeffs,
              const.mat = constr_A,
              const.dir = constr_equal,
              const.rhs = constr_B)
output3$solution
output3$solution * c(80, 100, 110, 110) #profits
sum(output3$solution * objective_coeffs)#market cost
sum(output3$solution * c(80, 100, 110, 110)) #market payout
