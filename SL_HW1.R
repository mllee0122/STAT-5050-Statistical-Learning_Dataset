#Statistical Learning_Homework1
#107064522

#8.(a)
college = read.csv("C:/Users/dozle/Desktop/College.csv")

#8.(b)
rownames (college) = college[,1]
fix (college)
college = college[,-1]
fix (college)

#8.(c) i.
summary(college)

#8.(c) ii.
pairs(college[,1:10])

#8.(c) iii.
plot(college$Private, college$Outstate, main = "Private versus Outstate")

#8.(c) iv.
Elite = rep("No", nrow(college))
fix(Elite)
Elite[college$Top10perc > 50] = "Yes"
Elite = as.factor(Elite)
college = data.frame(college, Elite)
summary(college$Elite)
plot(college$Elite, college$Outstate, main = "Elite versus Outstate")

#8.(c) v.
par(mfrow=c(2,2))
#college$Apps
hist(college$Apps, breaks = 10)
hist(college$Apps, breaks = 20)
hist(college$Apps, breaks = 30)
hist(college$Apps, breaks = 40)
#college$Accept
hist(college$Accept, breaks = 10)
hist(college$Accept, breaks = 20)
hist(college$Accept, breaks = 30)
hist(college$Accept, breaks = 40)
#college$Enroll
hist(college$Enroll, breaks = 10)
hist(college$Enroll, breaks = 20)
hist(college$Enroll, breaks = 30)
hist(college$Enroll, breaks = 40)
#college$Top10perc
hist(college$Top10perc, breaks = 10)
hist(college$Top10perc, breaks = 20)
hist(college$Top10perc, breaks = 30)
hist(college$Top10perc, breaks = 40)
#college$Top25perc
hist(college$Top25perc, breaks = 10)
hist(college$Top25perc, breaks = 20)
hist(college$Top25perc, breaks = 30)
hist(college$Top25perc, breaks = 40)
#college$F.Undergrad
hist(college$F.Undergrad, breaks = 10)
hist(college$F.Undergrad, breaks = 20)
hist(college$F.Undergrad, breaks = 30)
hist(college$F.Undergrad, breaks = 40)
#college$P.Undergrad
hist(college$P.Undergrad, breaks = 10)
hist(college$P.Undergrad, breaks = 20)
hist(college$P.Undergrad, breaks = 30)
hist(college$P.Undergrad, breaks = 40)
#college$Outstate
hist(college$Outstate, breaks = 10)
hist(college$Outstate, breaks = 20)
hist(college$Outstate, breaks = 40)
hist(college$Outstate, breaks = 80)
#college$Room.Board
hist(college$Room.Board, breaks = 10)
hist(college$Room.Board, breaks = 20)
hist(college$Room.Board, breaks = 40)
hist(college$Room.Board, breaks = 80)
#college$Books
hist(college$Books, breaks = 10)
hist(college$Books, breaks = 20)
hist(college$Books, breaks = 30)
hist(college$Books, breaks = 40)
##college$Personal
hist(college$Personal, breaks = 10)
hist(college$Personal, breaks = 20)
hist(college$Personal, breaks = 40)
hist(college$Personal, breaks = 80)
#college$PhD
hist(college$PhD, breaks = 10)
hist(college$PhD, breaks = 20)
hist(college$PhD, breaks = 40)
hist(college$PhD, breaks = 80)
#college$Terminal
hist(college$Terminal, breaks = 10)
hist(college$Terminal, breaks = 20)
hist(college$Terminal, breaks = 40)
hist(college$Terminal, breaks = 80)
#college$S.F.Ratio
hist(college$S.F.Ratio, breaks = 10)
hist(college$S.F.Ratio, breaks = 20)
hist(college$S.F.Ratio, breaks = 40)
hist(college$S.F.Ratio, breaks = 80)
#college$perc.alumni
hist(college$perc.alumni, breaks = 10)
hist(college$perc.alumni, breaks = 20)
hist(college$perc.alumni, breaks = 40)
hist(college$perc.alumni, breaks = 80)
#college$Expend
hist(college$Expend, breaks = 10)
hist(college$Expend, breaks = 20)
hist(college$Expend, breaks = 40)
hist(college$Expend, breaks = 80)
#college$Grad.Rate
hist(college$Grad.Rate, breaks = 10)
hist(college$Grad.Rate, breaks = 20)
hist(college$Grad.Rate, breaks = 40)
hist(college$Grad.Rate, breaks = 80)

#8.(c) vi.
plot(college$Enroll, college$F.Undergrad)
cor(college$Enroll, college$F.Undergrad)

summary(college$Terminal)
nrow(subset(college, Terminal > 92))
summary(subset(college, Terminal > 92))

####################################
#10.(a)
install.packages("MASS")
library(MASS)
Boston
?Boston
dim(Boston)

#10.(b)
pairs(Boston)

#10.(C)
cor(Boston, Boston$crim)

#10.(d)
par(mfrow=c(1,3))
#Boston$crim
hist(Boston$crim[Boston$crim > 0.08204], main = "Boston$crim > 1st Qu.")
hist(Boston$crim[Boston$crim > 0.25651], main = "Boston$crim > Median")
hist(Boston$crim[Boston$crim > 3.61352], main = "Boston$crim > Mean")
#Boston$tax
hist(Boston$tax[Boston$tax > 187], breaks = 30, main = "Boston$tax > Min.")
hist(Boston$tax[Boston$tax > 330], breaks = 30, main = "Boston$tax > Median")
hist(Boston$tax[Boston$tax > 408.2], breaks = 30, main = "Boston$tax > 3rd Qu.")
#Boston$ptratio
hist(Boston$ptratio[Boston$ptratio>12.6], breaks = 30, main = "Boston$tax > Min.")
hist(Boston$ptratio[Boston$ptratio>18.46], breaks = 30, main = "Boston$tax > Mean")
hist(Boston$ptratio[Boston$ptratio>20.2], breaks = 30, main = "Boston$tax > 3rd Qu.")

#10.(e)
dim(subset(Boston, chas == 1))
nrow(subset(Boston, chas == 1))

#10.(f)
median(Boston$ptratio)


#10.(g)
#Lowest Median Value
LMV = subset(Boston, medv == min(Boston$medv))
t(LMV)
summary(Boston)

#10.(h)
nrow(subset(Boston, rm > 7))
nrow(subset(Boston, rm > 8))
summary(subset(Boston, rm > 8))
summary(Boston)
