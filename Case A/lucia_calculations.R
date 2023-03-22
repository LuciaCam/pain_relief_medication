library(data.table)

A = data.table("Promoted" = c(1,1,1,1,1,1,1,0,0,0), "Contributed" = c(1,1,1,1,1,1,0,0,0,0))
cor(A)

B = data.table("Feeling" = c(rep(1, 5), rep(0, 16), rep(NA, 4)),
               "Contributed" = c(1,1,1,1,NA, rep(0, 7), rep(NA, 9), 1,1,NA,NA))

cor(B, use="na.or.complete")

C = data.table("Answered_Contribution" = c(rep(1, 13), rep(0, 12)),
               "Answered_Feeling" = c(rep(0, 6), rep(1, 7), 0,0,0,rep(1,9)))

cor(C)