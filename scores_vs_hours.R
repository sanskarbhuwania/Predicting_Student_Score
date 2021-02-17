df <- read.csv('data.csv')
print(head(df))

# visualizing the data
library(ggplot2)
pl <- ggplot (data=df, aes (x=Hours, y=Scores)) + geom_point(shape='triangle') + geom_smooth()
print(pl)

# finding all the numeric columns
num.col <- sapply(df,is.numeric)
print(num.col)

# finding correlations between columns
cor.col <- cor(df[,num.col])   # columns hours and scores are closely related
print(cor.col)
  # visualizing the correlation
  library(corrplot)
  corrplot(cor.col, method = 'color') # columns hours and scores are closely related

# creating model for linear regression
model <- lm(Scores~Hours, data = df)
#print(summary(model))
plot(model)

Scores.predicted <- predict(model,df)
compare <- data.frame(Scores.predicted,df$Scores)
colnames(compare) <- c("predicted", "Actual")
print(head(compare))

result1 <- as.data.frame(9.25)
colnames(result1) <- "Hours"
result <- predict(model, result1)
print (paste("Score predicted for Hours - 9.25 is:", as.vector(result)))

# Mean-square and Root-square error
mse <- mean ((compare$Actual - compare$predicted)^2)
print (paste("Mean Square Error is:", mse))
rmse <- mse^0.5
print (paste("Root-mean square error is:", rmse)) 

# R2
sse <- sum ((compare$Actual - compare$predicted)^2)
sst <- sum ((mean(df$Scores) - compare$Actual)^2)
R2 <- 1- sse/sst
print(paste("R2 is:", R2))