bw_lm = lm(Hotwings~Beer, data=Beerwings)
summary(bw_lm)

#validation metrics
y = Beerwings$Hotwings
yhat = predict(bw_lm, Beerwings)

n = nrow(Beerwings)
SSE = sum((y - yhat)^2)
MSE = SSE/n
RMSE = sqrt(MSE)
MAD = mean(abs(y-yhat))
SST = sum((y-mean(y))^2)
R2 = 1 - SSE/SST

c(SSE, MSE, RMSE, MAD, R2)

#predicting new observations

newdata = data.frame(Beer=16)

predict(bw_lm, newdata, interval = "confidence")

#training testing validation 
training_indices = sample(n, round(0.7*n, 0))
training_set = Beerwings[training_indices,]
test_set = Beerwings[-training_indices,]

train_lm = lm(Hotwings~Beer, data = training_set)
ytest = test_set$Hotwings
yhattest = predict(train_lm, test_set)
mse_test = mean((ytest - yhattest)^2)
c(mse_test)



