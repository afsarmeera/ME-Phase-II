library(dplyr)
#a$a.factor <- factor(a$a)
max = apply(a , 2 , max)
min = apply(a, 2 , min)
na.omit(max)
na.omit(min)
scaled = as.data.frame(scale(a, center = min, scale = max - min))
na.omit(a)
#a$a<-as.factor(a$a)
library(neuralnet)
na.omit(scaled)
set.seed(2)
NN = neuralnet(a ~ d+p+b ,scaled, hidden = 3 , linear.output = T,threshold = 0.01 )
plot(NN)

predict_testNN = compute(NN, scaled)

predict_testNN$net.result<-round(predict_testNN$net.result)

a$a<-predict_testNN$net.result

##############################################