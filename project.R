acs15 <- read.csv("~/Downloads/us-census-demographic-data/acs2015_county_data.csv")

library(ggplot2)
library(usmap)
#plotting map information
names(acs15)[names(acs15) == "CensusId"] <- "fips"
p <- ggplot(acs15, aes(State, Professional))
p + geom_boxplot() + labs(title="Percent of office workers by state") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

p <- ggplot(acs15, aes(State, Construction))
p + geom_boxplot() + labs(title="Percent of construction workers by state") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

p <- ggplot(acs15, aes(State, MeanCommute))
p + geom_boxplot()+ labs(title="Mean Commute by State")+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



plot_usmap(data = acs15, values = "Construction") + 
  scale_fill_continuous(
    low = "white", high = "blue", name = "% working in construction", label = scales::comma
  ) + theme(legend.position = "right")



names(joined)[names(joined) == "CensusId"] <- "fips"

plot_usmap(data = joined, values = "per_dem") + 
  scale_fill_continuous(
    low = "white", high = "blue", name = "% voting Democrat", label = scales::comma
  ) + theme(legend.position = "right")

plot_usmap(data = acs15, values = "Professional") + 
  scale_fill_continuous(
    low = "white", high = "blue", name = "% working in professional jobs", label = scales::comma
  ) + theme(legend.position = "right")

plot_usmap(data = acs15, values = "White") + 
  scale_fill_continuous(
    low = "white", high = "blue", name = "% of white population", label = scales::comma
  ) + theme(legend.position = "right")


ggplot(data=train, 
       aes(x=White, y=Professional, col=party)) + geom_point()


plot(train$White, yWhite, pch = 16, xlab = "% of white population", ylab = "% voting for Democrat")
plot(train$Construction, yCons, pch = 16, xlab = "% of population working in construction", ylab = "% voting for Democrat")
plot(train$Transit, yPub, pch = 16, xlab = "% taking public transit", ylab = "% voting for Democrat")
plot(train$PrivateWork, yPriv, pch = 16, xlab = "% working in private sector", ylab = "% voting for Democrat")


lines(xnumeracy, ynumeracy, col = "red", lwd = 2)


plot_usmap(data = acs15, values = "Income") + 
  scale_fill_continuous(
    low = "white", high = "blue", name = "Income", label = scales::comma
  ) + theme(legend.position = "right")

plot_usmap(data = acs15, values = "TotalPop", color = "red") + 
  scale_fill_continuous(name = "Population (2015)", label = scales::comma) + 
  theme(legend.position = "right")

plot_usmap(data = acs15, values = "Construction") + 
  scale_fill_continuous(
    low = "white", high = "blue", name = "% in Construction", label = scales::comma
  ) + theme(legend.position = "right")

plot_usmap(data = acs15, values = "Office") + 
  scale_fill_continuous(
    low = "white", high = "blue", name = "% in Office Work", label = scales::comma
  ) + theme(legend.position = "right")

plot(acs15$Professional, acs15$Income)

plot(acs15$White, acs15$Income)

plot(acs15$Black, acs15$Income)

plot(acs15$Hispanic, acs15$Income)

plot(acs15$Asian, acs15$Income)

library(caret)

election <- read.csv("~/Documents/19-20/CSC441/us-census-demographic-data/2016_US_County_Level_Presidential_Results.csv")
#joining 2 table
joined = merge(acs15, election,  by.x='CensusId', by.y='combined_fips')
joined = na.omit(joined)

# removing variables
joined = joined[c(-2,-37, -47)]
joined = select(joined, -c(state_abbr, County, X))

joined$party = ifelse(joined$per_dem > 0.5, "Democrat", "Republican")

voted =  ifelse(joined$per_dem > 0.5, 1, 0)

vars = joined[c(3:36)]
vars$PercentCitizen = joined$Citizen/joined$TotalPop
vars$party = joined$party
vars$logpop = log10(joined$TotalPop)
vars$percentmen = joined$Men/(joined$Women+joined$Men)

vars = select(vars, -c(Men, Women, Citizen, IncomeErr, IncomePerCapErr, Employed))
  #vars[c(-1,-2,-3,-10,-12,-14,-29)]


set.seed(123)
#decision tree
dt = sort(base::sample(nrow(vars), nrow(vars)*.7))
train<-vars[dt,]
test<-vars[-dt,]

votingtree = rpart(party ~ ., data=train)
votingtree
rpart.plot(votingtree)
fancyRpartPlot(votingtree)
  



pred = predict(votingtree, newdata=test, type='class')

sum(test$party == pred) / length(pred)

votingtree2 = rpart(party ~ White + Construction + Transit + Income + percentmen +
                      PrivateWork, data=train)
rpart.plot(votingtree2)


pred2 = predict(votingtree2, newdata=test, type='class')

votedpred =  ifelse(pred2 == "Democrat", 1, 0)

retrieved <- sum(votedpred)
sum(test$party == pred2) / length(pred2)
pred2 = as.factor(predict(votingtree2, newdata=test, type='class'))
cm1 = confusionMatrix(pred2, as.factor(test$party), positive = "Democrat")
cm1
cm1$byClass

votedtr =  as.factor(ifelse(train$party == "Democrat", 1, 0))
votedte =  as.factor(ifelse(test$party == "Democrat", 1, 0))

discrete = train[c("White", "Construction", "Transit", "Income", "Native",
        "logpop", "MeanCommute")]

discrete = cut(train$White, breaks=0:7)

DiscX = cut(x, breaks=0:7)
#naive bayes
nb = naive_bayes(votedtr ~ White + Construction + Transit + Income + percentmen +
                  PrivateWork, data=train, usekernel = TRUE)
summary(nb)
nb

pred3 = predict(nb, test,  type = "prob" )
pred3 = ifelse(pred3[,2] >0.5, "Democrat", "Republican")
cm2 = confusionMatrix(table(pred3, test$party), positive = "Democrat")
cm2
cm2$byClass


#logistic
logvartrain = na.omit(as.matrix(train[c("White", "Construction", "Transit", "Income", "Native",
                        "logpop", "MeanCommute")]))

logvote <- glm(votedtr ~ White + Construction + Transit + Income + percentmen +
                 PrivateWork, data = train, family = "binomial")
summary(logvote)


vif(logvote2)

vnum = as.numeric(votedtr) 

pr <- knn(train,test,cl=vnum,k=5)



ggplot(data=train, 
       aes(x=White, y=Construction, col=party)) + geom_point()

ggplot(data=train, 
       aes(x=White, y=Transit, col=party)) + geom_point()


logvote2 <- glm(formula = votedtr ~ White + Construction + Transit +
                  PrivateWork, family = "binomial", data=train)
summary(logvote2)
detach(train)
train2 = train[,c("White", "Construction", "Transit", "PrivateWork")]
train2$votetr = as.numeric(votedtr)-1


pred4 = predict(logwhite, test, type="response")
pred4part = ifelse(pred4 >= 0.5, "Democrat", "Republican")
cm3 = confusionMatrix(table(pred4part, test$party))
cm3$byClass
cm3

pred5 = predict(logvote2, test, type="response")
pred5part = ifelse(pred5 >= 0.5, "Democrat", "Republican")
cm4 = confusionMatrix(as.factor(pred5part), as.factor(test$party), positive = "Democrat")
cm4
cm4$byClass

