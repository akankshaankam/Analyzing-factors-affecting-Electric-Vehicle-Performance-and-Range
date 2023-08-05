getwd()
install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)
data <- read.csv("Electric_Vehicle_Population_Data.csv", header = TRUE)
View(data)
set.seed(1000)
s_data <- sample_n(data, 1000)
s_data
View(s_data)
attach(s_data)

#3
summary(s_data)
summary(s_data$Model.Year)
summary(s_data$Electric.Range)

boxplot(s_data$Electric.Range ~ s_data$Model.Year, data=data,
        main="Boxplot of Electric Range vs Model Year",
        xlab="Model Year", ylab="Electric Range",
        col="lightblue", border="black")


#scatterplot
plot(Model.Year, Electric.Range, 
     main="Electric Range vs Model Year by make", 
     xlab="Model Year", 
     ylab="Electric Range (Miles)", xlim=c(2010,2024), 
     ylim=c(5,350), pch=18, col="blue", cex.lab=1.2)

cor(Model.Year,Electric.Range)



##
# Create dummy variables for Electric Vehicle Type
ev_type_dummies <- model.matrix(~ s_data$Electric.Vehicle.Type - 1)
CAFV_Eligible <- ifelse(s_data$Clean.Alternative.Fuel.Vehicle..CAFV..Eligibility
                        == "Clean Alternative Fuel Vehicle Eligible", 1, 0)
mm <- lm(s_data$Electric.Range ~ s_data$Model.Year
         + s_data$Clean.Alternative.Fuel.Vehicle..CAFV..Eligibility,
         data = s_data)
summary(mm)



##
model <- lm(s_data$Electric.Range ~ s_data$Model.Year 
            + s_data$Clean.Alternative.Fuel.Vehicle..CAFV..Eligibility, data = s_data)
summary(model)


##
resid_model <- resid(model)
fitted_m <- fitted(model)

par(mfrow = c(2, 2))
plot(fitted_m, resid_model, axes = TRUE, frame.plot = TRUE, xlab = 'Fitted Values', ylab = 'Residuals')
abline(h = 0, col = "red")
hist(resid_model)



##
qf(0.95,df1=2,df2=997)
summary(model)


##
install.packages("car")
library(car)
m <- lm(data$Electric.Range ~ data$Model.Year + data$Clean.Alternative.Fuel.Vehicle..CAFV..Eligibility
        + data$Model.Year * data$Clean.Alternative.Fuel.Vehicle..CAFV..Eligibility)
summary(m)
Anova(m, type=3)
interaction.plot(data$Model.Year, data$Clean.Alternative.Fuel.Vehicle..CAFV..Eligibility,data$Electric.Range, col=1:2)


##
two_way_aov <- aov(data$Electric.Range ~ data$Model.Year + data$Clean.Alternative.Fuel.Vehicle..CAFV..Eligibility, data = s_data)
summary(two_way_aov)
plot(two_way_aov, 1:4)


alpha <- 0.05
df1 <- 1 
df2 <- 998 
qf(0.95, df1, df2)



