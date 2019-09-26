
# To do survival analysis we have to install a package called survival



# install.packages("survival")

library(survival)


my_lung_data <- lung


re_adjusted_status <- function(x){
  if(x==1){rs=0}
  if(x==2){rs=1}
  return(rs)
}


# creating a new column
for(x in 1:length(my_lung_data$status)){
  my_lung_data$re_adjust_status[x] <- re_adjusted_status(my_lung_data$status[x])
}


head(my_lung_data)


my_lung_data


my_surv_object <- Surv(time = my_lung_data$time,event = my_lung_data$re_adjust_status)


class(my_surv_object)


# Printing the surv object compeletely
my_surv_object



# Printing the head of the my_surv_object
head(my_surv_object)



# plotting kaplan meier curve with single line only....
# 1 signifies tha single curve will be used for all patients in the dataset
my_curve_fit <- survfit(my_surv_object~1)


# This will give lots of data
my_curve_fit


# median of time
# median of time and above median are not same ....
# above median is something else

median(my_lung_data$time)



# Lets see what above median is 

# median survival time is the time at which survivorship function is 0.5 
plot(my_curve_fit)


# We dont want to plot lower and upper confidence....
plot(my_curve_fit,conf.int = "none", main = "Kaplan Mier Curve", ylab = "SurvivorShip")



# To see the median of the curve at which survivorshipt function will be 0.5
abline(h=0.5)
abline(v=310)

abline(v=285)
abline(v=363)

my_curve_fit_gender <- survfit(my_surv_object~my_lung_data$sex)


my_curve_fit_gender


plot(my_curve_fit_gender)



# For seperately identifying the Kaplan Mier Curve for Gender
plot(my_curve_fit_gender, col = c("green","blue"))



plot(my_curve_fit_gender, col = c("green","blue"),conf.int = "both")




# Tick marks for all the censoring....
plot(my_curve_fit_gender, col = c("green","blue"),mark.time = T)



# lty is used for bringing lines in front of legend....
legend("topright", c("MALE","FEMALE"), col = c("green","blue"),lty = 1)

abline(h=0.5)
abline(v=270, col = "green")
abline(v=426, col="blue")



# Performing Logrank Test to test the statistical difference between the two genders....
survdiff(my_surv_object~my_lung_data$sex)


# p value is very less than .05 hence there is statistical difference 
# Also the chi square value is very much too
# We can see the chi square table of degree of freedom and p value



# inverse of the curve can also be plotted 
plot(my_curve_fit_gender, col = c("green","blue"),mark.time = T, fun = "event")



