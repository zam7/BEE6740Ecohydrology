# Biological Processes 2018
# In Class Assignment PS3

setwd("~/github/BEE6740Ecohydrology")

Q = 10^8 # L/d
S_o = 200 # mg BOD/L
Xv_o = 100 # mg VSS/L
fd_o = 0.8 # fraction of Xv_o
fd = 0.8 # fraction of Xv
Y = 0.5 # g Xa / g BOD
b = 0.1 # 1/day
q_hat = 8 # g BOD / (g Xa * day)
K = 25 # mg BOD / L

theta_x_min = (K + S_o)/(S_o*(Y*q_hat-b)-b*K) # days
theta_x_max = 20 # days
theta_array = seq(theta_x_min,theta_x_max,length = 100) # days

Xi_o = (1-fd_o)*Xv_o # mg incalcitrant VSS/L
btheta = b*theta_array # unitless
fg = 1-fd_o

#Qw_Xvw_mgperd = Q*(Xi_o + Y*(S_o-K*((1+b*theta_array)/(Y*q_hat*theta_array - (1+b*theta_array))))
           # * (1+(1-fd)*b*theta_array)/(1+b*theta_array)) # mg VSS/day

fun = 1+(b*theta_array)
term1 = 1+((1-fd_o)*(b*theta_array)/(fun))
term2 = S_o - ((K*fun)/((Y*q_hat*theta_array)-(fun)))
combo = Q*(Xi_o + (Y*term1*term2))
Qw_Xvw_kgperd = combo / 1000000 # kg VSS/day

Qw_Xaw_mgperd = fd*Q*Y*(1/(1+btheta))*(S_o - K*(1+btheta)/(Y*q_hat*theta_array - (1+btheta)))
Qw_Xaw_kgperd = Qw_Xaw_mgperd / 1000000 # kg VSS/day


plot(theta_array, Qw_Xvw_kgperd, xlab = "theta (days)", ylab = "biomass wasted (kg/day)", main = "kg biomass wasted per day as a function of solids retention time")
points(theta_array, Qw_Xaw_kgperd, pch = 17)
legend( x= "right", y=0.92, 
        legend=c("Xv in waste","Xa in waste"),
        pch=c(21, 17))


