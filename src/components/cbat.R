#Vermeulen equation
#CBAT = calculated bioavailable testosterone
#Rcode from: https://labrtorian.com/tag/vermeulen-equation/
# where testosterone is reported in nmol/L (same as UKB), 
# sex hormone binding globulin (SHBG) is reported in nmol/L (same as UKB), 
# and albumin is reported in g/L (same as UKB)
# see also http://www.issam.ch/freetesto.htm which does the Vermeulen calc.
# Ruth et al 2020 does the Morris for BT and I agree.

cbat <- function(TT,SHBG,ALB = 43){
    Ka <- 3.6*10^4 #called kat in ruth
    Kshbg <- 10^9 #called kt in ruth
    N <- 1 + Ka*ALB/69000 #called k1 in ruth
    a <- N*Kshbg #called a in ruth
    b <- N + Kshbg*(SHBG - TT)/10^9 
    c <- -TT/10^9
    FT <- (-b + sqrt(b^2 - 4*a*c))/(2*a)*10^9
    cbat <- N*FT
    #return(list(free.T = FT, cbat = cbat))
    BT<-exp(-.266+.955*log(TT)-0.228 * log (SHBG)) #Morris equation
    return(list(freeTest = FT, bioavailableTest = BT))
}

# cbat(TT = 3.47, SHBG = 50, ALB = 40)
# 
# #3.47 nmol/L = 100.08 ng/dL
# 100.08/3.47 #[1] 28.8415
# #divide by 28.8 to go from ng/dL to nmol/L
# 
# #228.4 ng/dL
# 228.4/28.8
# 7.93
# 
# cbat(7.93, SHBG = 40)
# #0.1351578 nmol/L to ng/dL #conversion : http://unitslab.com/node/136
# 0.1351578*28.8
# 
# #free T in nmol/L
# TT1<-3.47
# SHBG1<-50
# 
# 
# BT<-exp(-.266+.955*log(TT1)-0.228 * log (SHBG1))
# BT