#AFSI_16
cap2x=cap2[-1]
reer2x=reer2[-1]
loans2x=loans2[-1]
deposit2x=deposit2[-1]
wi2x=wi2[-1]

DI=(cap2x[-(26:28)]+diff(credit2)[-(26:28)]+diff(is2)[-(26:28)])/3
VI=(diff(if2)[-(26:28)]+diff(cad2)[-(26:28)]+reer2x[-(26:28)]+diff(pcredit)[-(26:28)]+loans2x[-(26:28)]+deposit2x[-(26:28)])/6
WI=(diff(wg2)[-(26:28)]+diff(wc2)[-(26:28)]+wi2x[-(26:28)])/3




AFSI_16=diff(DI)*3/12+diff(VI)*6/12+diff(WI)*3/12


auto.arima(AFSI_16)
plot(ts(AFSI_16))
modelx1 <- arima(AFSI_16, c(1,0,0)) 
summary(modelx1)
modelx1_pred<-forecast(modelx1,h=4)
plot(modelx1_pred)

#diff model_16
auto.arima(diff(AFSI_16))
plot(ts(diff(AFSI_16)))
modelx1.d <- arima(diff(AFSI_16, c(0,0,1))) 
summary(modelx1.d)
modelx1.d_pred<-forecast(modelx1.d,h=4)
plot(modelx1.d_pred)


#AFSI_17
f.DI=(cap2[-1]+diff(credit2)+diff(is2))/3
f.VI=(diff(if2)+diff(cad2)+reer2[-1]+diff(pcredit)+loans2[-1]+deposit2[-1])/6
f.WI=(diff(wg2)+diff(wc2)+wi2[-1])/3

AFSI_17=diff(f.DI)*3/12+diff(f.VI)*6/12+diff(f.WI)*3/12

auto.arima(AFSI_17) #(2,0,0)
plot(ts(AFSI_17))
modelx2 <- arima(AFSI_17, c(2,0,0)) 
summary(modelx2)
modelx2_pred<-forecast(modelx2,h=4)
plot(modelx2_pred)
adf.test(AFSI_17)


#diff
auto.arima(AFSI_17) #(0,1,1)
plot(ts(AFSI_17))
modelx2 <- arima(AFSI_17, c(0,0,1)) 
summary(modelx2)
modelx2_pred<-forecast(modelx2,h=4)
plot(modelx2_pred)