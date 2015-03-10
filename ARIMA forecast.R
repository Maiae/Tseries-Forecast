#### DEMAND FORECAST OF MULTIPLE TIME SERIES ###
#### input the time series object: datats ####
#### Created by Eduardo Maia ####
library(forecast)
library(tseries)


# ADF test for stationarity
# Small p-values suggest the data is stationary (mean does not change drastically over time) and does not need to be differenced
# use as an input to ARIMA model
isStationary <- function (n) {
	0.05 > adf.test(tsclean(datats[,n], replace.missing=TRUE), alternative="stationary")$p.value
}
isStationary(2)

ptm <- proc.time()
stationary.series <- logical(60)
for(i in 1:60){
	tryCatch({
		stationary.series[i] <- isStationary(i)
	}, error=function(e){cat("ERROR in series", i,":", conditionMessage(e), "\n")	} )
}
stationary.series <- as.data.frame(stationary.series)
proc.time() - ptm


# run auto ARIMA with seasonal dummy variables for weekly cycles starting on Monday
# use stationary indicator as input
arima.fit <- function(n) {
	weekDay <- seasonaldummy(tsclean(datats[,n], replace.missing=TRUE));
	auto.arima(tsclean(datats[,n], replace.missing=TRUE), ic="aic", stepwise=TRUE, xreg=weekDay, stationary=stationary.series[n,] )
}



# ARIMA forecast function using future seasonal dummy variables for weekly cycles starting on Monday
# specify horizon for forecast using multiple of 7 days
# horizon=7 will produce a 7-step ahead forecast
arima.fc <- function(n, horizon){
	weekDayf <- (seasonaldummy(datats))[2:(horizon+1),];
	forecast(arima.fit(n), h=horizon, xreg=weekDayf)
}



# Forecast Quality
# create vector and populate with MASE (Mean Absolute Scaled Error)
# label forecast according to in-sample MASE metric
ptm <- proc.time()
MASE <- numeric(60)
for (i in 1:60) {
	tryCatch({
		MASE[i] <- accuracy(arima.fit(i))[,6]
	},  error=function(e){cat("ERROR in series", i,":", conditionMessage(e), "\n")	})
}
View(MASE)

forecast.quality <- data.frame(cbind(route=as.vector(colnames(datats)), MASE, 
																		 quality=ifelse(MASE<=0.2500, "GOOD", ifelse((MASE<=0.5000 & MASE>0.2500), "OK", "POOR"))
))
print(forecast.quality)
proc.time() - ptm



# Forecast Plot Function
arima.fc.plot <- function(n, horizon, i) {
	plot(arima.fc(n, horizon), include=i, type="o", pch=20,
			 main=paste(horizon,"-step ahead Net Draw Forecast (route ", n, ")", sep=""),
			 ylab="Net Draw", xlab="Week")
	mtext( paste(colnames(datats)[n], "     MASE=", round(MASE[n], 3),"     Forecast Quality=", forecast.quality[n,3] ))
	grid(NA, 5, lty = 6, col="lightgray")
}


#########################
###  Forecast Output  ###
#########################
run.output <- function(n, horizon) {
				fc <- arima.fc(n, horizon);
					cbind(route=rep(as.character(forecast.quality$route[n]), horizon),
								quality=rep(as.character(forecast.quality$quality[n]), horizon),
								date=format(seq(max(route_net_draw$date)+1, (max(route_net_draw$date)+horizon), "days"), format="%Y-%m-%d"),
								forecast=round(fc$mean, 0),
								lower=round(fc$lower, 0),
								upper=round(fc$upper, 0)
								)
					}
View(output)
run.output(15, 7)

ptm <- proc.time()
output <- data.frame()
	for (i in 1:60)  {
				tryCatch({
					output <- rbind(output, run.output(i, 7));
				},  error=function(e){cat("ERROR in series", i,":", conditionMessage(e), "\n")	})
	}
proc.time() - ptm


# write output to csv
	write.csv(output, file=paste("Net_Draw_Forecast_",output$date[1],"_to_",output$date[7],".csv", sep=""), row.names=FALSE, na="")



#################################
###  FORECAST TEST AND PLOTS  ###
#################################
forecast.test <- function (x) {
			fit <- arima.fit(x);
				summary(fit);
			fc <- arima.fc(x, 6);
				print(fc);
			plot(fc);
				lines(fitted(fit), col="red");
			arima.fc.plot(x, 6, 60);
				lines(week34ts[,x]);
			AIC(fit);
			tsdisplay((fc$residuals), plot.type="partial", points="TRUE", pch=20, col="dark blue", main=(colnames(datats)[x]));
			accuracy(fit)[,6];
		}
