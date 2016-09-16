x <- read.csv("data_2015_june/BernaAllReservation_2011_20150630.csv")
table(x$BOOKING_CHANNEL)

internet_channels <- c("HOTEL.DE AG", "HRS-HOTEL RESERVATION SERVICE", "INITALIA.IT",
                       "Laterooms", "LATEROOMS LTD", "Orbitz", "ORBITZ", "ORBITZ WORLDWIDE INC.",
                       "VENERE NET", "AGODA COMPANY PTE, LTD", "ALESIA DISTRIBUTION",
                       "EASYTOBOOK.COM", "HOTELREZ LTD", "LASTMINUTE.COM", "Lastminute (GHE)",
                       "RATESTOGO", "TRANSHOTEL", "TRANSHOTEL.", "TRAVEL CONSULT OY", "TRAVELEUROPE",
                       "WOTIF.COM")
gds_channels <- c("Sabre", "GALILEO", "Amadeus", "Worldspan", "Pegasus")
expedia <- c("EXPEDIA (HC) TRAVEL CORPORATED", "EXPEDIA TRAVEL CORPORATED")
hp <- c("HOME PAGE", "Website")
cc <- "Call Center Berna"
new_levels <- c("", "bookings europe b.v.", "hotel berna milano", "home page", 
                "other internet channels", "gds", "expedia", "Call Center Berna")
levs <- sapply(c(as.list(new_levels[1:3]), list(hp), list(internet_channels), list(gds_channels), 
                 list(expedia), list(cc)), function(g) {
                   u <- rep(FALSE, nlevels(x$BOOKING_CHANNEL))
                   u[tolower(levels(x$BOOKING_CHANNEL)) %in% tolower(g)] <- TRUE
                   u
})
levels(x$BOOKING_CHANNEL) <- c(NA, new_levels)[levs %*% seq_len(ncol(levs)) + 1]
X <- x[, c(1, 13:14, 24)]
X$ARRIVALDATE <- as.Date(X$ARRIVALDATE)
X$DEPARTUREDATE <- as.Date(X$DEPARTUREDATE)

library(zoo)
X$Month <- as.yearmon(X$ARRIVALDATE)

prop_ch <- sapply(unique(X$Month), function(m) {
  f <- X[X$Month == m , "BOOKING_CHANNEL"]
  prop.table(table(f))
})
colnames(prop_ch) <- unique(X$Month)
Z <- as.data.frame(t(prop_ch))
Z$Month <- as.yearmon(as.numeric(row.names(Z)))
names(Z)

colnames(Z)[1] <- "Missing"
#Z <- Z[ , -c(1,3)]
Z <- Z[order(Z$Month) , ]

###############
##Graphs

xrange <- range(Z$Month)
yrange <- range(0, 0.6)
plot(xrange, yrange, type="n", xlab="Time (Months)", ylab="Percentage") 
colors <- rainbow(length(colnames(Z))-1)
linetype <- 1:(length(colnames(Z))-1)
plotchar <- seq(18,18+(length(colnames(Z))-2),1)
for (i in 1:(length(colnames(Z))-1)) {
  lines(Z$Month, Z[ , i], type="b", lwd=2,
      lty=linetype[i], col=colors[i]) 
}
title("Reservations by Channel")
for (i in 1:(length(colnames(Z))-1)) {
  legend("topleft", colnames(Z)[1:(length(colnames(Z))-1)], cex=0.7, col=colors, lwd=2,
         lty=linetype, bty = "n")
}


Z$OTA <- Z$`bookings europe b.v.` + Z$expedia +Z$`other internet channels`
Z$direct <- Z$`home page` + Z$`hotel berna milano` + Z$`Call Center Berna`
Z <- Z[c(1, 3, 10, 11, 9)]


xrange <- range(Z$Month)
yrange <- range(0, max(Z[ , 1:4]))
plot(xrange, yrange, type="n", xlab="Time (Months)", ylab="Percentage") 
colors <- rainbow(length(colnames(Z))-1)
linetype <- 1:(length(colnames(Z))-1)
plotchar <- seq(18,18+(length(colnames(Z))-2),1)
for (i in 1:(length(colnames(Z))-1)) {
  lines(Z$Month, Z[ , i], type="b", lwd=2,
        lty=linetype[i], col=colors[i]) 
}
title("Reservations by Channel")
for (i in 1:(length(colnames(Z))-1)) {
  legend("topleft", colnames(Z)[1:(length(colnames(Z))-1)], cex=0.7, col=colors, lwd=2,
         lty=linetype, bty = "n")
}

