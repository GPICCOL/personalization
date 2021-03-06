individual <- read.csv("H1individual.csv")

Levels <- levels(individual$Date)
ind <- grep("^20", Levels)
Levels[ind] <- sapply(strsplit(Levels[ind], "/"), function(x) paste(x[c(2, 3, 1)], collapse = "/"))
levels(individual$Date) <- Levels

individual$Month <- format(strptime(individual$Date, "%m/%d/%Y"), "%Y-%m")

## Hypothesis 1
aggregate <- read.csv("H1aggregate.csv")
X <- data.frame(Month = rep(aggregate$Month, 2),
                Stays = rep(aggregate$Stays, 2),
                Persons = c(aggregate$phPers, aggregate$vPers),
                count = c(aggregate$countPhPers, aggregate$countVPers),
                virtualization = !is.na(aggregate$vPers),
                method = rep(c("physical", "virtual"), each = nrow(aggregate)))
X <- na.omit(X)

## Number of reservations with personalization
sum(table(individual$Method)[c("physical", "virtual")])
## Number of reservations
nrow(individual)
sum(aggregate$Stays)

## Table 2
tab2 <- with(aggregate, rbind(Stays = c(mean = mean(Stays), sd = sd(Stays), min = min(Stays), max = max(Stays)),
              t(sapply(c("physical", "virtual"), function(method) {
                  x <- table(factor(subset(individual, Method == method, Month)$Month))
                  c(mean = mean(x), sd = sd(x), min = min(x), max = max(x))
              })),
              do.call("rbind", with(subset(individual, Method %in% c("virtual", "physical")),
                                    tapply(P_ItemCount, factor(Method), function(x)
                                           c(mean = mean(x), sd = sd(x), min = min(x), max = max(x)))))))
tab2[,1:2] <- round(tab2[,1:2], digits = 2)
tab2

## Table 3 
g1 <- glm(P_ItemCount ~ Method, data = subset(individual, Method %in% c("virtual", "physical")),
          family = "poisson")
g2 <- glm(cbind(Persons, Stays - Persons) ~ method + virtualization, data = X, family = "binomial")
round(predict(g2, newdata = data.frame(method = c("physical", "physical", "virtual"), virtualization = c(FALSE, TRUE, TRUE)), type = "response"),
      digits = 4)
tab3 <- round(rbind(coef(summary(g1)),
                    coef(summary(g2))), digits = 2)
tab3 <- cbind(tab3, c(NA, NA, round(exp(coef(g2)), digits = 2)))
tab3

## Table 4
x <- read.csv("Berna complete - 2011-20140630.csv", encoding = "UTF-8", quote = "\"", sep = ",",
              strip.white = TRUE, as.is = TRUE) 
x$Departure[grep("201$", x$Departure)] <- 
  sapply(grep("201$", x$Departure), function(w)   
   gsub("201$", format(as.Date(x$Arrival[w], "%A, %B %d, %Y"), "%Y"), x$Departure[w]))
x$ar <- as.Date(x$Arrival, "%A, %B %d, %Y")
x$ar[is.na(x$ar)] <- as.Date(x$Arrival[is.na(x$ar)], "%Y/%m/%d")
x$dp <- as.Date(x$Departure, "%A, %B %d, %Y")
x$dp[is.na(x$dp)] <- as.Date(x$Departure[is.na(x$dp)], "%Y/%m/%d")
x$LoS <- with(x, dp-ar)
is.na(x$ADR[which(x$ADR == 0)]) <- TRUE
x$ROOM_TYPE <- factor(x$ROOM_TYPE)
levels(x$ROOM_TYPE) <- gsub("^ ", "", levels(x$ROOM_TYPE))
## ROOM_TYPE = "PX" is set to NA
levs <- sapply(c("^COM", "^Q", "^ST", "^S(3|P)"), function(g) {
    u <- rep(FALSE, nlevels(x$ROOM_TYPE))
    u[grep(g, levels(x$ROOM_TYPE))] <- TRUE
    u
    })
levels(x$ROOM_TYPE) <- c(NA, "COMFORT", "QUALITY", "SINGOLA (BASIC)", "SUPERIOR")[levs %*% 1:4 + 1]
x$PP <- x$PrefPrice
x$PrefPrice[is.na(x$PrefPrice)] <- 0
is.na(x$PP[which(x$PP == 0)]) <- TRUE
dim(x)

tab4 <- list(Rating = t(apply(x[, c("Service", "Value")], 2, table)),
             Room = table(x$ROOM_TYPE),
             VAE = table(x$Personalization),
             round(t(sapply(x[c("ADR", "LoS", "PP", "ADULTS", "CHILDREN")], function(x) {
                 x <- as.numeric(x)
                 c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE), min = min(x, na.rm = TRUE), max = max(x, na.rm = TRUE))
             })), digits = 2))
tab4
x$ADR <- scale(x$ADR)
             
## Table 5
M <- na.omit(x[, c("Service", "ROOM_TYPE", "LoS", "ADR", "Personalization", "ADULTS", "CHILDREN", "PrefPrice")])
M$Service <- ordered(M$Service, c(2.5, 5, 7.5, 10))
m0 <- ordinal::clm(Service ~ Personalization + ADR + LoS + ROOM_TYPE + ADULTS + CHILDREN + PrefPrice, data = subset(M, ADULTS + CHILDREN > 0 & PrefPrice < 50), Hess = TRUE)
tab5 <- round(cbind(coef(summary(m0)), exp(coef(m0))), digits = 3)
tab5[1:3, ncol(tab5)] <- NA
tab5

## Table 6
M <- na.omit(x[, c("Value", "ROOM_TYPE", "LoS", "ADR", "Personalization", "ADULTS", "CHILDREN", "PrefPrice")])
M$Value <- ordered(M$Value, c(2.5, 5, 7.5, 10))
m0 <- ordinal::clm(Value ~ Personalization + ADR + LoS + ROOM_TYPE + ADULTS + CHILDREN + PrefPrice, data = subset(M, ADULTS + CHILDREN > 0 & PrefPrice < 50), Hess = TRUE)
tab6 <- round(cbind(coef(summary(m0)), exp(coef(m0))), digits = 3)
tab6[1:3, ncol(tab6)] <- NA
tab6

## Hypothesis 3
repeated <- read.csv("Berna - RepeatedPair - 2011 - 06302014_BothChannels.csv", sep = ",")
dim(repeated)
is.na(repeated$ADR[which(repeated$ADR == 0)]) <- TRUE
repeated$ROOM_TYPE <- factor(repeated$ROOM_TYPE)
levels(repeated$ROOM_TYPE) <- gsub("^ ", "", levels(repeated$ROOM_TYPE))
## ROOM_TYPE = "PM" and "PX" is set to NA
levs <- sapply(c("^COM", "^Q", "^ST", "^S(3|P)"), function(g) {
    u <- rep(FALSE, nlevels(repeated$ROOM_TYPE))
    u[grep(g, levels(repeated$ROOM_TYPE))] <- TRUE
    u
    })
levels(repeated$ROOM_TYPE) <- c(NA, "COMFORT", "QUALITY", "SINGOLA (BASIC)", "SUPERIOR")[levs %*% 1:4 + 1]
repeated$ar <- as.Date(repeated$ARRIVALDATE, "%Y/%m/%d")
repeated$dp <- as.Date(repeated$DEPARTUREDATE, "%Y/%m/%d")
repeated$LoS <- with(repeated, dp-ar)
repeated$Method <- factor(repeated$Method, c("", "Physical", "Virtual"), c("None", "Physical", "Virtual"))
repeated$ADR <- scale(repeated$ADR)

length(unique(repeated$GUEST_ID))
table(subset(repeated, Repeated == 0, Personalization))

channel <- repeated
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
new_levels <- c("", "bookings europe b.v.", "hotel berna milano", "home page", "internet", "gds", "expedia")
levs <- sapply(c(as.list(new_levels[1:3]), list(hp), list(internet_channels), list(gds_channels), list(expedia), list(cc)), function(g) {
    u <- rep(FALSE, nlevels(channel$BOOKING_CHANNEL))
    u[tolower(levels(channel$BOOKING_CHANNEL)) %in% tolower(g)] <- TRUE
    u
    })

levels(channel$BOOKING_CHANNEL) <- c(NA, new_levels)[levs %*% seq_len(ncol(levs)) + 1]
wide <- reshape(channel[, c("Personalization", "GUEST_ID", "Repeated", "BOOKING_CHANNEL", "ADR", "ROOM_TYPE", "ADULTS", "CHILDREN", "LoS", "Method")], 
                timevar = "Repeated", idvar = "GUEST_ID", direction = "wide")

dim(wide)
table(wide$Method.0)
table(wide$BOOKING_CHANNEL.0 %in% c("bookings europe b.v.", "expedia", "internet"))
table(wide$BOOKING_CHANNEL.0 == "home page")

## Table 7
g1 <- glm(BOOKING_CHANNEL.1 == "home page" ~ I(Method.0 == "Virtual") + ADR.1 + ROOM_TYPE.1 + ADULTS.1 + CHILDREN.1, 
          data = subset(wide, !BOOKING_CHANNEL.0 %in% c("", "gds", "hotel berna milano", "home page")), family = "binomial")
round(cbind(coef(summary(g1)), Odds.Ratio = exp(coef(g1))), digits = 3)

agg <- with(subset(wide, !BOOKING_CHANNEL.0 %in% c("", "gds", "hotel berna milano", "home page")),
            table(Virtual = Method.0 == "Virtual", Homepage = BOOKING_CHANNEL.1 == "home page"))
prop.table(agg, 1)[2,2] / prop.table(agg, 1)[1,2] - 1

## Table 8
g2 <- glm(BOOKING_CHANNEL.1 != "home page" ~ I(Method.0 == "Virtual") + ADR.1 + ROOM_TYPE.1 + ADULTS.1 + CHILDREN.1, 
          data = subset(wide, BOOKING_CHANNEL.0 == "home page"), family = "binomial")
round(cbind(coef(summary(g2)), Odds.Ratio = exp(coef(g2))), digits = 3) 

agg <- with(subset(wide, BOOKING_CHANNEL.0  == "home page"),
            table(Virtual = Method.0 == "Virtual", Homepage = BOOKING_CHANNEL.1 != "home page"))
prop.table(agg, 1)[2,2] / prop.table(agg, 1)[1,2] - 1


##########################
##Extra analysis of Traces
library(parallel)
options(mc.cores = detectCores(all.tests = FALSE, logical = FALSE))
#options(mc.cores = 1L)
library(tm)

##Analysis of Traces
#p is the pattern to be search from kw
#d is the charachter vector to search into
keywCount  <- function(d, p) {
  found <- grep(paste("\\b", p, "\\b", sep = ""), d)
  if(length(found) > 0) {
    num <- length(found)
  }
  else {
    num <- 0
  }
  return(num)
}

dfCreate  <- function(p, d){
  m <- mclapply(p, function(x) sapply(d, keywCount, x, USE.NAMES = FALSE))
  return(m)
}

tfile  <- "NewTracesForAmenitiesCounting_only2_clean.csv"
kwfile  <- "PhysicalRequestKeywordsAll.csv"

kw  <- read.csv(kwfile, stringsAsFactor = FALSE, strip.white = TRUE)
kw  <- kw[, -1]
kw  <- unlist(kw)
kw  <- kw[kw != ""]
names(kw) <- gsub('\\d', "", names(kw))
kw  <- kw[!duplicated(kw)]
kw  <- iconv(kw, to = "ASCII", sub = "")
#drops  <- c("bath", "baths", "pillow", "pillows")
#kw  <- kw[!kw %in% drops]
traces  <- read.csv(tfile, stringsAsFactor = FALSE, strip.white = TRUE)
traces  <- as.character(traces[ , "internalComment"])
traces  <- iconv(traces,to = "ASCII", sub = "")

#traces  <- traces[1:1000]

df  <- as.data.frame(dfCreate(kw, traces))
colnames(df)  <- kw
df$text  <- traces
df  <- df[ , c(length(df), 1:(length(df)-1))]
l  <- split(colSums(df[, -1]), names(kw))
rez <- sapply(unique(names(kw)), function(x) sum(rowSums(df[ , which(names(kw) == x) + 1]) > 0))
l2  <- l[7:8]
rez[c("Minibar", "In.room.amenities")]  <- sapply(l2, sum)[c("Minibar", "In.room.amenities")]
sum(rez)
round(rez/sum(rez), 3)
sort(round(prop.table(rez), 3))
round(cumsum(sort(prop.table(rez), decreasing = TRUE)), 3)

#This dataframe only has reviews with at least one match
ddf  <- df[sort(unique(unlist(sapply(df[ , -1], function(x) which(x > 0))))), c("text", names(which(summary(lapply(df[ , -1], function(x) which(x > 0)))[ , 1] > 0)))]

#write.table(ddf, file = "rez.csv", sep = ",", row.names = FALSE)
#save(l, file = "obj.Rdata")

##Analysis of HGRM options
kwfile  <-  "BernaPrefKey_categorized.csv"
chfile <- "allPreferences.csv"
kw  <- read.csv(kwfile, stringsAsFactor = FALSE, strip.white = TRUE)
kw$Category  <- factor(kw$Category)
table(kw$Category)
ch  <- read.csv(chfile, stringsAsFactor = FALSE, strip.white = TRUE)
ch  <- ch[, -(ncol(ch)-2:0)]
ch  <- ch[-(which(ch$X11 == 11)), ]
ch[is.na(ch)]  <- 0
ch[, 4:ncol(ch)]  <- as.integer(as.logical(as.matrix(ch[ , 4:ncol(ch)])))
ch$Combined  <- gsub("'", "", ch$Combined, fixed = TRUE)
l <- strsplit(ch$Combined, "(,|-)")
m  <- matrix(0L, ncol = nrow(kw), nrow = nrow(ch))
colnames(m)  <- kw$preferenceID
preferences  <- as.integer(unlist(l))
stay  <- rep(seq_along(l), sapply(l, length))
pm  <- cbind(stay, preferences)[preferences > 0, ]
pm  <- cbind(pm, as.integer(factor(pm[ , 2], levels = colnames(m))))
m[pm[ , c(1, 3)]] <- 1
pos <- which(ch[, c(4:(ncol(ch)-2),ncol(ch))] > 0, arr.ind = TRUE)
zz <- gsub("X", "", colnames(ch))[-c(1:3,41)]
pos  <- cbind(pos, as.integer(factor(zz[pos[, 2]], levels = colnames(m))))
m[pos[ , c(1, 3)]] <- 1
cs  <- colSums(m)
names(cs)  <- kw$AmenityName
l <- split(cs, kw$Category)
rez <- sapply(l, sum)
sort(round(prop.table(rez), 3))
round(cumsum(sort(prop.table(rez), decreasing = TRUE)), 3)
sum(sapply(l, sum))
sum(sapply(l, sum))/nrow(m)

###############
sessionInfo()

