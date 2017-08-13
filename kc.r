library(httr)
library(lubridate)

first <- Sys.Date()
#last <- first + years(2) # this doesn't work on leap-days
last <- first + days(730) # 2 years forward

##apiurl <- 'http://api.kennedy-center.org/api/Calendar/GetCalendarData/2017-7-16/2018-8-1'
#apiurl <- 'http://api.kennedy-center.org/api/Calendar/GetCalendarData/2017-7-16/2017-7-17'
#apiurl <- 'http://api.kennedy-center.org/api/Calendar/GetCalendarData/2017-7-22/2017-7-23'
#apiurl <- 'http://api.kennedy-center.org/api/Calendar/GetCalendarData/2017-7-22/2018-7-22'
#apiurl <- 'http://api.kennedy-center.org/api/Calendar/GetCalendarData/2017-5-1/2018-5-1'
#apiurl <- "http://api.kennedy-center.org/api/Calendar/GetCalendarData/2017-07-29/2018-09-16"
#apiurl <- "http://api.kennedy-center.org/api/Calendar/GetCalendarData/2017-07-29/2018-09-17"
#apiurl <- "http://api.kennedy-center.org/api/Calendar/GetCalendarData/2017-07-29/2018-09-23"
#apiurl <- "http://api.kennedy-center.org/api/Calendar/GetCalendarData/2017-07-29/2019-12-31"

# note: no performances are returned for the actual "last" day of the below query, so I assume it 
# selects performances up to midnight of the last day, which is the first instant of that day.
apiurl <- paste0('http://api.kennedy-center.org/api/Calendar/GetCalendarData/', first, '/', last)

pg <- GET(apiurl)

raw <- content(pg)

df <- do.call(rbind, raw)
df2 <- as.data.frame(df)

#df2$AvailableModesOfSale[sapply( l, paste0, collapse="")]
df2$AvailableModesOfSale[sapply(length, paste0, collapse="")]

df2$GenreList[sapply(df2$GenreList, function(l){ifelse(length(l),FALSE,TRUE)})] <- NA

df2$AvailableModesOfSale[sapply(df2$AvailableModesOfSale, is.null)] <- NA
df2$CustomerModeOfSaleDescription[sapply(df2$CustomerModeOfSaleDescription, is.null)] <- NA
df2$EventCode[sapply(df2$EventCode, is.null)] <- NA
df2$VenueCode[sapply(df2$VenueCode, is.null)] <- NA
df2$PromoMessage[sapply(df2$PromoMessage, is.null)] <- NA
df2$Series[sapply(df2$Series, is.null)] <- NA
df2$DisplayOnSaleDate[sapply(df2$DisplayOnSaleDate, is.null)] <- NA
df2$DisplayDonorOnSale[sapply(df2$DisplayDonorOnSale, is.null)] <- NA

df3 <- apply(df2, 2, unlist)

df3$GenreList <- sapply(df2$GenreList, paste0, collapse=",")
df3$AvailableModesOfSale <- sapply(df2$AvailableModesOfSale, paste0, collapse=",")

df4 <- as.data.frame(df3, stringsAsFactors = FALSE)

trim <- function (x) gsub("^\\s+|\\s+$", "", x)
convDate <- function(x) as.numeric(sub("[)].*", "", sub(".*[(]", "", x))) / 1000

df5 <- transform(df4
                 ,Title = trim(Title)    
                 ,Today = convDate(Today)
                 ,PerfDate = convDate(PerfDate)
                 ,DisplayPerfDate = trim(DisplayPerfDate)
                 ,DisplayPerfTime = trim(DisplayPerfTime)
                 ,BlurbAbstract = trim(BlurbAbstract)
                 ,GenreList = trim(GenreList)
                 ,AvailableModesOfSale = trim(AvailableModesOfSale)
                 ,CustomerModeOfSaleDescription = trim(CustomerModeOfSaleDescription)
                 ,EventCode = trim(EventCode)
                 ,VenueCode = trim(VenueCode)
                 ,VenueDescription = trim(VenueDescription)
                 ,OnSaleDate = convDate(OnSaleDate)
                 ,DisplayOnSaleDate = trim(DisplayOnSaleDate)
                 ,DonorOnSale = convDate(DonorOnSale)
                 ,DisplayDonorOnSale = trim(DisplayDonorOnSale)
)

TS <- function(x) as.POSIXct(x, origin='1970-01-01')

df5 <- transform(df5
                 ,TodayTS = TS(Today)
                 ,PerfTS = TS(PerfDate)
                 ,OnSaleTS = TS(OnSaleDate)
                 ,DonorOnSaleTS = TS(DonorOnSale)
)


findPerf <- function(artist) df5[sort(unique(c(grep(artist, df5$Title), grep(artist, df5$BlurbAbstract)))), ]

findPerf('Ben Folds')

