

setwd("c:/Users/dhadley/Documents/GitHub/Grammys")

d <- read.csv("./data/grammysTo2014.csv")


# Munge
d$WinnersBand <- gsub( " *\\(.*?\\) *", "", d$Winners)


# create a df of artists
d$isArtist <- grepl("artist", d$Winners)

d$artists <- sub(', art.*', '', d$WinnersBand)

d$artistsSep <- gsub("&",",",d$artists) # I need to figure out Mumford & Sons; Peter Paul & Mary etc

artists <- d[which(d$isArtist == TRUE),]



# http://stackoverflow.com/questions/15347282/split-string-and-insert-as-new-rows
s <- strsplit(artists$artistsSep, split = ",")

d2 <- data.frame(Category = rep(artists$Category, sapply(s, length)), artistFinal = unlist(s))

# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

d2$artistFinal <- trim(d2$artistFinal)

d2$Tab <- 1

d3 <- aggregate(Tab ~ artistFinal, d2, sum ) # makes a two-way table
d3 <- d3[order(-d3$Tab),] 
