library(ggplot2)
library(scales)

setwd("c:/Users/dhadley/Documents/GitHub/Grammys")
setwd("/Users/dphnrome/Documents/Git/Grammys")

#### d ####
d <- read.csv("./data/grammysTo2014.csv")


# Munge d
# Remove the names of band members from the parentheses
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





#### top wikipedia ####
top <- read.delim("~/GitHub/Grammys/data/top.csv")
males <- top[1:22,]
females <- top[23:39,]

p1 <- hist(males$Grammys)
p2 <- hist(females$Grammys)

plot( p1, col=rgb(0,0,1,1/4), xlim=c(0,30))  # first histogram
plot( p2, col=rgb(1,0,0,1/4), xlim=c(0,30), add=T)  # second


## Nominations http://en.wikipedia.org/wiki/Grammy_Award_records

N <- read.csv("./data//nominations.csv", stringsAsFactors = F)
N[6,1] <- "Beyonce"

for (i in 1:37 ) {
  if(N[i,2] == "") N[i,2] = N[i-1,2]
}

mostN <- N[1:15,]
mostN$NominationsFif <- mostN$Nominations
mostN[3,3] = 68
mostN[4,3] = 63
mostN[5,3] = 62
mostN[6,3] = 61
mostN[7,3] = 55
mostN[9,3] = 48
mostN[12,3] = 45
mostN[13,3] = 43
mostN[14,3] = 43

mostN$NominationsFif <- as.numeric(mostN$NominationsFif)



#### Visualize ####
purple = "#9b59b6"

my.theme <- 
  theme(#plot.background = element_rect(fill="white"), # Remove background
    panel.grid.major = element_blank(), # Remove gridlines
    # panel.grid.minor = element_blank(), # Remove more gridlines
    # panel.border = element_blank(), # Remove border
    panel.background = element_blank(), # Remove more background
    axis.ticks = element_blank(), # Remove axis ticks
    axis.text=element_text(size=9), # Enlarge axis text font
    axis.title=element_text(size=8), # Enlarge axis title font
    plot.title=element_text(size=15,  hjust=0) # Enlarge, left-align title
    #,axis.text.x = element_text(angle=60, hjust = 1) # Uncomment if X-axis unreadable 
  )


ggplot(mostN, aes(x=reorder(mostN$Artist, mostN$NominationsFif), y=mostN$NominationsFif)) + 
  geom_bar(colour="white", fill=purple) + 
  my.theme + ggtitle("Top 15 Most Grammy Nominations") + xlab(NULL) +
  ylab("Nominations as of 57th Grammy Awards") + 
  geom_text(aes(label = mostN$NominationsFif), size = 2, color= "grey", hjust = -.25)+
  coord_flip() +
  scale_y_continuous(labels = comma)

ggsave("./plots/grammyNoms.png", dpi=300, width=5, height=5)