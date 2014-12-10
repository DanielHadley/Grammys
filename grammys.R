library(ggplot2)
library(scales)

setwd("c:/Users/dhadley/Documents/GitHub/Grammys")
setwd("/Users/dphnrome/Documents/Git/Grammys")

#### d ####
d <- read.csv("./data/grammysTo2014.csv")


# Munge d
# Remove the names of band members from the parentheses
d$WinnersBand <- gsub( " *\\(.*?\\) *", "", d$Winners)

d$Tab <- 1


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


d3 <- aggregate(Tab ~ artistFinal, d2, sum ) # makes a two-way table
d3 <- d3[order(-d3$Tab),] 





#### top wikipedia ####
top <- read.delim("./data/top.csv", stringsAsFactors = F)
top$isFemale <- FALSE
top$isFemale[23:39] <- TRUE
males <- top[1:22,]
females <- top[23:39,]

top[23,2] <- "Alison Krauss"
top[25,2] <- "Beyonce"

topTop <- top[1:25,]



## Nominations http://en.wikipedia.org/wiki/Grammy_Award_records

N <- read.csv("./data/nominations.csv", stringsAsFactors = F)
N[6,1] <- "Beyonce"
N[22,1] <- "Bjork"

for (i in 1:37 ) {
  if(N[i,2] == "") N[i,2] = N[i-1,2]
}

N$Nominations <- as.numeric(N$Nominations)


mostN <- N[1:15,] # Most nominations

Nwo <- N[18:37,] # most without winning






#### Visualize ####
lime_green = "#2ecc71"
soft_blue = "#3498db"
pinkish_red = "#e74c3c"
purple = "#9b59b6"
teele = "#1abc9c"
nice_blue = "#2980b9"

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


ggplot(mostN, aes(x=reorder(mostN$Artist, mostN$Nominations), y=mostN$Nominations)) + 
  geom_bar(colour="white", fill=purple) + 
  my.theme + ggtitle("Top 15 Grammy Nominations") + xlab(NULL) +
  ylab("Nominations as of 57th Grammy Awards") + 
  geom_text(aes(label = mostN$Nominations), size = 2, color= "grey", hjust = -.25)+
  coord_flip() +
  scale_y_continuous(labels = comma)

ggsave("./plots/grammyNoms.png", dpi=300, width=5, height=5)


ggplot(Nwo, aes(x=reorder(Nwo$Artist, Nwo$Nominations), y=Nwo$Nominations)) + 
  geom_bar(colour="white", fill=purple) + 
  my.theme + ggtitle("Most Nominations Without An Award") + xlab(NULL) +
  ylab("Nominations as of 57th Grammy Awards") + 
  geom_text(aes(label = Nwo$Nominations), size = 2, color= "grey", hjust = -.25)+
  coord_flip() +
  scale_y_continuous(labels = comma)

ggsave("./plots/grammyNomswo.png", dpi=300, width=5, height=5)


ggplot(topTop, aes(x=reorder(topTop$Artist, topTop$Grammys), y=topTop$Grammys)) + 
  geom_bar(colour="white", fill=lime_green) + 
  my.theme + ggtitle("Top 25 Individual Grammy Winners") + xlab(NULL) +
  ylab("Grammys as of 56th Grammy Awards") + 
  geom_text(aes(label = topTop$Grammys), size = 2, color= "grey", hjust = -.25)+
  coord_flip() +
  scale_y_continuous(labels = comma)

ggsave("./plots/grammyWins.png", dpi=300, width=5, height=5)



t <- aggregate(d$Tab ~ d$Year, FUN=sum)
ggplot(t, aes(x=t$'d$Year', y=t$'d$Tab')) + 
  geom_line(colour=lime_green, size = 2) + 
  my.theme + ggtitle("Grammy Award Categories Over Time") + xlab("Year") +
  ylab("Grammy Awards Per Year") + 
  scale_y_continuous(labels = comma)

ggsave("./plots/grammyAwards.png", dpi=300, width=5, height=5)
