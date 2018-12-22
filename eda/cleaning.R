library(stringr)
library(tibble)

# whisky

df <- read.csv("data/whisky.csv")

df["spirit.type"] <- tolower(df["spirit.type"][[1]]) %>%
    toTitleCase()

df["cask.type"] <- tolower(df["cask.type"][[1]]) %>%
    toTitleCase()

df["author"] <- tolower(df["author"][[1]]) %>%
    toTitleCase()

distiller <- str_extract(as.character(df["distiller.location"][[1]]), ".*\\s+//") %>%
    gsub("\\s+//", "", .) %>%
    tolower() %>%
    toTitleCase()

df <- add_column(df, distiller = distiller, .before = "distiller.location")

df["distiller.location"] <- str_extract(as.character(df["distiller.location"][[1]]), "(//|\\,)\\s+.*") %>%
    gsub(".*\\,\\s+", "", .) %>%
    gsub("//\\s+", "", .) %>%
    tolower() %>%
    toTitleCase() %>%
    gsub("^Usa", "USA", .)

df["cost"] <- strrep("$", df["cost"][[1]])

year <- as.character(df["age"][[1]]) %>%
    tolower() %>%
    str_extract("\\d+\\s+y") %>%
    str_extract("\\d+") %>%
    as.numeric()

month <- as.character(df["age"][[1]]) %>%
    tolower() %>%
    str_extract("\\d+\\s+m") %>%
    str_extract("\\d+") %>%
    as.numeric()

day <- as.character(df["age"][[1]]) %>%
    tolower() %>%
    str_extract("\\d+\\s+d") %>%
    str_extract("\\d+") %>%
    as.numeric()

year[is.na(year)] <- 0
month[is.na(month)] <- 0
day[is.na(day)] <- 0

age <- ceiling(year + month / 12 + day / 365)
age[age == 0] <- NA
df["age"] <- age

write.csv(df, "data/whisky_clean_distiller.csv", row.names = F)

df <- read.csv("app/whisky3.csv")
df.app <- df[c("id", "name", "type", "distillery", "origin", "abv", "rating", "cost", "peaty", "smoky", "briny", "salty", "oily", "full", "rich", "sweet", "vanilla", "floral", "fruity", "tart", "herbal", "spicy")]
write.csv(df.app, "app/whisky2.csv", row.names = F)

# color.csv

c <- c("red", "yellow", "aqua", "blue", "light-blue", "green", "navy", "teal", "olive", "lime", "orange", "fuchsia", "purple", "maroon", "black")
c <- data.frame(t(c))
names(c) <- as.character(sort(unique(df$type)))
write.csv(c, "app/color.csv", row.names = F)

# cluster.csv

cluster.5.5[cluster.5.5 == "1"] <- "44"
cluster.5.5[cluster.5.5 == "2"] <- "33"
cluster.5.5[cluster.5.5 == "3"] <- "22"
cluster.5.5[cluster.5.5 == "4"] <- "55"
cluster.5.5[cluster.5.5 == "5"] <- "11"

cluster.5.5[cluster.5.5 == "11"] <- "1"
cluster.5.5[cluster.5.5 == "22"] <- "2"
cluster.5.5[cluster.5.5 == "33"] <- "3"
cluster.5.5[cluster.5.5 == "44"] <- "4"
cluster.5.5[cluster.5.5 == "55"] <- "5"

cluster.5 <- rbind(cluster.5.1, cluster.5.2, cluster.5.3, cluster.5.4, cluster.5.5) %>%
  apply(2, median)

df.cluster.5 <- data.frame(id = df$id, cluster.5 = cluster.5)
write.csv(df.cluster.5, "app/cluster5.csv", row.names = F)
df.cluster <- data.frame(id = df$id, cluster.3 = cluster.3, cluster.4 = cluster.4, cluster.5 = cluster.5)
write.csv(df.cluster, "app/cluster.csv", row.names = F)

# rename cluster 

df.cluster <- read.csv("app/cluster.csv")

df.cluster$cluster.3[df.cluster$cluster.3 == 1] <- "not fruity, not smoky"
df.cluster$cluster.3[df.cluster$cluster.3 == 2] <- "smoky, not fruity"
df.cluster$cluster.3[df.cluster$cluster.3 == 3] <- "fruity, not smoky"

df.cluster$cluster.4[df.cluster$cluster.4 == 1] <- "rich, not fruity, not smoky"
df.cluster$cluster.4[df.cluster$cluster.4 == 2] <- "not fruity, not smoky, not rich"
df.cluster$cluster.4[df.cluster$cluster.4 == 3] <- "smoky, not fruity"
df.cluster$cluster.4[df.cluster$cluster.4 == 4] <- "fruity, not smoky"

df.cluster$cluster.5[df.cluster$cluster.5 == 1] <- "rich, not fruity, not smoky"
df.cluster$cluster.5[df.cluster$cluster.5 == 2] <- "not fruity, not smoky, not rich"
df.cluster$cluster.5[df.cluster$cluster.5 == 3] <- "average fruity, average smoky"
df.cluster$cluster.5[df.cluster$cluster.5 == 4] <- "smoky, not fruity"
df.cluster$cluster.5[df.cluster$cluster.5 == 5] <- "fruity, not smoky"

write.csv(df.cluster, "app/cluster.csv", row.names = F)

# regroup whisky type

library(widyr)

df <- read.csv("app/whisky.csv")
df$type <- as.character(df$type)
df$type[df$type == "American Single Malt"] <- "Single Malt"
df$type[df$type == "Blended American Whiskey"] <- "Blended"
df$type[df$type == "Flavored Whiskey"] <- "Flavored"
df$type[df$type == "Other Whiskey"] <- "Other"
df$type[df$type == "Peated Blend"] <- "Blended"
df$type[df$type == "Peated Blended Malt"] <- "Blended Malt"
df$type[df$type == "Peated Single Malt"] <- "Single Malt"
df$type[df$type == "Wheat Whiskey"] <- "Wheat"
df$type[df$type == "Blended Grain"] <- "Blended"
df$type <- as.factor(df$type)
write.csv(df, "app/whisky.csv", row.names = F)
