library(corrplot)
library(caret)
library(ggplot2)
library(leaflet)
library(plotly)
library(stringr)
library(tools)
library(countrycode)
library(tibble)
library(doParallel)
library(e1071)
library(dplyr)
library(reshape2)

# rating distribution by author

df -> read.csv("data/whisky.csv")

df.profile <- df[, seq(9, 22)]

author.rank <- df %>%
  group_by(author) %>%
  summarise(ratings = length(author)) %>%
  .[order(.$ratings, decreasing = T), ]

author <- df %>%
  group_by(author) %>%
  summarise(ratings = length(author)) %>%
  .[.$ratings < 10, ] %>%
  .$author

df <- df[!(df$author %in% author), ] %>%
  group_by(author) %>%
  mutate(rating = scale(rating)) %>%
  ungroup()

df <- mutate_at(df, vars(seq(15, 28)), scale)

ggplot(df) +
  geom_boxplot(aes(author, rating))

ggplot(df) +
  geom_boxplot(aes(author, rating))

# predict rating from flavors

set.seed(112358)

data <- df[, c(7, 9:22)]

test.index <- createDataPartition(data$rating, times = 1, p = 0.2, list = F)

train <- data[-test.index, ]
test <- data[test.index, ]

model <- train(rating ~ ., method = "lm", data = train)
# model <- svm(rating ~ ., data = train)

prediction <- predict(model, test)
result <- postResample(prediction, test$rating)
result

# correlation

df <- read.csv("../data/whisky.csv")
df.profile <- df[, 9:22]

corr <- cor(df.profile)
corr.mtest <- cor.mtest(df.profile)

corrplot(corr,
  method = "color", type = "upper", p.mat = corr.mtest$p, addCoef.col = "black",
  sig.level = .05, pch.col = "red", tl.col = "black", number.cex = .8
)

corr.large <- corr
corr.large[corr.large < 0.4] <- NA

corrplot(corr.large,
  method = "color", type = "upper", p.mat = corr.mtest$p, addCoef.col = "black",
  sig.level = .05, pch.col = "red", tl.col = "black", number.cex = .8
)

corr.small <- corr
corr.small[corr.small > 0.1] <- NA

corrplot(corr.small,
  method = "color", type = "upper", p.mat = corr.mtest$p, addCoef.col = "black",
  sig.level = .05, pch.col = "red", tl.col = "black", number.cex = .8
)

# pca

df <- read.csv("../data/whisky.csv")
df.profile <- df[, 9:22]

pca <- prcomp(df.profile)
summary(pca)$importance[3, 1:3, drop = F]
pca.top <- data.frame()

k <- 2
for (i in seq(dim(pca$rotation)[2])) {
  pca.i <- sort(abs(pca$rotation[, i]), decreasing = T)[1:2]
  names <- names(pca.i)
  pca.top <- rbind(pca.top, data.frame(pc = i, name = names, value = pca.i, unique = !(names %in% pca.top$name)))
}
pca.top[pca.top["unique"] == T, ][, -4]

write.csv(pca$rotation, "app/pca_rotation.csv")

# cluster and exp.profile

df <- read.csv("app/data/whisky.csv")
df.profile <- df[, 9:22]
cluster.kmeans <- kmeans(df.profile, 5)

df["cluster"] <- as.character(cluster.kmeans$cluster)
df <- df[df$rating >= 80, ]
df.profile <- df[, 9:23]

df.profile <- group_by(df.profile, cluster) %>% summarise_all(funs(median)) %>% ungroup()
exp.profile <- df.profile[c(4, 3, 2, 1), ][, -1]
exp.profile <- cluster.kmeans$centers[-5, -15]
exp.profile[1, ] <- (cluster.kmeans$centers[2, ] + cluster.kmeans$centers[4, ]) / 2
exp.profile[2, ] <- cluster.kmeans$centers[1, ]
exp.profile[3, ] <- cluster.kmeans$centers[3, ]
exp.profile[4, ] <- cluster.kmeans$centers[5, ]
write.csv(exp.profile, "data/exp_profile.csv", row.names = F)

# box plot based on cluster

df["cluster"] <- as.character(cluster.kmeans$cluster)
df.profile["cluster"] <- as.character(cluster.kmeans$cluster)

df.profile %>%
  group_by(cluster) %>%
  summarise_all(funs(mean)) %>%
  ungroup()

df.melt <- melt(df.profile, id.vars = c("cluster"))

ggplot(df.melt) +
  geom_boxplot(aes(x = variable, y = value)) + facet_wrap(~cluster)

# plot

df["cluster"] <- as.character(cluster.kmeans$cluster)

df.plot <- cbind(df, scale(pca$x))

plot_ly(df.plot,
  x = ~PC1, y = ~(-PC2), z = ~PC3, type = "scatter3d", mode = "markers", marker = list(size = ~rating)
) %>%
  layout(scene = list(xaxis = list(title = "fruity"), yaxis = list(title = "smoky"), zaxis = list(title = "rich")))

# correlation network

df <- read.csv("app/whisky.csv")
df.pca <- read.csv("app/pca.csv")
df.pca.rotation <- read.csv("app/pca_rotation.csv")
df.cluster <- read.csv("app/cluster.csv")

profile <- df[, 9:22]
df.corr.flavor <- cor(profile)
df.corr.whisky <- cor(t(profile))

df.plot <- merge(df[, 1:8], df.pca, by = "id") %>%
  merge(df.cluster, by = "id")

n <- dim(df.corr.whisky)[1]
# n <- 100
lines <- list()
k <- 1
for (i in 1:n) {
  print(i)
  for (j in (i + 1):n) {
    if (df.corr.whisky[i, j] > 0.95 & df.corr.whisky[i, j] < 1) {
      l <- list(
        type = "line",
        line = list(color = rgb(colorRamp(c("white", "black"))(df.corr.whisky[i, j] - 0.3), max = 255), width = 0.05),
        x0 = df.pca[i, ]$PC1,
        y0 = df.pca[i, ]$PC2,
        x1 = df.pca[j, ]$PC1,
        y1 = df.pca[j, ]$PC2
      )
      lines[[k]] <- l
      k <- k + 1
    }
  }
}
nodes <- plot_ly(
  data = df.plot, x = ~PC1, y = ~(-PC2),
  type = "scatter", mode = "markers",
  color = ~cluster.5, marker = list(size = 2.5),
  text = ~id,
  hoverinfo = "text"
)

p <- layout(
  nodes,
  title = "Correlation Network (cor > 0.95)",
  shapes = lines,
  xaxis = list(title = "Fruity & Floral", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
  yaxis = list(title = "Peaty & Smoky", showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
)

htmlwidgets::saveWidget(p, "test.html")

# whisky review tf analysis

review <- read.csv("app/data/review.csv")[, c(1, 3)]

review$review <- gsub("'[a-z]{0,2}\\s", " ", review$review)

review$review <- sapply(review$review, as.character)

review <- unnest_tokens(review, word, review) %>%
  anti_join(stop_words)
review$word <- wordStem(review$word)
review %>%
  count(word, sort = T) %>%
  filter(n > 200) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

# vocabulary tree

library(yaml)
library(data.tree)
library(reshape2)
library(networkD3)
flavor <- yaml.load_file("data/flavor.yaml", as.named.list = T)
flavor <- melt(flavor)
flavor <- flavor[, c(3, 2, 1)]
names(flavor) <- c("l1", "l2", "l3")

write.csv(flavor, "app/data/flavor_vocabulary2.csv", row.names = F)

flavor <- read.csv("app/data/flavor_vocabulary.csv")

flavor <- as.data.frame(flavor[, 1])
flavor$pathString <- paste("flavor", flavor$l1, flavor$l2, flavor$l3, sep = "|")
flavor.tree <- as.Node(flavor, pathDelimiter = "|")
# radialNetwork
flavor.tree.list <- ToListExplicit(flavor.tree, unname = T)
radialNetwork(flavor.tree.list, fontSize = 14)
# simpleNetwork
flavor.tree.network <- ToDataFrameNetwork(flavor.tree)
simpleNetwork(flavor.tree.network, fontSize = 12)
# igraph
library(igraph)
plot(as.igraph(flavor.tree, directed = TRUE, direction = "climb"))

# distillery map

library(leaflet)

df.distillery <- read.csv("app/data/distillery.csv")

icon <- makeIcon(
  iconUrl = "app/icon/distillery.png",
  iconWidth = 30, iconHeight = 30,
  iconAnchorX = 15, iconAnchorY = 21
)
# visulization using leaflet
leaflet(df.distillery) %>%
  addProviderTiles(providers$Esri.WorldStreetMap) %>%
  addMarkers(~lng, ~lat,
    label = ~as.character(name),
    popup = paste("<div class='leaflet-popup-scrolled' style='max-width:600px;max-height:200px'>", df.distillery$about, "</div>"),
    clusterOptions = markerClusterOptions(),
    icon = icon
  )


df <- df[, c("peaty", "smoky", "briny", "salty", "oily", "full", "rich", "sweet", "vanilla", "floral", "fruity", "tart", "herbal", "spicy")]

# radialNetwork plot

flavor <- read.csv("app/data/flavor_vocabulary.csv")
size <- c(18, 16, 10)[3]
flavor$pathString <- paste("flavor", flavor$l1, flavor$l2, flavor$l3, sep = "|")
flavor.tree <- as.Node(flavor, pathDelimiter = "|")
flavor.tree.list <- ToListExplicit(flavor.tree, unname = T)
radialNetwork(flavor.tree.list, fontSize = size)
