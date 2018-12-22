library(rvest)
library(tidyverse)
library(stringr)
library(data.table)
library(gdata)

# \
#  > Scraping
# /

# whole page
page.link <- "http://www.example.com"
page.html <- read_html(page.link)
# article
article <- page.html %>%
  html_nodes("div.m-all
               article")
# title
hodgepodge <- article %>%
  html_nodes("[itemprop=name]") %>%
  html_text()
# rating
rating <- article %>%
  html_nodes("[itemprop=ratingValue]") %>%
  html_text() %>%
  as.numeric()
# category
category <- article %>%
  html_nodes("[itemprop = category]") %>%
  html_text() %>%
  as.factor()
# price
price <- article %>%
  html_nodes("[itemprop = price]") %>%
  html_text() %>%
  gsub("\\,|(\\.\\d+)", "", .) %>%
  str_extract("\\d+") %>%
  as.numeric()
# currency
price.currency <- article %>%
  html_nodes("[itemprop = priceCurrency]") %>%
  html_attr("content") %>%
  as.factor()
# description
review.content <- article %>%
  html_nodes("[itemprop= description]") %>%
  html_text() %>%
  gsub("(\n)\\s+", "", .)
# author
review.author <- article %>%
  html_nodes("[itemprop=author]") %>%
  html_text() %>%
  as.factor()
# review date
review.date <- article %>%
  html_nodes("div.review-text a") %>%
  html_text()

# \
#  > Tidy Data
# /

# extract info from hodgepodge
# alcohol by volume
abv <- str_extract(hodgepodge, "(\\d|\\.)*%") %>% # pattern: xx.x%
  gsub("%", "", .) %>% # get numeric value
  as.numeric()
# whisky age
age <- str_extract(hodgepodge, "\\d+\\s+[Yy]ear") %>% # pattern: xx year (old)
  gsub("([Yy]ear)|\\s+", "", .) %>% # get numeric value
  as.numeric()
# distilled year
distill.year <- str_extract(hodgepodge, "(\\s+(19|20)\\d{2})") %>% # pattern: 19xx | 20xx
  gsub("\\s+", "", .) %>% # get numeric value
  as.numeric()
# if vintage or not
vintage <- grepl("[Vv]intage", hodgepodge)
# actual distillery when sold by bottler
distilled.at <- str_extract(hodgepodge, "\\(([Dd]istilled\\s+[Aa]t)((\\s+|[[:punct:]]+)\\w+)+\\)") %>% # pattern: distilled at ..
  gsub("\\(|\\)|([Dd]istilled\\s+[Aa]t\\s+)", "", .) # get distillery name
# cask number
cask <- gsub(",", "", hodgepodge) %>%
  str_extract("([Cc]ask\\s*(([Nn]o\\.)|#)\\s*(\\d|\\.|\\,)+)|(\\([Cc]ask.*\\))") %>% # pattern: cask No. xx | cask #xx | (cask xx)
  gsub("\\(|\\)|([Nn]o\\.\\s*)|(#\\s*)|([Cc]ask\\s*)", "", .) %>% # get numeric value
  as.numeric()
# batch number
batch <- str_extract(hodgepodge, "([Bb]atch\\s*(([Nn]o\\.)|#)*\\s*(\\d)+)|(\\([Bb]atch.*\\))") %>% # pattern: batch no. | batch #
  gsub("([Bb]atch)|([Nn]o\\.)|#|\\(|\\)", "", .) %>% # get the numeric value
  as.character()
# whisky name
name <- hodgepodge %>%
  gsub("\\(.*\\)", "", .) %>% # pattern: (...)
  gsub("\\,.*", "", .) %>% # pattern: ,
  gsub("(\\d+\\s+[Yy]ear)|([Oo]ld)", "", .) %>% # pattern: xx year old
  gsub("\\s(19|20)\\d{2}", "", .) %>% # pattern: 19xx | 20xx
  gsub("[Vv]intage", "", .) %>% # pattern: vintage
  gsub("([Cc]ask\\s*(([Nn]o\\.)|#)\\s*(\\d|\\.|\\,)+)|(\\([Cc]ask.*\\))", "", .) %>% # pattern: cask No. xx | cask #xx | (cask xx)
  gsub("([Bb]atch\\s*(([Nn]o\\.)|#)*\\s*(\\d)+)|(\\([Bb]atch.*\\))", "", .) %>% # pattern: batch no. | batch #
  gsub("([a-z])([A-Z]|[0-9])", "\\1 \\2", .) %>% # pattern: ..sD.. | ..d7..
  gsub("(\\'|\")((\\w+[[:punct:]]*\\s*)+)(\\'|\")", "\\2", .) %>% # pattern: '...' | "..."
  gsub("\\“|\\”|\\‘|\\’", "", .) %>% # pattern: “ | ” | ‘ | ’
  gsub("\\#(\\D|$)", "", .) %>% # pattern: #xx | #$
  gsub(" \\'", " ", .) %>% # pattern: \\s\\'
  gsub("\\s{2,}", " ", .) %>% # pattern: \\s{2,}
  gsub("\\s+$", "", .) %>% # pattern: \\s$
  gsub("^\\s+", "", .)
# match distillery by whisky name
distillery.df1 <- read.xls("./data/distillery_1.xls")
distillery.df1 <- data.table(sapply(distillery.df1, as.character))
distillery.df2 <- read.xls("./data/distillery_2.xls")
distillery.df2 <- data.table(sapply(distillery.df2, as.character))
distillery.df <- rbind(distillery.df1, distillery.df2[!(distillery.df2$distillery %in% distillery.df1$distillery)], fill = T)
distillery.name <- as.character(distillery.df$distillery)
distillery <- vector(mode = "character", length = length(name))
for (i in seq_len(length(name))) {
  dn <- distillery.name[unlist(sapply(
    distillery.name,
    function(x) {
      grepl(
        tolower(gsub("\\s*", "", x)),
        tolower(gsub("\\s*", "", name[i]))
      )
    }
  ))]
  if (!is_empty(dn)) {
    distillery[i] <- dn[1]
  } else {
    distillery[i] <- NA
  }
  if (!is.na(distilled.at[i])) {
    distillery[i] <- distilled.at[i]
  }
}
# calculate reviewing year
review.year <- as.numeric(str_extract(review.date, "\\d+"))
review.season <- as.factor(str_extract(review.date, "\\w+"))
# estimate age and year
age.estimate <- review.year - distill.year - 1
distill.year[age.estimate < 3] <- NA
bottle.year <- age + distill.year
year.estimate <- review.year - age - 1
age[is.na(age)] <- age.estimate[is.na(age)]
age[age < 3] <- NA
distill.year[is.na(distill.year)] <- year.estimate[is.na(distill.year)]

# \
#  > Create Data Frame
# /

# create data table
whisky.dt <- data.table(
  name, age, distill.year, bottle.year, year.estimate, abv,
  cask, batch, vintage, distillery, category, rating, price, price.currency,
  review.author, review.year, review.season, review.content
)
# filter out single malt scotch
whisky.dt <- whisky.dt[whisky.dt$category == "Single Malt Scotch", ]
whisky.dt$id <- seq_len(dim(whisky.dt)[1])

# \
#  > Write CSV
# /

write.csv(whisky.dt, file = "data/whisky.csv")
