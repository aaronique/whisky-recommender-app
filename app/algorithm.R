# \
#  > sort.by.name - literally mean it
# /

sort.by.name <- function(x) {
  x[sort(names(x))]
}

# \
#  > standardize - convert a value set to 0 to 1
# /

standardize <- function(x) {
  if (max(x) == min(x)) {
    return((x + 1) / (x + 1))
  } else {
    return((x - min(x)) / (max(x) - min(x)))
  }
}

# \
#  > get.user.profile - generate user flavor profile
# /

get.user.profile <- function(profile, fav.id, exp.level, exp.profile, pref.flavor, weights) {
  user.profile <- data.frame()
  id <- profile$id
  profile <- profile[, -c(1, 2, 3)]
  q <- matrix(nrow = 2, ncol = 14)
  w <- weights
  # favorite whisky
  if (length(fav.id) == 0 || !any(fav.id %in% id)) {
    q[1, ] <- 0
    w[1] <- 0
  } else {
    fading <- 2^(-seq(1:length(fav.id)))
    fav.profile <- colSums(profile[id %in% fav.id, ] * t(fading)) / sum(fading)
    for (i in 1:14) {
      q[1, i] <- ecdf(profile[, i])(fav.profile[i]) * w[1]
    }
  }
  # experience level
  if (exp.level == 0) {
    q[2, ] <- 0
    w[2] <- 0
  } else if (exp.level == 1 || exp.level == 2) {
    for (i in 1:14) {
      q[2, i] <- ecdf(profile[, i])(exp.profile[exp.level, i]) * w[2]
    }
  } else if (exp.level == 3) {
    if (q[1, 2] >= q[1, 11]) {
      for (i in 1:14) {
        q[2, i] <- ecdf(profile[, i])(exp.profile[3, i]) * w[2]
      }
    } else {
      for (i in 1:14) {
        q[2, i] <- ecdf(profile[, i])(exp.profile[4, i]) * w[2]
      }
    }
  }
  # prefered flavor
  if (all(w == 0) & all(pref.flavor == 0)) {
    q <- rep(0.5, 14)
  } else if (all(w == 0) & any(pref.flavor == 1)) {
    q <- (5^pref.flavor - 1) / 5^pref.flavor
  } else {
    q <- (colSums(q) / sum(w) + 3^pref.flavor - 1) / 3^pref.flavor
  }
  # user profile
  for (i in 1:14) {
    user.profile[1, i] <- quantile(profile[, i], q[i])
  }
  names(user.profile) <- names(profile)
  user.profile
}

# \
#  > get.topk - get top k items for user
# /

get.topk <- function(profile, user.profile, exclusion, k) {
  profile <- profile[!(profile$id %in% exclusion), ]
  if (dim(profile)[1] == 0) {
    return(data.frame())
  }
  if (dim(profile)[1] < k) {
    k <- dim(profile)[1]
  }
  corr <- as.numeric(cor(t(user.profile), t(profile[, -c(1, 2, 3)])))
  id <- order(corr, decreasing = T)
  topk <- data.frame(id = profile[, 1][id[1:k]], corr = corr[id][1:k]) %>%
    merge(profile, by = "id")
  row.names(topk) <- topk$id
  topk
}

# \
#  > get.score - calculate matching scores
# /

get.score <- function(topk, n, weights) {
  if (n > dim(topk)[1]) {
    n <- dim(topk)[1]
  }
  w <- weights
  score <- topk$rating * topk$corr
  set <- topk[order(score, decreasing = T), ][1, ]
  set$score <- (w[1] * set$corr + w[2] * set$rating + w[3] * 1) / sum(w)
  if (n >= 2) {
    for (i in 2:n) {
      topk <- topk[!(topk$id %in% set$id), ]
      diversity <- rowMeans(1 - cor(t(topk[, 4:17]), t(set[, 4:17]))) * rowMeans(standardize(adist(topk$name, set$name)))
      diversity <- standardize(diversity)
      score <- sort((w[1] * topk$corr + w[2] * topk$rating + w[3] * diversity) / sum(w), decreasing = T)[1]
      id <- as.numeric(names(score))
      set <- rbind(set, cbind(topk[topk$id == id, ], score))
    }
  }
  set[order(set$score, decreasing = T), ]
}

# \
#  > output.valuebox - display whisky card
# /

output.valuebox <- function(data, rindex, total, color) {
  if (total > dim(data)[1]) {
    total <- dim(data)[1]
  }
  if (rindex <= total) {
    r <- data[rindex, ]
    return(renderValueBox({
      valueBox(
        paste0(floor(r$score * 100), "%"),
        HTML(paste0(
          r$id, ": ", r$name, " - ", r$abv, "%", br(),
          r$type, br(),
          r$distiller, " - ", r$origin, br(),
          r$cost, " - ", r$rating, "/100"
        )),
        color = color[r$type]
      )
    }))
  } else {
    return(NULL)
  }
}
