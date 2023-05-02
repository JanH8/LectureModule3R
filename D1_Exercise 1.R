library(tidyverse)

# load dataset
football = as_tibble(read.csv(
  "Data/football.csv",
  header = TRUE,
  sep = ";",
  dec = ","
))

# add some variables
football$shot_to_goal_rate = football$Shots / football$Goals
football$tackle_success_rate = football$TacklesWon / (football$TacklesWon + football$TacklesLost)
football$pass_completion_rate = football$PassesCompleted / (football$PassesCompleted + football$PassesUnsuccessful)

# calculate spearman rank correllation
football_reduced = select(football, !c(2:5))
for (col in colnames(football_reduced)) {
  print(cor(
    x = football_reduced$Rank,
    select(football_reduced, col),
    method = "spearman"
  ))
}

# dertermine variables with absolute spearman coefficient greater than 0,7
correllated_variables = c()
for (col in colnames(football_reduced)) {
  cor = cor(x = football_reduced$Rank,
            select(football_reduced, col),
            method = "spearman")
  if (abs(cor) > 0.7) {
    correllated_variables = c(correllated_variables, col)
  }
}


ranks = football_reduced$Rank

# creating color vector, Borussia M'Gladbach is green, every other team is black
colors = c()
for (r in ranks) {
  if (r == 7) {
    # hardcoded! not pretty!
    colors = c(colors, "green")
  } else {
    colors = c(colors, "black")
  }
}

for (variable in correllated_variables) {
  values = pull(football_reduced, variable)
  plot(
    values,
    ranks,
    xlab = variable,
    ylab = "Rank",
    col = colors,
    pch = 19
  )
}
