# Load the .rdata file - replace "path_to_your_rdata_file.rdata" with the actual path to the file
load("example-grading.data.RData")

# Function
calculate.grades <- function(group, individual) {
  if (!is.numeric(group) || length(group) != 1 || group < 0 || group > 100) {
    stop("Group grade must be a single numeric value between 0 and 100")
  }
  if (!is.numeric(individual) || any(individual < 1) || any(individual > 5)) {
    stop("Individual must be a numeric vector with values between 1 and 5")
  }
  
  # Calculate final grades
  final_grades <- sapply(individual, function(score) {
    multiplier <- if (score >= 4.5) 1.1
    else if (score >= 4) 1.05
    else if (score >= 3.5) 1
    else if (score >= 3) 0.9
    else if (score >= 2.5) 0.8
    else if (score >= 2) 0.6
    else if (score >= 1.5) 0.4
    else 0.1
    
    return(group * multiplier)
  })
  
  # Round to two decimal points
  final_grades <- round(final_grades, 2)
  
  # Make sure final grades are between 0 and 100
  final_grades <- pmax(pmin(final_grades, 100), 0)
  
  return(final_grades)
}

# Process each team using the data from the .rdata file
results <- list()
for (team in unique(student.df$group.name)) {
  team_individuals <- student.df$individual.score[student.df$group.name == team]
  team_group_score <- group.df$group.score[group.df$group.name == team]
  
  results[[team]] <- calculate.grades(group = team_group_score, individual = team_individuals)
}

# Print results
for (team in names(results)) {
  cat("Team:", team, "\n")
  print(results[[team]])
  cat("\n")
}
