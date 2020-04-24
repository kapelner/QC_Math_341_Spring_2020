pacman::p_load(data.table)

X = fread("2020_SPR_MATH_341 - mid2raw.csv")
X[, Email := NULL]
answers = as.character(X[1, ])
nQ = ncol(X)

#create the scores for each question
for (j in 1 : nQ){
  X[, (paste0(c("Q", j, "s"), collapse = "")) := as.integer(NA)]
}

#grade each question
for (i in 1 : nrow(X)){
  for (j in 1 : nQ){
    student_answer = tolower(X[i, get(paste0(c("Q", j), collapse = ""))])
    #this calculation is messy... TO-DO: do it nicer
    X[i, (paste0(c("Q", j, "s"), collapse = ""))] = 10 - (
      length(setdiff(
        strsplit(student_answer, split = "")[[1]],
        strsplit(answers[j], split = "")[[1]])) +
        length(setdiff(
          strsplit(answers[j], split = "")[[1]],
          strsplit(student_answer, split = "")[[1]]))
    )
  }
}
# length(setdiff(strsplit("ahi", split = "")[[1]], strsplit("ah", split = "")[[1]]))
# length(setdiff(strsplit("ah", split = "")[[1]], strsplit("ahi", split = "")[[1]]))
#spotcheck
X
fwrite(X, "midterm_graded.csv")
