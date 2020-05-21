pacman::p_load(data.table)

X = data.frame(fread("341final.csv"))
X = X[, -1]
num_pts = as.numeric(X[1, ])
answers = tolower(as.character(X[2, ]))
nQ = ncol(X)

#create the scores for each question
for (j in 1 : nQ){
  X[, (paste0(c("Q", j, "s"), collapse = ""))] = as.integer(NA)
}

for (i in 2 : nrow(X)){
  for (j in 1 : nQ){
    X[i, j] = paste0(sort(strsplit(tolower(data.frame(X)[i, j]), split = "")[[1]]), collapse = "")
  }
}
X = data.table(X)

#grade each question
for (i in 2 : nrow(X)){
  for (j in 1 : nQ){
    student_answer = tolower(X[i, get(paste0(c("Q", j), collapse = ""))])
    #this calculation is messy... TO-DO: do it nicer
    X[i, (paste0(c("Q", j, "s"), collapse = ""))] = num_pts[j] - (
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
fwrite(X, "final_graded.csv")
