#================================================================#
# Example
#================================================================#
# Example 1
## Find a, b, c, d, e, f, g, h, p such that ab - cd = ef & ef + gh = ppp
eqt1 <- creat_eqt(2, "-", parm = c("a", "b", "c", "d", "e", "f"))
eqt2 <- creat_eqt(3, "+", parm = c(0, "e", "f", 0, "g", "h", "p", "p", "p"))

## From base 10 to 34
sol10 <- simul_solver(eqt1, eqt2, base = 10)
sol16 <- simul_solver(eqt1, eqt2, base = 16)
sol22 <- simul_solver(eqt1, eqt2, base = 22)
sol28 <- simul_solver(eqt1, eqt2, base = 28)
sol34 <- simul_solver(eqt1, eqt2, base = 34)
sol40 <- simul_solver(eqt1, eqt2, base = 40)

## Example 1.2 with extra constraints
simul_solver(eqt1, eqt2, base = 10, constraints = list(list(parm = c("a", "b"), constraint = "(a != 8) & (b == 0)")))
simul_solver(eqt1, eqt2, base = 34, constraints = list(list(parm = "a", constraint = "a %in% 5:6")))


# Example 2
## Find a, b, c, d, e such that ab - cd = ee
## Run the code below and follow the steps:
## 1. Type "2" and press [ENTER]
## 2. Type "-" and press [ENTER]
## 3. Type "TRUE" or "T" and press [ENTER]
## 4. Type "a" and press [ENTER]
## 5. Type "b" and press [ENTER]
## 6. Type "c" and press [ENTER]
## 7. Type "d" and press [ENTER]
## 8. Type "e" and press [ENTER]
## 9. Type "e" and press [ENTER]
## 10. Type "TRUE" or "T" and press [ENTER]
solver(creat_eqt_readline())


# Example 3
## Find a, b, c, d such that ab + 1c = ddd
solver(creat_eqt(3, "+", parm = c(0, "a", "b", 0, 1, "c", "d", "d", "d")))


# Example 4
## Find a, b, c, d, e, f, g s.t. abc - def = gg
solver(creat_eqt(3, "-", parm = c("a", "b", "c", "d", "e", "f", 0, "g", "g")))


# Example 4
## Find a, b, c, d, eee s.t. ab - cd = eee
solver(creat_eqt(3, "*", parm = c(0, "a", "b", 0, "c", "d", "e", "e", "e")))


#================================================================#
# Experimental & Dangerous Zone
#================================================================#
## Don't try this
# eqt1 <- creat_eqt(4, "-", parm = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l"))
# eqt2 <- creat_eqt(5, "+", parm = c(0, "i", "j", "k", "l", 0, "m", "n", "o", "p", "q", "q", "q", "q", "q"))
# 
# sol1 <- solver(eqt1, base = 17)
# sol2 <- solver(eqt2, base = 17)
# sol <- merge(sol1, sol2, by = intersect(names(sol1), names(sol2)))
# sol <- rm_duplicate(sol[, order(names(sol)), with = FALSE])
# sol

# Experimental
eqt1_dan <- creat_eqt(4, "-", parm = c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l"))
eqt2_dan <- creat_eqt(5, "+", parm = c(0, "i", "j", "k", "l", 0, "m", "n", "o", "p", "q", "q", "q", "q", "q"))
sol2_dan <- solver(eqt2_dan, base = 17)


sol_dan <- NULL
for (r_i in 1:nrow(sol2_dan)) {
  sol_p_i <- unlist(sol2_dan[r_i])
  
  parms_eqt2_in_eqt1 <- sol_p_i[eqt2_dan$target %in% eqt1_dan$target]
  parms_eqt2_not_in_eqt1 <- sol_p_i[!(eqt2_dan$target %in% eqt1_dan$target)]
  parms_eqt1_not_in_eqt2 <- eqt1_dan$target[!(eqt1_dan$target %in% eqt2_dan$target)]
  
  equal <- lapply(1:length(parms_eqt2_in_eqt1), function(i) {
    list(
      parm = names(parms_eqt2_in_eqt1)[i], 
      constraint = paste(names(parms_eqt2_in_eqt1)[i], " == " , parms_eqt2_in_eqt1[i], sep = "")
    )
  })
  not_equal <- lapply(parms_eqt1_not_in_eqt2, function(v) {
    list(
      parm = v, 
      constraint = paste("!(", v, " %in% c(", paste(parms_eqt2_not_in_eqt1, collapse = ", "), "))", sep = "")
    )
  })
  constraints_i <- c(equal, not_equal)
  sol1_i <- solver(eqt1_dan, base = 17, constraints = constraints_i)
  if (nrow(sol1_i) > 0) {
    sol_dan <- rbind(
      sol_dan, 
      merge(sol1_i, sol2_dan[r_i, ], by = intersect(names(sol1_i), names(sol2_dan[r_i, ])))
    )
  }
  cat(paste(rep("\b", nchar(nrow(sol2_dan)) * 2 + 3), collapse = ""), format(r_i, width = nchar(nrow(sol2_dan))), " / ", nrow(sol2_dan), sep = "")
  flush.console()
  if (r_i == nrow(sol2_dan)) cat("\nEnd\n")
}





# Experimental
eqt2_dan <- creat_eqt(5, "+", parm = c(0, "i", "j", "k", "l", 0, "m", "n", "o", "p", "q", "q", "q", "q", "q"))
sol2_dan <- solver(eqt2_dan, base = 17)

sol_dan <- NULL
for (i in 1:nrow(sol2_dan)) {
  sol1_dan_i <- solver(creat_eqt(
    4, "-", 
    parm = c("a", "b", "c", "d", "e", "f", "g", "h", unlist(sol2_dan[i, list(i, j, k, l)])), 
    constraints = list(
      list(
        parm = letters[1:8], 
        constraint = paste("!(", letters[1:8], " %in% ", "c(", paste(unlist(sol2_dan[i, ]), collapse = ", "), "))", sep = "", collapse = " & ")
      )
    )
  ), base = 17)
  if (nrow(sol1_dan_i) > 0) {
    sol_dan <- rbind(
      sol_dan, 
      cbind(sol1_dan_i, sol2_dan[i])
    )
  }
  
  cat("\b\b\b\b\b\b\b\b", i)
  flush.console()
}


