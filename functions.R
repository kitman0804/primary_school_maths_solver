#================================================================#
# Create table of possible combinations (width = 1)
#================================================================#

get_add <- function(base = 10) {
  all <- as.data.table(expand.grid(d1 = 0:(base - 1), d2 = 0:(base - 1)))[d1 >= d2]
  all0 <- all[, list(d1, d2, add = d1 + d2 - ((d1 + d2) >= base) * base)] # Addition
  all1 <- all[, list(d1, d2, add = d1 + d2 + 1 - ((d1 + d2 + 1) >= base) * base)] # Addition + 1 (carry)
  
  all <- rbind(all0, all1)
  all <- all[order(d1, d2)]
  return(all)
}

get_sub <- function(base = 10) {
  all <- as.data.table(expand.grid(d1 = 0:(base - 1), d2 = 0:(base - 1)))
  all0 <- all[, list(d1, d2, sub = d1 - d2 + ((d1 - d2) < 0) * base)] # Subtraction
  all1 <- all[, list(d1, d2, sub = d1 - d2 - 1 + ((d1 - d2 - 1) < 0) * base)] # Subtraction - 1 (borrow)
  
  all <- rbind(all0, all1)
  all <- all[order(d1, d2)]
  return(all)
}




#================================================================#
# Define the problem (the equation to be solved)
#================================================================#

creat_eqt <- function(max_width = 1, operation = "+", parms, constraints = NULL) {
  mtx <- matrix(parms, nrow = 3, ncol = max_width, byrow = TRUE)
  
  unknowns <- t(apply(mtx, 1, function(r_i) {
    !grepl("^[0-9]+$", r_i)
  }))
  most_sig <- t(apply(mtx, 1, function(r_i) {
    non_num <- !grepl("^[0-9]+$", r_i)
    non_zero_num <- !grepl("^0+$", r_i)
    most_sig <- cumsum(non_num | non_zero_num) == 1
    most_sig
  }))
  most_sig_not_zero <- mtx[unknowns & most_sig]
  most_sig_not_zero <- lapply(most_sig_not_zero, function(v) {
    list(parm = v, constraint = paste(v, "!= 0"))
  })
  constraints <- c(constraints, most_sig_not_zero)
  target <- sort(unique(as.vector(mtx[unknowns])))
  
  interchange <- NULL
  if (operation %in% c("+", "*")) {
    for (j in 1:ncol(mtx)) {
      x <- mtx[1:2, j]
      if (sum(!grepl("^[0-9]+$", x)) > 1) interchange <- c(interchange, list(x))
    }
  }
  
  rules <- lapply(1:ncol(mtx), function(i) {
    num1 <- paste(mtx[1, 1:i], " * base^", (i - 1):0, sep = "", collapse = " + ")
    num2 <- paste(mtx[2, 1:i], " * base^", (i - 1):0, sep = "", collapse = " + ")
    num3 <- paste(mtx[3, 1:i], " * base^", (i - 1):0, sep = "", collapse = " + ")
    
    rule <- paste("((", num1, ") ", operation, " (", num2, ")) == (", num3, ")", sep = "")
    if (i != ncol(mtx)) rule <- c(rule, paste("((", num1, ") ", operation, " (", num2, ") ", operation, " 1) == (", num3, ")", sep = ""))
    return(rule)
  })
  equation <- rules[[max_width]]
  
  eqt <- list(
    equation_matrix = mtx, 
    target = target, 
    equation = equation, 
    max_width = max_width, 
    unknowns = unknowns,
    interchange = interchange,
    operation = operation, 
    rules = rules, 
    constraints = constraints
  )
  
  class(eqt) <- append(class(eqt), "prim_chick_eqt")
  return(eqt)
}




creat_eqt_readline <- function() {
  ok <- FALSE
  j <- 0
  while (!as.logical(ok)) {
    j <- j + 1
    if (j > 4) stop("Stop playing!")
    cat("How many digit in the largest number?\n")
    max_width <- as.numeric(readline(prompt = ">>>> "))
    cat("What is your operation? (+/-)\n")
    operation <- readline(prompt = ">>>> ")
    
    mtx <- outer(1:3, 1:max_width, paste, sep = "_")
    # parm = c(letters, unlist(t(outer(letters, letters, paste0))))
    
    cat("Your equation is in the form of:\n\n")
    cat_eqt(mtx, operation)
    cat("Is it correct? (T/F)\n")
    ok <- readline(prompt = ">>>> ")
  }
  
  ok <- FALSE
  j <- 0
  while(!as.logical(ok)) {
    j <- j + 1
    if (j > 4) stop("Stop playing!")
    entry <- vector(mode = "character", length = length(mtx))
    cat("What are entries?\n")
    for (i in 1:length(entry)) {
      entry[i] <- readline(prompt = paste(">>>> ", t(mtx)[i], "= "))
      if (entry[i] == "") {
        j <- j + 1
        break
      }
    }
    eqt <- creat_eqt(max_width = max_width, operation = operation, parms = entry)
    print(eqt)
    cat("Is it correct? (T/F)\n")
    ok <- readline(prompt = ">>>> ")
  }
  
  return(eqt)
}

is.prim_chick_eqt <- function(x) {
  "prim_chick_eqt" %in% class(x)
}

cat_eqt <- function(mtx, operation) {
  mtx <- apply(mtx, 2, function(col) format(col, width = max(nchar(col)), just = "right"))
  mtx[] <- paste("[", mtx, "]", sep = "")
  mtx[] <- gsub("\\[(\\s*)0\\]", " \\1  ", mtx)
  mtx <- cbind(format(c("", operation, ""), width = nchar(operation) + 1, just = "right"), mtx)
  hline <- rep("-", sum(nchar(mtx[1, ])) + ncol(mtx))
  hline <- paste(hline, collapse = "")
  
  cat("Equation to be solved:\n\n")
  cat(mtx[1, ], "\n")
  cat(mtx[2, ], "\n")
  cat(hline, "\n")
  cat(mtx[3, ], "\n")
  cat("\n\n")
}

print.prim_chick_eqt <- function(x) {
  cat_eqt(x$equation_matrix, x$operation)
}




#================================================================#
# Remove duplicated rows and columns
#================================================================#

rm_dup_row_col <- function(x) {
  UseMethod("rm_dup_row_col", x)
}

rm_dup_row_col.data.table <- function(x) {
  setkey(x)
  x <- x[!duplicated(x)]
  cnames <- names(x)
  if (length(cnames) == 1) {
    return(x)
  } else if (length(cnames) <= 80) {
    filters <- paste(apply(combn(cnames, 2), 2, paste, collapse = "!="), sep = "", collapse = "&")
    x <- x[eval(parse(text = filters))]
  } else {
    filters <- apply(combn(cnames, 2), 2, paste, collapse = "!=")
    for (filter in filters) x <- x[eval(parse(text = filter))]
  }
  return(x)
}




#================================================================#
# Solver function
#================================================================#

# Solver (fast but memory demanding)
solver <- function(eqt, base = 10, constraints = NULL) {
  require(data.table)
  if (eqt$operation == "*") stop("Sorry. Multiplication is not supported at this moment. :(")
  target <- eqt$target
  equation <- eqt$equation
  rules <- eqt$rules
  interchange <- eqt$interchange
  constraints <- c(constraints, eqt$constraints)
  constraints_uni <- constraints[sapply(constraints, function(x) length(x$parm) == 1)]
  constraints_multi <- constraints[!sapply(constraints, function(x) length(x$parm) == 1)]
  eqt_mtx <- eqt$equation_matrix
  
  if (eqt$operation == "+") {
    all <- get_add(base)
  } else if (eqt$operation == "-") {
    all <- get_sub(base)
  }
  all <- rm_dup_row_col(all)
  
  #partial solutions
  sol_partial <- lapply(1:ncol(eqt_mtx), function(j) {
    target_exist <- (!duplicated(eqt_mtx[, j])) & (eqt_mtx[, j] %in% target)
    if (sum(target_exist) == 0) return(NULL)
    sol_j <- copy(all[, target_exist, with = FALSE])
    names(sol_j) <- eqt_mtx[, j][target_exist]
    # Handle constraints
    for (constr in constraints_uni) {
      if (sum((constr$parm %in% names(sol_j))) == length(constr$parm)) {
        sol_j <- sol_j[eval(parse(text = constr$constraint))]
      }
    }
    return(sol_j)
  })
  
  not_null <- !sapply(sol_partial, is.null)
  sol_partial <- sol_partial[not_null]
  rules <- rules[not_null]
  
  sol <- NULL
  if (length(sol_partial) > 1) {
    for (i in 1:length(sol_partial)) {
      sol_p_i <- sol_partial[[i]]
      common_col <- intersect(names(sol), names(sol_p_i))
      if (length(common_col) > 0) {
        sol <- merge(sol, sol_p_i, by = common_col, allow.cartesian = TRUE)
      } else {
        if (i == 1) {
          sol <- sol_p_i
        } else {
          sol <- sol_p_i[, cbind(sol), by = names(sol_p_i)] # sol_p_i should be of fewer rows than sol
        }
      }
      sol <- sol[eval(parse(text = paste(rules[[i]], collapse = " | ")))]
      sol <- rm_dup_row_col(sol)
    }
  }
  for (constr in constraints_multi) {
    if (sum((constr$parm %in% names(sol))) == length(constr$parm)) {
      sol <- sol[eval(parse(text = constr$constraint))]
    }
  }
  
  sol <- sol[eval(parse(text = equation))]
  if (!is.null(interchange)) {
    for (vrbs in interchange) {
      sol_inter <- sol[, c(which(names(sol) %in% vrbs), which(!(names(sol) %in% vrbs))), with = FALSE]
      names(sol_inter)[1:2] <- names(sol_inter)[2:1]
      sol <- rbind(sol, sol_inter)
    }
  }
  sol <- sol[, order(names(sol)), with = FALSE]
  class(sol) <- append(class(sol), "prim_chick_sol")
  return(sol)
}


# Solver for loop version (slow but memory saving)
solver_forloop <- function(eqt, base = 10, constraints = NULL) {
  require(data.table)
  if (eqt$operation == "*") stop("Sorry. Multiplication is not supported at this moment. :(")
  target <- eqt$target
  equation <- eqt$equation
  rules <- eqt$rules
  interchange <- eqt$interchange
  constraints <- c(constraints, eqt$constraints)
  constraints_uni <- constraints[sapply(constraints, function(x) length(x$parm) == 1)]
  constraints_multi <- constraints[!sapply(constraints, function(x) length(x$parm) == 1)]
  eqt_mtx <- eqt$equation_matrix
  
  if (eqt$operation == "+") {
    all <- get_add(base)
  } else if (eqt$operation == "-") {
    all <- get_sub(base)
  }
  all <- rm_dup_row_col(all)
  
  #partial solutions for j-th digit
  sol_partial <- lapply(1:ncol(eqt_mtx), function(j) {
    target_exist <- (!duplicated(eqt_mtx[, j])) & (eqt_mtx[, j] %in% target)
    if (sum(target_exist) == 0) return(NULL)
    sol_j <- copy(all[, target_exist, with = FALSE])
    names(sol_j) <- eqt_mtx[, j][target_exist]
    # Handle constraints
    for (constr in constraints_uni) {
      if (sum((constr$parm %in% names(sol_j))) == length(constr$parm)) {
        sol_j <- sol_j[eval(parse(text = constr$constraint))]
      }
    }
    return(sol_j)
  })
  
  not_null <- !sapply(sol_partial, is.null)
  sol_partial <- sol_partial[not_null]
  rules <- rules[not_null]
  
  sol <- NULL
  if (length(sol_partial) > 1) {
    for (i in 1:length(sol_partial)) {
      sol_p_i <- sol_partial[[i]]
      common_col <- intersect(names(sol), names(sol_p_i))
      if (length(common_col) > 0) {
        sol <- merge(sol, sol_p_i, by = common_col, allow.cartesian = TRUE)
        sol <- rm_dup_row_col(sol)
        sol <- sol[eval(parse(text = paste(rules[[i]], collapse = " | ")))]
      } else {
        if (i == 1) {
          sol <- sol_p_i
        } else {
          tmp_sol <- NULL
          for (r_i in 1:nrow(sol_p_i)) {
            tmp <- cbind(sol_p_i[r_i], sol)
            tmp <- rm_dup_row_col(tmp)
            tmp <- tmp[eval(parse(text = paste(rules[[i]], collapse = " | ")))]
            tmp_sol <- rbind(tmp_sol, tmp)
            cat("\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b", r_i)
            flush.console()
          }
          cat("\n")
          sol <- tmp_sol
        }
      }
    }
  }
  
  for (constr in constraints_multi) {
    if (sum((constr$parm %in% names(sol))) == length(constr$parm)) {
      sol <- sol[eval(parse(text = constr$constraint))]
    }
  }
  sol <- sol[eval(parse(text = equation))]
  if (!is.null(interchange)) {
    for (vrbs in interchange) {
      sol_inter <- sol[, c(which(names(sol) %in% vrbs), which(!(names(sol) %in% vrbs))), with = FALSE]
      names(sol_inter)[1:2] <- names(sol_inter)[2:1]
      sol <- rbind(sol, sol_inter)
    }
  }
  sol <- sol[, order(names(sol)), with = FALSE]
  class(sol) <- append(class(sol), "prim_chick_sol")
  return(sol)
}


# Simultaneous solver
simul_solver <- function(..., solver_fun = solver, base = 10, constraints = NULL) {
  time_exe <- system.time({
    args <- as.list(match.call())[-1]
    eqts <- lapply(args, eval)
    eqts <- eqts[sapply(eqts, is.prim_chick_eqt)]
    target_num <- sapply(eqts, function(eqt) length(eqt$target))
    target_num <- order(target_num)
    eqts <- eqts[target_num]
    partial_sol <- vector("list", length(eqts))
    
    for (i in 1:length(eqts)) {
      partial_sol[[i]] <- solver_fun(eqts[[i]], base = base, constraints = constraints)
    }
    
    merge_sol <- function(sol1, sol2) {
      common_vrb <- intersect(names(sol1), names(sol2))
      merge(sol1, sol2, by = common_vrb, allow.cartesian = TRUE)
    }
    if (length(partial_sol) > 1) {
      sol <- Reduce(f = merge_sol, partial_sol)
    } else {
      sol <- partial_sol[[1]]
    }
    sol <- rm_dup_row_col(sol[, order(names(sol)), with = FALSE])
  })
  
  sol <- list(sol = sol, time_exe = time_exe)
  class(sol) <- append(class(sol), "prim_chick_sol")
  return(sol)
}

print.prim_chick_sol <- function(x) {
  print(x$sol)
}

# Simultaneous solver (version 2)
simul_solver2 <- function(..., solver_fun = solver, base = 10, constraints = NULL) {
  time_exe <- system.time({
    args <- as.list(match.call())[-1]
    eqts <- lapply(args, eval)
    eqts <- eqts[sapply(eqts, is.prim_chick_eqt)]
    target_num <- sapply(eqts, function(eqt) length(eqt$target))
    target_num <- order(target_num)
    eqts <- eqts[target_num]
    
    merge_sol <- function(sol1, sol2) {
      common_vrb <- intersect(names(sol1), names(sol2))
      merge(sol1, sol2, by = common_vrb, allow.cartesian = TRUE)
    }
    
    for (i in 1:length(eqts)) {
      cat(paste(rep("\b", (nchar(length(eqts)) * 2 + 4) * (i > 1)), collapse = ""), format(i, width = nchar(length(eqts))), " / ", length(eqts), sep = "")
      flush.console()
      if (i == 1) {
        sol <- solver_fun(eqts[[i]], base = base, constraints = constraints)
        next
      } else {
        tmp_sol <- vector("list", nrow(sol))
      }
      for (v_i in 1:nrow(sol)) {
        target <- eqts[[i]]$target
        vrbs_in_sol <- target[target %in% names(sol)]
        vrbs_not_in_sol <- target[!(target %in% names(sol))]
        vrb_in_sol_val <- unlist(sol[v_i, vrbs_in_sol, with = FALSE])
        used_val <- unlist(sol[v_i, !(names(sol) %in% vrbs_in_sol), with = FALSE])
        used_val <- paste("c(", paste(used_val, collapse = ", "), ")")
        
        extra_constraints <- list(
          list(
            parm = vrbs_in_sol, 
            constraint = paste(vrbs_in_sol, " == " , vrb_in_sol_val, sep = "", collapse = "&")
          ), 
          list(
            parm = vrbs_not_in_sol, 
            constraint = paste("!(", vrbs_not_in_sol, " %in% ", used_val, ")", sep = "", collapse = "&")
          )
        )
        tmp <- solver_fun(eqts[[i]], base = base, constraints = c(constraints, extra_constraints))
        tmp_sol[[i]] <- merge_sol(tmp, sol)
      }
      sol <- do.call(rbind, tmp_sol)
    }
    cat(paste(rep("\b", nchar(length(eqts)) * 2 + 4), collapse = ""), "Done.\n")
    
    sol <- rm_dup_row_col(sol[, order(names(sol)), with = FALSE])
  })
  
  sol <- list(sol = sol, time_exe = time_exe)
  class(sol) <- append(class(sol), "prim_chick_sol")
  return(sol)
}
