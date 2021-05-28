check_belong                 <- function(value, name, allowed, len) {
  if (is.infinite(len)) {
    for (i in 1:length(value)) {
      if (!(value[i] %in% allowed)){
        stop(name, " must contain values only in ",
             paste(allowed, collapse = ", "))
      }
    }
  } else {
    if (any(length(value) != 1, !(value %in% allowed))) {
      stop(name, " must be set to one of ", paste(allowed, collapse = ", "))
    }
  }
}

check_default                <- function(condition, condition_name, value, name,
                                         default) {
  if (condition) {
    if (!(all.equal(value, default) == TRUE)) {
      warning(name, " has been changed from its default value, but this will ",
              "have no effect given the chosen value of ", condition_name)
    }
  }
}

check_Delta                  <- function(Delta, type) {
  if (any(!(length(Delta) %in% 1:2), !is.numeric(Delta), Delta >= 1,
          is.infinite(Delta))) {
    stop("Delta must be a numeric vector of length 1 or 2, whose elements all ",
         "belong to (-Inf, 1)")
  } else if (type %in% c("nearopt", "power_family")) {
    if (length(Delta) == 1) {
      return(c("DeltaE" = Delta, "DeltaF" = Delta))
    } else {
      return(c("DeltaE" = Delta[1], "DeltaF" = Delta[2]))
    }
  } else if (type %in% c("haybittle_peto", "triangular")) {
    return(Delta)
  } else if (type == "obrien_fleming") {
    return(0)
  } else if (type == "pocock") {
    return(0.5)
  } else if (type == "wang_tsiatis") {
    if (length(Delta) == 2) {
      stop("When shape = \"wang_tsiatis\", Delta must be a numeric of length 1")
    }
    return(Delta)
  }
}

check_gs_boundaries          <- function(J, n, a, r) {
  if (any(!is.numeric(n), length(n) != J, n%%1 != 0)) {
    stop("n must be a numeric vector of length ", J,
         ", containing integer elements")
  }
  if (any(!is.numeric(a), length(a) != J)) {
    stop("a must be a numeric vector of length ", J)
  }
  if (any(!is.numeric(r), length(r) != J)) {
    stop("r must be a numeric vector of length ", J)
  }
  if (J > 1) {
    for (j in 1:(J - 1)) {
      if (all(is.finite(a[j]), a[j]%%1 != 0)) {
        stop("Finite elements of a must be integers")
      }
      if (all(is.finite(r[j]), r[j]%%1 != 0)) {
        stop("Finite elements of r must be integers")
      }
    }
  }
  if (!is.finite(a[J])) {
    stop("a[J] must be finite")
  }
  if (!is.finite(r[J])) {
    stop("r[J] must be finite")
  }
  if (a[J] != r[J] - 1) {
    stop("a[J] must be one less than r[J]")
  }
}

check_integer_range          <- function(value, name, range, len) {
  check         <- F
  if (is.finite(len)) {
    if (any(length(value) != len, !is.numeric(value), value%%1 != 0,
            value <= range[1], value >= range[2])) {
      check     <- T
      if (len == 1) {
        segment <- " a single integer that belongs to {"
      } else {
        segment <- paste(" an integer vector of length", len, "whose elements",
                         "all belong to {")
      }
    }
  } else if (any(value%%1 != 0, !is.numeric(value), value <= range[1],
                 value >= range[2])) {
    check       <- T
    segment     <- " an integer vector whose elements all belong to {"
  }
  if (check) {
    if (range[1] + 2 == range[2]) {
      stop(name, " must be equal to ", range[1] + 1)
    } else if (range[1] + 3 == range[2]) {
      stop(name, segment, range[1] + 1, ", ", range[1] + 2, "}")
    } else if (range[1] + 4 == range[2]) {
      stop(name, segment, range[1] + 1, ", ", range[1] + 2, ", ", range[1] + 3,
           "}")
    } else if (all(is.infinite(range))) {
      stop(name, segment, "..., -1, 0, 1, ...}")
    } else if (is.infinite(range[1])) {
      stop(name, segment, "..., ", range[2] - 2, ", ..., ", range[2] - 1, "}")
    } else if (is.infinite(range[2])) {
      stop(name, segment, range[1] + 1, ", ", range[1] + 2, ", ...}")
    } else {
      stop(name, segment, range[1] + 1, ", ..., ", range[2] - 1, "}")
    }
  }
  return(as.integer(value))
}

check_logical                <- function(value, name) {
  if (!is.logical(value)) {
    stop(name, " must be a logical variable")
  }
}

check_OptGS_des              <- function(des, name, stages) {
  if (!("OptGS_des" %in% class(des))) {
    stop(name, " must be of class \"OptGS_des\"")
  }
  if (!missing(stages)) {
    if (!(des$J %in% stages)) {
      stop(name, " must be a design with ", stages, " stages")
    }
  }
}

check_OptGS_est              <- function(est, name) {
  if (!("OptGS_est" %in% class(est))) {
    stop(name, " must be of class \"OptGS_est\"")
  }
}

check_OptGS_opchar           <- function(opchar, name) {
  if (!("OptGS_opchar" %in% class(opchar))) {
    stop(name, " must be of class \"OptGS_opchar\"")
  }
}

check_real_range             <- function(value, name, range, len) {
  if (is.finite(len)) {
    if (any(length(value) != len, !is.numeric(value), value < range[1],
            value > range[2])) {
      if (len == 1) {
        stop(name, " must be a single numeric that belongs to [", range[1], ",",
             range[2], "]")
      } else {
        stop(name, " must be a numeric vector of length ", len, ", whose ",
             "elements all belong to [", range[1], ",", range[2], "]")
      }
    }
  } else {
    if (any(!is.numeric(value), value < range[1], value > range[2])) {
      stop(name, " must be a numeric vector whose elements all belong to [",
           range[1], ",", range[2], "]")
    }
  }
}

check_real_range_strict      <- function(value, name, range, len) {
  if (is.finite(len)) {
    if (any(length(value) != len, !is.numeric(value), value <= range[1],
            value >= range[2])) {
      if (len == 1) {
        stop(name, " must be a single numeric that belongs to (", range[1], ",",
             range[2], ")")
      } else {
        stop(name, " must be a numeric vector of length ", len, ", whose ",
             "elements all belong to (", range[1], ",", range[2], ")")
      }
    }
  } else {
    if (any(!is.numeric(value), value <= range[1], value >= range[2])) {
      stop(name, " must be a numeric vector whose elements all belong to (",
           range[1], ",", range[2], ")")
    }
  }
}

check_shape_par              <- function(shape_par, shape) {

}

check_tau                    <- function(tau) {
  if (!is.numeric(tau)) {
    stop("tau must be numeric")
  } else if (any(is.infinite(tau))) {
    stop("tau must contain only finite values")
  }
  if (sum(duplicated(tau)) > 0) {
    warning("tau contains duplicated values")
  }
}

check_w                      <- function(w) {
  if (any(length(w) != 4, !is.numeric(w), w < 0, sum(w[-4]) == 0)) {
    stop("w must be a numeric vector of length 4, with all elements greater ",
         "than or equal to 0, and with at least one of the first 3 elements ",
         "strictly positive")
  }
  w/sum(w)
}
