check_belong            <- function(value, name, allowed, len) {
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

check_default           <- function(condition, condition_name, value, name,
                                    default) {
  if (condition) {
    if (!all(value == default)) {
      warning(name, " has been changed from its default value, but this will ",
              "have no effect given the chosen value of ", condition_name)
    }
  }
}

check_Delta             <- function(Delta, type) {
  if (any(!(length(Delta) %in% 1:2), !is.numeric(Delta), Delta >= 1,
          is.infinite(Delta))) {
    stop("Delta must be a numeric vector of length 1 or 2, whose elements all ",
         "belong to (-\u221E, 1)")
  } else if (length(Delta) == 1) {
    if (type == "AR") {
      return(c(DeltaA = Delta, DeltaR = Delta))
    } else {
      return(c(DeltaE = Delta, DeltaF = Delta))
    }
  } else {
    if (type == "AR") {
      return(c(DeltaA = Delta[1], DeltaR = Delta[2]))
    } else {
      return(c(DeltaE = Delta[1], DeltaF = Delta[2]))
    }
  }
}

check_w                 <- function(w) {
  if (any(length(w) != 4, !is.numeric(w), w < 0, sum(w[-4]) == 0)) {
    stop("w must be a numeric vector of length 4, with all elements greater ",
         "than or equal to 0, and with at least one of the first 3 elements ",
         "strictly positive")
  }
  w/sum(w)
}

check_timing            <- function(timing, J) {
  if (any(length(timing) != J, !is.numeric(timing), timing <= 0, timing[J] != 1,
          timing[2:J] <= timing[1:(J - 1)])) {
    stop("timing must be a strictly monotonically increasing numeric vector of",
         " length J (", J, "), whose Jth element is equal to 1 and whose first",
         " element is strictly positive")
  }
}

check_integer_range     <- function(value, name, range, len) {
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

check_k                 <- function(k, des) {
  if (missing(k)) {
    return(1:des$J)
  } else if (!all(k %in% 1:des$J)) {
    stop("k must contain values in [1, des$J].")
  }
}

check_logical           <- function(value, name) {
  if (!is.logical(value)) {
    stop(name, " must be a logical variable")
  }
}

check_real_range        <- function(value, name, range, len) {
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

check_real_range_strict <- function(value, name, range, len) {
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

check_shape_par         <- function(shape_par, shape) {

}

check_sigma             <- function(sigma) {
  if (any(!(length(sigma) %in% 1:2), !is.numeric(sigma), sigma <= 0,
          is.infinite(sigma))) {
    stop("sigma must be a numeric vector of length 1 or 2, whose elements all ",
         "belong to (0, \u221E)")
  } else if (length(sigma) == 1) {
    return(c(sigma0 = sigma, sigma1 = sigma))
  } else {
    return(c(sigma0 = sigma[1], sigma1 = sigma1))
  }
}

sub_num                 <- function(n) {
  codes  <- c("\u2080", "\u2081", "\u2082", "\u2083", "\u2084", "\u2085",
              "\u2086", "\u2087", "\u2088", "\u2089")
  if (n < 10) {
    code <- codes[n + 1]
  } else {
    code <- paste(codes[n%/%10 + 1], codes[n%%10 + 1], sep = "")
  }
  return(code)
}

theme_OptGS             <- function(base_size = 11, base_family = "") {
  ggplot2::theme_grey(base_size = base_size, base_family = base_family) +
    ggplot2::theme(panel.background = ggplot2::element_rect(fill   = "white",
                                                            colour = NA),
                   panel.border     = ggplot2::element_rect(fill   = NA,
                                                            colour = "grey70",
                                                            size   = 0.5),
                   panel.grid.major = ggplot2::element_line(colour = "grey87",
                                                            size   = 0.25),
                   panel.grid.minor = ggplot2::element_line(colour = "grey87",
                                                            size   = 0.125),
                   axis.ticks       = ggplot2::element_line(colour = "grey70",
                                                            size   = 0.25),
                   legend.key       = ggplot2::element_rect(fill   = "white",
                                                            colour = NA),
                   strip.background = ggplot2::element_rect(fill   = "grey70",
                                                            colour = NA),
                   strip.text       =
                     ggplot2::element_text(colour = "white",
                                           size   = ggplot2::rel(0.8)),
                   legend.title     = ggplot2::element_blank(),
                   legend.position  = "bottom",
                   plot.margin      = ggplot2::unit(c(0.3, 0.5, 0.3, 0.3),
                                                    "cm"),
                   plot.title = element_text(hjust = 0.5),
                   complete         = T)
}

check_real_pair_range_strict <- function(value1, value2, name1, name2, range){
  if (any(length(value1) != 1, value1 <= range[1], value1 >= range[2])){
    stop(name1, " must be a single number in (", range[1], ",", range[2], ")")
  }
  if (any(length(value2) != 1, value2 <= range[1], value2 >= range[2])){
    stop(name2, " must be a single number in (", range[1], ",", range[2], ")")
  }
  if (value1 >= value2){
    stop(name1, " must be strictly less than ", name2)
  }
}

check_integer_pair_range <- function(value1, value2, name1, name2, range){
  if (any(length(value1) != 1, value1 <= range[1], value1 >= range[2])){
    stop(name1, " must be a single integer in (", range[1], ",", range[2], ")")
  }
  if (any(length(value2) != 1, value2 <= range[1], value2 >= range[2])){
    stop(name2, " must be a single integer in (", range[1], ",", range[2], ")")
  }
  if (value1 > value2){
    stop(name1, " must be less than or equal to ", name2)
  }
}

check_stopping <- function(futility, efficacy){
  if (!is.logical(futility)){
    stop("futility must be a logical variable")
  }
  if (!is.logical(efficacy)){
    stop("efficacy must be a logical variable")
  }
  if (all(!futility, !efficacy)){
    stop("At least one of futility and efficacy must be set to TRUE")
  }
}

check_optimality <- function(optimality, point_prior, beta_prior, futility,
                             efficacy){
  if (!(optimality %in% c("minimax", "null_ess", "alt_ess", "null_med",
                          "alt_med", "prior", "admissable"))){
    stop("optimality must be set to one of \"minimax\", \"null_ess\", \"alt_ess\", \"null_med\", \"alt_med\", \"prior\" or \"admissable\"")
  }
  if (optimality == "prior"){
    if (all(!is.null(point_prior), !is.null(beta_prior))){
      stop("point_prior and beta_prior cannot both be specified when optimality is set to \"prior\"")
    }
    if (all(is.null(point_prior), is.null(beta_prior))){
      stop("One of point_prior and beta_prior must be specified when optimality is set to \"prior\"")
    }
    if (!is.null(point_prior)){
      if (any(length(point_prior) != 1, !is.finite(point_prior),
              point_prior < 0, point_prior > 1)){
        stop("point_prior must be a single number in [0,1]")
      }
    }
    if (!is.null(beta_prior)){
      if (any(length(beta_prior) != 2, any(!is.finite(beta_prior)),
              any(beta_prior <= 0))){
        stop("beta_prior must be a vector of length 2, containing values in (0,Inf)")
      }
    }
  } else {
    if (!is.null(point_prior)){
      warning("point_prior is not missing. This will have no effect as optimality is not set to \"prior\"")
    }
    if (!is.null(beta_prior)){
      warning("beta_prior is not missing. This will have no effect as optimality is not set to \"prior\"")
    }
  }
  if (all(any(!futility, !efficacy), optimality == "delta_minimax")){
    warning("If futility or efficacy are set to FALSE the delta-minimax design is equivalent to the minimax design")
  }
}

check_ensign <- function(ensign, futility){
  if (!is.logical(ensign)){
    stop("ensign must be a logical variable")
  }
  if (all(!futility, ensign)){
    stop("ensign cannot be set to TRUE and futility to FALSE")
  }
}

row_match <- function(vec, mat) {
  cvec <- do.call("paste", c(vec[, , drop = FALSE], sep = "\r"))
  cmat <- do.call("paste", c(mat[, , drop = FALSE], sep = "\r"))
  return(match(cvec, cmat, nomatch = NA_integer_))
}

check_gs_boundaries <- function(J, n, a, r) {
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
