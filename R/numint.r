

splitN <- function(N, nchunks) {
  nchunks <- min(N, nchunks)
  ans <- rep(N %/% nchunks, nchunks)
  ans[nchunks] <- ans[nchunks] + N %% nchunks
  ans
}

#' Monte Carlo Integration
#'
#' This function performes numerical integration using monte carlo integration
#'
#'
#' @param fn A function
#' @param ... Further arguments to be passed to -fn-
#' @param a Numeric vector. Lower bound
#' @param b Numeric vector. Upper bound
#' @param N Integer scalar. Sample size
#' @param ncores Integer scalar. Number of cores (parallel)
#' @param cl An object of class cluster
#'
#' @details \code{code}, \emph{emph}, \pkg{igraph}, \CRANpkg{igraph}
#' \eqn{\pi}{pi}
#'
#' \deqn{}{}
#'
#' @return An object of class
#' @aliases NumInt
#' @author George
#' @references
#' Weisstein, Eric W. "Monte Carlo Integration." From MathWorld--A Wolfram Web Resource. \url{http://mathworld.wolfram.com/MonteCarloIntegration.html}
#' @family Numerical Methods
#' @export
#'
#' @examples
#' # Simple example -------
#' ans <- num_int(dnorm, mean = .5, a = -6, b = 7, N = 1e3)
#'
#' # Multiple proc -----
#' \dontrun{
#'
#' ans <- num_int(dnorm, mean = .5, a = -6, b = 7, N = 1e8,
#' ncores = 20)
#'
#' }
num_int <- function(fn, ..., a, b, N = 100, ncores = 1, cl = NULL) {

  # Getting the call
  call <- match.call()

  # Checking length
  if (length(b) != length(a))
    stop("-a- and -b- must have the same length.")

  # Checking values
  if (!all(a < b))
    stop("There are some values a > b (all must a < b)")

  # Checking parallel
  if (!length(cl)) {
    cl <- parallel::makeCluster(ncores)
    toload <- loadedNamespaces()
    invisible(parallel::clusterCall(cl, function(x) {
      sapply(x, library, character.only = TRUE)
    }, x = toload))
    on.exit(parallel::stopCluster(cl))
  }

  # Sampling
  # samp <- Map(function(lb, ub) runif(N, lb, ub), lb = a, ub = b)
  # samp <- do.call(cbind, samp)

  # Computing the volume
  V <- prod(b - a)

  # Computing density
  f       <- function(x) fn(x, ...)

  # Distributing indices across processors
  # idx     <- splitIndices(N, length(cl))
  # fsample <- parLapply(cl, lapply(idx, function(w) samp[w,,drop=FALSE]), f)
  fsample <- parallel::parLapply(cl, splitN(N, length(cl)), function(n, a, b) {
    samp <- Map(function(lb, ub) runif(n, lb, ub), lb = a, ub = b)
    samp <- do.call(cbind, samp)
    f(samp)
  }, a = a, b = b)
  fsample <- unlist(fsample)

  ans     <- V*sum(fsample)/N

  # Preparing arguments
  env <- new.env()
  environment(f) <- env
  args <- list(...)
  environment(args) <- env

  # Returning an object of class numint
  structure(
    list(
      val = ans,
      vol = V,
      fsample = fsample,
      call = call,
      sd = sd(fsample*V),
      N=N,
      a = a,
      b = b,
      f = f,
      args = args
    ),
    class = "numint"
  )
}

#' @rdname num_int
#' @export
numint <- num_int

# Plotting method
#' @rdname num_int
#' @param x An object of class numint
#' @param y Ignored
#' @param main Title
#' @param col Color
#' @export
plot.numint <- function(x, y = NULL, main = "Monte Carlo Integration", col=blues9[4],...) {

  n <- 100

  # Computing values
  xran <- c(x$a[1], x$b[1])
  vals <- NULL
  if (length(x$a > 1)) {
    vals <- Map(function(a,b) rep((a + b)/2, n), a = x$a[-1], x$b[-1])
    vals <- do.call(cbind, vals)
  }

  # Computing coordinates
  vals <- cbind(seq(xran[1], xran[2], length.out = n), vals)
  y <- apply(vals, 1, x$f)


  # Adding missing points
  coordinates <- cbind(vals[,1], y)
  coordinates <- rbind(coordinates, c(xran[2], 0), c(xran[1], 0))

  # Plotting
  plot.new()
  plot.window(xlim = xran, ylim = range(y))
  polygon(coordinates, col = col, ...)

  # Adding axis
  axis(1);axis(2)

  # A nice title
  title(main = main)

  # And a neat legend
  legend("topright",
         legend = substitute(
           Volume~~a %+-% b,
           list(
             a = sprintf("%.4f", x$val),
             b = sprintf("%.4f", x$sd))
         ),
         bty = "n"
  )

  # Returning the coordinates used for the plot
  invisible(cbind(x = vals, y = y))
}

# printing method
#' @rdname num_int
#' @export
print.numint <- function(x, ...) {
  with(x, cat(sprintf("MONTE CARLO INTEGRATION\nN: %i\nVolume: %.4f\n", N, vol)))
  with(x, cat(sprintf("%.4f +- %.4f", val, sd)))

  invisible(x)
}
