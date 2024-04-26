#'Calculate spatial area-weighted average of multidimensional arrays
#'
#'This function computes a spatial area-weighted average of n-dimensional arrays 
#'being possible to select a region and to add a mask to be applied when 
#'computing the average.

library(multiApply)

WeightedSum <- function(data, lon, lat, region = NULL, mask = NULL, 
                         londim = 'lon', latdim = 'lat', na.rm = TRUE, 
                         ncores = NULL) {
  # Check inputs
  # data
  if (is.null(data)) {
    stop("Parameter 'data' cannot be NULL.")
  }
  if (!is.array(data)) {
    stop("Parameter 'data' must be a numeric array.")
  }
  dim_names <- names(dim(data))
  if (is.null(dim_names)) {
    stop("Parameter 'data' must have dimension names.")
  }
  # lon, lat
  if (is.null(lon) | is.null(lat)) {
    stop("Parameters 'lon' and 'lat' cannot be NULL.")
  }
  if (!is.numeric(lon) | !is.numeric(lat)) {
    stop("Parameters 'lon' and 'lat' must be numeric.")
  }
  if (!is.null(dim(lon)) | !is.null(dim(lat))) {
    if (length(dim(lon)) == 1 & length(dim(lat)) == 1) {
      lon <- as.vector(lon)
      lat <- as.vector(lat)
    } else {
      stop("Parameters 'lon' and 'lat' need to be a vector.")
    }
  }
  # londim
  if (is.numeric(londim)) {
    warning("Numeric 'londim' is deprecated, use dimension names instead. The ", 
            "corresponding dimension name will be assigned.")
    londim <- dim_names[londim]
  }
  if (!is.character(londim)) {
    stop("Parameter 'londim' must be a character string.")
  }
  if (length(londim) > 1) {
    warning("Parameter 'londim' must be of length 1. Only the first value ", 
            "will be used.")
    londim <- londim[1]
  }
  if (!londim %in% names(dim(data))) {
    stop("Parameter 'londim' is not found in 'data'.")
  }
  if (dim(data)[londim] != length(lon)) {
    stop(paste0("The longitudinal dimension of parameter 'data' must be of the ", 
                "same length as parameter 'lon'."))
  }
  # latdim
  if (is.numeric(latdim)) {
    warning("Numeric 'latdim' is deprecated, use dimension names instead. The ", 
            "corresponding dimension name will be assigned.")
    latdim <- dim_names[latdim]
  }
  if (!is.character(latdim)) {
    stop("Parameter 'latdim' must be a character string.")
  }
  if (length(latdim) > 1) {
    warning("Parameter 'latdim' must be of length 1. Only the first value ", 
            "will be used.")
    latdim <- latdim[1]
  }
  if (!latdim %in% names(dim(data))) {
    stop("Parameter 'latdim' is not found in 'data'.")
  }
  if (dim(data)[latdim] != length(lat)) {
    stop(paste0("The latitudinal dimension of parameter 'data' must be of the ", 
                "same length as parameter 'lat'."))
  }
  # region
  if (!is.null(region)) {
    if (length(region) != 4) {
      stop(paste0("The region argument has to be of length four indicating ", 
                  "the minimum longitude, the maximum longitude, the minimum ", 
                  "latitude and the maximum latitude of the region to be averaged."))
    }
  }
  # mask
  if (!is.null(mask)) {
    if (!all(dim(data)[which(names(dim(data)) %in% c(londim, latdim))] %in% 
             dim(mask)[which(names(dim(mask)) %in% c(londim, latdim))])) {
      stop("Parameter 'mask' must have the same spatial dimensions of data.")
    }
  }
  # na.rm
  if (!is.logical(na.rm) | length(na.rm) > 1) {
    stop("Parameter 'na.rm' must be one logical value.")
  }
  # ncores
  if (!is.null(ncores)) {
    if (!is.numeric(ncores) | length(ncores) > 1) {
      stop("Parameter 'ncores' must be either NULL or a positive integer.")
    } else if (ncores %% 1 != 0 | ncores <= 0) {
      stop("Parameter 'ncores' must be a positive integer.")
    }
  }
  
  if (!is.null(region)) {
    aux <- SelBox(data, lon = lon, lat = lat, region = region, londim = londim, 
                  latdim = latdim, mask = mask)
    data <- aux$data
    lon <- aux$lon
    lat <- aux$lat
    if (!is.null(mask)) {
      mask <- aux$mask
    }
  }
  
  # Compute the weights
  cosphi <- t(array(cos(lat * pi / 180), dim = c(length(lat), length(lon))))
  nblat <- length(lat)
  nblon <- length(lon)
  dlon <- diff(lon) * pi / 180
  dlon <- c(dlon, dlon[1])
  dlon <- array(dlon, dim = c(nblon, nblat))
  dlat <- diff(lat) * pi / 180
  dlat <- c(dlat, dlat[1])
  dlat <- t(array(dlat, dim = c(nblat, nblon)))
  weight <- (dlon * dlat * cosphi) 
  
  if (is.null(mask)) {
    res <- Apply(data = list(data), 
                 target_dims = c(londim, latdim), 
                 fun = .WeightedSum,
                 mask = NULL, 
                 weight = weight, 
                 na.rm = na.rm, 
                 ncores = ncores)$output1
  } else {
    res <- Apply(data = list(data, mask), 
                 target_dims = c(londim, latdim), 
                 fun = .WeightedSum,
                 weight = weight, 
                 na.rm = na.rm, 
                 ncores = ncores)$output1
  }
  return(res)
}

.WeightedSum <- function(data, mask = NULL, weight, na.rm = TRUE) {
  if (!is.null(mask)) {
    data[mask < 0.5] <- NA
  }
  weight[is.na(data)] <- NA
  # coeff <- sum(weight, na.rm = na.rm)
  sum <- sum(weight * data, na.rm = na.rm)
  return(sum)
}
