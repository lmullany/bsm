get_store <- function(feature_store) {
  if (is.function(feature_store)) feature_store() else feature_store
}
