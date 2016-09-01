# We attached a age_corr_matrix.csv file in canvas to run this function
similarityMat <- function(priceMat, sharesMat, unique_tickers, unique_dates) {
  ageMat = read.csv("age_corr_mat.csv", header=TRUE)
  AgeMat = as.matrix(ageMat[,2:101])
  return(AgeMat)
}
