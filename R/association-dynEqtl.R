#' @name dynEqtl
#' @title Gene-variant association for any pair of gene and variant (may be significant or not significant)
#' @description Calculation based on latest GTEx release
#' @export
dynEqtl <- function(
  gencodeID = NULL,
  variantID = NULL,
  tissueID = NULL,
  datasetID = 'gtex_v7'
){
  # gencodeID = 'ENSG00000077348.4'
  # variantID = 'rs4674'
  # tissueID = 'Brain_Cerebellum'
  if(any(is.null(gencodeID) | is.null(variantID) | is.null(tissueID))){
    stop('Please provide rqeuired fields (gencodeID, variantID, tissueID)')
  }
  gtex.url <- paste0(
    "https://gtexportal.org/rest/v1/association/dyneqtl?",
    "gencodeId=",
    gencodeID,
    "&variantId=",
    variantID,
    "&tissueSiteDetailId=",
    tissueID,
    "&datasetId=",
    datasetID
  )
  gtex.request <- httr::GET(
    url = gtex.url
  )
  if(gtex.request$status_code == 200){
    # cat('Connection estabilshed! ')
  }else{
    cat('Connection not established')
  }
  gtex.content <- httr::content(gtex.request)
  varinfo <- strsplit(gtex.content$variantId,split = '\\_')[[1]]
  ref <- paste0(varinfo[3],varinfo[3])
  alt <- paste0(varinfo[4],varinfo[4])
  het <- paste0(varinfo[3],varinfo[4])
  varGeno <- c(ref,het,alt)
  names(varGeno) <- 0:2
  
  df_ <- data.frame(
    genotype = varGeno[as.character(unlist(gtex.content$genotypes))],
    expr = unlist(gtex.content$data),
    stringsAsFactors = FALSE
  )
  return(list(
    json = gtex.content,
    geno.expr = df_
  ))
}
