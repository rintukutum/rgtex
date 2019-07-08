#' @name singleTissueEqtl
#' @title Association information per tissue
#' @description calculates the gene-variant association
#' @export
singleTissueEqtl <- function(
  snpID = NULL,
  gencodeID = NULL,
  geneSymbol = NULL,
  tissueID = NULL,
  datasetID = NULL
){
  snp.check <- !is.null(snpID)
  geneSymbol.check <- !is.null(geneSymbol)
  gencodeID.check <- !is.null(gencodeID)
  dataset.check <- !is.null(datasetID)
  if(any(c(snp.check,geneSymbol,gencodeID.check))){
    
  }else{
    stop("Please provide SNP ID or gene sysmbol or gencode ID")
  }
  temp.url <- "https://gtexportal.org/rest/v1/association/singleTissueEqtl?format=json"
  if(snp.check){
    temp.snpID <- paste0("&snpId=",snpID)
  }else{
    temp.snpID <- ''
  }
  if(geneSymbol.check){
    temp.geneSymbol <- paste0("&geneSymbol=",geneSymbol)
  }else{
    temp.geneSymbol <- ''
  }
  if(gencodeID.check){
    temp.gencodeID <- paste0("&gencodeId=",gencodeID)
  }else{
    temp.gencodeID <- ''
  }
  if(!is.null(tissueID)){
    temp.tissueID <- paste0("&tissueSiteDetailId=",tissueID)
    if(!any(gtex.meta$tissueID %in% tissueID)){
      stop('Please check the list of tissue IDs!')
    }
  }else{
    stop('Please provide any tissueID')
  }
  # c('gtex_v6p','gtex_v7')
  if(dataset.check){
    temp.datasetID <- paste0("&datasetId=",datasetID)
  }else{
    stop('Please provide GTEx dataset ID')
  }
  gtex.url <- paste0(
    temp.url,
    temp.geneSymbol,
    temp.gencodeID,
    temp.snpID,
    temp.tissueID,
    temp.datasetID
  )
  #library('httr')
  gtex.request <- httr::GET(
    url = gtex.url
  )
  if(gtex.request$status_code == 200){
    cat('Connection estabilshed!')
  }
  gtex.content <- httr::content(gtex.request)
  return(gtex.content)
}