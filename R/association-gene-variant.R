#' @name gtexAsso.GencodeVariant
#' @title Association information 
#' @description calculates the gene-variant association 
#' @source https://gtexportal.org/home/api-docs/#/
#' @return gene level
gtexAsso.GencodeVariant <- function(
  gencodeID = 'ENSG00000105341.14',
  variantID = 'rs4674',
  tissueSiteDetailID = 'Adipose_Subcutaneous',
  datasetID = 'gtex_v7'
){
  #' check functions 
  #' later
  gtex.url = paste0(
    'https://gtexportal.org/rest/v1/association/dyneqtl?',
    'gencodeId=',
    gencodeID,
    '&',
    'variantId=',
    variantID,
    '&',
    'tissueSiteDetailId=',
    tissueSiteDetailID,
    '&',
    'datasetId=',
    datasetID
  )
  #library('httr')
  gtex.request <- httr::GET(
    url = gtex.url
  )
  if(gtex.request$status_code == 200){
    cat('Connection estabilshed!')
  }
  gtex.content <- httr::content(gtex.content)
  
  box <- lapply(gtex.content$boxplots,function(x){
    x.attr <- x[c('lowerLimit','median','numSamples','q1','q3','upperLimit')]
    xx <- list(
      attr = unlist(x.attr),
      outliers = as.numeric(unlist(x['outliers']))
    )
    return(xx)
  }
  )
  gtex.elements <- list(
    data = data.frame(
      "expression" = unlist(gtex.content$data),
      "genotype" = unlist(gtex.content$genotypes),
      stringsAsFactors = FALSE
    ),
    extra = list(
      "tissueSiteDetailId" = gtex.content$tissueSiteDetailId,
      "variantId" =  gtex.content$variantId,
      "hetCount"=  unlist(gtex.content$hetCount),
      "homoAltCount"= unlist(gtex.content$homoAltCount),
      "homoRefCount"= unlist(gtex.content$homoRefCount)
    ),
    score = list(
      "maf" = gtex.content$maf,
      "nes" = gtex.content$nes,
      "pValue" = gtex.content$pValue,
      "pValueThreshold"= gtex.content$pValueThreshold,
      "tStatistic" = gtex.content$tStatistic
    ),
    "boxplot" = box
  )
  return(gtex.elements)
}