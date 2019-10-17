


#' 
#' @param doc_load DOC loaded to lake in mol C day^-1 
#' @param doc_lake mass of DOC in lake in mol C 
#' @param water_out volume of water leaving the lake through stream / groundwater in m^3 day^-1 
#' @param lake_vol volume of the lake in m^3 
#' @param decay Decay rate of DOC in fraction day^-1 
#' 
predict_lake_doc = function(doc_load, doc_lake, water_out, lake_vol, decay){
  
  cur_doc_conc = doc_lake / lake_vol # mol C / m^3 
  
  doc_predict = doc_lake - water_out * cur_doc_conc + doc_load - doc_lake * decay  # mol C 
  
  return(list(doc_predict = doc_predict, decay = decay)) 
}

