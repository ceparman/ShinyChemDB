


#' Substructure search using smiles
#'
#' @param sim_db  Similarity database
#' @param counts_db fingerprint counts database
#' @param smiles  smiles to search
#' @param max_mismatches max number of mismatched fingerprint bits
#' @param threshold %overlap required for inital search
#' @param  au=2 parameter for fmcsBatch final similarity
#' @param  a1=0 parameter for fmcsBatch final similarity
#' @param  bu=1 parameter for fmcsBatch final similarity
#' @param  bl=0 parameter for fmcsBatch final similarity
#' @param numParallel number of processors parameter for fmcsBatch final similarity
#' @return data frame with Query_Size,Target_Size,MCS_Size,Tanimoto_Coefficient,Overlap_Coefficient,id
#' @export
#'
#' @examples
#'
#'\dontrun{
#'
#'smiles <- "CC(=O)OC1=CC=CC=C1C(=O)O"
#'
#'sim_db <- mongolite::mongo(db="chemdb",url = "mongodb://localhost:27017" ,collection = "similarity")
#'
#'
#' smiles_substructure_search(sim_db_db,smiles)
#' }
#'

smiles_substructure_search <- function(sim_db,smiles,max_mismatches = 2, threshold = .85,
                                       min_overlap_coefficient = .8,
                                       al=0,au=2,bl =0, bu=1,numParallel = 2){

     mols <- rcdk::parse.smiles(smiles)


    query_fp <- rcdk::get.fingerprint(mols[[1]],depth = 2,size = 2048,type = "circular",circular.type = 'ECFP6',verbose = T)

    query_bits <- as.character(query_fp@bits) #bits to query

    qn<-   length( query_bits )


    min_length <- qn - max_mismatches  #min fp length

    qmin <- qn *threshold  #minimum fingerprint overlap

    sub_agg <- paste0('[  {"$match": {"mfp_count": {"$gte":',  min_length ,'}}},
                  {"$project": {
                  "common":{"$size": {"$setIntersection": ["$mfp_bits",',jsonlite::toJSON(paste0(query_bits)),']}},
                  "smiles":1,
                   "id":1}},
                   {"$match":{"common":{"$gte":',qmin,'}}}    ]')




    a<- sim_db$aggregate(sub_agg)



  if(nrow(a) < 1){ return(data.frame(Message = "No results")) } else{

    sdfset<- suppressWarnings( ChemmineR::smiles2sdf(a$smiles))

    similarities <-as.data.frame(  fmcsR::fmcsBatch( ChemmineR::smiles2sdf(smiles),sdfset),
                                    al=al,au=au, bl=bl,bu=bu,numParallel = numParallel)


    similarities$id <- a$id

    final_set <-   similarities |>   dplyr::filter(Overlap_Coefficient >=  min_overlap_coefficient)


    print(final_set)
    return(final_set)
}
}
