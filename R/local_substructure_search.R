

#' local_sunstructure_search
#'
#' @param smiles
#' @param db_info
#' @param thershold
#' @param max_records
#'
#' @return
#' @export
#'
#' @examples
local_smiles_substructure_search <- function(smiles,db_info,max_mismatches, threshold, min_overlap_coefficient,
                                    al,au,bl, bu,numParallel , max_records){
  #
#Setup databases




  url_path = paste0(db_info$dbscheme ,db_info$creds$user,":",db_info$creds$pass,db_info$dbinstance ,"/database")

  sim_db <- mongo(db="chemdb",url = url_path ,collection = "molecules")




  results <- mongochem::smiles_substructure_search(sim_db,smiles,max_mismatches, threshold/100,
                                                               min_overlap_coefficient/100,
                                                               al,au,bl, bu,numParallel)

  saveRDS(results,"results.Rds")

 if(nrow(results) > 0) {

  results <- results  %>% mutate(Tanimoto_Coefficient = round(Tanimoto_Coefficient,3)) %>%
                          mutate(Overlap_Coefficient = round(Overlap_Coefficient,3)) %>%
                       arrange(desc(Tanimoto_Coefficient))  %>% head(max_records)



  query <- paste0( '{"_id": {"$in":[', paste0('{"$oid" :"',results$`id`,'"}',collapse = ",") ,']}}')


  molecules <- sim_db$find( query =  query, fields =
                              '{"ID": true,"smiles": true,"Collection": true, "Common name":true ,"Molecular Weight":true,"Formula":true}'
  ) |> mutate(id = `_id`)


  results <- results |> dplyr::left_join(molecules,by = c("id" = "id")) |>
    select(-`_id`,-id)


}

  sim_db$disconnect()


  return(results)
}

# smiles <- "C1=CC=C(C(=C1)C(=O)O)OP(=O)=O"
#
# all_creds <- jsonlite::fromJSON(safer::decrypt_string(Sys.getenv("mongo_db_string")))
# creds <-all_creds[["user"]]
#
# db_info <-   db_info <- list (
#   dbscheme  = 'mongodb+srv://',
#   dbinstance =  '@cluster0.41ox5.mongodb.net',
#   dbname  = 'chemdb',
#   creds = creds
# )
#
# threshold <- .6
# max_records <- 10
