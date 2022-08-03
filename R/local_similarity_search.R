

#' local_similarity_search
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
local_similarity_search <- function(smiles,db_info,threshold,max_records,debug=FALSE){
  #
#Setup databases




  url_path = paste0(db_info$dbscheme ,db_info$creds$user,":",db_info$creds$pass,db_info$dbinstance ,"/database")

  ss_db <- mongolite::mongo(db="chemdb",url = url_path ,collection = "molecules")

  ss_db2 <- mongolite::mongo(db="chemdb",url = url_path ,collection = "mfp_counts")


  results <- mongochem::smiles_similarity_search(sim_db = ss_db,counts_db = ss_db2,smiles = smiles,threshold = threshold/100)

  if( nrow(results>0 ) ) {

  results <- results %>% head(max_records) %>% mutate(tanimoto = round(tanimoto,3)) %>%  arrange(desc(tanimoto))




  query <- paste0( '{"_id": {"$in":[', paste0('{"$oid" :"',results$`_id`,'"}',collapse = ",") ,']}}')


  molecules <- ss_db$find( query =  query, fields =
                             '{"ID": true,"smiles": true,"Collection": true, "Common name":true ,"Molecular Weight":true,"Formula":true,"link":true}'
                           )


  results <- results |> dplyr::left_join(molecules) |>
                        select(-`_id`)


  }


  ss_db$disconnect()
  ss_db2$disconnect()

  results
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
