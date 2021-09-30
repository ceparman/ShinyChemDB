
#library(httr2)

#smiles <- c('C1CCC1CC(CN(C)(C))CC(=O)CC')

#pubchem_smiles_substructure_search(smiles,threshold = 9)


pubchem_smiles_substructure_search <- function(smiles,MatchIsotopes=F,MatchCharges=F,MatchTautomers=F,RingsNotEmbedded=F,
                                               SingleDoubleBondsMatch=T,ChainsMatchRings=T,StripHydrogen=F,Stereo = "ignore",
                                               maxRecords=20,debug=F){


  convertLogical <- function(val) {
    if (is.logical(val)) {
      if (val == T) {
        return("true")
      } else {
        return("false")
      }


    } else return(NULL)

}


  if(debug) {print("running query")
    print(paste(smiles,maxRecords,MatchIsotopes))
    }

  url <- paste0("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/substructure/smiles/",
                smiles,
                "/JSON",
                "?MatchIsotopes=",convertLogical(MatchIsotopes),
                "&MatchCharges=",convertLogical(MatchCharges),
                "&MatchTautomers=",convertLogical(MatchTautomers),
                "&RingsNotEmbedded=",convertLogical(RingsNotEmbedded),
                "&SingleDoubleBondsMatch=",convertLogical(SingleDoubleBondsMatch),
                "&ChainsMatchRings=",convertLogical(ChainsMatchRings),
                "&StripHydrogen=",convertLogical(StripHydrogen),
                "&Stereo=",Stereo,
                "&MaxRecords=",maxRecords)

  if(debug) {print(url)}


  r1 <-httr2::request(url) %>%  httr2::req_options(http_version = 1) %>% httr2::req_method("POST") %>% httr2::req_perform()

  listkey <-  httr2::resp_body_json(r1)$Waiting$ListKey


  result_url <- paste0("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/listkey/",
                       listkey,"/cids/JSON")
  r2 <-httr2::request(result_url)   %>% httr2::req_method("POST") %>% req_perform()


   while(!is.null(httr2::resp_body_json(r2)$Waiting$Message )){


      r2 <-httr2::request(result_url) %>%  httr2::req_options(http_version = 1) %>% httr2::req_method("POST") %>% httr2::req_perform()
          Sys.sleep(2)
   }

cid_list <- unlist(resp_body_json(r2)$IdentifierList$CID)


compound_url <- paste0("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/cid/",
                       paste0(cid_list,collapse=","),
                       "/property/MolecularFormula,MolecularWeight,CanonicalSMILES/CSV"
                       )

table<- read.csv(compound_url)

table$`Compound Page` <- paste0("https://pubchem.ncbi.nlm.nih.gov/#query=",table$CID)



  DT::datatable(data = table)
 }


