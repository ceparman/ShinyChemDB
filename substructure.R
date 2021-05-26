ef SubSearch(pattern, mol_collection, chirality=False):
  """
    Search MOL_COLLECTION for molecules with PATTERN
    as a substructure.
    :param pattern: An rdmol object that represents a desired substructure pattern.
    :param mol_collection: A MongoDB collection.
    :param chirality: Whether or not to include stereochemistry in search. Defaults to False.
    :return: A list of SMILES that have the desired substructure pattern.
    """
results = []

query_smiles <- "CC(=O)OC1=CC=CC=C1C(=O)O"

query_smiles <- sdf2smiles(fmcstest[1])[[1]]@smiles

mols <- parse.smiles(query_smiles)

query_fp <- get.fingerprint(mols[[1]],depth = 2,size = 1024,type = "circular",circular.type = 'ECFP4',verbose = T)

query_bits <- as.character(query_fp@bits)

qfp_len <-   length( query_bits )



db <- mongo(db="chemdb",url = "mongodb://localhost:27017" ,collection = "similarity")

docs<- db$find(query = toJSON(
                      list(mfp_count = list(`$gte` = qfp_len ),
                           mfp_bits = list(`$all` = query_bits ))  #This is exact hits only, need to add missmatches
                     ,auto_unbox = T)
)




          a <- list( `$project` =
                                  list ( T = list(`$unwind` = "$mfp_bits"),
                                         Q =     query_bits,
                                         commontoboth =  list(`$setIntersection` = c("$T","$Q")
                                                    ),
                                      `_id` =  0
          )



          )





          counts <- db$aggregate( toJSON(a,auto_unbox = T))


hits<- list()
for(d in 1:length(docs)){

hits[[d]] <-   fmcs(smiles2sdf(query_smiles),smiles2sdf( docs$smiles[d]))

}

  rdmol = Chem.Mol(molDoc['rdmol'])
if rdmol.HasSubstructMatch(pattern, useChirality=chirality):
  results.append(molDoc['index'])
return results



#------------------------------------
db <- mongo(db="chemdb",url = "mongodb://localhost:27017" ,collection = "similarity")

query_smiles <- "CC(=O)Oc1ccccc1C(=O)O"

#query_smiles <- sdf2smiles(fmcstest[1])[[1]]@smiles

mols <- parse.smiles(query_smiles)


query_fp <- get.fingerprint(mols[[1]],depth = 2,size = 1024,type = "circular",circular.type = 'ECFP4',verbose = T)

query_bits <- as.character(query_fp@bits)

qfp_len <-   length( query_bits )

max_mismatches <- 2

qmax <- qfp_len + max_mismatches

qmin <- qfp_len - max_mismatches

qn <- qfp_len
threshold <- .7




sub_agg <- paste0('[  {"$match": {"mfp_count": {"$gte":', qmin,'}}},
                  {"$project": {
                  "common":{"$size": {"$setIntersection": ["$mfp_bits",',toJSON(paste0(query_bits)),']}},
                  "smiles":1,
                   "id":1}},
                   {"$match":{"common":{"$gte":',qmin*threshold,'}}}    ]')





a<- db$aggregate(sub_agg)



sdfset<- smiles2sdf(a$smiles)


fb <- fmcsBatch(smiles2sdf(query_smiles),sdfset,au=2, bu=1,numParallel = 15)

fdf <- as.data.frame(fb)
fdf$id <- a$id

ffdf <- fdf %>% filter(Overlap_Coefficient >.98 , MCS_Size >= 31)ffdf <- fdf %>% filter(Overlap_Coefficient >.98 , MCS_Size >= 31)







sub_agg <- paste0('[  {"$match": {"mfp_count": {"$gte":', qmin,'}}},
      {"$project": {
      "tanimoto": {"$let": {
        "vars": {"common": {"$size": {"$setIntersection": ["$mfp_bits",',toJSON(paste0(qfp)),']}}},
        "in": {"$divide": ["$$common", {"$subtract": [{"$add": [',qn, ',"$mfp_count"]}, "$$common"]}]}
      }},
      "smiles": 1,
      "id": 1
    }},
    {"$match": {"tanimoto": {"$gte":', threshold,'}}}
  ]')








