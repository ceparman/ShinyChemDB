


library(mongolite)
library(jsonlite)
library(config)
library(ShinyPlatform)
library(chemblr)
library(webchem)
library(ChemmineR)
library(fingerprint)
library(rcdk)



#Creatr db connection

all_creds <- jsonlite::fromJSON(safer::decrypt_string("VM4MQrjDweGlYbsnWMSFYvvyZpP3Pe2K9jnj3wYdGLSSuOtvPKuNdO3XNxZ9fAH5TbvHVaXw6u9uNt+O9m5qMwcYNvsW+ThiHJQ76/0ucArAlYv/nYa3F982CPKMRmw175d5mexwSeByRJacw7HMqVHXp9qObawTpENfKkipR/RuYOMA/rckRZfRaX1f6rS9Am8HNfNB8c/lLr6vMDvmrKzy/h+Ij8tAuyHUMdVNAGl/wemeSFon/eUu"))

creds <-all_creds[["user"]]
#

dbscheme  <- 'mongodb+srv://'
dbinstance <-  '@cluster0.41ox5.mongodb.net'
dbname <- 'chemdb'

url_path = paste0(dbscheme ,creds$user,":",creds$pass,dbinstance ,"/database")

db <- mongo(db="chemdb",url = url_path ,collection = "similarity")

#load example data
data(sdfsample)
sdfset <- sdfsample

#Create df for loading

#Need generate a JSON for direct load.

pubchem_id <- sdfid(sdfset)



smiles <- NULL
  for(i in 1 :100){
smiles <- c(smiles,  as.character(sdf2smiles(sdfset[i])[[1]]))
  }

rdmol<- NULL

for(i in 1 :100){
  rdmol <- c(rdmol,  convertFormat("SMI","MOL",smiles[i]))
}

#fingerprints



documents <- NULL

for(i in 1 :100){

  mols <- parse.smiles(smiles[i])
  fp <- get.fingerprint(mols[[1]],depth = 2,size = 1024,type = "circular",circular.type = 'ECFP4',verbose = T)

  mfp_bits <- as.character(fp@bits)
  mfp_count <- length(mfp_bits)

#Create full object


  document <- toJSON(list(  id =  pubchem_id[i],
                     smiles =  smiles[i],
                     mfp_bits = mfp_bits,
                     mfp_count = mfp_count
                    )
    )


  documents <- c(documents,document)

  }


#insert into database
db$insert(documents)

#Create indexes

db$index(add = '{"mfp_bits" : 1}')
db$index(add = '{"mfp_count" : 1}')



#Create counts table



count_get <- function(blist,bit){

          if (is.null( blist[[bit]])  ) {
                      blist[bit] <- 1
          } else {
            blist[bit] <- blist[[bit]]  + 1
          }

            blist

          }


all_ids <-  db$find(fields= '{"id":true}')

chunk_size <- 8

mfp_counts <- list()

n_chunks <- floor(nrow(all_ids)/chunk_size) +1


for(i in 1:n_chunks){

    ids_to_get <- na.omit(all_ids[ (1+ (i -1)*chunk_size):(i*chunk_size) ,2 ])


 fp<-   db$find(toJSON(list(id = list(`$in` = ids_to_get))))


 bits <- as.character(unlist(fp$mfp_bits))


 for( b in bits){
   mfp_counts <- count_get(mfp_counts,b)

 }


}

db2 <- mongo(db="chemdb",url = url_path ,collection = "mfp_counts")

mfp_counts2 <- data.frame(bit = names(mfp_counts),
                          count = unlist(mfp_counts)
                          )
db2$insert(mfp_counts2)

db2$index(add = '{"bit" : 1}')

#Query bd

mols <- parse.smiles(smiles[13])


qfp <- as.character(
     get.fingerprint(mols[[1]],depth = 2,size = 1024,type = "circular",circular.type = 'ECFP4',verbose = T)@bits )


threshold <- 0.3

qn <- length(qfp)                           # Number of bits in query fingerprint
qmin <- ceiling(qn * threshold)     # Minimum number of bits in results fingerprints
qmax <- qn / threshold              # Maximum number of bits in results fingerprints
ncommon <- qn - qmin + 1



req_bits <- db2$find( query = toJSON ( list(
                       bit = list( `$in`= qfp)
                              )

                         ),
                limit=ncommon,
                sort = '{"count": -1}'

            )




  df <- db$find( query = toJSON( list(
                   mfp_bits = list(`$in` = req_bits$bit ),
                   mfp_count = list(`$gte` = qmin , `$lte` = qmax)
                   )
                  )
                )


  intersection <- unlist(lapply( df$mfp_bits , function(x) length( intersect(x, qfp))) )

  pn <-  df$mfp_count

  tanimoto = intersection / (pn + qn - intersection)

  df$id[tanimoto > threshold]





  aggregate <- paste0('[  {"$match": {"mfp_count": {"$gte":', qmin,', "$lte":', qmax,'}, "mfp_bits": {"$in":', toJSON(paste0(req_bits$bit)),'}}},
     {"$project": {
      "tanimoto": {"$let": {
        "vars": {"common": {"$size": {"$setIntersection": ["$mfp_bits",',toJSON(paste0(qfp)),']}}},
        "in": {"$divide": ["$$common", {"$subtract": [{"$add": [',qn, ',"$mfp_count"]}, "$$common"]}]}
      }},
      "smiles": 1,
      "chembl_id": 1
    }},
    {"$match": {"tanimoto": {"$gte":', threshold,'}}}
  ]')


db$aggregate(aggregate)

