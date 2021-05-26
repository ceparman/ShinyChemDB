
library(mongolite)
library(jsonlite)
library(config)
library(ShinyPlatform)
library(chemblr)
library(webchem)
library(ChemmineR)
library(ChemmineOB)
library(fingerprint)
library(rcdk)
library(fmcsR)
library(dplyr)


library(foreach)
library(doParallel)


#sdfstr <- read.SDFstr("/data/ChemData/chembl_28.sdf")


#load representations

chemreps<-read.csv2("/data/ChemData/chembl_28_chemreps.txt",sep = "\t")



#getwd()

#setwd("/data/ChemData/")

#write.SDFsplit(x=sdfstr, filetag="myfile", nmol=100000)




#setup parallel backend to use many processors
cores=detectCores()
cl <- makeCluster(cores[1]-3) #not to overload your computer
registerDoParallel(cl)



#Start looping over files


#sdfset <- read.SDFset("/data/ChemData/myfile0000001_0100000.sdf",skipErrors)

#valid <- validSDF(sdfset)
#sdfset <- sdfset[valid]

#full_sdf <- readRDS("myfile0000001_0100000.RDS")



all_creds <- jsonlite::fromJSON(safer::decrypt_string("VM4MQrjDweGlYbsnWMSFYvvyZpP3Pe2K9jnj3wYdGLSSuOtvPKuNdO3XNxZ9fAH5TbvHVaXw6u9uNt+O9m5qMwcYNvsW+ThiHJQ76/0ucArAlYv/nYa3F982CPKMRmw175d5mexwSeByRJacw7HMqVHXp9qObawTpENfKkipR/RuYOMA/rckRZfRaX1f6rS9Am8HNfNB8c/lLr6vMDvmrKzy/h+Ij8tAuyHUMdVNAGl/wemeSFon/eUu"))

creds <-all_creds[["user"]]
#

dbscheme  <- 'mongodb+srv://'
dbinstance <-  '@cluster0.41ox5.mongodb.net'
dbname <- 'chemdb'


#mongodb://localhost:27017



url_path = paste0(dbscheme ,creds$user,":",creds$pass,dbinstance ,"/database")

#db <- mongo(db="chemdb",url = url_path ,collection = "similarity")


db <- mongo(db="chemdb",url = "mongodb://localhost:27017" ,collection = "similarity")




#loop over files

files <- list.files("/data/ChemData/",pattern = "^my",full.names = T)
#skip first
for(f in 11:length(files)){

  sdfset <-  read.SDFset(files[f],skipErrors)



#Get pubchem_ids

pubchem_id <- sdfid(sdfset)

rep_set <- chemreps %>% filter(chembl_id %in% pubchem_id )

smiles <- rep_set$canonical_smiles


#rdmol<- NULL

#for(i in 1 :length(sdfset)){
#  rdmol <- c(rdmol,  convertFormat("SMI","MOL",smiles[i]))
#}

#fingerprints



documents <- NULL

documents <-  foreach(i =11:length(sdfset),.packages=c("rcdk","jsonlite")) %dopar% {

  mols <- parse.smiles(smiles[i])
  fp <- get.fingerprint(mols[[1]],depth = 2,size = 1024,type = "circular",circular.type = 'ECFP4',verbose = T)

  mfp_bits <- as.character(fp@bits)
  mfp_count <- length(mfp_bits)

  #Create full object


  document <- toJSON(list(  id =  pubchem_id[i],
                            smiles =  smiles[i],
                            mfp_bits = mfp_bits,
                            mfp_count = mfp_count
  ),auto_unbox = T
  )

  document

}


documents <- unlist(documents)

#Create db connection
print(files[f])
#insert into database
db$insert(documents)



}

#Create indexes

#db$index(add = '{"mfp_bits" : 1}')
#db$index(add = '{"mfp_count" : 1}')



#Create counts table



#count_get <- function(blist,bit){
#
#  if (is.null( blist[[bit]])  ) {
#    blist[bit] <- 1
#  } else {
#    blist[bit] <- blist[[bit]]  + 1
#  }
#
#  blist
#
#}


#all_ids <-  db$find(fields= '{"id":true}')
#
# chunk_size <- 100000
#
# mfp_counts <- list()
#
# n_chunks <- floor(nrow(all_ids)/chunk_size) +1
#
#
# for(i in 1:n_chunks){
#
#   ids_to_get <- na.omit(all_ids[ (1+ (i -1)*chunk_size):(i*chunk_size) ,2 ])
#
#
#   fp<-   db$find(toJSON(list(id = list(`$in` = ids_to_get))))
#
#
#   bits <- as.character(unlist(fp$mfp_bits))
#
#
#   for( b in bits){
#     mfp_counts <- count_get(mfp_counts,b)
#
#   }
#
# print(i)
# }

#db2 <- mongo(db="chemdb",url = url_path ,collection = "mfp_counts")

db2 <- mongo(db="chemdb",url = "mongodb://localhost:27017" ,collection = "mfp_counts")
#
# mfp_counts2 <- data.frame(bit = names(mfp_counts),
#                           count = unlist(mfp_counts)
# )
# db2$insert(mfp_counts2)
#






counts <- db$aggregate( '[{"$unwind":"$mfp_bits"},
                        {"$group":{"_id":"$mfp_bits",
                                   "count":{"$sum":1}
                                  }
                        },
                        {"$group":{"_id":null,
                                   "mfp_bits_details":{"$push":{"bit":"$_id","count":"$count"}}}},
                        {"$project":{"_id":0,"mfp_bits_details":1}}

                        ]'  )










mfp_counts <- counts$mfp_bits_details[[1]]



db2$insert(mfp_counts)



db2$index(add = '{"bit" : 1}')



#Query bd

#mols <- parse.smiles(smiles[13])

mols <- parse.smiles("O=C(Oc1ccccc1C(=O)O)C")

mols <- parse.smiles("CN1C=NC2=C1C(=O)N(C(=O)N2C)")
qfp <- as.character(
  get.fingerprint(mols[[1]],depth = 2,size = 1024,type = "circular",circular.type = 'ECFP4',verbose = T)@bits )


threshold <- 0.8

qn <- length(qfp)                           # Number of bits in query fingerprint
qmin <- ceiling(qn * threshold)     # Minimum number of bits in results fingerprints
qmax <- qn / threshold              # Maximum number of bits in results fingerprints
ncommon <- qn - qmin + 1



req_bits <- db2$find( query = toJSON ( list(
                                           bit = list( `$in`= qfp))),
                       limit=ncommon,
                      sort = '{"count": -1}'

                )


#
#
# df <- db$find( query = toJSON( list(
#   mfp_bits = list(`$in` = req_bits$bit ),
#   mfp_count = list(`$gte` = qmin , `$lte` = qmax)
# )
# )
# )
#
#
# intersection <- unlist(lapply( df$mfp_bits , function(x) length( intersect(x, qfp))) )
#
# pn <-  df$mfp_count
#
# tanimoto = intersection / (pn + qn - intersection)
#
# df$id[tanimoto > threshold]





aggregate <- paste0('[  {"$match": {"mfp_count": {"$gte":', qmin,', "$lte":', qmax,'}, "mfp_bits": {"$in":', toJSON(paste0(req_bits$bit)),'}}},
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


system.time(db$aggregate(aggregate))

db$aggregate(aggregate)



stopCluster(cl)



