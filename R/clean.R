##' @title Clean the pheontype and expression data from TCGA
##' @description Delete the duplicated individual and rename the colnames.
##'
##' @details nothing
##' @param phe data.frame, the raw expression matrix with colname of molecular id and individual ids.
##' @param expr data.frame, the raw expression matrix with colname of molecular id and individual ids.
##' @return data.frame, the clean expression matrix with colname of molecular id and individual ids.
##' @examples 
##' data(phe);data(expr)
##' cleanData = clean(phe,expr)
##' 
clean <- function(phe,expr){
  phe = cleanphe(phe)
  expr = cleanExpr(expr)
  common_id = intersect(phe$bcr_patient_barcode,colnames(expr))
  phe = phe[phe$bcr_patient_barcode %in% common_id,]
  expr = t(expr[,common_id])
  phe_expr = cbind(phe,expr)
  return(phe_expr)
}


##' @title Clean the phenotype data from TCGA
##' @description Given a phenotype data frame, delete the individual with NA .
##'
##' @details nothing
##' @param phe data.frame, the raw expression matrix with colname of molecular id and individual ids.
##' @return data.frame, the clean expression matrix with colname of molecular id and individual ids.
##' @examples 
##' data(phe);phe = cleanphe(phe)
cleanphe <- function(phe){
  #remove survial time and vital status with NA
  phe = unique(phe[!is.na(phe[,"vital_status"]),])
  phe[,"survivalTime"] = ifelse(phe[,"vital_status"]=="Alive",phe[,"days_to_last_follow_up"],phe[,"days_to_death"])
  phe = phe[!is.na(phe[,"survivalTime"]),]
  row.names(phe) = phe[,"bcr_patient_barcode"]
  
  #convert vital status to numeric
  phe[,"survivalStatus"] = as.numeric(as.factor(phe[,"vital_status"]))
  return(phe)
}

##' @title Clean the expression data from TCGA
##' @description Delete the duplicated individual and rename the colnames.
##'
##' @details nothing
##' @param expr data.frame, the raw expression matrix with colname of molecular id and individual ids.
##' @return data.frame, the clean expression matrix with colname of molecular id and individual ids.
##' @examples 
##' data(expr);expr = cleanExpr(expr)
##' @importFrom stringr str_sub str_split_fixed
cleanExpr <- function(expr){
  stopifnot(str_detect(colnames(expr)[2],pattern="TCGA-.{2}-.{4}-.{3}-.{3}-.{4}-.{2}"))
  #rowName = colnames(expr)[1]
  expr = as.data.frame(expr)
  row.names(expr)=expr[,1]
  ids = data.frame(sample=colnames(expr[,-1]),bcr_patient_barcode = str_sub(colnames(expr[,-1]),1,12))
  uniqIds = by(ids,ids$bcr_patient_barcode,function(x){
    sample_type_code = str_sub(str_split_fixed(x$sample,"-",7)[,4],1,2)
    x$sample[which.min(sample_type_code)]
  })
  uniqIds = as.character(uniqIds)
  expr=expr[,uniqIds]
  colnames(expr) = str_sub(uniqIds,1,12)
  return(expr)
}

