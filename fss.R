#    Calculate Financial Statement Similarity (FSS) for Balance Sheet and Income Statement Items   #;
# Brown, Ma, and Tucker (2023), "Financial Statement Similarity", Contemporary Accounting Research #;
#                                  @ Author: Guang Ma                                              #;
#                                 Version 1.1, February 2024                                       #;
# This code is provided as a brief guidance on calculating the FSS measure introduced in BMT (2023)#;
# Please feel free to modify it to fit your needs, as long as proper reference to BMT is provided  #;



# Load the required library;

library(whitening)
library(reshape2)
library(readr)
library(data.table)


## Step 1. Data Pre-processing ##;

# load data, you may need to load additional library depending on your file format;
# your data source should contain the filtering variables in addition to the BS and IS items ; 
fs <- read_csv("Compustat data to be loaded.csv",
                 col_types = cols(  .default = col_double(),
                                    gvkey = col_character(),
                                    datadate = col_date("%d%b%Y"),
                                    fyear = col_integer(),
                                    indfmt = col_character(),
                                    conm = col_character(),
                                    scf = col_character(),
                                    acctstd = col_character(),
                                    exchg = col_double(),
                                    sich = col_double()))

# drop financial format = FS ;
fs <-fs[fs$indfmt=='INDL',]

# stock exchange AMEX, NYSE, and NASDAQ;
fs <- fs[fs$exchg>5&fs$exchg<20,]
# keep SCF = "7";
fs <-fs[fs$scf=='7',]
# Accounting standards, US GAAP
fs <-fs[fs$acctstd=='US',]
fs <-fs[!is.na(fs$acctstd),]

# remove ADR, Holding, Group, LP/LLP;
fs<-fs[!'adr' %in% strsplit(tolower(gsub('[[:punct:] ]+',' ',fs$conm))," ")[[1]],]
fs<-fs[!'group' %in% strsplit(tolower(gsub('[[:punct:] ]+',' ',fs$conm))," ")[[1]],]
fs<-fs[!'lp' %in% strsplit(tolower(gsub('[[:punct:] ]+',' ',fs$conm))," ")[[1]],]
fs<-fs[!'llp' %in% strsplit(tolower(gsub('[[:punct:] ]+',' ',fs$conm))," ")[[1]],]
fs<-fs[!'holding' %in% strsplit(tolower(gsub('[[:punct:] ]+',' ',fs$conm))," ")[[1]],]
fs<-fs[!'hldg' %in% strsplit(tolower(gsub('[[:punct:] ]+',' ',fs$conm))," ")[[1]],]
fs<-fs[!'grp' %in% strsplit(tolower(gsub('[[:punct:] ]+',' ',fs$conm))," ")[[1]],]


# filter observations;
# you can add more filters to fit your needs;
fs<-fs[floor(fs$sich/1000)!=6,]

# drop columns with all missings;
fs[fs==""]<-NA
fs<-fs[, colSums(is.na(fs))<nrow(fs)]

# drop duplicates ;
fs<-unique(fs)


# drop the grand totals and level 1 subtotals #;
fs <-subset(fs,select=-c(revt, xopr, opiti, nopi, pi, txt, ib, ni,
                         at,lt,seq, 
                         act,ppent, intan, ao, lct, lo))


# define industry group, 3-digit SIC for example;
fs$sich3 <- floor(fs$sich/10)


##          There are two approaches of applying Mahalanobis-transformation on the financial statement data        ##;
## The first approach calculates the transformation matrix first and applys the transformation in the second step. ##;
## The second approach returns the transformed data directly in one step. If you are only interested in the        ##;
## transformed data, Approach 2 is recommended.....................................................................##;




## Approach 1 ##;
## Step 2. M-transformation for each year ##;

# for a given year, drop columns with all missing #;
for (y in unique(fs$fyear)){
  dat <- fs[which(fs$fyear==y),]
  dat <-dat[, colSums(is.na(dat))<nrow(dat)]
  

  
  # Drop items that are perfectly correlated with other items ;
  dat[is.na(dat)] <- 0

  # keep only BS/IS numerical items in the matrix;
  m0 <- as.matrix(dat[,c("the starting column of BS/IS items":ncol(dat))]) 
  m1 <- m0[, qr(m0)$pivot[seq_len(qr(m0)$rank)]]
  
  # Calculate Mahalanobis transformation matrix;
  m1.cov <- cov(m1)
  Z.m <- whiteningMatrix(m1.cov,method="ZCA")
  colnames(Z.m) <- colnames(qr(m0)$qr)[seq_len(qr(m0)$rank)]
  rownames(Z.m) <- colnames(qr(m0)$qr)[seq_len(qr(m0)$rank)]

  # merge back GVKEy and SICH3;
  m2 <- cbind(dat[,c(1,2)],m1)
  names(m2)<-c("gvkey","sich3",colnames(qr(m0)$qr)[seq_len(qr(m0)$rank)])



## Step 3. Calculate FSS for each industry-year ##; 
  if (nrow(m2)>2) {
    # repeat for each industry within the year;
    for (i in unique(m2$sich3)){
      dat0 <- dat[which(dat$sich3==i),names(m2)]
      dat0 <-dat0[, colSums(is.na(dat0))<nrow(dat0)]
      
        "drop lower level subtotals when their components are more than 75% non-missing, take ACO for example "
      if (("aco" %in% colnames(dat0)) & (("acox" %in% colnames(dat0)) | ("xpp" %in% colnames(dat0)) | ("tsca" %in% colnames(dat0)))){
        if (sum(!is.na(dat0$acox))>=nrow(dat0)*0.75 | sum(!is.na(dat0$xpp))>=nrow(dat0)*0.75 | sum(!is.na(dat0$tsca))>=nrow(dat0)*0.75) {
        dat0 <-subset(dat0,select=-c(aco))
        }
      }
  
      
      if (nrow(dat0)>2){
        dat1 <- dat0[,c(3:ncol(dat0))]
        Z.m2 <- Z.m[colnames(dat1),colnames(dat1)]
        dat2 <- tcrossprod(as.matrix(dat1), Z.m2) 
        

        if (nrow(dat2)>2) {
          sim <- reshape2::melt(1-as.matrix(dist.cosine(dat2)))
          fss_pair <- data.table(cbind(i,y,m2[sim[,1],1],m2[sim[,2],1],sim[,3]))
          colnames(fss_pair)<-c("sich3","fyear","gvkey","gvkey2","fss_pair")
          fss_pair <- transform(fss_pair, fss_pair = as.numeric(fss_pair),fyear = as.numeric(fyear))
          fss_pair<-fss_pair[which(fss_pair$gvkey != fss_pair$gvkey2),]
          
          # calculate yearly average of the pairwise FSS to obtain the firm-year version FSS;
          fss <- aggregate(fss_pair[, 5], list(fss_pair$gvkey,fss_pair$fyear), mean)
          colnames(fss)<-c("gvkey","fyear","fss")
          
          print(c(y," industry ",i," FSS calculated"))
          
          # save to any file you like, being MySQL or CSV files;

          rm(sim,fss_pair,fss)
          gc()
          
          
        }
        rm(dat1,dat2, Z.m2)
      }
      rm(dat0)
    }
    
  }
  rm(dat,m1,m2,m3)
}



## Approach 2 ##;
## Step 2. M-transformation for each year ##;

# for a given year, drop columns with all missing #;
for (y in unique(fs$fyear)){
  dat <- fs[which(fs$fyear==y),]
  dat <-dat[, colSums(is.na(dat))<nrow(dat)]
  

  
  # Drop items that are perfectly correlated with other items ;
  dat[is.na(dat)] <- 0

  # keep only BS/IS numerical items in the matrix;
  m0 <- as.matrix(dat[,c("the starting column of BS/IS items":ncol(dat))]) 
  m1 <- m0[, qr(m0)$pivot[seq_len(qr(m0)$rank)]]
  m1.zca <- whiten(m1,method="ZCA")


  # merge back GVKEy and SICH3;

  m2 <- cbind(dat[,c(1,2)],m1.zca)
  names(m2)<-c("gvkey","sich3",colnames(qr(dat)$qr)[seq_len(qr(dat)$rank)])
  

## Step 3. Calculate FSS for each industry-year ##; 
  if (nrow(m2)>2) {
    # repeat for each industry within the year;
    for (i in unique(m2$sich3)){
      dat0 <- dat[which(dat$sich3==i),names(m2)]
      dat0 <-dat0[, colSums(is.na(dat0))<nrow(dat0)]
      
        "drop lower level subtotals when their components are more than 75% non-missing, take ACO for example "
      if (("aco" %in% colnames(dat0)) & (("acox" %in% colnames(dat0)) | ("xpp" %in% colnames(dat0)) | ("tsca" %in% colnames(dat0)))){
        if (sum(!is.na(dat0$acox))>=nrow(dat0)*0.75 | sum(!is.na(dat0$xpp))>=nrow(dat0)*0.75 | sum(!is.na(dat0$tsca))>=nrow(dat0)*0.75) {
        dat0 <-subset(dat0,select=-c(aco))
        }
      }
  
      
      if (nrow(dat0)>2){
        dat1 <- dat0[,c(3:ncol(dat0))]

        sim <- reshape2::melt(1-as.matrix(dist.cosine(dat1)))
        fss_pair <- data.table(cbind(i,y,m2[sim[,1],1],m2[sim[,2],1],sim[,3]))
        colnames(fss_pair)<-c("sich3","fyear","gvkey","gvkey2","fss_pair")
        fss_pair <- transform(fss_pair, fss_pair = as.numeric(fss_pair),fyear = as.numeric(fyear))
        fss_pair <- fss_pair[which(fss_pair$gvkey != fss_pair$gvkey2),]
        
        # calculate yearly average of the pairwise FSS to obtain the firm-year version FSS;
        fss <- aggregate(fss_pair[, 5], list(fss_pair$gvkey,fss_pair$fyear), mean)
        colnames(fss)<-c("gvkey","fyear","fss")
        
        print(c(y," industry ",i," FSS calculated"))
        
        # save to any file you like, being MySQL or CSV files;

        rm(sim,fss_pair,fss)
        gc()
          
          
        
        rm(dat1)
      }
      rm(dat0)
    }
    
  }
  rm(dat,m1,m1.zca,m2)
}

