corr <- function(direct,threshold = 0){
    corrvec<- vector()
    setwd(direct)
    file_names = list.files(pattern="*.csv")
    comp = complete("C:/R_Coursera/Course2/Week2/specdata/")
    comp = comp['id'][comp['nobs']>threshold]
    for (i in comp) {
        if (i>0) {
            filedir = paste(direct,file_names[i],sep="")
            df = read.csv(file=filedir, header=TRUE, sep=",")
            sul = df[['sulfate']][complete.cases(df[['sulfate']],df[['nitrate']])]
            nit = df[['nitrate']][complete.cases(df[['sulfate']],df[['nitrate']])]
            
            if (i==comp[1]) {
                corrvec = cor(sul,nit,use="complete.obs")
            }
            else{
                corrvec = c(corrvec,cor(sul,nit,use="complete.obs"))
            }
        }
        else{
            corrvec = c(corrvec,0)
        }

    }
    corrvec
}