#Run on Linux Platform
library(png)
setwd("/work/yzstat/rwang14/HCCpipeline")
source("/work/yzstat/rwang14/visible.txt")
input <- commandArgs(trailingOnly = TRUE)
jobid <- input[1]


#--------- Creat data structure ---------
  
  VisibleName = c("Vis_SV_0", "Vis_SV_36", "Vis_SV_72", "Vis_SV_108", "Vis_SV_144")
  FileNames = list.files("5-2-16 maize_images")
  #Input = function(FileNames, VisibleName ,ID_start, ID_stop, Geno_start, Geno_stop, Date_start,Date_stop){
  #FileNames = list.files("/work/yzstat/rwang14/HCCpipeline/5-2-16 maize_images", pattern = paste(".+52-",jobid,"-.",sep=''))
  #print (FileNames)
  N = length(FileNames)
  K = length(VisibleName)
  PlantID = substr(FileNames, start = 14, stop = 16)
  #PlantID = substr(FileNames, start = ID_start, stop = ID_stop)
  Genotype = substr(FileNames, start = 18, stop = 20)
  #Genotype = substr(FileNames, start = Geno_start, stop = Geno_stop)
  #plant id which has empty pot
  PlantIDX = PlantID[Genotype == 'xxx']
  #plant id which does not have empty pot
  PlantIDNX = PlantID[Genotype != 'xxx']
  #genotype names without empty pot
  GenotypeNX = Genotype[Genotype != 'xxx']
  #number of plants without empty pot
  PlantIDUnique = unique(PlantIDNX)
  #number of plants with empty pot
  PlantIDUniqueX = unique(PlantIDX)
  #number of genotypes which do not have empty pot
  GenotypeUnique = unique(GenotypeNX)
  Date = substr(FileNames, start = 27, stop = 31)
  #Date = substr(FileNames, start = Date_start, stop = Date_stop)
  DateX1 = Date[PlantID == PlantIDUniqueX[1]]
  #DateX = cbind(Date[PlantID == PlantIDUniqueX[1]], Date[PlantID == PlantIDUniqueX[2]])
  #results = c()
  
  #--------- Check data structure --------
  #plant ID does contain X
  NPX = c()
  for (i in 1 : length(PlantIDUniqueX)){
    #eg. if i = 1, there are 29 plants whose id is 141(whose genotype is xxx)
    #if i = m, there are ... plants whose id is mth plants whose genotype is xxx
    NPX[i] = length(PlantID[PlantID == PlantIDUniqueX[i]])
  }
  #Dates for plants with xxx
  DateX = matrix(0, max(NPX), length(PlantIDUniqueX)) 
  #eg. the first 29 rows and the 1st col means the date for the id 141 plant
  for (i in 1 : length(PlantIDUniqueX)){
    DateX[1 : NPX[i], i] = Date[PlantID == PlantIDUniqueX[i]]
  }
  
  NP = c()
  for (i in 1 : length(PlantIDUnique)){
    NP[i] = length(PlantID[PlantID == PlantIDUnique[i]])
  }
  DateNX = matrix(0, max(NP), length(PlantIDUnique))
  for (i in 1 : length(PlantIDUnique)){
    DateNX[1 : NP[i], i] = Date[PlantID == PlantIDUnique[i]]
  }
  
  for (i in 281 : 281){
       #length(PlantIDUnique)){
   # print(i)
    #--------- Align dates with background (sperately for the two rooms) --------
    PlantInd0 = FileNames[PlantID == PlantIDUnique[i]]
    #Dates for Plants without xxx
    DateInd = Date[PlantID == PlantIDUnique[i]]
   
    
    if (as.numeric(PlantIDUnique[i]) > 280){
      #Dates for the 21st PlantID with xxx: id288
      Datetemp = Date[PlantID == PlantIDUniqueX[21]]
      PlantIndX0 = FileNames[PlantID == PlantIDUniqueX[21]]
      Date0 = unique(Datetemp)
      Date1 = intersect(Date0, DateInd)
      PlantIndX = c()
      PlantInd = c()
      for (i1 in 1 : length(Date1)){
        indextempX = which(Datetemp == Date1[i1])[1]
        PlantIndX[i1] = PlantIndX0[indextempX]
        indextempNX = which(DateInd == Date1[i1])[1]
        PlantInd[i1] = PlantInd0[indextempNX]
      }
    }
    
    M = length(Date1)
    Res = matrix(0, M, 3 + 50)
    #PlantID for plants without x
    Res[, 1] = rep(PlantIDUnique[i], M)
    #Plant Genotypes for plantid without x
    Res[, 2] = rep(GenotypeNX[PlantIDNX == PlantIDUnique[i]][1], M)
    #Res[, 3] = DateInd
    #Dates for intersection with temp
    Res[, 3] = Date1
    temp = c()
    for (j in 1 : M){
      #print(j)
      shape = c()
      colorRGB = c()
      #path for the plant id with x
      path1X = paste("5-2-16 maize_images/", PlantIndX[j], sep = "")
      #path for the plant id with xxx inside of the folder VisibleName
      pathX = paste(path1X, "/", VisibleName[1], sep = "")
      #find the path for the image of the plant id with xxx
      pathImageX = paste(pathX, "/", "0_0_0.png", sep = "")
      imageX = readPNG(pathImageX)
      tempG = ColorG(imageX, rowThreshold = 0.002)
      #imageX = tempG$c
      #writePNG(imageX, "try/imageX.png")
      png(filename = "try/HistX.png")
      #hist(tempG$rowmean)
      dev.off()
      #dev.new()
      lowerRowBound = tempG$lowb
      print(PlantIDUnique)
      #
      if(!is.null(lowerRowBound)){
      for (k in 1 : K){
        print(k)
        path1 = paste("5-2-16 maize_images/", PlantInd[j], sep = "")
        path = paste(path1, "/", VisibleName[k], sep = "")
        pathImage = paste(path, "/", "0_0_0.png", sep = "")
        pathInfo = paste(path, "/", "info.txt", sep = "")
        path1X = paste("5-2-16 maize_images/", PlantIndX[j], sep = "")
        pathX = paste(path1X, "/", VisibleName[k], sep = "")
        pathImageX = paste(pathX, "/", "0_0_0.png", sep = "")
        
        Information = read.csv(pathInfo, head = FALSE, sep = "")
        Xsize = Information[2, 5]
        Ysize = Information[3, 5]
        
        imageOriginal = readPNG(pathImage)
        sizeOrignal = dim(imageOriginal)[c(1, 2)]
        ###
       
        #print(K)
        #print(M)
        #print(lowerRowBound)
        ####
        print(sizeOrignal)
        resultColor = ColorB(imageOriginal)
        ColBound = c(resultColor$lb, resultColor$rb)
        RI = matrix(1, sizeOrignal[1], sizeOrignal[2])
        RI[, c(1 : ColBound[1])] = 0
        RI[, c(ColBound[2] : sizeOrignal[2])] = 0
        RI[c(lowerRowBound : sizeOrignal[1]), ] = 0
        imageOriginalR = imageOriginal[, , 1]
        imageOriginalG = imageOriginal[, , 2]
        imageOriginalB = imageOriginal[, , 3]
        imageCutR = matrix(imageOriginalR[RI == 1], lowerRowBound - 1, ColBound[2] - ColBound[1] - 1)
        imageCutG = matrix(imageOriginalG[RI == 1], lowerRowBound - 1, ColBound[2] - ColBound[1] - 1)
        imageCutB = matrix(imageOriginalB[RI == 1], lowerRowBound - 1, ColBound[2] - ColBound[1] - 1)
        imageCut = array(c(imageCutR, imageCutG, imageCutB), dim = c(dim(imageCutR), 3))
        writePNG(imageCut, "/work/yzstat/rwang14/HCCpipeline/try/imageCut.png")
        #writePNG(imageOriginal, "try/imageOriginal.png")
        
        image1 = sample(imageOriginal, RowSample = 2, ColSample = 2)
        imageX = readPNG(pathImageX)
        imageX1 = sample(imageX, RowSample = 2, ColSample = 2)
        #if (k == 3 | (j >= 16 & k == 2) | (j >= 16 & k == 4)) {tempG1 = ColorG1(imageX1)} else {tempG1 = ColorG(imageX1, rowThreshold = 0.002)}
        #tempG1 = ColorG1(imageX1)
        tempG1 = ColorG(imageX1, rowThreshold = 0.002)
        imageX1G = tempG1$c
        imageContrast1 = abs(image1 - imageX1G)[, , 1 : 3]
        writePNG(image1, "/work/yzstat/rwang14/HCCpipeline/try/image1.png")
        writePNG(imageContrast1, "/work/yzstat/rwang14/HCCpipeline/try/imageContrast1.png")
        
        imageB1 = imageBinary(image1,  weight = c(-1, 2, -1), threshold1 = 30 / 255, threshold2 = 0.02)
        imageB2 = imageBinary(imageContrast1,  weight = c(1, -2, 1), threshold1 = 0.7, threshold2 = 0)
        imageB = imageB1 * imageB2
        writePNG(imageB1, "/work/yzstat/rwang14/HCCpipeline/try/imageBGreen.png")
        writePNG(imageB2, "/work/yzstat/rwang14/HCCpipeline/try/imageBConstrast.png")
        writePNG(imageB, "/work/yzstat/rwang14/HCCpipeline/try/imageB.png")
        
        imageBD = dilation(imageB, mask = matrix(1, 5, 5))
        imageBDE = erosion( imageBD, mask = matrix(1, 5, 5) )
        writePNG(imageBDE, "/work/yzstat/rwang14/HCCpipeline/try/imageBDE.png")
        
        imageBDEE = erosion( imageBDE, mask = matrix(1, 2, 2) )
        imageBDEED =  dilation( imageBDEE, mask = matrix(1, 2, 2) )
        writePNG(imageBDEED, "/work/yzstat/rwang14/HCCpipeline/try/imageBDEED.png")
        
        RI1 = sampleMatrix(RI, RowSample = 2, ColSample = 2)
        #the sum of 1s for each row
        RI1row = sum(rowSums(RI1) > 0)
        RI1col = sum(colSums(RI1) > 0)
        imageBF = matrix(imageBDEED[RI1 == 1], RI1row, RI1col)
        writePNG(imageBF, "/work/yzstat/rwang14/HCCpipeline/try/imageBFinal.png")
        
        ###---------- Second step segmentation by classification ---------###
        
        #imageS1R = image1[, , 1] * imageBDEED
        #imageS1G = image1[, , 2] * imageBDEED
        #imageS1B = image1[, , 3] * imageBDEED
        #S1R = imageS1R[imageS1R > 0]
        #S1G = imageS1G[imageS1G > 0]
        #S1B = imageS1B[imageS1B > 0]
        #RGBave = c(mean(S1R), mean(S1G), mean(S1B))
        #RGBstd = c(sd(S1R), sd(S1G), sd(S1B))
        
        #imageBBound = imageBinaryBound(image1, scale = 0.5)
        #imageBBound[is.na(imageBBound)] = 0
        #writePNG(imageBBound, "try/imageBBound.png")
        
        #imageBDBound = dilation(imageBBound, mask = matrix(1, 5, 5))
        #imageBDEBound = erosion( imageBDBound, mask = matrix(1, 5, 5) )
        #imageBDEEBound = erosion( imageBDEBound, mask = matrix(1, 3, 3) )
        #imageBDEEDBound =  dilation( imageBDEEBound, mask = matrix(1, 3, 3) )
        #writePNG(imageBDEEDBound,"try/imageBDEEDBound.png")
        #imageBW = imageBDEED + imageBDEEDBound
        #writePNG(imageBW,"try/imageBW.png")
        
        ### --------------------------------------------------------------###
        
        imageBW = imageBF
        imageCut1 = sample(imageCut, RowSample = 2, ColSample = 2)
        imagePR = imageCut1[, 1 : dim(imageBW)[2], 1] * imageBW
        imagePG = imageCut1[, 1 : dim(imageBW)[2], 2] * imageBW
        imagePB = imageCut1[, 1 : dim(imageBW)[2], 3] * imageBW
        pixelCount = sum(imageBW)
        plantSize = pixelCount * Xsize * Ysize * 2 * 2
        RowCount = rowSums(imageBW); ColCount = colSums(imageBW)
        NonzeroRow = which(RowCount > 0); NonzeroCol = which(ColCount > 0)
        plantheight = Ysize * (quantile(NonzeroRow, 0.975)[[1]] - quantile(NonzeroRow, 0.025)[[1]])
        plantwidth = Xsize * (quantile(NonzeroCol, 0.975)[[1]] - quantile(NonzeroCol, 0.025)[[1]])
        if (is.na(plantheight)) plantheight = 0
        if (is.na(plantwidth)) plantwidth = 0
        shape = c(shape, plantSize, plantheight, plantwidth, pixelCount)
        
        PR = imagePR[imagePR > 0]
        PG = imagePG[imagePG > 0]
        PB = imagePB[imagePB > 0]
        RGBavestd = c(mean(PR), sd(PR), mean(PG), sd(PG), mean(PB), sd(PB))
        RGBavestd[is.na(RGBavestd)] = 0
        colorRGB = c(colorRGB, RGBavestd)
      }
      Res[j, 4 : 53] = c(shape, colorRGB)
    }
    #results = rbind(results, Res)
    res1 = matrix(as.numeric(Res[, 4 : 53]), M, 50)
    res2 = data.frame(res1)
    res3 = cbind(Res[, 1 : 3], res2)
    
    Title = c("PlantID", "Genotype", "Date", "View1-Size", "View1-Height", "View1-Width", "View1-Count", 
              "View2-Size", "View2-Height", "View2-Width", "View2-Count", "View3-Size", "View3-Height", "View3-Width", "View3-Count", 
              "View4-Size", "View4-Height", "View4-Width", "View4-Count", "View5-Size", "View5-Height", "View5-Width", "View5-Count", 
              "View1-Rave", "View1-Rstd", "View1-Gave", "View1-Gstd", "View1-Bave", "View1-Bstd", 
              "View2-Rave", "View2-Rstd", "View2-Gave", "View2-Gstd", "View2-Bave", "View2-Bstd", 
              "View3-Rave", "View3-Rstd", "View3-Gave", "View3-Gstd", "View3-Bave", "View3-Bstd", 
              "View4-Rave", "View4-Rstd", "View4-Gave", "View4-Gstd", "View4-Bave", "View4-Bstd", 
              "View5-Rave", "View5-Rstd", "View5-Gave", "View5-Gstd", "View5-Bave", "View5-Bstd")
    writingName = paste("RS/sideview/", PlantIDUnique[i], ".csv", sep = "")
    print (PlantIDUnique)
    write.table(res3, writingName, row.names = FALSE, col.names = Title, sep = ",")
    }
  }
  #Input(FileNames, ID_start = 14, ID_stop = 16, Geno_start = 18, Geno_stop = 20 )
