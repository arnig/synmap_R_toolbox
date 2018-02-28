### FUNCTION 1

### This function calculates the grapheme-color mapping consistency and returns 
### 1. Each valid grapheme
### 2. A consistency score for that grapheme
### 3 - 5. The hex color values of each trial 
### 6. The average color value as hex
### 7 - 9. The rgb values of each trial (comma separated)
### 10. The average rgb value for the grapheme


SYN_calculate_consistency = function(data){
  d = data
  chars = vector()
  
  if (is.element('AsciiCharacter',names(data))){
    # a work-around for older synmap files where
    # the a variable name was 'AsciiCharacter'. 
    # Now 'UnicodeCharacter' is the standard
    chars = d$AsciiCharacter
  } else {
    for (i in seq_along(d$UnicodeCharacter)){
      chars[i] = toupper(rawToChar(as.raw(d$UnicodeCharacter[i])))  
    }  
  }
  
  omitIndex = is.nan(d$CharR)
  omittedGraphms = unique(chars[omitIndex])
  #make a matrix of normailized RGB-values
  nR = d$CharR/255
  nG = d$CharG/255
  nB = d$CharB/255
  normCols = cbind(nR,nG,nB) #bind into matrix  
  maxN = 3 #set the maximum number of mapping trials for a letter
  inclIndex = maxN == table(chars) #index of graphemes to include
  allGraphms = levels(as.factor(chars)) #vector of all graphemes
  allGraphms = allGraphms[inclIndex] #filter graphemes where trialN != maxN
  if (length(omittedGraphms) > 0){
    allGraphms = setdiff(allGraphms,omittedGraphms) #filter graphemes where at least 1 trial is NaN    
  }
  diffVector = vector() #allocate a vector for the difference scores
  #allocate a matrix for the color values of each trial + the average
  hexMat = matrix(nrow = length(allGraphms), ncol = 4)
  rgbMat = matrix(nrow = length(allGraphms), ncol = 12)
  # loop through all graphemes
  for (i in seq_along(allGraphms)){
    index = chars == allGraphms[i] #get an index to indicate the positions of the current grapheme
    dd = data.frame(normCols[index,]) #extract only current grapheme data
    
    #calculate the difference scores for each grapheme. See:
    #Eagleman, D. M., Kagan, A. D., Nelson, S. S., Sagaram, D., & Sarma, A. K. (2007). 
    #A standardized test battery for the study of synesthesia. 
    #Journal of Neuroscience Methods, 159, 139-145.
    r = abs(dd$nR[1] - dd$nR[2]) + abs(dd$nR[1] - dd$nR[3]) + abs(dd$nR[2] - dd$nR[3])
    g = abs(dd$nG[1] - dd$nG[2]) + abs(dd$nG[1] - dd$nG[3]) + abs(dd$nG[2] - dd$nG[3])
    b = abs(dd$nB[1] - dd$nB[2]) + abs(dd$nB[1] - dd$nB[3]) + abs(dd$nB[2] - dd$nB[3])
    diffVector[i]=(r+g+b) #record difference to diffVector    
    
    for (j in 1:dim(hexMat)[2]-1){
      #record color values of each trial into colorMat (hex format)
      hexMat[i,j] = rgb(dd$nR[j],dd$nG[j],dd$nB[j],maxColorValue = 1)      
    }
    
    rgbMat[i,1:9] = c(dd$nR[1]*255
                      ,dd$nG[1]*255
                      ,dd$nB[1]*255
                      ,dd$nR[2]*255
                      ,dd$nG[2]*255
                      ,dd$nB[2]*255
                      ,dd$nR[3]*255
                      ,dd$nG[3]*255
                      ,dd$nB[3]*255)
    
    hexMat[i,4] = rgb(mean(dd$nR),mean(dd$nG),mean(dd$nB),maxColorValue = 1)
    rgbMat[i,10:12] = c(round(mean(dd$nR*255)),round(mean(dd$nG*255)),round(mean(dd$nB*255)))    
  }
  outdat = cbind(allGraphms, as.character(sprintf('%f',diffVector)), hexMat, rgbMat)
  colnames(outdat) = c('Grapheme','diffScore','hex1','hex2','hex3','hexAvg','R1','G1','B1','R2','G2','B2','R3','G3','B3','R_avg','G_avg','B_avg')    
  return(outdat)
  return(omittedGraphms)
}

### FUNCTION 2

# This function simply deciphers the time stamp of the grapheme-color mapping session
# and returns it in a nice format
SYN_extract_time_of_screening = function(file){  
  d = read.table(file, header = T, sep = ';')
  print_time_str = d$TimeStamp[1]
  return(print_time_str) #save in workspace
}

### FUNCTION 3

# The functions plots the consistency scores of synesthetes, along with the color averages
# and the colors mapped during each of 3 trials with every grapheme. It also returns the
# number of valid graphemes, a list of omitted graphemes, the mean consistency of the 
# full set of graphemes and the standard deviation.
# The final parameter - BGcontrEnhancement - is set to False by default. When True, this parameter
# draws a dark gray rectangle behind low contrast graphemes.
# When False, no rectangle is drawn. 

if (!is.element('omittedGraphms', ls())){
  omittedGraphms = c(NA)
}

SYN_plot_profile = function(data,subj,omittedGraphms,printPDF,BGcontrEnhancement = F){
  # PDF vs. regular plot settings
  if (printPDF){
    pdfname = paste(subj, '-Synesthesia-Profile.pdf')
    pdf(pdfname, width = 10, height = 6)
  } else {
    dev.new(width = 10, height = 6)
  }
  #graphics settings
  par(xaxs = 'i')
  par(yaxs = 'i')
  par(mfrow = c(2,1))
  par(bg = rgb(215,215,215,maxColorValue = 255)) #set background brightness to the default value in the screening program 
  oldmar = par('mar')
  par(mar = c(oldmar + c(-3,0,-2,-1)))
  
  #draw a barplot of the consistency measures
  scores = as.numeric(data[,2])
  graphms = data[,1]
  if (max(as.numeric(scores)) < 1.2){
    yl = c(0,1.2)
  } else {
    yl = c(0,1.1 * max(scores))
  }
  
  plot(as.numeric(scores)
       ,type = 'n'
       ,xaxt = 'n'
       ,bty = 'n'
       ,xlab = ' '
       ,ylab = 'Consistency'
       ,xlim = c(0.5,length(graphms) + 0.5)
       ,ylim = yl
       ,main = paste('Subject ID: ',subj,sep = '')       
       ,pch = 19)
  # Print time of session in the upper right corner of the plot
  legend('topright', legend = c(NA), bty = 'n', title = print_time, cex = 0.7, text.col = 'white')
  lw = 500/length(graphms) # set the width of bars showing consistency scores
  #loop through all valid graphemes
  for (i in seq_along(graphms)){
    # draw bars
    arrows(i,0,i,scores[i], length = 0, angle = 0, lwd = lw, lend = 1, col = 'black')
  }  
  oldcex = par('cex') #record old fontsize
  par(cex = .8) # change font size
  axis(side = 1, at = 1:length(graphms), labels = graphms) #draw the x-axis
  par(cex = oldcex) # change default fontsize back
  abline(h = 1, lty = 'dotted', col = 'firebrick3', lwd = 3) #draw line to show the criterion for synesthesia
  
  #draw each trials grapheme-to-color mapping
  par(mar = c(1.1,1.1,1.1,.1)) #reduce the margins of the plot area
  # plot an empty plot
  plot(1:length(graphms)
       ,rep(3,length(graphms))
       ,type = 'n'
       #,main = 'Average Colors'
       ,xaxt = 'n'
       ,yaxt = 'n'       
       ,bty = 'n'
       ,ylab = ' '
       ,xlab = ' '
       ,ylim = c(-0.5,3))
  # define a matrix with hex color information
  colmat = data[,3:6] #get color information for each grapheme and all trials
  
  # loop through columns in colmat
  for (i in 1:dim(colmat)[2]){
    # set the position of graphemes within plot, dependent on the i-th appearance of a grapheme
    if (i == 1){
      ypos = 1.3
    } else if(i == 2){
      ypos = 1
    } else if(i == 3){
      ypos = .7
    }
    ### If a grapheme is too much like the background, a dark rectangle will be plotted
    ### as background for that grapheme. Here are some settings for that proces:
    scaleX = (dim(colmat)[1])/70 #scaler for rectangle in x-coords
    scaleY = 0.17 #scaler for rectangle in y-coords
    rectcol = 'gray40' # color for backgroun rectangle
    #loop through colmat rows
    for (j in 1:dim(colmat)[1]){
      #print average color of grapheme j
      if (i == 1){
        if (BGcontrEnhancement){
          currCols = data[j,3:6]
          rgbcol = col2rgb(currCols)
          maxDiff = vector()
          for (k in seq_along(rgbcol[1,])){
            maxDiff[k] = max(abs(rgbcol[,k]-215))
          }
          
          if (min(maxDiff) < 12){
            rect(.95*j - scaleX+ .5, 2.2 - scaleY, .95*j + scaleX + .5, 2.2 + scaleY
                 ,border = NA
                 ,col = rectcol)
            rect(.95*j - scaleX + .5, 1.3 + scaleY, .95*j + scaleX + .5, .7 - scaleY
                 ,border = NA
                 ,col = rectcol)
          }  
        }
        text(.95*j+.5,2.2,labels = data[j,1], col = colmat[j,4], cex = 1.5, font = 2) 
      }      
      #print the i-th color of grapheme j
      text(.95*j+.5,ypos,labels = data[j,1], col = colmat[j,i], cex = 1.2, font = 2)
    }
  }  
  center = mean(par('usr')[1:2]) # get the center coordinates of the plot
  text(center, 2.6, labels = 'Average Colors') # write caption in center
  text(center, 1.7, labels = 'Trial-by-Trial Colors') # write caption in center
  avgconst = round(mean(scores),3) # calculate grand average consistency of all graphemes
  sdconst = round(sd(scores),3) # ... and the standard deviation
  # print these values to plot
  text(3,0
       ,labels = paste('Mean consistency for '
                       ,length(scores)
                       ,' graphemes is: '
                       ,avgconst
                       ,' (sd: '
                       ,sdconst
                       ,')'
                       ,sep = '')
       ,adj = 0)
  # print omitted letters, if any, on profile
  if (is.element('omittedGraphs',ls())){
    #if (nchar(omittedGraphms)){
    text(3,-0.3
         ,labels = paste('These graphemes were included, but marked \"no color\" at least once: '
                         ,omittedGraphms
                         ,sep = '')
         ,adj = 0)  
  } 
  if (pdfTrue){
    dev.off()  
  }
  
  par(mar = oldmar) #reset margins
  
  
}


### FUNCTION 4

# This function lets the user pick out 1 or more files from a list, and sends those for
# further analysis. 

SYN_select_files = function(files){
  orgfiles = files
  select = readline('Do you wish to select a subset of these files? (n/y). n = all files; y = subset: ')
  if (select == 'y'){
    success = 0
    while (success != 'y'){
      print(orgfiles)
      selection = readline('Which files do you want to keep? Use space separated integers to denote the appropriate file indices: ')
      selV = as.numeric(unlist(strsplit(selection,split = ' ', fixed = T)))
      files = orgfiles[selV]
      print(files)
      success = readline('Are these files correct? (y/n): ')
    } 
  } else if(select == 'n'){
    files = orgfiles
  } else {
    print('You selected something other than y or n (case-sensitive). Press ESC to cancel and start again')
  }
  return(files)
}


### FUNCTION 5

### This function function writes the dataframe "data" to a folder 
### called "color-consistency-files". If this folder doesn't exist, it wil create it.
### The file name will be based on the variable "subjectName". The script doesn't care
### About the size or contents of the dataframe "data". 

SYN_write_consistency_color_file = function(data, subjectName){
  fname = paste(subjectName,'-color-mappings-and-consistency.txt', sep = '')
  outdir = 'color-consistency-files'
  
  #treat data
  tmp = substring(data[,3:6],2) # remove # from start of hex code
  newdata = cbind(data[,1:2],tmp,data[,7:18]) # re-assemble data file without #
  if (file.exists(outdir)){
    setwd(outdir)  
  } else {
    dir.create(outdir)  
    setwd(outdir)  
  }
  write.table(newdata, fname, row.names = F, sep = '\t', quote = F)
  setwd('../')
}




### Function 6
### This function prints a PDF with an overview
### of all grapheme-color pairs from a screening
### session, regardless of whether the "no color"
### option was used (which is excluded from the)
### "synesthesia profile" PDF. In essence, the function
### returns a color interpretation of raw data
### Furthermore, it writes a tab-delimited .txt file 
### with the RGB values of all trials, including NaN trials
### Input is: a data frame with raw data from synMap
SYN_get_all_trial_pairs = function(data){
  
  chars = vector() 
  if (is.element('AsciiCharacter',names(data))){
    # a work-around for older synmap files where
    # the a variable name was 'AsciiCharacter'. 
    # Now 'UnicodeCharacter' is the standard
    data$UnicodeCharacter = data$AsciiCharacter
  }
  
  # translate all Unicode character numbers to 
  # readable characters (uppercase)
  for (i in seq_along(data$UnicodeCharacter)){
    chars[i] = toupper(rawToChar(as.raw(data$UnicodeCharacter[i])))
  }
  # append 'chars' to dataframe
  data$chars = chars
  index = sort(data$chars) # get a sort index, if needed
  charlvs = levels(as.factor(data$chars)) # get a single vector with all chars
  attemptlvs = levels(as.factor(data$AttemptNumber)) # get the number of attempts
  sort_dat = data[index,] # sort the data, using the index created earlier
  
  # allocate some matrices for later
  colmat = matrix(nrow = length(charlvs), ncol = length(attemptlvs))
  pchmat = matrix(nrow = length(charlvs), ncol = length(attemptlvs))
  cexmat = matrix(nrow = length(charlvs), ncol = length(attemptlvs))
  setcex = 2.4 # the size of the colored squares
  # allocate a matrix for the output data
  rgb_mat = matrix(nrow = length(charlvs),ncol = (length(attemptlvs) * 3) + 1)
  rgb_mat[,1] = charlvs # make first column a char column
  
  
  for (j in seq_along(charlvs)){ # loop over all characters
    for (k in seq_along(attemptlvs)){ # loop over all attempts
      current_row = data[data$chars == charlvs[j] & data$AttemptNumber == attemptlvs[k],]
      # get rgb data into rgb_mat
      curr_rgb = c(current_row$CharR,current_row$CharG,current_row$CharB)
      rgb_mat[j,((k * 3)-1):((k * 3) + 1)] = curr_rgb
      # define hex color matrix for use in plot()
      if (is.na(current_row$CharR)){
        colmat[j,k] = NA # register NA/NaN as NA
      } else {
        # ... otherwise, register the hex color in the matrix
        colmat[j,k] = rgb(red = current_row$CharR
                          ,green = current_row$CharG
                          ,blue = current_row$CharB
                          ,maxColorValue = 255)  
      }
      # do the same for other matrices
      if (is.na(colmat[j,k])){
        # what kind of plot point should be used?
        # If NA, then a square with X
        pchmat[j,k] = 7 
        cexmat[j,k] = .8*setcex # ... also, resize NA squares
      } else {
        pchmat[j,k] = 22 # if not NA, then square with bg color
        cexmat[j,k] = setcex # ... and standard cex (size of square)
      }
    }
    
  }
  # prepare PDF output
  pdfname = paste(data$User[1], 'all_trial_colors.pdf')
  pdf(pdfname, width = 2, height = 10)
  oldpar = par()
  # prepare graphical parameters
  par(mar = c(2,0,3,0) + .1)
  par(bg = 'gray50')
  # make an empty plot
  plot(1:10,(-1:-10)
       ,type = 'n'
       ,bty = 'n'
       ,main = paste('User: ', data$User[1], sep = '')
       ,xaxt = 'n'
       ,yaxt = 'n'
       ,xlim = c(.5,length(attemptlvs) + 1.5))
  axis(side = 1, at = 2:4, labels = c('1','2','3'))
  
  # some padding for the plot
  xpadd = 0.5
  ypadd = 0.2
  y = seq(from = -1 + ypadd, to = -10 + ypadd, length.out = length(charlvs))
  
  # add the colored squares to the plot
  for (h in seq_along(attemptlvs)){
    x = rep(h, length(charlvs)) # define x-coords for each column
    if (h == 1){
      points(x,y,pch = charlvs)  # first plot the graphemes
    } else {
      # ... next, plot the colored squares
      points(x,y,bg = colmat[,(h-1)],pch = pchmat[,(h-1)], cex = cexmat[,(h-1)])
    }
  }
  
  
  par(oldpar) # reset graphical parameters
  dev.off()  # and close graphical dev. to output PDF
  
  # prepare output data file
  colnames(rgb_mat) = c('char','R1','G1','B1','R2','G2','B2','R3','G3','B3')
  #ordered_graph_col_pairs = cbind(charlvs,colmat) # hex colors
  write.table(rgb_mat
              ,file = paste(data$User[1], '_ordered_color_pairs.txt', sep = '')
              ,sep = '\t'
              ,row.names = F
  )
}