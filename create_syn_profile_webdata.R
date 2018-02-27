## An R script to map the grapheme-color synesthesia profiles of multiple 
## participants or multiple screening sessions.
## INPUT: A path to the folder containing the grapheme-color mapping data (.csv)
## OUTPUT:  
###   1. A .pdf file containing the grapheme-color profile of all participants
###   2. .txt files for each subject containing the average color of each valid 
###     grapheme, with consistency scores, rgb codes, ...

### Árni Gunnar Ásgeirsson
### Leiden University
### May 2016

# load functions
source('syn_functions_webdata.R')


### prompt for datafolder
datafolder = choose.dir()
gohome = setwd(datafolder) # save orginial directory

### print datafiles to console
print('--- These are the data files in your folder:')
allfiles = dir(pattern = 'log_ID_') #collect only 'log_ID' files into a vector
print(allfiles) 
print('--- end of file list ---')

### !! function !! ###
### Select the files you want to analyze 
files = SYN_select_files(allfiles)

# prompt user on whether to save a file with consistency scores and more
write_consistency_file = readline('Do you want to save a .txt file of consistency scores and color information (y/n): ')  

### prompt for pdf versions of the subject profiles. Alternatiely, it will plot to the plot window.
pdfplot = readline('Do you want your plots to be printed as .pdf files (y/n): ')
if (pdfplot == 'y'){ # register boolean value that will be used in by the plotting function
  pdfTrue = T 
} else {
  pdfTrue = F
}

for (f in seq_along(files)){ # loop over all screening files
  subj = d$User[1]
  tmp = unlist(strsplit(files[f],split = '_', fixed = T))[5]
  print(paste('--- Processing file: ',files[f])) # print current filename to console
  
  ### !! function !! ###
  ### function to extract the time of each grapheme-color mapping 
  ### session from the screening file header
  print_time = SYN_extract_time_of_screening(files[f])
  
  # read mapping data into an R dataframe
  d = read.table(files[f], sep = ';',header = T)
  if (dim(d)[2] < 2){
    print('!!! Trying tab-delimited import !!!')
    d = read.table(files[f], sep = '\t',header = T)
  }
  if (dim(d)[2] < 2){
    print('!!! Trying comma-delimited import !!!')
    d = read.table(files[f], sep = ',',header = T)
  }
  if (dim(d)[2] < 2){
    print('!!! Unable to find correct delimiter !!!')
    print('!!! To avoid package installations !!!')
    print('!!! delimiter detection is not fully automatic !!!')
    print('!!! Please use tab, comma (,) or semi-colon (;) !!!')
    stop('!!! Script exectution stopped !!!')
  }
  
  
  # get a list of graphemes used in mapping session
  unique_graphms = levels(as.factor(toupper(rawToChar(as.raw(d$UnicodeCharacter)))))
  
  ### !! function !! ###
  # run function to calculate consistency and get averaged colors
  consist = SYN_calculate_consistency(d)  

  # extract the graphemes that were properly analyzed 
  graphms = consist[,1]
  # register omitted graphemes ("no color")
  #omitted = paste(setdiff(unique_graphms,graphms),collapse = ',') # get omitted graphemes
    
  ### !! function !! ###
  # run plot script 
  adjust_bg_contrast = F
  SYN_plot_profile(consist,subj,omittedGraphms,pdfTrue,adjust_bg_contrast)
  
  ### write a datafile
  if (write_consistency_file == 'y'){    
    ### !! function !! ###
    SYN_write_consistency_color_file(consist,subj) #call function to write file
  }
} # end of looping over screening files

setwd(gohome) # reset working directory