# Process policy set data and create barplots representing decision variables
# Script modified from version provided by Reclamation in June 2022
# Modified by E Stark August 2023, then M Pernat August 2023

#################################### SETUP #####################################

rm(list=ls())
options( java.parameters = "-Xmx4g" )
options(scipen = 999)

require(devtools)
library(plotrix)
library(readxl)

dir = ""
archive.file <- "AllPolicies.txt"

df <- read.table(archive.file, header=T)

# For testing, create snapshot of original df

df_og = df


################################################################################
#### ALWAYS CHECK THE COLUMN INDICIES BEFORE SPLITTING OUT POLICY VARIABLES ####
################################################################################

######################### POWELL #########################

# Create a powell df (PTiering) for each policy

for (j in 1:nrow(df)){
  
  PTierEl = t(df[j, 1:5])
  PRels = t(df[j, 6:10])
  MeadRefs = t(df[j, 21:25])
  BalMaxOffset = t(df[j, 11:15])
  BalMinOffset = t(df[j, 16:20])
  
  PTiering = as.data.frame(cbind(PTierEl, 
                                 MeadRefs, 
                                 BalMaxOffset, 
                                 BalMinOffset, 
                                 PRels))
  
  row.names(PTiering) = c("PT1", 
                          "PT2", 
                          "PT3", 
                          "PT4", 
                          "PT5")
  
  colnames(PTiering) = c("PTierEl", 
                         "MeadRefEl", 
                         "BalMaxOffset", 
                         "BalMinOffset", 
                         "PRels")
  
  # For testing, create snapshot of PTiering before any translations
  
  PTieringOG = PTiering
  
  # Replace vals in cols 2-5 w/ row that has highest value of prim. rel. (col 5)
  
  for (i in 1:(nrow(PTiering)-1)){
    #i=1
    if (PTiering[i,1] == PTiering[i+1,1]){
      PTiering[i+1,2] = PTiering[i,2]
      PTiering[i+1,3] = PTiering[i,3]
      PTiering[i+1,4] = PTiering[i,4]
      PTiering[i+1,5] = PTiering[i,5]
    } else { 
      PTiering[i+1,2] = PTiering[i+1,2]
      PTiering[i+1,3] = PTiering[i+1,3]
      PTiering[i+1,4] = PTiering[i+1,4]
      PTiering[i+1,5] = PTiering[i+1,5]
    }
    
  }
  
  # Create bottom tier (if bottom row elev = 3370, lowest tier > 3370 becomes 
  # bottom tier; mead ref, primary rel w/ 99999; just keep elev and release 
  # range)
  
  # For testing, create snapshot of variables before bottom tier 
  PTiering_preBottom = PTiering
  
  if (PTiering[nrow(PTiering), 1] == 3370 && PTiering[1, 1] != 3370){
    
    next_min = which(PTiering[, 1] == 
                       min(PTiering$PTierEl[PTiering$PTierEl > 3370]))
    
    PTiering[next_min,"MeadRefEl"] = PTiering[next_min,"PRels"] = 99999999
    
  }
  
  # Make MeadRefEl = 99999999 if Min and Max offset are 0
  
   for (i in 1:(nrow(PTiering))){

     if (PTiering[i, 3] == 0 && PTiering[i, 4] == 0){
       
       PTiering[i, 2] = 99999999
       
       }
   }
  
  for (i in 1:(nrow(PTiering) - 1)){
    
    for (k in (i + 1):nrow(PTiering)){
      
      if (all(PTiering[i, 2:5] == PTiering[k, 2:5])){
        
        PTiering[k, ] <- PTiering[i, ]
        
      } else{
        
        break
        
      }
      
    }
    
  }
  
  # Replace repeats w/ 3370 and replace values for rows that were already 3370
  
  for (i in 1:(nrow(PTiering))){
    
    if (any(PTiering[i, 1] == PTiering[-i, 1]) | PTiering[i,1] == 3370){
      
      PTiering[i,1] = 3370
      PTiering[i,2] = 99999999
      PTiering[i,3] = 99999999
      PTiering[i,4] = 99999999
      PTiering[i,5] = 99999999
      
    } else { 
      
      PTiering[i,1] = PTiering[i,1]
      PTiering[i,2] = PTiering[i,2]
      PTiering[i,3] = PTiering[i,3]
      PTiering[i,4] = PTiering[i,4]
      PTiering[i,5] = PTiering[i,5]
      
    }
    
  }
  
  
  
  # Sort based on col 1
  
  PTiering = PTiering[order(PTiering$PTierEl, decreasing = TRUE), ] 
  
  # Distribute columns of powell df back into a single row and combine w/ 
  # un-condensed mead variables & objective values
  
  df[j, 1:5] = t(PTiering$PTierEl)
  df[j, 21:25] = t(PTiering$MeadRefEl)
  df[j, 11:15] = t(PTiering$BalMaxOffset)
  df[j, 16:20] = t(PTiering$BalMinOffset)
  df[j, 6:10] = t(PTiering$PRels)
  
}

######################### MEAD #########################

short_elev = t(df[, 27:34])
rownames(short_elev) = c("T1e", "T2e", "T3e", "T4e", "T5e", "T6e", "T7e", "T8e")

df[, 35:42] = apply(df[, 35:42], 2, function(x) as.numeric(x))
short_vol = df[, 35:42]/1000
short_vol = apply(short_vol, 1, rev) #reverse order of volumes
row.names(short_vol) = c("T1V", "T2V", "T3V", "T4V", "T5V", "T6V","T7V", "T8V")

# Create empty dataframes

compressed_vol = short_vol
compressed_elev = short_elev

# If repeating elevations, replace volume w/ highest. Replacing the elevations 
# needs to come first b/c of how RW handles the variables

for(i in 1:ncol(compressed_vol)){
  
  for(j in 1:nrow(compressed_vol)){
    
    if (any(compressed_elev[j,i] == compressed_elev[-j,i])){
      
      compressed_vol[j,i] = max(compressed_vol[which(compressed_elev[,i] == 
                                                       compressed_elev[j,i]), i])
    }
  }
}

# If repeating volumes, replace elevation w/ highest

for(i in 1:ncol(compressed_vol)){
  
  for(j in 1:nrow(compressed_vol)){
    
    if (any(compressed_vol[j,i] == compressed_vol[-j,i])){
      
      compressed_elev[j,i] = max(compressed_elev[which(compressed_vol[,i] == 
                                                         compressed_vol[j,i]), i])
    }
  }
}


# Now replace all repeated rows w/ 895 & 99999999, then order descending and 
# ascending to produce condensed tables

for(i in 1:ncol(compressed_vol)){
  
  for(j in 2:nrow(compressed_vol)){
    
    if (any(compressed_vol[j,i] == compressed_vol[1:(j-1),i])){
      
      compressed_elev[j,i] = 895
      compressed_vol[j,i] = 99999999
    }
  }
}

# Replace any rows that have vol = 0 w/ 895 and 999999999 (to address 0s in 
# first tiers)

for(i in 1:ncol(compressed_vol)){
  
  for(j in 1:nrow(compressed_vol)){
    
    if (compressed_vol[j,i] == 0){
      
      compressed_elev[j,i] = 895
      compressed_vol[j,i] = 99999999
    }
  }
}

# Replace any rows that have elev = 895 w/ 999999999 (to address tiers that 
# originally had volumes w/ elevation = 895)

for(i in 1:ncol(compressed_vol)){
  
  for(j in 1:nrow(compressed_vol)){
    
    if (compressed_elev[j,i] == 895){
      
      compressed_vol[j,i] = 99999999
    }
  }
}

# Sort based on elevation only

compressed_elev = apply(compressed_elev, 2, as.numeric)
condensed_elev = apply(compressed_elev, 2, sort, decreasing=T)
condensed_vol = apply(compressed_vol, 2, sort, decreasing=F)

# Re-combine

condensed_policies = as.data.frame(rbind(condensed_elev, condensed_vol))

row.names(condensed_policies) = c("T1e", "T2e", "T3e", "T4e", "T5e", "T6e",
                                  "T7e", "T8e", "T1V", "T2V", "T3V", "T4V", 
                                  "T5V", "T6V", "T7V", "T8V")

colnames(condensed_policies) = c(1:ncol(condensed_policies))

# Turn 99999999 into 0 now that tables have been sorted **leaving this out for 
# now b/c may do policy plotting where anything w/ 9999999 is ignored

# condensed_policies[condensed_policies==99999999] = 0

condensed_policies = t(condensed_policies)

# Calculate elevations for surplus tier

surplus_size = data.frame(df[, 26])
surplus_elev = 1200 - surplus_size

# If surplus went away, replace 1200s w/ 99999999

surplus_elev[surplus_elev == 1200] = 99999999

# Add surplus elevation to condensed policy df

condensed_policies_full = cbind(surplus_elev, condensed_policies)

# Replace Mead variables in original df w/ condensed variables

df = cbind(condensed_policies_full, 
           df[, 1:25], 
           df[, 43:59])

names(df) = c("surplus_elev", names(df)[2:17], "PT1e", "PT2e", "PT3e", "PT4e",
              "PT5e", "PT1Rel", "PT2Rel", "PT3Rel", "PT4Rel", "PT5Rel",
              "MaxOffset1", "MaxOffset2", "MaxOffset3", "MaxOffset4", 
              "MaxOffset5", "MinOffset1", "MinOffset2", "MinOffset3", 
              "MinOffset4", "MinOffset5", "MeadRef1", "MeadRef2", "MeadRef3", 
              "MeadRef4", "MeadRef5", "Avg.Powell.PE", "Powell.Release.LTEMP", 
              "Avg.Mead.PE", "Avg.LB.Shortage", "LB.Shortage.Freq", "Mead.1020", 
              "Powell.3525", "Powell.3490", "Powell.WY.Release", "Start.In.EQ", 
              "LF.Deficit", "Max.Delta.Shortage", "Avg.Policy.Shortage", "Mead.1000")


############### WRITE CONDENSED DATAFRAME TO CSV ###############

output.file <- paste0(dir, "experiment2_seed8_different/condensed_policies.csv")

write.csv(df, output.file, row.names = FALSE)


################################ SCALING SCRIPT ################################

# Create a matrix of the objective columns from original df

archive.mat = as.matrix(df[,44:47])

archive.scaled = matrix(999, nrow = nrow(archive.mat), ncol = ncol(archive.mat))

for (i in 1:ncol(archive.mat)){
  
  archive.scaled[, i] = rescale(archive.mat[, i], c(0, 1))
  
}

colnames(archive.scaled) = paste(colnames(archive.mat), "SCALED", sep = " ")

# Write out scaled and unscaled matricies

write.csv(archive.scaled, "scaled_objectives.csv")
write.csv(archive.mat, "uscaled_objectives.csv")

