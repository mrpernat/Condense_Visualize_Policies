# Prepare data frames for stacked bar plotting of policy decision variables
# Original: Nathan Bonham 2/10/21, Modified by E Stark Aug 2023


library(tidyr)
library(dplyr)
library(ggplot2)
library(plotly)
library(tidyr)
library(dplyr)
library(patchwork)

rm(list = ls())

# Read in Archive (condensed)
archive.file <- "AllPoliciesCondensed.csv"

archive.df = read.csv(archive.file, header = TRUE)


################### CREATE DATAFRAMES FOR STACKED BAR PLOTS ####################


######################### MEAD #########################

lastDV = which(colnames(archive.df) == 'T8V')

dvs = archive.df[, 1:lastDV]

# Prepare for plotting. Add full pool and deadpool columns

elevation = dvs[1:9]
elevation = cbind(rep(1220, nrow(elevation)), elevation)
names(elevation) = c("Top", names(elevation[2:10]))
elevation$dead.pool=895

# When surplus tier doesn't exist, need to replace value to make elev_delta work

for(j in 1:nrow(elevation)){
  
  if (elevation[j, 2] == 99999999){
    
    elevation[j, 2] = 1220
  }
}

# Save max elevation for ggplotting later

elev_max = unlist(apply(elevation[2:10], 1, max))

# Initialize elev_delta

elev_delta = elevation[, 1:10]

# Difference between subsequent tier elevations, used for plotting

for(i in 2:11){
  
  elev_delta[i - 1] = elevation[i - 1] - elevation[i]
  
}

volume = dvs[10:17]
volume[volume == 99999999] = 0
volume$dead.pool = 0
vol_max = apply(volume, 1, max)

# Identify actual tiers. Sum number of tiers. Want this for filtering in web app

tier_bin = volume > 0

nTiers = apply(tier_bin, 1, sum)

# Create shortage volume labels, ex) V = 500 KAF

volume_labs = matrix(NA, nrow = nrow(volume), ncol = ncol(volume))

for (i in 1:ncol(volume_labs)){
  
  volume_labs[, i] = paste(volume[, i], ' KAF', sep='')
  
}

volume_labs[volume_labs == "0 KAF"] = NA
volume_labs = data.frame(volume_labs)

# Add 2 columns to volume df so it becomes the same number of columns as 
# elev_delta (which has top and surplus tiers now)

volume = cbind(rep(NA, nrow(volume)), rep(NA, nrow(volume)), volume)
names(volume) = c("Top","Surplus", names(volume[3:11]))

volume_labs = cbind(rep("Surplus", nrow(volume_labs)), 
                    rep("Normal", nrow(volume_labs)),
                    volume_labs)

names(volume_labs) = names(volume)

# Add policy ID to each data frame
elevation$policy = as.character(1:nrow(dvs))
elev_delta$dead_pool = 895
elev_delta$policy = as.character(1:nrow(dvs))
volume$policy = as.character(1:nrow(dvs))
volume_labs$policy = as.character(1:nrow(dvs))

piv_col = which(colnames(elev_delta)=='dead_pool')

# Wide to long format for ggplot
elevation = pivot_longer(elevation, cols=1:piv_col, names_to = 'Tier')
elev_delta = pivot_longer(elev_delta, cols=1:piv_col, names_to = 'Tier') 

# Need to reorder the factor levels such that dead pool is last

elev_delta$Tier = factor(elev_delta$Tier, levels = c("Top",
                                                     "surplus_elev",
                                                     "T1e",
                                                     "T2e",
                                                     "T3e",
                                                     "T4e",
                                                     "T5e",
                                                     "T6e",
                                                     "T7e",
                                                     "T8e",
                                                     "dead_pool"))

elev_delta = dplyr::rename(elev_delta, 'delta'='value')

# Stacked bar plot order depends on the level order

volume = pivot_longer(volume, cols=1:piv_col, names_to = 'Tier')
volume$value = as.numeric(volume$value)
volume_labs = pivot_longer(volume_labs, cols=1:piv_col, names_to = 'Tier')
volume_labs = data.frame(volume_labs, elevation = elevation$value)

# Remove label for surplus when it doesn't really exist (can start at row 2 b/c 
# row 1 is Top)

for (i in 2:nrow(volume_labs)){
  
  if(volume_labs[i, 2] == "Surplus" && volume_labs[i, 4] == 1220){
    
    volume_labs[i - 1, 3] = NA
  }
}


# Add color to volume df

volume_col = as.data.frame(read.csv("C:/Users/mape2635/OneDrive - UCB-O365/DMDU/Policy Set Comparison/vol_gradient.csv"))
volume_col$color = as.character(volume_col$color)

# added bigger volumes to txt file: 2600, 3000, 4000 taf

# for(i in 1:nrow(volume_col)){
#   
#   volume_col[i, 2] = paste(as.character("#"), volume_col[i, 2], sep = "")
#   
# }

volume$color = ""
volume$value = as.numeric(volume$value)

for(i in 1:nrow(volume)){
  
  if(is.na(volume[i, 3]) || as.numeric(volume[i,3]) == 0){
    
    volume[i,4] = "#000000"
  }
  else if(as.numeric(volume[i, 3]) > 0){
    
    p = as.numeric(volume[i, 3])
    volume[i,4] = volume_col[which(volume_col$volume == p), 2]
    
  }
}

for(i in 1:nrow(volume)){
  
  if (volume[i, 2] == "Top"){
    
    volume[i, 4] = "#6d46a5"
  }
  
  if (volume[i, 2] == "Surplus"){
    
    volume[i, 4] = "#4659a5"
  }
  
  if (volume[i, 2] == "PartSurplus"){
    
    volume[i, 4] = "#bdbfce"
  }
  
}

df = data.frame(elev_delta,
              v_lab = volume_labs$value,
              v_col = volume$color,
              elevation = volume_labs$elevation,
              volume = volume$value)

df$Tier = rep(c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k'), 
              length(unique(df$policy)))

df$TierName = rep(c('Surplus', 'Normal', '1', '2', '3', '4', '5', '6', '7', '8', 'Dead Pool'), 
                length(unique(df$policy)))

# Change to number and add zeros for image ordering in tableau

df$policy = as.numeric(df$policy)
df$policy = sprintf("%04d", df$policy)

############# Create data frame for policy labels, SOM node labels, and label y position

# stacked_bar_plot_data=list(long_data=df)#, wide_data=policy_SOM_labs)
# saveRDS(stacked_bar_plot_data, file='data/outputs/data for stacked mead bar plot.rds')
# 
# write.csv(df, "df.csv")



##################################  POWELL  ##################################

firstpDV = which(colnames(archive.df) == 'PT1e')
lastpDV = which(colnames(archive.df) == 'MeadRef5')

pdv = archive.df[, firstpDV:lastpDV]

#rel_ranges.df=read.table("powell_release_range_table.txt", header = T, sep = )

#################### Create data frame for stacked bar plots ###################

# Translate dummy variables into NA

pdv[pdv == 99999999] = NA

# Prepare for plotting. Add equalization and deadpool columns.

p.elevation = pdv[1:5]
p.elevation = cbind(rep(3700, nrow(p.elevation)), p.elevation)
names(p.elevation) = c("Equalization", names(p.elevation[2:ncol(p.elevation)]))
p.elevation$dead.pool = 3370

# Initialize elev_delta

p.elev_delta = p.elevation[, 1:6]

# Difference between tier elevations, used for plotting

for(i in 2:ncol(p.elevation)){
  
  p.elev_delta[i - 1] = p.elevation[i - 1] - p.elevation[i]
}

# Mead reference elevations
mead_ref = pdv[, 21:25]
mead_ref = cbind(rep(NA, nrow(mead_ref)), mead_ref)
names(mead_ref) = c("Equalization", names(mead_ref[2:ncol(mead_ref)]))

# Start with min offset

minoffsets = pdv[16:20] / 1000000
minoffsets = cbind(rep(NA, nrow(minoffsets)), minoffsets)
names(minoffsets) = c("Equalization", names(minoffsets[2:ncol(minoffsets)]))

# Max offsets

maxoffsets = pdv[11:15] / 1000000
maxoffsets = cbind(rep(NA, nrow(maxoffsets)), maxoffsets)
names(maxoffsets) = c("Equalization", names(maxoffsets[2:ncol(maxoffsets)]))

# Primary releases

releases = pdv[, 6:10]
releases = releases/1000000 #change to MAF
releases=cbind(rep(NA, nrow(releases)), releases)
names(releases) = c("Equalization", names(releases[2:ncol(releases)]))

# Create labels
tier_labs = matrix(NA, nrow = nrow(p.elevation), ncol=6) #hardcoded for now
tier_labs = as.data.frame(tier_labs)

# Special case for equalization tier

for(k in 1:nrow(tier_labs)){
  
  if(p.elev_delta[k, 1] > 0){
    
    tier_labs[k, 1] = "Equalization"
  }
}

for(i in 1:nrow(tier_labs)){
  
  for(j in 2:ncol(tier_labs)){
    
    if(is.na(mead_ref[i, j]) == F){
      
      tier_labs[i, j] = paste("Release", releases[i, j], "maf;", 
                              "Balance ", releases[i, j] - minoffsets[i, j], "-", 
                              releases[i, j] + maxoffsets[i, j], 
                             "maf if Mead <", mead_ref[i, j], "ft",  sep = " ")
    }
    
    else if(is.na(releases[i, j])==F) {
      
      tier_labs[i, j] = paste("Release", releases[i,j], "maf;")
      
    }
    
    else{
      
       tier_labs[i, j]=NA
     }
  }
}

names(tier_labs) = c("Equalization Label", "Tier1 Label", "Tier2 Label", 
                     "Tier3 Label", "Tier4 Label", "Tier5 Label" )


# Add dead pool and policy number to every df
p.elevation$policy = as.character(1:nrow(pdv))

p.elev_delta$dead.pool = 3370
p.elev_delta$policy = as.character(1:nrow(pdv))

tier_labs$dead.pool = NA
tier_labs$policy = as.character(1:nrow(pdv))

mead_ref$dead.pool = 3370
mead_ref$policy = as.character(1:nrow(pdv))

minoffsets$dead.pool = 3370
minoffsets$policy = as.character(1:nrow(pdv))

maxoffsets$dead.pool = 3370
maxoffsets$policy = as.character(1:nrow(pdv))

releases$dead.pool = 3370
releases$policy = as.character(1:nrow(pdv))

# Pivot longer

p.elevation = pivot_longer(p.elevation, cols = 1:ncol(p.elevation) - 1, names_to = 'Tier')
p.elev_delta = pivot_longer(p.elev_delta, cols = 1:ncol(p.elev_delta) - 1, names_to = 'Tier')
tier_labs = pivot_longer(tier_labs, cols = 1:ncol(tier_labs) - 1, names_to = 'Tier')
mead_ref = pivot_longer(mead_ref, cols = 1:ncol(mead_ref) - 1, names_to = 'Tier')
minoffsets = pivot_longer(minoffsets, cols = 1:ncol(minoffsets) - 1, names_to = 'Tier')
maxoffsets = pivot_longer(maxoffsets, cols = 1:ncol(maxoffsets) - 1, names_to = 'Tier')
releases=pivot_longer(releases, cols = 1:ncol(releases) - 1, names_to = 'Tier')

p.df = data.frame(p.elevation, delta = p.elev_delta$value, t_lab = tier_labs$value)
p.df$Tier = rep(c('a', 'b', 'c', 'd', 'e', 'f', 'g'), length(unique(df$policy)))
p.df$TierName = rep(c('Equalization', 'Tier1', 'Tier2', 'Tier3', 'Tier4', 'Tier5', 'Dead Pool'), length(unique(p.df$policy)))

# Change to number and add zeros for image ordering in tableau
p.df$policy = as.numeric(p.df$policy)
p.df$policy = sprintf("%04d", p.df$policy)

#########################
# 
# stacked_bar_plot_data=list(long_data=p.df)#, wide_data=policy_SOM_labs)
# saveRDS(stacked_bar_plot_data, file='data for stacked powell bar plot.rds')
# 
# write.csv(p.df, "p.df.csv")


##################### Plotting ##########################

output.folder <- ""

for (i in 1:nrow(archive.df)){
  
  policy_id = sprintf("%04d", i)

  powell_pol = ggplot(subset(p.df, policy %in% c(policy_id)), 
                      aes(fill = Tier, y = delta, x = policy, label = t_lab)) +  
    
    geom_bar(position = "stack", 
             stat = "identity", 
             color = "black", 
             show.legend = FALSE) +
    
    scale_fill_manual(values=c(
      "#1BBC9B", 
      "#67CC8E",
      "#96ED89",
      "#45BF55",
      "#79BD8F",
      "#289976",
      "#26A69A")) +
    
    geom_text(position = position_stack(vjust = .5), size = 2.5) +
    
    theme_minimal() +
    
    ggtitle("Lake Powell") +
    
    theme(plot.title = element_text(hjust = .5)) +
    
    ylab("PE") +
    
    scale_y_continuous(breaks = seq(3385, 3700, by = 20)) +
    
    coord_cartesian(ylim = c(3385.5, 3700))


  mead_col = as.character(df$v_col[which(df$policy == policy_id)])

  mead_pol = ggplot(data=subset(df, policy %in% c(policy_id)), 
                    aes(fill=Tier, y=delta, x=policy, label=v_lab)) +
    
    geom_bar(position="stack", stat="identity",  color="black",  show.legend = FALSE) +
    
    scale_fill_manual(values = mead_col) +
    
    geom_text(position = position_stack(vjust = .5), size = 2.5) +
    
    theme_minimal() +
    
    ggtitle("Lake Mead") +
    
    theme(plot.title = element_text(hjust = .5)) +
    
    ylab('PE') +
    
    scale_y_continuous(breaks = seq(910, 1220, by = 20)) +
    
    coord_cartesian(ylim=c(910, 1220))
  

  powell_pol + mead_pol

  both.fig = powell_pol + mead_pol

  ggsave(paste(output.folder, policy_id, ".png", sep = ""), 
        plot = both.fig, 
        device = "png",
        width = 1650, height = 1420, units = "px",
        dpi = 200)

}



