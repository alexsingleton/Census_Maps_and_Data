###################################################################################################
# Setup and download data
#########################################################

library(tidyverse)
library(sf)
library(magrittr)
library(tmap)
library(arrow)
library(magick)
library(kableExtra)
library(jsonlite)

# Download the Output Areas

url<- "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Output_Areas_2021_EW_BGC_V2/FeatureServer/0/query?where=1%3D1&outFields=OA21CD&outSR=4326&f=json"
boundary <- st_read(url)

# Download the LAD Boundaries

url <- "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Local_Authority_Districts_December_2023_Boundaries_UK_BGC/FeatureServer/0/query?where=1%3D1&outFields=LAD23CD,LAD23NM,LAD23NMW&outSR=4326&f=json"
boundary_lad <- st_read(url)
st_write(boundary_lad,"boundary_lad.gpkg")

# Download the OA to LAD lookup
url <- "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/OA21_LAD23_EW_LU/FeatureServer/0/query?where=1%3D1&outFields=OA21CD,LAD23CD&returnGeometry=false&outSR=4326&f=json"
OA_LAD <- st_read(url) %>% as_tibble()




# Download LAD to Region - then select out unique records (original is an Ward table)

LAD_RGN <- st_read("https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/WD23_LAD23_CTY23_OTH_UK_LU/FeatureServer/0/query?where=1%3D1&outFields=LAD23CD,RGN23CD,RGN23NM&outSR=4326&f=json")
LAD_RGN %<>%
  filter(RGN23CD != "N92000002") %>%
  filter(RGN23CD != "S92000003") %>%
  distinct()


#Append LAD / Region to OA Boundaries

boundary %<>%
  left_join(OA_LAD,by = "OA21CD") 
boundary %<>%
  left_join(LAD_RGN,by = "LAD23CD") 

# Get a list of LAD

LAD_list <- boundary %>% 
  select(LAD23CD) %>%
  filter(!is.na(LAD23CD)) %>%
  st_drop_geometry() %>%
  pull() %>%
  unique()

########################################################
# Pull in census 2021 data, calculate PCT
#########################################################

# Get Census Table Lists

census_tables <- read_csv("https://github.com/alexsingleton/Census_2021_Output_Areas/raw/main/Table_Metadata.csv",show_col_types = FALSE)
write_csv(census_tables,"Census_Metadata.csv")


# Read Census Table (Remove ts041 - number of households,ts006 - density)

C_Table_Name_List <- census_tables %>% select(Table_ID) %>% unique() %>% pull()
C_Table_Name_List %<>% setdiff(c("ts041","ts006"))

for (ct_tmpID in C_Table_Name_List) {
  
  # Download Census Table
  CT_tmp <- read_csv(paste0("https://github.com/alexsingleton/Census_2021_Output_Areas/blob/main/output_data/csv/",ct_tmpID,".csv?raw=true"),show_col_types = FALSE)
  
  # Calculate Percentages
  CT_tmp %<>%
    
    mutate_at(vars(-1:-2), list(PCT = ~(. / !!sym(paste0(ct_tmpID,"0001"))*100)))
  
  assign(ct_tmpID,CT_tmp)
  rm(ct_tmpID,CT_tmp)
  
}


########################################################
# Create Website Skeleton
#########################################################  

# Setup quarto

system("quarto create-project website --type website:blog")

#remove unwanted files from the template

unlink("website/posts/*", recursive = TRUE,force=TRUE)
unlink("website/about.qmd")
unlink("website/profile.jpg")
unlink("website/_quarto.yml")
unlink("website/index.qmd")
unlink("docs",recursive = TRUE,force=TRUE)

# Create Yaml / menu

fileConn <- "./website/_quarto.yml"
cat('project:',file=fileConn,append=TRUE,sep="\n")
cat('  type: website',file=fileConn,append=TRUE,sep="\n")
cat('  output-dir: ../docs',file=fileConn,append=TRUE,sep="\n\n")
cat('website:',file=fileConn,append=TRUE,sep="\n")
cat('  title: "England and Wales Census Maps 2021"',file=fileConn,append=TRUE,sep="\n")
cat('  google-analytics: "G-7L3L3PPQ6F"',file=fileConn,append=TRUE,sep="\n")
cat('  navbar:',file=fileConn,append=TRUE,sep="\n")
cat('      background: light',file=fileConn,append=TRUE,sep="\n")
cat('      search: true',file=fileConn,append=TRUE,sep="\n")
cat('      left:',file=fileConn,append=TRUE,sep="\n")
cat('       - text: "NE"',file=fileConn,append=TRUE,sep="\n")
cat('         file: posts/E12000001.qmd',file=fileConn,append=TRUE,sep="\n")
cat('       - text: "NW"',file=fileConn,append=TRUE,sep="\n")
cat('         file: posts/E12000002.qmd',file=fileConn,append=TRUE,sep="\n")
cat('       - text: "Yorkshire and The Humber"',file=fileConn,append=TRUE,sep="\n")
cat('         file: posts/E12000003.qmd',file=fileConn,append=TRUE,sep="\n")
cat('       - text: "EM"',file=fileConn,append=TRUE,sep="\n")
cat('         file: posts/E12000004.qmd',file=fileConn,append=TRUE,sep="\n")
cat('       - text: "WM"',file=fileConn,append=TRUE,sep="\n")
cat('         file: posts/E12000005.qmd',file=fileConn,append=TRUE,sep="\n")
cat('       - text: "EE"',file=fileConn,append=TRUE,sep="\n")
cat('         file: posts/E12000006.qmd',file=fileConn,append=TRUE,sep="\n")
cat('       - text: "London"',file=fileConn,append=TRUE,sep="\n")
cat('         file: posts/E12000007.qmd',file=fileConn,append=TRUE,sep="\n")
cat('       - text: "SE"',file=fileConn,append=TRUE,sep="\n")
cat('         file: posts/E12000008.qmd',file=fileConn,append=TRUE,sep="\n")
cat('       - text: "SW"',file=fileConn,append=TRUE,sep="\n")
cat('         file: posts/E12000009.qmd',file=fileConn,append=TRUE,sep="\n")
cat('       - text: "Wales"',file=fileConn,append=TRUE,sep="\n")
cat('         file: posts/W92000004.qmd',file=fileConn,append=TRUE,sep="\n")
cat('format:',file=fileConn,append=TRUE,sep="\n")
cat('  html:',file=fileConn,append=TRUE,sep="\n")
cat('    theme: litera',file=fileConn,append=TRUE,sep="\n")
cat('    css: styles.css',file=fileConn,append=TRUE,sep="\n")

#Create new index.qmd

fileConn <- paste0("./website/index_h.txt")
cat('---',file=fileConn,append=TRUE,sep="\n")
cat('page-layout: full',file=fileConn,append=TRUE,sep="\n")
cat('title-block-banner: false',file=fileConn,append=TRUE,sep="\n")
cat('listing:',file=fileConn,append=TRUE,sep="\n")
cat('  id: main-listing',file=fileConn,append=TRUE,sep="\n")
cat('  filter-ui: [title]',file=fileConn,append=TRUE,sep="\n")
cat('  sort-ui: false',file=fileConn,append=TRUE,sep="\n")
cat('  contents:',file=fileConn,append=TRUE,sep="\n")
cat('   - posts/E12000001',file=fileConn,append=TRUE,sep="\n")
cat('   - posts/E12000002',file=fileConn,append=TRUE,sep="\n")
cat('   - posts/E12000003',file=fileConn,append=TRUE,sep="\n")
cat('   - posts/E12000004',file=fileConn,append=TRUE,sep="\n")
cat('   - posts/E12000005',file=fileConn,append=TRUE,sep="\n")
cat('   - posts/E12000006',file=fileConn,append=TRUE,sep="\n")
cat('   - posts/E12000007',file=fileConn,append=TRUE,sep="\n")
cat('   - posts/E12000008',file=fileConn,append=TRUE,sep="\n")
cat('   - posts/E12000009',file=fileConn,append=TRUE,sep="\n")
cat('   - posts/W92000004',file=fileConn,append=TRUE,sep="\n")
cat('  type: table',file=fileConn,append=TRUE,sep="\n")
cat('  fields: [image,title]',file=fileConn,append=TRUE,sep="\n")

cat('---',file=fileConn,append=TRUE,sep="\n")

system(paste("cat","./website/index_h.txt"," ./template/home_template.qmd","> ", "./website/index.qmd"))
unlink("./website/index_h.txt")


############################################################
# Setup files and directories to create the data / maps
############################################################  

# Create Directory Structure

for (i in 1:nrow(LAD_RGN)) {
  
  dir.create(paste0("./website/posts/",LAD_RGN[i,"RGN23CD"],"/",LAD_RGN[i,"LAD23CD"],"/"),recursive=TRUE)
  dir.create(paste0("./website/posts/",LAD_RGN[i,"RGN23CD"],"/",LAD_RGN[i,"LAD23CD"],"/maps/"),recursive=TRUE)
  
}

###############################

# Create OA Files for each LAD

for (lad in LAD_list) {
  
  
  tmp <- boundary %>%
    filter(LAD23CD == lad)
  
  rgn <- tmp %>% st_drop_geometry() %>% select(RGN23CD) %>% distinct() %>% pull()
  
  tmp %<>%
    select(OA21CD)
  
  write_sf(tmp, paste0("./website/posts/",rgn,"/",lad,"/",lad,".gpkg")) 
  
  rm(tmp,rgn)
  
}

###############################  

# Create Census LAD CSV for each Census Table

for (CT in C_Table_Name_List) { # loop through Census Tables
  
  tmp_CTAB <- get(CT) # Get the Census Table
  tmp_CTAB %<>%
    left_join(OA_LAD,by = c("OA" = "OA21CD")) # Append the LAD codes
  tmp_CTAB %<>%
    left_join(LAD_RGN,by = c("LAD23CD" = "LAD23CD"))  # Append the Region codes
  
  
  # This Check is Needed to Account for Tables that are only Produced for Wales
  
  wales_only_tables <- c("ts032","ts033","ts034","ts035","ts036")
  
  if (CT %in% wales_only_tables) {
    
    LAD_list_Wales_check <-  grep("^W", LAD_list, value = TRUE)
    
  } else {
    
    LAD_list_Wales_check <- LAD_list
    
  }
  
  
  for (lad in LAD_list_Wales_check) { # loop through each LAD
    
    # Cut the Census Table down for the LAD
    tmp_CTAB_LAD <- tmp_CTAB %>%
      filter(LAD23CD == lad) 
    
    # Get the Region
    rgn <- tmp_CTAB_LAD %>% st_drop_geometry() %>% select(RGN23CD) %>% distinct() %>% pull()
    
    # Remove the Lookups
    tmp_CTAB_LAD %<>%
      select(-LAD23CD,-RGN23CD)
    
    # Write CSV
    write_csv(tmp_CTAB_LAD, paste0("./website/posts/",rgn,"/",lad,"/",lad,"_",CT,".csv")) 
    
    rm(tmp_CTAB_LAD,rgn,lad)
    
  }
  
  rm(tmp_CTAB)
}

###############################

# Setup a Listing page for each Region

RG <- LAD_RGN %>% select(RGN23CD) %>% pull() %>% unique()
RG_N <- LAD_RGN %>% select(RGN23NM) %>% pull() %>% unique()


for (i in 1:length(RG)) {
  
  fileConn <- paste0("./website/posts/",RG[i],".qmd")
  cat('---',file=fileConn,append=TRUE,sep="\n")
  cat(paste0('title: "',RG_N[i],'"'),file=fileConn,append=TRUE,sep="\n")
  cat('listing:',file=fileConn,append=TRUE,sep="\n")
  cat(paste0("  contents: ",RG[i]),file=fileConn,append=TRUE,sep="\n")
  cat('  type: grid',file=fileConn,append=TRUE,sep="\n")
  cat('  fields: [image,title]',file=fileConn,append=TRUE,sep="\n")
  cat('  sort-ui: false',file=fileConn,append=TRUE,sep="\n")
  cat('  filter-ui: [title]',file=fileConn,append=TRUE,sep="\n")
  cat('page-layout: full',file=fileConn,append=TRUE,sep="\n")
  cat('title-block-banner: false',file=fileConn,append=TRUE,sep="\n")
  cat('---',file=fileConn,append=TRUE,sep="\n")
}

###############################

# Generate the QMD to Build the Maps for each LAD

for (i in 1:nrow(LAD_RGN)) {
  
  lad_name <- boundary_lad %>% 
    st_drop_geometry() %>% 
    filter(LAD23CD == LAD_RGN[i,"LAD23CD"]) %>%
    select(LAD23NM) %>%
    pull()
  
  unlink(paste0("./website/posts/",LAD_RGN[i,"RGN23CD"],"/",LAD_RGN[i,"LAD23CD"],"/index.qmd"))
  unlink(paste0("./website/posts/",LAD_RGN[i,"RGN23CD"],"/",LAD_RGN[i,"LAD23CD"],"/tmp_head.qmd"))
  
  
  # Create a tmp YAML
  
  fileConn <- paste0("./website/posts/",LAD_RGN[i,"RGN23CD"],"/",LAD_RGN[i,"LAD23CD"],"/tmp_head.qmd")
  cat('---',file=fileConn,append=TRUE,sep="\n")
  cat(paste0('title: "',lad_name,'"'),file=fileConn,append=TRUE,sep="\n")
  cat(paste0("categories: [",LAD_RGN[i,"LAD23CD"],"]"),file=fileConn,append=TRUE,sep="\n")
  cat('image: "map.png"',file=fileConn,append=TRUE,sep="\n")
  cat('---',file=fileConn,append=TRUE,sep="\n")
  
  rm(lad_name)
  
  
  # Append YAML to the Template in the Blog Folder
  
  system(paste("cat",fileConn," ./template/index.qmd","> ", paste0("./website/posts/",LAD_RGN[i,"RGN23CD"],"/",LAD_RGN[i,"LAD23CD"],"/index.qmd")))
  unlink(paste0("./website/posts/",LAD_RGN[i,"RGN23CD"],"/",LAD_RGN[i,"LAD23CD"],"/tmp_head.qmd"))
  
  
} 

###############################    

# Copy the All Data Page from the Template Folder

system("cp template/all_census_data.qmd website/all_census_data.qmd")


#################
# Make the Website!!!
############################################################ 
start_time <- Sys.time()
system("quarto render website --cache-refresh")
end_time <- Sys.time()
end_time - start_time