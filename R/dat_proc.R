# setup -------------------------------------------------------------------

box::use(
  dplyr[...], 
  tidyr[...],
  here[...], 
  googlesheets4[read_sheet,gs4_deauth],
  googledrive[drive_deauth]
)

gs4_deauth()
drive_deauth()

# import, format data from Google Drive -----------------------------------

# data here https://drive.google.com/drive/u/0/folders/1ZbpbBfIxb5-BnXhYjymVNMzB88DqZeAO
id <- '1x_ytLD6ro--QzcCClnDvFC04m7PmbVbtReVQkNktyd4'

yr1 <- read_sheet(id, sheet = 'Yr1-transect')
yr2 <- read_sheet(id, sheet = 'Yr2-transect')