library(dplyr)
library(arcgislayers)
library(readr)


df <- read_csv("data/hawaii_zone_dist3.csv") |>
  select(!("...1")) |>
  mutate(zone_clean = str_trim(toupper(zone_raw)))

# we could split it up into four parts, of each county, and then clean individually
# will read in CSVs later
# ==========================================
# PART 1: HONOLULU COUNTY
# ==========================================
df_honolulu <- df %>% 
  filter(county == "honolulu") %>% 
  mutate(
    zone_main_type = case_when(
      # Conservation
      str_detect(zone_clean, "^P-[12]|^P$|^F-1|^F$|^C") ~ "Conservation",
      
      # Public Space
      str_detect(zone_clean, "^OPEN") ~ "Public Space",
      
      # Agricultural
      str_detect(zone_clean, "^AG") ~ "Agricultural",
      
      # Residential (Honolulu specific: A-1, A-2, A-3 are apartment complexes)
      str_detect(zone_clean, "^RS|^RD|^RM|^R-\\d|^RA$|^A-[123]$|^RES|^APA") ~ "Residential",
      
      # Commercial + Industrial (Private Sector Employment)
      str_detect(zone_clean, "^B-|^BMX|^V-|^H-|^AMX|^IMX|^I-[123]$|^I$|^RESORT") ~ "Commercial",
      
      # Other
      str_detect(zone_clean, "^PD|^UNZONED") ~ "Other",
      TRUE ~ "Other"
    )
  )

# ==========================================
# PART 2: HAWAII COUNTY
# ==========================================
df_hawaii <- df %>% 
  filter(county == "hawaii") %>% 
  mutate(
    zone_main_type = case_when(
      # Conservation
      str_detect(zone_clean, "^RE$|^FR$") ~ "Conservation",
      
      # Public Space
      str_detect(zone_clean, "^OPEN$") ~ "Public Space",
      
      # Agricultural (Hawaii specific: A-1a, A-5a, etc. mean Agricultural)
      str_detect(zone_clean, "^A-\\d|^A$|^IA$|^FA") ~ "Agricultural",
      
      # Residential
      str_detect(zone_clean, "^RS|^RD|^RM|^RCX|^RA") ~ "Residential",
      
      # Commercial + Industrial + Resort (Private Sector Employment)
      str_detect(zone_clean, "^CN|^CG|^CV|^MC|^ML|^MG|^V") ~ "Commercial",
      
      # Other
      str_detect(zone_clean, "^PD|^UNZONED") ~ "Other",
      TRUE ~ "Other"
    )
  )

# ==========================================
# PART 3: KAUAI COUNTY AND MAUI COUNTY
# ==========================================
df_kauai <- df %>% 
  filter(county == "kauai")|>
  mutate(
    zone_main_type = case_when(
      # --- CONSERVATION ---
      str_detect(zone_clean, "^CONSERVATION") ~ "Conservation",
      str_detect(zone_clean, "^SPECIAL TREATMENT - (ECOLOGICAL|CULTURAL)$") ~ "Conservation",
      
      # --- PUBLIC SPACE ---
      str_detect(zone_clean, "^OPEN SPACE") ~ "Public Space",
      str_detect(zone_clean, "UNIVERSITY DISTRICT") ~ "Public Space",
      
      # --- AGRICULTURAL ---
      str_detect(zone_clean, "^AGRICULTURAL") ~ "Agricultural",
      
      # --- RESIDENTIAL ---
      str_detect(zone_clean, "^RESIDENTIAL|^RURAL") ~ "Residential",
      
      # --- COMMERCIAL (Private Sector Employment) ---
      str_detect(zone_clean, "COMMERCIAL|RESORT|INDUSTRIAL|PLANTATION CAMP") ~ "Commercial",
      
      # --- OTHER ---
      str_detect(zone_clean, "UNZONED|PLANNING DEPARTMENT|PROJECT DISTRICT|PLAN DEVELOPMENT|PLANNING AREA") ~ "Other",
      
      TRUE ~ "Other"
    )
  )


df_maui <- df %>% 
  filter(county == "maui") |>
  mutate(
    zone_main_type = case_when(
      # --- AGRICULTURAL ---
      str_detect(zone_clean, "^AG ") ~ "Agricultural",
      
      # --- RESIDENTIAL ---
      str_detect(zone_clean, "RESIDENTIAL|APARTMENT|^A-[12]|DUPLEX|RURAL|^RU-") ~ "Residential",
      str_detect(zone_clean, "MULTI FAMILY|^R\\s") ~ "Residential",
      
      # --- COMMERCIAL (Private Sector Employment) ---
      str_detect(zone_clean, "BUSINESS|HOTEL|RESORT|INDUSTRIAL|^M-[123]|^M1 ") ~ "Commercial",
      str_detect(zone_clean, "TECHNOLOGY PARK|MRTP|KRTP|^WCT|COMMERCIAL MIXED") ~ "Commercial",
      str_detect(zone_clean, "^SBR ") ~ "Commercial", # Service Business Residential
      
      # --- CONSERVATION ---
      str_detect(zone_clean, "OPEN SPACE|OPEN ZONE|^OS|BEACH RIGHT-OF-WAY|DRAINAGE") ~ "Conservation",
      
      # --- PUBLIC SPACE ---
      str_detect(zone_clean, "^PK|^PARK|^GC\\s|PUBLIC|AIRPORT") ~ "Public Space",
      
      # --- OTHER ---
      str_detect(zone_clean, "UNZONED|NOT ZONED|INTERIM|RESERVE|PROJECT DISTRICT|^PD\\s") ~ "Other",
      str_detect(zone_clean, "HISTORIC DISTRICT|CIVIC IMPROVEMENT") ~ "Other",
      
      TRUE ~ "Other"
    )
  )
# ==========================================
# PART 4: MERGE AND CLEANUP
# ==========================================
df_final <- bind_rows(df_honolulu, df_hawaii, df_kauai, df_maui)  %>% 
 select(-zone_clean) # Drop the temporary cleanup column

# Verify the classifications by county
df_final %>% 
  count(county, zone_main_type) %>% 
  print(n = 23)

left_out <- df_final |>
  filter(zone_main_type == "Other") |>
  count(zone_raw, county)

write.csv(df_final, "data/hawaii_blocks_cat.csv")

