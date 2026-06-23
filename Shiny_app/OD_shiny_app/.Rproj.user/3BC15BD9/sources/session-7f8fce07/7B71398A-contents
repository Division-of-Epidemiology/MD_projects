
# ============================================================
#  STEP 1: Geocode EMS and ME Addresses
#  Run this FIRST as its own script before touching the Shiny app.
#  It saves geocoded results to CSV so you only have to do this once
#  (geocoding 1000s of addresses can take a few minutes).
# ============================================================


library(tidygeocoder)
library(dplyr)
library(readr)

# ── 1. Load your raw EMS data ────────────────────────────────
ems_raw <- read.csv(
  "C:/Users/mdickson/OneDrive - Metro Nashville Gov/Desktop/CVS_Files/EMS_2326.csv",
  stringsAsFactors = FALSE
)

# Build a single full-address column from your separate columns.
# ADJUST these column names to match your actual EMS column names.
ems_raw <- ems_raw %>%
  mutate(
    full_address = paste(Address, Zip, sep = ", ")
  )

# ── 2. Geocode EMS addresses (Census batch geocoder — free, no API key) ──
ems_geocoded <- ems_raw %>%
  geocode(
    address = full_address,
    method  = "census",
    lat     = lat,
    long    = lng
  )

# Check how many succeeded
cat("EMS geocoded:", sum(!is.na(ems_geocoded$lat)), "of", nrow(ems_geocoded), "\n")

# Save so you never have to re-run this step
write_csv(
  ems_geocoded,
  "C:/Users/mdickson/OneDrive - Metro Nashville Gov/Desktop/CVS_Files/EMS_geocoded.csv"
)

# ── 3. Load your raw ME data ─────────────────────────────────
me_raw <- read.csv(
  "C:/Users/mdickson/OneDrive - Metro Nashville Gov/Desktop/CVS_Files/ME_2326.csv",
  stringsAsFactors = FALSE
)

# ADJUST these column names to match your actual ME column names.
me_raw <- me_raw %>%
  mutate(
    full_address = paste(Injury_Location, IL_City, IL_State, Zip, sep = ", ")
  )

# ── 4. Geocode ME addresses ──────────────────────────────────
me_geocoded <- me_raw %>%
  geocode(
    address = full_address,
    method  = "census",
    lat     = lat,
    long    = lng
  )

cat("ME geocoded:", sum(!is.na(me_geocoded$lat)), "of", nrow(me_geocoded), "\n")

write_csv(
  me_geocoded,
  "C:/Users/mdickson/OneDrive - Metro Nashville Gov/Desktop/CVS_Files/ME_geocoded.csv"
)

# ── 5. Quick check — see which addresses FAILED to geocode ──
ems_failed <- ems_geocoded %>% filter(is.na(lat))
me_failed  <- me_geocoded  %>% filter(is.na(lat))

cat("\nEMS failed addresses:", nrow(ems_failed), "\n")
cat("ME failed addresses:",  nrow(me_failed),  "\n")


# ============================================================
#  STEP 2: Assign Census Tracts
#  - EMS/ME: spatial join lat/long -> tract (exact, point-in-polygon)
#  - ED: apportion zip-level daily counts -> tracts using HUD crosswalk
#  Run this AFTER step1_geocode.R. Saves tract-tagged files to CSV.
# ============================================================

library(sf)
library(dplyr)
library(readr)
library(readxl)

# ── 1. Load your census tract shapefile ──────────────────────
tracts <- st_read(
  "C:/Users/mdickson/OneDrive - Metro Nashville Gov/Desktop/CVS_Files/CT_boundaries/ct.shp"
) %>%
  st_transform(crs = 4326) %>%   # WGS84, matches lat/long from geocoding
  select(GEOID=GEOCODE, geometry)        # keep just what we need

# ── 2. EMS — spatial join (exact) ────────────────────────────
ems_geo <- read_csv(
  "C:/Users/mdickson/OneDrive - Metro Nashville Gov/Desktop/CVS_Files/EMS_geocoded.csv"
) %>%
  filter(!is.na(lat), !is.na(lng))

ems_points <- st_as_sf(ems_geo, coords = c("lng", "lat"), crs = 4326, remove = FALSE)

ems_tract <- st_join(ems_points, tracts, join = st_within) %>%
  st_drop_geometry()   # drop spatial geometry, keep GEOID as a regular column

cat("EMS matched to tract:", sum(!is.na(ems_tract$GEOID)), "of", nrow(ems_tract), "\n")

write_csv(
  ems_tract,
  "C:/Users/mdickson/OneDrive - Metro Nashville Gov/Desktop/CVS_Files/EMS_tract.csv"
)

# ── 3. ME — spatial join (exact) ─────────────────────────────
me_geo <- read_csv(
  "C:/Users/mdickson/OneDrive - Metro Nashville Gov/Desktop/CVS_Files/ME_geocoded.csv"
) %>%
  filter(!is.na(lat), !is.na(lng))

me_points <- st_as_sf(me_geo, coords = c("lng", "lat"), crs = 4326, remove = FALSE)

me_tract <- st_join(me_points, tracts, join = st_within) %>%
  st_drop_geometry()

cat("ME matched to tract:", sum(!is.na(me_tract$GEOID)), "of", nrow(me_tract), "\n")

write_csv(
  me_tract,
  "C:/Users/mdickson/OneDrive - Metro Nashville Gov/Desktop/CVS_Files/ME_tract.csv"
)

# ── 4. ED — apportion via HUD ZIP-to-Tract crosswalk ─────────
# Download the crosswalk first from:
# https://www.huduser.gov/portal/datasets/usps_crosswalk.html
# -> choose "ZIP-TRACT" file, most recent quarter, save as .xlsx

crosswalk <- read_excel(
  "C:/Users/mdickson/OneDrive - Metro Nashville Gov/Desktop/CVS_Files/ZIP_TRACT_122025.xlsx"
) %>%
  rename(
    zip   = ZIP,
    GEOID = TRACT,
    weight = RES_RATIO     # residential allocation ratio: fraction of zip's residential addresses in this tract
  ) %>%
  mutate(
    zip   = as.character(zip),
    GEOID = as.character(GEOID)
  ) %>%
  select(zip, GEOID, weight)

# Load raw ED data (zip-level, with dates)
ed_raw <- read_csv(
  "C:/Users/mdickson/OneDrive - Metro Nashville Gov/Desktop/CVS_Files/ESSENCE_2326.csv"
) %>%
  mutate(
    date = as.Date(Date, format = "%m/%d/%Y"),
    zip  = as.character(Zipcode)
  ) %>%
  filter(!is.na(date), !is.na(zip))

# Step A: get ED daily case counts per zip (one row per case -> count by date+zip)
ed_daily_zip <- ed_raw %>%
  count(date, zip, name = "cases")

# Step B: join to crosswalk weights, apportion fractional cases to each tract
ed_tract_apportioned <- ed_daily_zip %>%
  inner_join(crosswalk, by = "zip") %>%
  mutate(est_cases = cases * weight) %>%
  group_by(date, GEOID) %>%
  summarise(est_cases = sum(est_cases), .groups = "drop")

cat("ED apportioned rows:", nrow(ed_tract_apportioned), "\n")
cat("ED total estimated cases (should ~match raw total):",
    round(sum(ed_tract_apportioned$est_cases), 1),
    "vs raw:", nrow(ed_raw), "\n")

write_csv(
  ed_tract_apportioned,
  "C:/Users/mdickson/OneDrive - Metro Nashville Gov/Desktop/CVS_Files/ED_tract_estimated.csv"
)

