
# ============================================================================
# STAGE 1: Find all records mentioning narcan

filter_narcan_mentions <- function(df, narrative_col) {
  
  cat("Stage 1: Filtering for narcan mentions...\n")
  
  df_with_flag <- df %>%
    mutate(
      row_id = row_number(),
      has_narcan = str_detect(tolower(.data[[narrative_col]]), 
                              "\\b(narcan|naloxone)\\b")
    )
  
  narcan_subset <- df_with_flag %>%
    filter(has_narcan)
  
  cat(sprintf("\n=== STAGE 1 RESULTS ===\n"))
  cat(sprintf("Total records: %d\n", nrow(df)))
  cat(sprintf("Records with narcan: %d (%.1f%%)\n", 
              nrow(narcan_subset), 
              100 * nrow(narcan_subset) / nrow(df)))
  cat(sprintf("Records without narcan: %d (%.1f%%)\n",
              sum(!df_with_flag$has_narcan),
              100 * mean(!df_with_flag$has_narcan)))
  
  return(list(
    all_data = df_with_flag,
    narcan_subset = narcan_subset
  ))
}


# Option B: Use LLM on the narcan subset
classify_narcan_subset_llm <- function(narcan_subset, narrative_col, 
                                       model = "llama3.2",
                                       save_progress = TRUE,
                                       progress_file = "narcan_llm_progress.rds") {
  
  cat("\nStage 2: Running LLM on narcan subset...\n")
  cat(sprintf("Processing %d records with narcan mentions\n\n", nrow(narcan_subset)))
  
  # Check for existing progress
  if (save_progress && file.exists(progress_file)) {
    cat("Loading progress...\n")
    results <- readRDS(progress_file)
    start_row <- length(results) + 1
    if (start_row > nrow(narcan_subset)) {
      cat("Already complete!\n")
      return(narcan_subset %>% mutate(narcan_category = results))
    }
    cat(sprintf("Resuming from row %d\n", start_row))
  } else {
    results <- character(0)
    start_row <- 1
  }
  
  start_time <- Sys.time()
  
  for (i in start_row:nrow(narcan_subset)) {
    narrative <- narcan_subset[[narrative_col]][i]
    
    results[i] <- classify_narcan_final_simple(narrative, model)
    
    if (i %% 10 == 0) {
      elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
      rate <- (i - start_row + 1) / elapsed
      remaining <- (nrow(narcan_subset) - i) / rate
      
      cat(sprintf("%d/%d (%.1f%%) - %.1f min left - Last: %s\n",
                  i, nrow(narcan_subset), 100*i/nrow(narcan_subset), 
                  remaining, results[i]))
      
      if (save_progress) saveRDS(results, progress_file)
    }
  }
  
  if (save_progress) saveRDS(results, progress_file)
  
  narcan_subset_coded <- narcan_subset %>%
    mutate(narcan_category = results)
  
  cat("\n=== LLM RESULTS ===\n")
  cat(sprintf("Bystander: %d\n", sum(results == "BYSTANDER")))
  cat(sprintf("PD: %d\n", sum(results == "PD")))
  cat(sprintf("Other: %d\n", sum(results == "OTHER")))
  cat(sprintf("None: %d\n", sum(results == "NONE")))
  
  return(narcan_subset_coded)
}

# Simple, clear LLM classifier
classify_narcan_final_simple <- function(narrative, model = "llama3.2") {
  
  if (is.na(narrative) || nchar(str_trim(narrative)) == 0) {
    return("NONE")
  }
  
  prompt <- paste0(
    "Read this EMS narrative. Did someone give Narcan BEFORE the EMS crew writing this report arrived?\n\n",
    
    "NARRATIVE:\n",
    narrative, "\n\n",
    
    "Look for these patterns indicating pre-EMS narcan administration:\n",
    "1. 'prior to arrival/ems arrival' + someone gave it\n",
    "2. 'per bystander/family' + gave/administered/have\n",
    "3. 'family/bystander stated/sts they gave/have'\n",
    "4. 'before we arrived' + non-EMS person\n",
    "5. 'Mission staff/shelter employees' + gave/administered\n",
    "6. 'mother/father/family gave/have pt narcan'\n",
    "7. 'Police/officer' + gave/administered\n",
    "8. Any variation of family/friend + narcan + before EMS\n",
    
    "Important: 'family sts they have pt narcan' means family gave narcan (sts = states, have = gave)\n",
    
    "CODE AS NONE only if:\n",
    "1. 'We gave' / 'crew administered' = the reporting EMS unit\n",
    "2. Mentions narcan but NO indication of who gave it\n",
    "3. Completely unclear who administered it\n",
    
    "Categories:\n",
    "- BYSTANDER: witness, wife, son, passenger, friends, friend, mother, bystander, family, sister, roommates, grandma, grandson, grandpa, boyfriend, girlfriend, caregiver, uncle, aunt, stranger, stepmother, stepfather, layperson, daughter, roomate, niece, acquaintance, dad, clerk, neighbors, cousin, spouse, employee, staff, passerby, mate, coworker, mom, workers, neighbor, father, husband, brother, staff members\n",
    "- PD: officer, mnpd, sheriff, guards, jail, inmate, cop, guard, prison, police, jail staff, pd\n",
    "- OTHER: nurses, doctors, hospital staff\n",
    "- NONE: EMS crew gave it, or unclear who\n",
    
    "EXAMPLES:\n",
    "- 'Family sts they have pt 4mg narcan prior to ems arrival' = BYSTANDER\n",
    "- 'Mother gave him 4mg narcan intranasal' = BYSTANDER\n",
    "- 'Mission staff gave 3 doses prior to our arrival' = BYSTANDER\n",
    "- 'Staff administered Narcan and called for ems' = BYSTANDER\n",
    "- 'Per bystanders they gave pt narcan' = BYSTANDER\n",
    "- 'Officer gave narcan before we arrived' = PD\n",
    "- 'Hospital nurse gave narcan' = OTHER\n",
    "- 'We administered 4mg narcan' = NONE\n",
    
    "Answer ONE WORD: BYSTANDER, PD, OTHER, or NONE"
  )
  
  tryCatch({
    response <- request("http://localhost:11434/api/generate") %>%
      req_body_json(list(
        model = model,
        prompt = prompt,
        stream = FALSE,
        options = list(temperature = 0.2, num_predict = 20)
      )) %>%
      req_timeout(60) %>%
      req_perform()
    
    result <- resp_body_json(response)
    
    # FIX: Safely extract the response text
    if (is.list(result) && !is.null(result$response)) {
      response_text <- as.character(result$response)
      response_text <- toupper(str_trim(response_text[1]))  # Take first element if vector
    } else {
      return("ERROR")
    }
    
    # Check response
    if (grepl("BYSTANDER", response_text)) return("BYSTANDER")
    if (grepl("PD|POLICE", response_text)) return("PD")
    if (grepl("OTHER", response_text)) return("OTHER")
    return("NONE")
    
  }, error = function(e) {
    return("ERROR")
  })
}
# ============================================================================
# STAGE 3: Merge results back to full dataset
# ============================================================================

merge_results_to_full <- function(all_data, coded_subset) {
  
  cat("\nStage 3: Merging results back to full dataset...\n")
  
  # Get the coded results
  coded_results <- coded_subset %>%
    select(row_id, narcan_category)
  
  # Merge back
  final_results <- all_data %>%
    left_join(coded_results, by = "row_id") %>%
    mutate(
      # Fix: if has narcan but no one gave it before EMS = OTHER
      # if no narcan mentioned at all = keep as NONE for clarity
      narcan_category = case_when(
        has_narcan & is.na(narcan_category) ~ "OTHER",
        has_narcan & narcan_category == "NONE" ~ "OTHER",
        !has_narcan ~ "NO_NARCAN",
        TRUE ~ narcan_category
      ),
      Bystander = as.numeric(narcan_category == "BYSTANDER"),
      PD_jail = as.numeric(narcan_category == "PD"),
      Other = as.numeric(narcan_category == "OTHER"),
      # Keep has_narcan flag for verification
      narcan_mentioned = if_else(has_narcan, "YES", "NO")
    ) %>%
    select(-row_id, -has_narcan)
  
  cat("\n=== FINAL RESULTS ===\n")
  cat(sprintf("Total records: %d\n", nrow(final_results)))
  cat(sprintf("Records with narcan mentioned: %d (%.1f%%)\n",
              sum(final_results$narcan_mentioned == "YES"),
              100*mean(final_results$narcan_mentioned == "YES")))
  cat(sprintf("Bystander: %d (%.1f%%)\n", 
              sum(final_results$Bystander), 100*mean(final_results$Bystander)))
  cat(sprintf("PD: %d (%.1f%%)\n", 
              sum(final_results$PD_jail), 100*mean(final_results$PD_jail)))
  cat(sprintf("Other: %d (%.1f%%)\n", 
              sum(final_results$Other), 100*mean(final_results$Other)))
  cat(sprintf("No narcan mentioned: %d (%.1f%%)\n",
              sum(final_results$narcan_category == "NO_NARCAN"),
              100*mean(final_results$narcan_category == "NO_NARCAN")))
  
  return(final_results)
}

# Import manual codes if you went the manual review route
import_manual_review <- function(all_data, coded_csv, narrative_col) {
  
  cat("Importing manual codes...\n")
  
  coded <- read_csv(coded_csv, col_types = cols(
    row_id = col_double(),
    bystander = col_double(),
    pd = col_double(),
    other = col_double()
  ))
  
  coded_clean <- coded %>%
    select(row_id, bystander, pd, other) %>%
    mutate(
      bystander = replace_na(bystander, 0),
      pd = replace_na(pd, 0),
      other = replace_na(other, 0),
      narcan_category = case_when(
        bystander == 1 ~ "BYSTANDER",
        pd == 1 ~ "PD",
        other == 1 ~ "OTHER",
        TRUE ~ "NONE"
      )
    )
  
  return(merge_results_to_full(all_data, coded_clean))
}


# ============================================================================
# COMPLETE WORKFLOW - ALL THREE STAGES
# ============================================================================

complete_three_stage_workflow <- function(df, narrative_col, 
                                          method = "llm",  # "llm" or "manual"
                                          model = "llama3.2") {
  
  cat("\n========================================\n")
  cat("THREE-STAGE NARCAN DETECTION WORKFLOW\n")
  cat("========================================\n\n")
  
  # STAGE 1: Filter for narcan mentions
  stage1 <- filter_narcan_mentions(df, narrative_col)
  
  if (method == "manual") {
    # STAGE 2: Export for manual review
    cat("\nStage 2: Preparing for manual review...\n")
    export_narcan_for_review(stage1$narcan_subset, narrative_col)
    
    cat("\n========================================\n")
    cat("NEXT STEPS:\n")
    cat("1. Open narcan_subset_review.csv in Excel\n")
    cat("2. Code the bystander, pd, other columns\n")
    cat("3. Save the file\n")
    cat("4. Run: final <- import_manual_review(df, 'narcan_subset_review.csv', '", 
        narrative_col, "')\n")
    cat("========================================\n\n")
    
    return(stage1$narcan_subset)
    
  } else {
    # STAGE 2: Use LLM
    stage2 <- classify_narcan_subset_llm(stage1$narcan_subset, narrative_col, model)
    
    # STAGE 3: Merge back
    final_results <- merge_results_to_full(stage1$all_data, stage2)
    
    return(final_results)
  }
}


file.remove("narcan_llm_progress.rds")
# Get random 100 records
random_sample <- ncan[sample(nrow(ncan), 100), ]
random_sample <- random_sample %>%
  mutate(Incident_Narrative = iconv(Incident_Narrative, to = "UTF-8", sub = ""))

results <- complete_three_stage_workflow(random_sample, "Incident_Narrative", method = "llm")
View(results %>% select(Incident_Narrative, narcan_category, Bystander, PD_jail, Other))

write.csv(results, "C:/Users/mdickson/OneDrive - Metro Nashville Government/Documents/CVS_Files/Narcan/Narcan/test2.csv", row.names = FALSE)

