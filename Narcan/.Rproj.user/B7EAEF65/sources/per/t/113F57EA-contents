library(tidyverse)
library(httr2)
library(jsonlite)


##testing the ollama connection before running it with the narrative data
test_ollama_connection <- function() {
  cat("Testing Ollama connection...\n\n")
  
  # Test 1: Is Ollama running?
  tryCatch({
    response <- request("http://localhost:11434/api/tags") %>%
      req_perform()
    
    cat("✓ Ollama is running!\n")
    
    models <- resp_body_json(response)
    if (length(models$models) > 0) {
      cat("\nAvailable models:\n")
      for (m in models$models) {
        cat("  -", m$name, "\n")
      }
    } else {
      cat("\n✗ No models found. Run: ollama pull llama3.2\n")
      return(FALSE)
    }
    
  }, error = function(e) {
    cat("✗ Cannot connect to Ollama\n")
    cat("Make sure Ollama is running\n")
    cat("Error:", e$message, "\n")
    return(FALSE)
  })
  
  # Test 2: Can we generate text?
  cat("\nTesting text generation...\n")
  tryCatch({
    test_response <- request("http://localhost:11434/api/generate") %>%
      req_body_json(list(
        model = "llama3.2",
        prompt = "Say hello",
        stream = FALSE
      )) %>%
      req_perform() %>%
      resp_body_json()
    
    cat("✓ Text generation works!\n")
    cat("Response:", test_response$response, "\n")
    return(TRUE)
    
  }, error = function(e) {
    cat("✗ Text generation failed\n")
    cat("Error:", e$message, "\n")
    return(FALSE)
  })
}

test_ollama_connection()

classify_narcan_tuned <- function(narrative, model = "llama3.2", verbose = FALSE) {
  
  if (is.na(narrative) || nchar(str_trim(narrative)) == 0) {
    return("NONE")
  }
  
  prompt <- paste0(
    "Read this EMS narrative. Did someone give Narcan BEFORE the EMS crew writing this report arrived?\n\n",
    
    "NARRATIVE:\n",
    narrative, "\n\n",
    
    "STRONG INDICATORS someone gave narcan BEFORE EMS:\n",
    "1. 'prior to arrival' / 'prior to ems arrival' / 'prior to our arrival'\n",
    "2. 'per bystander/family' + they gave/administered\n",
    "3. 'bystanders/family stated they gave'\n",
    "4. 'before we arrived' / 'before ems'\n",
    "5. Friend/family/witness/roommate + gave + narcan\n",
    "6. Police/officer/sheriff + gave + narcan (before EMS)\n",
    
    "CODE AS NONE if:\n",
    "1. 'We gave' / 'crew administered' / 'medics gave' = the reporting EMS unit\n",
    "2. Just mentions narcan but doesn't say who gave it\n",
    "3. Passive 'narcan was given' with NO mention of who\n",
    
    "WHO gave it?\n",
    "- BYSTANDER: family, friends, civilians, witnesses, bystanders, shelter staff\n",
    "- PD: police, officers, sheriff, deputies, jail staff\n",
    "- OTHER: nurses, doctors, hospital staff, medics from another unit\n",
    "- NONE: your EMS crew gave it, or not mentioned, or unclear\n",
    
    "EXAMPLES:\n",
    "- 'Per bystanders they gave pt 8mg narcan prior to ems arrival' = BYSTANDER\n",
    "- 'Bystanders report they administered narcan' = BYSTANDER\n",
    "- 'Officer gave narcan before we arrived' = PD\n",
    "- 'We administered 4mg narcan' = NONE\n",
    "- 'Patient received narcan' (who gave it?) = NONE\n",
    
    "Answer with ONE WORD ONLY: BYSTANDER, PD, OTHER, or NONE"
  )
  
  if (verbose) {
    cat("Processing...\n")
  }
  
  tryCatch({
    response <- request("http://localhost:11434/api/generate") %>%
      req_body_json(list(
        model = model,
        prompt = prompt,
        stream = FALSE,
        options = list(
          temperature = 0.2,
          num_predict = 20,
          top_p = 0.9
        )
      )) %>%
      req_timeout(60) %>%
      req_perform()
    
    result <- resp_body_json(response)
    
    if (verbose) {
      cat("Model response:", result$response, "\n")
    }
    
    response_text <- toupper(str_trim(result$response))
    
    # Extract just the first word
    first_word <- str_extract(response_text, "^\\w+")
    
    if (verbose) {
      cat("Extracted:", first_word, "\n")
    }
    
    if (str_detect(first_word, "BYSTANDER")) {
      return("BYSTANDER")
    } else if (str_detect(first_word, "PD|POLICE")) {
      return("PD")
    } else if (str_detect(first_word, "OTHER")) {
      return("OTHER")
    } else {
      return("NONE")
    }
    
  }, error = function(e) {
    warning(paste("Error:", e$message))
    return("ERROR")
  })
}

# Stage 1: Quick regex filter to find potential cases
quick_filter_narcan <- function(narrative) {
  narrative_lower <- tolower(narrative)
  
  # Has narcan mention?
  if (!str_detect(narrative_lower, "\\b(narcan|naloxone)\\b")) {
    return("NO_NARCAN")
  }
  
  # Very obvious bystander patterns
  if (str_detect(narrative_lower, 
                 "\\b(per|according to) (bystander|family|friend|witness).{0,50}(gave|administered).{0,40}(narcan|naloxone)")) {
    return("LIKELY_BYSTANDER")
  }
  
  # Obvious prior to arrival
  if (str_detect(narrative_lower, 
                 "(prior to|before).{0,30}(arrival|ems).{0,50}(narcan|naloxone)") &&
      str_detect(narrative_lower, "\\b(bystander|family|friend|police|officer)\\b")) {
    return("LIKELY_PRE_EMS")
  }
  
  # Clear EMS administration
  if (str_detect(narrative_lower, 
                 "\\b(we|crew|medic) (gave|administered).{0,40}(narcan|naloxone)") &&
      !str_detect(narrative_lower, "(prior to|before).{0,30}arrival")) {
    return("LIKELY_EMS")
  }
  
  # Needs LLM review
  return("NEEDS_REVIEW")
}

# Stage 2: Process with two-stage approach
process_two_stage <- function(df, narrative_col, model = "llama3.2") {
  
  cat("Stage 1: Quick filtering...\n")
  
  df_filtered <- df %>%
    mutate(
      row_id = row_number(),
      quick_filter = sapply(.data[[narrative_col]], quick_filter_narcan)
    )
  
  # Summary of quick filter
  cat("\n=== Quick Filter Results ===\n")
  cat("No narcan mentioned:", sum(df_filtered$quick_filter == "NO_NARCAN"), "\n")
  cat("Likely bystander:", sum(df_filtered$quick_filter == "LIKELY_BYSTANDER"), "\n")
  cat("Likely pre-EMS:", sum(df_filtered$quick_filter == "LIKELY_PRE_EMS"), "\n")
  cat("Likely EMS only:", sum(df_filtered$quick_filter == "LIKELY_EMS"), "\n")
  cat("Needs LLM review:", sum(df_filtered$quick_filter == "NEEDS_REVIEW"), "\n\n")
  
  # Stage 2: LLM only on unclear cases
  needs_llm <- df_filtered %>% filter(quick_filter == "NEEDS_REVIEW")
  
  if (nrow(needs_llm) > 0) {
    cat("Stage 2: LLM processing", nrow(needs_llm), "unclear cases...\n\n")
    
    llm_results <- character(nrow(needs_llm))
    for (i in 1:nrow(needs_llm)) {
      llm_results[i] <- classify_narcan_tuned(
        needs_llm[[narrative_col]][i], 
        model, 
        verbose = FALSE
      )
      
      if (i %% 10 == 0) {
        cat(sprintf("LLM processed %d/%d\n", i, nrow(needs_llm)))
      }
    }
    
    needs_llm <- needs_llm %>%
      mutate(llm_result = llm_results)
  }
  
  # Combine results
  final_results <- df_filtered %>%
    left_join(
      needs_llm %>% select(row_id, llm_result),
      by = "row_id"
    ) %>%
    mutate(
      narcan_category = case_when(
        quick_filter == "NO_NARCAN" ~ "NONE",
        quick_filter == "LIKELY_BYSTANDER" ~ "BYSTANDER",
        quick_filter == "LIKELY_PRE_EMS" ~ "BYSTANDER",
        quick_filter == "LIKELY_EMS" ~ "OTHER",
        quick_filter == "NEEDS_REVIEW" ~ llm_result,
        TRUE ~ "NONE"
      ),
      Bystander = as.numeric(narcan_category == "BYSTANDER"),
      PD_jail = as.numeric(narcan_category == "PD"),
      Other = as.numeric(narcan_category == "OTHER")
    ) %>%
    select(-row_id, -quick_filter, -llm_result)
  
  # Summary
  cat("\n=== FINAL RESULTS ===\n")
  cat(sprintf("Bystander: %d (%.1f%%)\n", 
              sum(final_results$Bystander), 100*mean(final_results$Bystander)))
  cat(sprintf("PD: %d (%.1f%%)\n", 
              sum(final_results$PD_jail), 100*mean(final_results$PD_jail)))
  cat(sprintf("Other: %d (%.1f%%)\n", 
              sum(final_results$Other), 100*mean(final_results$Other)))
  
  return(final_results)
}

results <- process_two_stage(test, "Incident_Narrative")

test_first <- classify_narcan_tuned(test$Incident_Narrative[29], verbose = TRUE)

testcode <- process_balanced(test, "Incident_Narrative", model="llama3.2")
write.csv(testcode, "C:/Users/mdickson/OneDrive - Metro Nashville Government/Documents/CVS_Files/Narcan/Narcan/test2.csv", row.names = FALSE)
