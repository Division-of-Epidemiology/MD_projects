
write.csv(test_code, "C:/Users/mdickson/OneDrive - Metro Nashville Government/Documents/CVS_Files/Narcan/Narcan/words.csv", row.names = FALSE)


bystander_words <- c("witness", "wittness", "wife", "wifew", "son", "passenger", "friends", "friend", "mother", "bystander", "bystanders", "family", "sister", "sisters", "sisster", "roommates", "rommates", "grandma", 
                     "grandson", "grandpa", "boyfriend", "girlfriend", "girlfirend", "caregiver", "bysstander", "bystadner", "bys", "uncle", "aunt", "stranger", "strangers", "stepmother", "stepfather",
                     "mates", "layperson", "daughter", "bistanders", "roomate", "roomates", "niece", "girlfriends", "boyfriends", "acquaintance", "dad", "clerk", "bystander's", "neighbors", "cousin", "spouse",
                     "employee", "staff", "passerby", "employees", "mate", "coworker", "mom", "workers", "by stander's", "standers", "neighbor", "father", "mother", "husband", "brother", "staff members")

other_words <- c("nurses", "firefighters", "fire", "paramedic", "crew", "medic", "ems", "RN", "teacher", "pharmacist", "hospital",  "doctors",
                 "hosptial", "provider", "providers", "paramedics", "physicians", "partner", "doctor", "responders", "patient", "first responders")

PD_words <- c("officer",  "mnpd", "sheriff", "sherriff",   "guards", "jail", "inmate", 
              "cop", "guard", "prison", "police", "jail staff", "pd")

narcan_pattern <- "(nar+can|narkan|narcan.?|naloxone|nalaxon|nalaxone)"


action_pattern <- "(given|administered|gave|used)"

make_regex <- function(actor_words) {
     paste0(
         "(",
         # Active voice: actor ... action ... narcan
           "\\b(", paste(tolower(actor_words), collapse = "|"), 
         ")\\b(?:[^.?!]|\\.(?=\\d))*?\\b", action_pattern, "\\b(?:[^.?!]|\\.(?=\\d))*?\\b", narcan_pattern,
         "|",
         # Passive voice: narcan ... action ... by actor
           "\\b", narcan_pattern, 
         "\\b(?:[^.?!]|\\.(?=\\d))*?\\b(was\\s+)?", action_pattern, 
         "\\b(?:[^.?!]|\\.(?=\\d))*?\\bby\\b(?:[^.?!]|\\.(?=\\d))*?(", paste(tolower(actor_words), collapse = "|"), ")\\b",
         ")"
       )
}

test_code <- test %>%
  mutate(Incident_Narrative = iconv(Incident_Narrative, to = "UTF-8", sub = "")) %>%
  mutate(
    PD_jail = ifelse(
      str_detect(
        tolower(Incident_Narrative),
        regex(make_regex(PD_words), ignore_case = TRUE)
      ),
      1, 0
    ),
    Bystander = ifelse(
      str_detect(
        tolower(Incident_Narrative),
        regex(make_regex(bystander_words), ignore_case = TRUE)
      ),
      1, 0
    ),
    Other = ifelse(
      str_detect(
        tolower(Incident_Narrative),
        regex(make_regex(other_words), ignore_case = TRUE)
      ),
      1, 0
    )
  )
