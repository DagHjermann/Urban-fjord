#
# From Claude 19.06.2025
#


# Prompt 1
# I want to match to sets of data based on names. 
# Here is dataset 1, in R format:

# Answer 1, excerpt:
#
# Could you please share:
#   
# 1. Dataset 2 - the other set of data you want to match against
# 2.  Matching criteria - are you looking for:
#   Exact name matches?
#   Fuzzy/approximate matches (handling variations in spelling/formatting)?
#   Translation matches (since some names appear to be in Norwegian/other languages)?
#
# Prompt 2:
# 
# I am looking for both translational matches, as well as matches for abbreviations that are inside parantheses. For the latter matches, the match should be exact.
# Dataset 1 shall be matched using NAME.
# Dataset 2 shall be matched using Substances or Abbreviation.
# Dataset 2 follows:

# Create Norwegian-English translation dictionary
norwegian_english_dict <- list(
  # Metals
  "Sølv" = "Silver",
  "Tinn" = "Tin", 
  "Jern" = "Iron",
  "Nikkel" = "Nickel",
  "Kobber" = "Copper",
  "Sink" = "Zinc",
  "Arsen" = "Arsenic",
  "Kadmium" = "Cadmium",
  "Antimon" = "Antimony",
  "Bly" = "Lead",
  "Krom" = "Chromium",
  "Kvikksølv" = "Mercury",
  "Lantan" = "Lanthanum",
  "Praseodym" = "Praeseodymium",
  "Neodym" = "Neodymium",
  "Samarium" = "Samarium",
  "Europium" = "Europium",
  "Gadolinium" = "Gadolinium",
  "Terbium" = "Terbium",
  "Dysprosium" = "Dysprosium",
  "Holmium" = "Holmium",
  "Erbium" = "Erbium",
  "Thulium" = "Thulium",
  "Ytterbium" = "Ytterbium",
  "Lutetium" = "Lutetium",
  "Yttrium" = "Yttrium",
  "Scandium" = "Scandium",
  "Cerium" = "Cerium",
  
  # Phosphates (Norwegian names with English equivalents)
  "Trifenylfosfat" = "Triphenyl phosphate",
  "Trikresylfosfat" = "Tricresyl phosphate", 
  "Tri(1-klor-2-propyl)fosfat" = "Tris(1-chloropropyl) phosphate",
  "Tri(1,3-diklor-2-propyl)fosfat" = "Tris(1,3-dichloro-2-propyl) phosphate",
  "Triisobutylfosfat" = "Triisobutyl phosphate",
  "2-etylhexyldifenylfosfat" = "2-Ethylhexyl diphenyl phosphate",
  "Tri(2-kloroetyl)fosfat" = "Tris(2-chloroethyl) phosphate",
  "Dibutylfenylfosfat" = "Dibutyl phenyl phosphate",
  "Tris(2-etylhexyl)fosfat" = "Tris(2-ethylhexyl) phosphate",
  "Butyldifenylfosfat" = "Butyl diphenyl phosphate",
  "Trietylfosfat" = "Triethyl phosphate",
  "Tri(2-butoxyetyl)fosfat" = "Tris(2-butoxyethyl) phosphate",
  
  # Phenols
  "Bisfenol P" = "Bisphenol P",
  
  # Siloxanes (Norwegian descriptions)
  "oktametylsyklotetrasiloksan" = "2,2,4,4,6,6,8,8-Octamethyl-1,3,5,7,2,4,6,8-tetroxatetrasilocane",
  "dekametylsyklopentasiloksan" = "2,2,4,4,6,6,8,8,10,10-Decamethyl-1,3,5,7,9,2,4,6,8,10-pentoxapentasilecane",
  "dodekametylsykloheksasiloksan" = "Dodecamethylcyclohexasiloxane",
  "tris(trimetylsiloksy)fenylsilan" = "tris(trimethylsiloxy)phenylsilane",
  "octamethyltrisiloxane" = "Octamethyltrisiloxane (L3)",
  "decamethyltetrasiloxane" = "Decamethyltetrasiloxane (L4)",
  "dodecamethylpentasiloxane" = "Dodecamethylpentasiloxane (L5)",
  
  # Chlorinated paraffins
  "Kortkjedede" = "Short-chain chlorinated paraffins (C10-C13)",
  "Mellomkjedede" = "Medium-chain chlorinated paraffins (C14-C17)",
  "Langkjedede" = "Long-chain chlorinated paraffins (C>17)",
  
  # Others
  "Klorheksidin" = "Chlorhexidine",
  "Tebuconazol" = "Tebuconazol",
  "Metyl-1H-benzotriazole" = "Methyl-1H-benzotriazole",
  "Octocrylen" = "Octocrylene",
  "Dibromo-aldrin" = "Dibromoaldrin",
  "Homosalate" = "Homosalate",
  "Acetyl cedrene" = "Acetyl cedrene",
  "Tonalide" = "Tonalide",
  "Celestolide" = "Celestolide",
  "Galaxolide" = "Galaxolide",
  "Traseolide" = "Traseolide",
  "Phantolide" = "Phantolide",
  "Iso-E-super" = "Iso-E-super",
  "Triclosan" = "Triclosan",
  "Chlorpyrifos" = "Chlorpyrifos",
  "Benzophenone-3" = "Benzophenone-3",
  "Permitrin" = "Permitrin",
  "Dodecylphenol" = "Dodecylphenol"
)

# Function to extract abbreviation from parentheses
extract_abbreviation <- function(text) {
  # Extract text within parentheses
  matches <- regmatches(text, gregexpr("\\(([^)]+)\\)", text))
  if (length(matches[[1]]) > 0) {
    # Remove parentheses and return the content
    return(gsub("[()]", "", matches[[1]][1]))
  }
  return(NA)
}

# Function to clean names for better matching
clean_name <- function(text) {
  # Remove content in parentheses and trim whitespace
  cleaned <- gsub("\\s*\\([^)]+\\)", "", text)
  cleaned <- trimws(cleaned)
  return(cleaned)
}

# Function to perform matching
match_datasets <- function(dataset1, dataset2, norwegian_english_dict) {
  
  # Initialize results dataframe
  results <- data.frame(
    Dataset1_NAME = character(),
    Dataset1_METHOD_ID = integer(),
    Dataset2_Substances = character(),
    Dataset2_Abbreviation = character(),
    Dataset2_Group = character(),
    Match_Type = character(),
    Match_Quality = character(),
    stringsAsFactors = FALSE
  )
  
  # Process each row in dataset1
  for (i in 1:nrow(dataset1)) {
    name1 <- dataset1$NAME[i]
    method_id <- dataset1$METHOD_ID[i]
    
    # Extract abbreviation from dataset1 if present
    abbrev1 <- extract_abbreviation(name1)
    clean_name1 <- clean_name(name1)
    
    # Initialize match variables
    match_found <- FALSE
    match_type <- ""
    match_quality <- ""
    matched_substance <- ""
    matched_abbreviation <- ""
    matched_group <- ""
    
    # 1. First try exact abbreviation match
    if (!is.na(abbrev1)) {
      for (j in 1:nrow(dataset2)) {
        if (abbrev1 == dataset2$Abbreviation[j]) {
          match_found <- TRUE
          match_type <- "Exact Abbreviation Match"
          match_quality <- "High"
          matched_substance <- dataset2$Substances[j]
          matched_abbreviation <- dataset2$Abbreviation[j]
          matched_group <- dataset2$Group[j]
          break
        }
      }
    }
    
    # 2. Try exact substance name match (clean names)
    if (!match_found) {
      for (j in 1:nrow(dataset2)) {
        clean_substance <- clean_name(dataset2$Substances[j])
        if (clean_name1 == clean_substance) {
          match_found <- TRUE
          match_type <- "Exact Substance Name Match"
          match_quality <- "High"
          matched_substance <- dataset2$Substances[j]
          matched_abbreviation <- dataset2$Abbreviation[j]
          matched_group <- dataset2$Group[j]
          break
        }
      }
    }
    
    # 3. Try translation match
    if (!match_found && clean_name1 %in% names(norwegian_english_dict)) {
      english_translation <- norwegian_english_dict[[clean_name1]]
      for (j in 1:nrow(dataset2)) {
        clean_substance <- clean_name(dataset2$Substances[j])
        if (english_translation == clean_substance) {
          match_found <- TRUE
          match_type <- "Translation Match (Norwegian-English)"
          match_quality <- "High"
          matched_substance <- dataset2$Substances[j]
          matched_abbreviation <- dataset2$Abbreviation[j]
          matched_group <- dataset2$Group[j]
          break
        }
      }
    }
    
    # 4. Try direct abbreviation match (name1 as abbreviation)
    if (!match_found) {
      for (j in 1:nrow(dataset2)) {
        # Remove spaces and special characters for comparison
        clean_abbrev2 <- gsub("[^A-Za-z0-9:]", "", dataset2$Abbreviation[j])
        clean_name1_abbrev <- gsub("[^A-Za-z0-9:]", "", name1)
        
        if (clean_name1_abbrev == clean_abbrev2) {
          match_found <- TRUE
          match_type <- "Direct Abbreviation Match"
          match_quality <- "High"
          matched_substance <- dataset2$Substances[j]
          matched_abbreviation <- dataset2$Abbreviation[j]
          matched_group <- dataset2$Group[j]
          break
        }
      }
    }
    
    # 5. Try fuzzy matching for similar substance names
    if (!match_found) {
      best_distance <- Inf
      best_match_idx <- NA
      
      for (j in 1:nrow(dataset2)) {
        clean_substance <- clean_name(dataset2$Substances[j])
        distance <- stringdist(tolower(clean_name1), tolower(clean_substance), method = "jw")
        
        if (distance < best_distance && distance < 0.3) {  # Threshold for fuzzy matching
          best_distance <- distance
          best_match_idx <- j
        }
      }
      
      if (!is.na(best_match_idx)) {
        match_found <- TRUE
        match_type <- "Fuzzy Substance Name Match"
        match_quality <- ifelse(best_distance < 0.1, "Medium", "Low")
        matched_substance <- dataset2$Substances[best_match_idx]
        matched_abbreviation <- dataset2$Abbreviation[best_match_idx]
        matched_group <- dataset2$Group[best_match_idx]
      }
    }
    
    # Add result to dataframe
    if (match_found) {
      results <- rbind(results, data.frame(
        Dataset1_NAME = name1,
        Dataset1_METHOD_ID = method_id,
        Dataset2_Substances = matched_substance,
        Dataset2_Abbreviation = matched_abbreviation,
        Dataset2_Group = matched_group,
        Match_Type = match_type,
        Match_Quality = match_quality,
        stringsAsFactors = FALSE
      ))
    } else {
      # Record unmatched items
      results <- rbind(results, data.frame(
        Dataset1_NAME = name1,
        Dataset1_METHOD_ID = method_id,
        Dataset2_Substances = "NO MATCH",
        Dataset2_Abbreviation = "NO MATCH",
        Dataset2_Group = "NO MATCH",
        Match_Type = "No Match Found",
        Match_Quality = "None",
        stringsAsFactors = FALSE
      ))
    }
  }
  
  return(results)
}

