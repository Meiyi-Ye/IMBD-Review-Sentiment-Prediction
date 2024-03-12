# Required libraries
library(lexicon)
library(tm)
library(tokenizers)
library(textstem)
library(tidytext)
library(dplyr)
library(stopwords)

# Load the original movie review data
reviews <- read.csv("IMDB Dataset.csv")

# Check if the data is properly read in
dim(reviews)
head(reviews)

# checking for NA values (Both FALSE indicating complete data)
anyNA(reviews$review)
anyNA(reviews$sentiment)

################################################
### Basic Cleaning of Punctuation and Format ###
################################################

clean_text <- function(x) {
  # transform all text to lowercase
  x <- tolower(x)
  
  # remove break indicators
  x <- gsub("<br /><br />", " ", x)
  
  # remove punctuation
  x <- gsub("[[:punct:]]", " ", x)
  
  # remove numbers
  x <- gsub("[[:digit:]]", " ", x)
  
  # remove extra spaces
  x <- gsub("\\s+"," ", x)
  
  # remove letters left from contractions
  x <- gsub("\\st\\s"," ", x)
  x <- gsub("\\sll\\s"," ", x)
  x <- gsub("\\ss\\s"," ", x)
  x <- gsub("\\sre\\s"," ", x)
  x <- gsub("\\sd\\s"," ", x)
  x <- gsub("\\sve\\s"," ", x)
  
  # remove stop words
  stopwords <- stopwords::stopwords(source = "stopwords-iso")
  # x <- trimws(gsub(paste0("\\b", stopwords, "\\b", collapse = "|"), "", x))
  x <- unlist(strsplit(x, " "))
  x <- x[!x %in% stopwords]
  x <- paste(x, collapse = " ")
  return(x)
}

# Adding new column representing the clean text 
clean_review <- unlist(lapply(reviews$review, clean_text))

############################
### Lemmatizing the Text ###
############################

clean_review <- lemmatize_strings(clean_review)

#####################################
### Mapping Adverbs -> Adjectives ###
#####################################

# Function to replace adverbs with their corresponding adjectives
replace_adverb <- function(word) {
  # Mapping of adverb to adjective
  adverb_adjective_list <- c("academically" = "academic", "accurately" = "accurate", "actively" = "active",
                             "adventurously" = "adventurous", "aggressively" = "aggressive", "alertly" = "alert",
                             "ambitiously" = "ambitious", "analytically" = "analytical", "artistically" = "artistic",
                             "assertively" = "assertive", "attractively" = "attractive", "boldly" = "bold",
                             "businesslike" = "businesslike", "calmly" = "calm", "capably" = "capable",
                             "carefully" = "careful", "cautiously" = "cautious", "cheerfully" = "cheerful",
                             "cleverly" = "clever", "competently" = "competent", "competitively" = "competitive",
                             "confidently" = "confident", "conscientiously" = "conscientious", "basically" = "basic", 
                             "conservatively" = "conservative", "considerately" = "considerate",
                             "consistently" = "consistent", "cooperatively" = "cooperative",
                             "courageously" = "courageous", "creatively" = "creative", "curiously" = "curious",
                             "deliberately" = "deliberate", "determinedly" = "determined", "dignifiedly" = "dignified",
                             "discreetly" = "discreet", "dominantly" = "dominant", "eagerly" = "eager",
                             "efficiently" = "efficient", "energetically" = "energetic", "enormously" = "enormous",
                             "fair-mindedly" = "fair-minded", "farsightedly" = "farsighted", "firmly" = "firm",
                             "flexibly" = "flexible", "forcefully" = "forceful", "formally" = "formal",
                             "frankly" = "frank", "generously" = "generous", "happily" = "happy",
                             "healthily" = "healthy", "helpfully" = "helpful", "highly" = "high",
                             "honestly" = "honest", "hugely" = "huge", "humorously" = "humorous",
                             "imaginatively" = "imaginative", "imposingly" = "imposing", "independently" = "independent",
                             "individualistically" = "individualistic", "industriously" = "industrious",
                             "informally" = "informal", "intellectually" = "intellectual",
                             "intelligently" = "intelligent", "inventively" = "inventive",
                             "kindly" = "kind", "largely" = "large", "leisurely" = "leisurely",
                             "liberally" = "liberal", "likably" = "likable", "logically" = "logical",
                             "loyally" = "loyal", "mammothly" = "mammoth", "maturely" = "mature",
                             "methodically" = "methodical", "meticulously" = "meticulous", "mildly" = "mild",
                             "moderately" = "moderate", "modestly" = "modest", "motivated" = "motivated",
                             "naturally" = "natural", "obligingly" = "obliging", "optimistically" = "optimistic",
                             "organized" = "organized", "originally" = "original", "outgoingly" = "outgoing",
                             "painstakingly" = "painstaking", "patiently" = "patient", "perseveringly" = "persevering",
                             "persistently" = "persistent", "pleasantly" = "pleasant", "poisedly" = "poised",
                             "politely" = "polite", "practically" = "practical", "precisely" = "precise",
                             "progressively" = "progressive", "prudently" = "prudent", "purposefully" = "purposeful",
                             "quickly" = "quick", "quietly" = "quiet", "rationally" = "rational",
                             "realistically" = "realistic", "reflectively" = "reflective", "relaxedly" = "relaxed",
                             "reliably" = "reliable", "reservedly" = "reserved", "resourcefully" = "resourceful",
                             "responsibly" = "responsible", "robustly" = "robust", "sensibly" = "sensible",
                             "sensitively" = "sensitive", "seriously" = "serious", "significantly" = "significant",
                             "sincerely" = "sincere", "sociably" = "sociable", "spontaneously" = "spontaneous",
                             "stably" = "stable", "strategically" = "strategic", "steadily" = "steady",
                             "strongly" = "strong", "mindedly" = "minded", "successfully" = "successful",
                             "supportively" = "supportive", "tactfully" = "tactful", "teachably" = "teachable",
                             "tenaciously" = "tenacious", "thoroughly" = "thorough", "thoughtfully" = "thoughtful",
                             "brutality" = "brutal", "publicly" = "public", "thankfully" = "thankful", 
                             "tightly" = "tight", "timidly" = "timid", "tiredly" = "tired", "triumphantly" = "triumphant",
                             "truly" = "true", "unexpectedly" = "unexpected", "usually" = "usual", "vastly" = "vast",
                             "violently" = "violent", "warmly" = "warm", "wearily" = "weary", "wildly" = "wild", "wisely" = "wise",
                             "wonderfully" = "wonderful", "yearly" = "yearly", "youthfully" = "youthful",
                             "aggressively" = "aggressive", "alertly" = "alert", "ambitiously" = "ambitious",
                             "analytically" = "analytical", "artistically" = "artistic", "assertively" = "assertive",
                             "attractively" = "attractive", "boldly" = "bold", "businesslike" = "businesslike",
                             "cautiously" = "cautious", "cleverly" = "clever", "competitively" = "competitive",
                             "conscientiously" = "conscientious", "conservatively" = "conservative",
                             "considerately" = "considerate", "consistently" = "consistent", "cooperatively" = "cooperative",
                             "creatively" = "creative", "curiously" = "curious", "determinedly" = "determined",
                             "dignifiedly" = "dignified", "discreetly" = "discreet", "dominantly" = "dominant",
                             "efficiently" = "efficient", "energetically" = "energetic", "enormously" = "enormous",
                             "fair-mindedly" = "fair-minded", "farsightedly" = "farsighted", "firmly" = "firm",
                             "flexibly" = "flexible", "forcefully" = "forceful", "formally" = "formal",
                             "frankly" = "frank", "generously" = "generous", "healthily" = "healthy",
                             "helpfully" = "helpful", "highly" = "high", "hugely" = "huge",
                             "humorously" = "humorous", "imaginatively" = "imaginative", "imposingly" = "imposing",
                             "independently" = "independent", "individualistically" = "individualistic",
                             "industriously" = "industrious", "informally" = "informal", "intellectually" = "intellectual",
                             "intelligently" = "intelligent", "inventively" = "inventive", "largely" = "large",
                             "leisurely" = "leisurely", "liberally" = "liberal", "likably" = "likable",
                             "logically" = "logical", "loyally" = "loyal", "mammothly" = "mammoth",
                             "maturely" = "mature", "methodically" = "methodical", "meticulously" = "meticulous",
                             "mildly" = "mild", "moderately" = "moderate", "modestly" = "modest",
                             "motivated" = "motivated", "obligingly" = "obliging", "optimistically" = "optimistic",
                             "organized" = "organized", "originally" = "original", "outgoingly" = "outgoing",
                             "painstakingly" = "painstaking", "patiently" = "patient", "perseveringly" = "persevering",
                             "persistently" = "persistent", "pleasantly" = "pleasant", "poisedly" = "poised",
                             "practically" = "practical", "precisely" = "precise", "progressively" = "progressive",
                             "prudently" = "prudent", "purposefully" = "purposeful", "rationally" = "rational",
                             "realistically" = "realistic", "reflectively" = "reflective", "relaxedly" = "relaxed",
                             "reliably" = "reliable", "reservedly" = "reserved", "resourcefully" = "resourceful",
                             "responsibly" = "responsible", "robustly" = "robust", "sensibly" = "sensible",
                             "sensitively" = "sensitive", "seriously" = "serious",
                             "significantly" = "significant", "sociably" = "sociable", "spontaneously" = "spontaneous",  
                             "stably" = "stable", "strategically" = "strategic", "steadily" = "steady",
                             "mindedly" = "minded", "successfully" = "successful", "supportively" = "supportive",
                             "tactfully" = "tactful", "teachably" = "teachable", "tenaciously" = "tenacious",
                             "brutality" = "brutal", "publicly" = "public", "thoroughly" = "thorough",
                             "thoughtfully" = "thoughtful", "rationally" = "rational", "reflectively" = "reflective", 
                             "suddenly" = "sudden")
  
  adjective <- adverb_adjective_list[word]
  if (!is.na(adjective)) {
    return(adjective)
  } else {
    return(word)  # Return the original word if not found in the list
  }
}

# Function to turn adverbs into adjectives
replace_adverbs_with_adjectives <- function(text_vector) {
  # Tokenize each string, replace adverbs, and concatenate back into sentences
  processed_sentences <- lapply(strsplit(text_vector, " "), function(sentence) {
    replaced_words <- sapply(sentence, replace_adverb)
    return(paste(replaced_words, collapse = " "))
  })
  
  return(unlist(processed_sentences))
}

processed_text <- replace_adverbs_with_adjectives(reviews$clean_review)
reviews$clean_review <- processed_text


# ** (below not used in actual data cleaning, but apart of 'limitations'
# /'improvements' section of the report)
##############################################################################
### More Detailed Adverb -> Adjective Mapping Using Part of Speech Tagging ###

# # Use the 'udpipe' package for part-of-speech tagging
# library(udpipe)
# library(here)

# # Download and load the pre-trained English model
# my_ud_model <- udpipe_download_model(language = "english-ewt")
# my_ud_model <- udpipe_load_model(file = "english-ewt-ud-2.5-191206.udpipe")

# # Function to identify adverbs in a string
# identify_adverbs <- function(text, ud_model) {
#   # Tokenize and annotate the text
#   udpipe_annotation <- udpipe::udpipe_annotate(ud_model, x = text) %>%
#     as.data.frame() %>%
#     dplyr::select(-sentence)
#   
#   # Extract adverbs based on the Universal POS tag (upos or xpos)
#   adverbs <- udpipe_annotation$lemma[(udpipe_annotation$upos == "ADV" | udpipe_annotation$xpos == "ADV")]
#   
#   # Return the adverbs
#   return(adverbs)
# }

# # Function to identify unique adverbs in a vector of strings
# identify_unique_adverbs <- function(text_vector) {
#   # Initialize an empty vector to store adverbs
#   all_adverbs <- character(0)
#   
#   # Iterate through each string in the vector
#   for (text in text_vector) {
#     # Identify adverbs in the current string
#     adverbs_in_text <- identify_adverbs(text, my_ud_model)
#     
#     # Add unique adverbs to the vector
#     all_adverbs <- union(all_adverbs, adverbs_in_text)
#   }
#   
#   # Return the vector of unique adverbs
#   return(all_adverbs)
# }

# # Example usage
# text_vector1 <- reviews$clean_review[1:10000]
# unique_adverbs <- identify_unique_adverbs(text_vector1)
# # Print the result
# cat("Unique adverbs:", unique_adverbs, "\n")

# In this case, we will be able to identify all adverbs in our data 
# and do a more extensive adverb to adjective mapping
