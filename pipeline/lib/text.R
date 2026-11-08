# -----------------------------------------------------------------------------
# Text normalisation shared by the role, seniority and employer classifiers.
# -----------------------------------------------------------------------------

#' Normalise a job title for rule matching.
#'
#' Upper-cases, transliterates accents, removes internal job codes such as
#' "JC50" or "KBGFJG222324-13", expands common abbreviations (SR -> SENIOR,
#' MGR -> MANAGER, ...) and collapses punctuation to single spaces. Characters
#' that carry meaning in titles (& + # /) are kept.
normalize_title <- function(x) {
  t <- toupper(as.character(x))
  t <- iconv(t, from = "UTF-8", to = "ASCII//TRANSLIT", sub = " ")
  t[is.na(t)] <- ""
  # Internal requisition / job codes: >= 2 letters followed by >= 2 digits
  t <- gsub("\\b[A-Z]{2,}[0-9]{2,}[A-Z0-9]*(-[0-9]+)?\\b", " ", t, perl = TRUE)
  t <- gsub("\\.NET\\b", " DOTNET ", t, perl = TRUE)
  t <- gsub("\\bI\\.T\\.?", " IT ", t, perl = TRUE)
  t <- gsub("\\bV\\.P\\.?", " VP ", t, perl = TRUE)
  t <- gsub("C\\+\\+", " CPLUSPLUS ", t, perl = TRUE)
  t <- gsub("C#", " CSHARP ", t, fixed = TRUE)
  t <- gsub("[^A-Z0-9&+#/ ]", " ", t, perl = TRUE)
  t <- gsub("\\s*/\\s*", " / ", t, perl = TRUE)
  t <- gsub("\\s*&\\s*", " & ", t, perl = TRUE)
  abbrev <- c(
    "SR" = "SENIOR", "SNR" = "SENIOR", "JR" = "JUNIOR", "MGR" = "MANAGER",
    "MGMT" = "MANAGEMENT", "ENGR" = "ENGINEER", "ASSOC" = "ASSOCIATE",
    "DIR" = "DIRECTOR", "ASST" = "ASSISTANT", "SVCS" = "SERVICES",
    "TECHNOLOGIST" = "TECHNOLOGIST"
  )
  for (k in names(abbrev)) {
    t <- gsub(paste0("\\b", k, "\\b"), abbrev[[k]], t, perl = TRUE)
  }
  t <- gsub("\\s+", " ", t, perl = TRUE)
  trimws(t)
}

#' Title-case a normalised (upper-case) title for display.
display_title <- function(x) {
  x <- tolower(x)
  out <- gsub("(^|\\s)([a-z])", "\\1\\U\\2", x, perl = TRUE)
  # Keep common acronyms upper-case
  acr <- c("IT", "QA", "UX", "UI", "AI", "ML", "BI", "HR", "VP", "SVP", "EVP",
           "AVP", "SAP", "ERP", "CRM", "ETL", "SRE", "SDE", "SDET", "DBA",
           "CEO", "CTO", "CFO", "COO", "CIO", "II", "III", "IV", "NLP", "AWS",
           "GCP", "US", "USA", "R&D", "FP&A", "M&A", "RN", "MD", "PM")
  for (a in acr) {
    out <- gsub(paste0("\\b", tools::toTitleCase(tolower(a)), "\\b"), a, out, perl = TRUE)
  }
  gsub("\\bIos\\b", "iOS", gsub("\\bDotnet\\b", ".NET", gsub("\\bCplusplus\\b", "C++",
       gsub("\\bCsharp\\b", "C#", out, perl = TRUE), perl = TRUE), perl = TRUE), perl = TRUE)
}

#' Normalise an O*NET-SOC / SOC code to "NN-NNNN.NN" (".00" when absent).
normalize_soc <- function(x) {
  x <- toupper(trimws(as.character(x)))
  m <- regmatches(x, regexec("([0-9]{2})-?([0-9]{4})(\\.([0-9]{1,2}))?", x))
  vapply(m, function(p) {
    if (length(p) < 3 || is.na(p[1]) || !nzchar(p[1])) return(NA_character_)
    suffix <- if (nzchar(p[5])) sprintf("%02d", as.integer(p[5])) else "00"
    paste0(p[2], "-", p[3], ".", suffix)
  }, character(1))
}
