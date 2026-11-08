# -----------------------------------------------------------------------------
# Seniority classification from job titles (pure functions, unit-tested).
#
# Levels, from most to least specific (the first matching level wins):
#   director  - Director, VP, Head of, Chief / C-suite, Partner, Dean
#   manager   - Manager, Supervisor, Associate/Assistant Director, AVP
#   lead      - Lead, Staff, Principal, Distinguished, level IV/V
#   senior    - Senior / Sr, level III, Associate Professor
#   entry     - Intern, Junior, Associate, Graduate, Postdoc, Resident, level I
#   mid       - level II, or no seniority keyword at all ("mid / unspecified")
#
# A second, independent seniority signal is DOL's prevailing-wage level
# (I-IV), see dol_wage_level().
# -----------------------------------------------------------------------------

SENIORITY_LEVELS <- data.table::data.table(
  level_id    = c("entry", "mid", "senior", "lead", "manager", "director"),
  level_name  = c("Entry / Junior", "Mid / Unspecified", "Senior",
                  "Lead / Staff / Principal", "Manager", "Director & Executive"),
  level_order = 1:6
)

DOL_LEVELS <- data.table::data.table(
  level_id    = c("dol_1", "dol_2", "dol_3", "dol_4"),
  level_name  = c("Level I (entry)", "Level II (qualified)",
                  "Level III (experienced)", "Level IV (fully competent)"),
  level_order = 1:4
)

# "X Manager" / "X Owner" titles that describe an individual-contributor role
# rather than people management. They are neutralised before level detection
# so that "Senior Product Manager" is Senior, not Manager.
.IC_MANAGER_ROLES <- paste(
  "PRODUCT MARKETING", "TECHNICAL PROGRAM", "TECHNICAL PRODUCT", "PRODUCT",
  "PROJECT", "PROGRAM", "PROGRAMME", "KEY ACCOUNT", "TECHNICAL ACCOUNT",
  "ACCOUNT", "BRAND", "CASE", "CATEGORY", "RELATIONSHIP", "PORTFOLIO",
  "PROPERTY", "RELEASE", "COMMUNITY", "CAMPAIGN", "CHANNEL", "PARTNER",
  "PARTNERSHIPS?", "VENDOR", "CONFIGURATION", "CHANGE", "CUSTOMER SUCCESS",
  "CLIENT SUCCESS", "GROWTH", "PRICING", "IMPLEMENTATION",
  sep = "|"
)

.RX <- list(
  ic_manager   = paste0("\\b(", .IC_MANAGER_ROLES, ") (MANAGER|MANAGERS|MANAGEMENT|OWNER)\\b"),
  group_role   = "\\bGROUP (PRODUCT|PROGRAM|PROJECT) ROLE\\b|\\bGROUP LEADER\\b",
  sub_director = "\\b(ASSOCIATE|ASSISTANT) (DIRECTOR|VICE PRESIDENT|VP)\\b|\\bAVP\\b",
  director     = paste0(
    "\\b(DIRECTOR|DIRECTORS|VICE PRESIDENT|VP|SVP|EVP|HEAD|CHIEF|PRESIDENT|DEAN|",
    "CHAIR|CHAIRMAN|CHAIRPERSON|FOUNDER|COFOUNDER|CEO|CTO|CFO|COO|CIO|CMO|CPO|",
    "CISO|CDO|CRO)\\b|\\bGENERAL MANAGER\\b|^PARTNER\\b|\\b(MANAGING|ASSOCIATE|SENIOR|EQUITY) PARTNER\\b"
  ),
  manager      = "\\b(MANAGER|MANAGERS|MANAGING|SUPERVISOR|SUPERINTENDENT|FOREMAN)\\b",
  mts_neutral  = "\\bMEMBER OF (THE )?TECHNICAL STAFF\\b",
  staff_entry  = paste0("\\bSTAFF (ACCOUNTANT|AUDITOR|CONSULTANT|ASSOCIATE|ASSISTANT|ANALYST|TAX|ACCOUNTING|AUDIT)\\b|",
                        "\\b(AUDIT|TAX|ASSURANCE|ADVISORY) STAFF\\b"),
  staff_neutral = "\\bSTAFF (NURSE|PHARMACIST|PHYSICIAN|ATTORNEY|THERAPIST|PSYCHIATRIST|PSYCHOLOGIST|ACCOUNTANTS)\\b",
  lead         = paste0(
    "\\b(LEAD|LEADER|PRINCIPAL|DISTINGUISHED|STAFF|EXPERT)\\b|",
    "\\b(TECHNICAL|SENIOR|DISTINGUISHED) FELLOW\\b|\\bCONSULTING MEMBER\\b|\\bSPECIALIST LEADER\\b"
  ),
  professor_full = "\\bPROFESSOR\\b",
  professor_sub  = "\\b(ASSISTANT|ASSOCIATE|ADJUNCT|VISITING) PROFESSOR\\b",
  professor_assoc = "\\bASSOCIATE PROFESSOR\\b",
  senior       = "\\b(SENIOR)\\b",
  entry        = paste0(
    "\\b(INTERN|INTERNS|INTERNSHIP|TRAINEE|APPRENTICE|JUNIOR|ENTRY|GRADUATE|",
    "NEW GRAD|EARLY CAREER|FRESHER|POSTDOC|POSTDOCTORAL|RESIDENT|FELLOW|ASSOCIATE)\\b|",
    "\\bPOST DOC(TORAL)?\\b|",
    "\\b(RESEARCH|TEACHING|GRADUATE|ENGINEERING|LABORATORY|LAB) ASSISTANT\\b|",
    "\\bASSISTANT (ENGINEER|SCIENTIST|ANALYST|ACCOUNTANT|ARCHITECT|DESIGNER|RESEARCHER)\\b"
  )
)

#' Extract a numeric job level (1-5) from roman numerals ("Engineer III"),
#' "L2" / "Level 2" tokens or a trailing digit ("Consultant 2"), in that order
#' of preference. Returns NA when no level token is present.
title_level_number <- function(t) {
  roman <- c(I = 1L, II = 2L, III = 3L, IV = 4L, V = 5L)
  r <- stringi::stri_extract_last_regex(t, "\\b(I|II|III|IV|V)\\b")
  out <- unname(roman[r])
  l <- stringi::stri_match_last_regex(t, "\\b(?:LEVEL |L)([1-5])\\b")[, 2]
  out[is.na(out)] <- as.integer(l[is.na(out)])
  d <- stringi::stri_match_last_regex(t, "\\b([1-5])$")[, 2]
  out[is.na(out)] <- as.integer(d[is.na(out)])
  out
}

#' Classify normalised job titles into seniority levels.
#'
#' @param title_norm Titles normalised with normalize_title().
#' @return Character vector of level ids (see SENIORITY_LEVELS).
classify_seniority <- function(title_norm) {
  t <- title_norm
  t[is.na(t)] <- ""
  # Neutralise individual-contributor "X Manager" role phrases
  t <- gsub(.RX$ic_manager, "\\1 ROLE", t, perl = TRUE)
  t <- gsub(.RX$mts_neutral, "MEMBER MTS", t, perl = TRUE)

  is_group   <- grepl(.RX$group_role, t, perl = TRUE)
  is_subdir  <- grepl(.RX$sub_director, t, perl = TRUE)
  t_dir      <- gsub(.RX$sub_director, " ", t, perl = TRUE)
  is_dir     <- grepl(.RX$director, t_dir, perl = TRUE)
  is_mgr     <- grepl(.RX$manager, t, perl = TRUE) | is_subdir | is_group
  is_staff_e <- grepl(.RX$staff_entry, t, perl = TRUE)
  t_lead     <- gsub(.RX$staff_entry, " ", gsub(.RX$staff_neutral, " ", t, perl = TRUE), perl = TRUE)
  is_lead    <- grepl(.RX$lead, t_lead, perl = TRUE)
  prof_full  <- grepl(.RX$professor_full, t, perl = TRUE) & !grepl(.RX$professor_sub, t, perl = TRUE)
  prof_assoc <- grepl(.RX$professor_assoc, t, perl = TRUE)
  is_senior  <- grepl(.RX$senior, t, perl = TRUE) | prof_assoc
  is_entry   <- (grepl(.RX$entry, t, perl = TRUE) & !prof_assoc &
                   !grepl("\\bASSISTANT PROFESSOR\\b", t, perl = TRUE)) | is_staff_e

  num <- title_level_number(t)

  out <- rep("mid", length(t))
  out[is_entry | (!is.na(num) & num == 1L)] <- "entry"
  out[is_senior | (!is.na(num) & num == 3L)] <- "senior"
  # Fellows qualified as senior/technical are leads, handled by the lead regex
  out[is_lead | prof_full | (!is.na(num) & num >= 4L)] <- "lead"
  out[is_mgr] <- "manager"
  out[is_dir] <- "director"
  # A bare numeral II with no other keyword stays "mid"
  out
}

#' Map DOL prevailing-wage levels ("I".."IV") to level ids.
dol_wage_level <- function(x) {
  x <- toupper(trimws(as.character(x)))
  out <- rep(NA_character_, length(x))
  out[x %in% c("I", "1", "LEVEL I")]    <- "dol_1"
  out[x %in% c("II", "2", "LEVEL II")]  <- "dol_2"
  out[x %in% c("III", "3", "LEVEL III")] <- "dol_3"
  out[x %in% c("IV", "4", "LEVEL IV")]  <- "dol_4"
  out
}
