# -----------------------------------------------------------------------------
# Role taxonomy: 14 role families -> 52 roles.
#
# Classification is two-stage:
#   1. TITLE_RULES  - ordered regular expressions on the normalised job title,
#                     each restricted to compatible SOC codes. These separate
#                     roles that share a SOC code (e.g. Data Engineer vs
#                     Software Engineer, both usually 15-1252).
#   2. SOC_RULES    - ordered SOC-code patterns (2018 and 2010 SOC, incl. O*NET
#                     8-digit detail codes) used when no title rule applies.
# Anything left is "other".
# -----------------------------------------------------------------------------

FAMILIES <- data.table::fread(text = "
family_id|family_name|family_order
software|Software Engineering|1
data_ai|Data & AI|2
it|IT, Cloud & Security|3
product|Product & Program Management|4
engineering|Hardware & Physical Engineering|5
finance|Finance & Accounting|6
business|Business, Consulting & Operations|7
marketing_sales|Marketing & Sales|8
design|Design & Architecture|9
healthcare|Healthcare|10
science|Science & Research|11
education|Education|12
legal_hr|Legal & HR|13
other|Other occupations|14
", sep = "|")

ROLES <- data.table::fread(text = "
role_id|role_name|family_id
swe|Software Engineer|software
frontend|Frontend / Web Developer|software
mobile|Mobile Developer|software
devops|DevOps / SRE / Cloud Engineer|software
qa|QA / Test Engineer|software
embedded|Embedded / Firmware Engineer|software
data_scientist|Data Scientist|data_ai
ml_engineer|ML / AI Engineer|data_ai
data_engineer|Data Engineer|data_ai
data_analyst|Data / BI Analyst|data_ai
research_scientist|Research Scientist (Computing)|data_ai
quant_stats|Statistician / Operations Research|data_ai
it_architect|Solutions / Systems Architect|it
systems_analyst|Systems Analyst / IT & ERP Consultant|it
security|Cybersecurity|it
network_sysadmin|Network, Systems Admin & IT Support|it
dba|Database Administrator|it
it_manager|IT & Technology Manager|it
it_other|Other IT & Computing|it
product_manager|Product Manager|product
program_manager|Project / Program Manager|product
electrical|Electrical / Hardware Engineer|engineering
mechanical|Mechanical / Aerospace / Robotics Engineer|engineering
industrial_mfg|Industrial / Manufacturing Engineer|engineering
civil|Civil / Environmental Engineer|engineering
other_engineering|Chemical, Materials & Other Engineering|engineering
eng_manager|Engineering Manager|engineering
financial_analyst|Financial / Investment Analyst|finance
accountant|Accountant / Auditor|finance
quant|Quant / Risk / Actuary|finance
consultant|Management Consultant|business
business_analyst|Business Analyst|business
operations|Operations / Supply Chain|business
general_mgmt|General & Executive Management|business
marketing|Marketing & Communications|marketing_sales
sales|Sales & Business Development|marketing_sales
ux_designer|UX / Product Designer|design
designer|Graphic / Industrial Designer|design
architect_building|Architect (Buildings)|design
physician|Physician / Surgeon / Dentist|healthcare
nurse|Nurse / Nurse Practitioner|healthcare
allied_health|Therapist, Pharmacist & Allied Health|healthcare
health_admin|Healthcare Manager|healthcare
counselor_social|Counselor / Social Worker|healthcare
life_scientist|Life & Medical Scientist|science
physical_scientist|Chemist, Physicist & Physical Scientist|science
social_scientist|Economist & Social Scientist|science
professor|Professor / Postsecondary Teacher|education
teacher|K-12 Teacher & Education Staff|education
legal|Lawyer / Legal|legal_hr
hr|HR / Recruiting|legal_hr
other|Other occupations|other
", sep = "|")
ROLES[, role_order := seq_len(.N)]

# Indeed Hiring Lab occupational sectors that correspond to each family; used
# to put DOL salary trends next to market-wide postings and wage growth.
FAMILY_INDEED_SECTORS <- list(
  software        = "Software Development",
  data_ai         = "Data & Analytics",
  it              = c("IT Infrastructure, Operations & Support", "IT Systems & Solutions"),
  product         = "Project Management",
  engineering     = c("Electrical Engineering", "Mechanical Engineering", "Civil Engineering", "Industrial Engineering"),
  finance         = c("Accounting", "Banking & Finance", "Insurance"),
  business        = c("Management", "Logistic Support"),
  marketing_sales = c("Marketing", "Sales", "Media & Communications"),
  design          = c("Arts & Entertainment", "Architecture"),
  healthcare      = c("Nursing", "Physicians & Surgeons", "Therapy", "Pharmacy", "Medical Technician", "Dental", "Medical Information"),
  science         = c("Scientific Research & Development", "Social Science"),
  education       = "Education & Instruction",
  legal_hr        = c("Legal", "Human Resources")
)

# SOC prefixes used to constrain title rules
SOC_ANY_PROF <- "^(11|13|15|17|19|23|25|27|29)-"
SOC_TECH     <- "^(15-|11-3021)"
SOC_COMPUTER <- "^15-"

# Ordered title rules: first match wins.
TITLE_RULES <- list(
  list(role = "product_manager",
       pattern = "\\bPRODUCT (MANAGER|MANAGEMENT|MANAGERS|OWNER|LEAD|LEADER|STRATEGY|STRATEGIST)\\b|\\bHEAD OF PRODUCT\\b|\\b(VP|VICE PRESIDENT|DIRECTOR)( OF)? PRODUCT\\b(?! (MARKETING|DESIGN|ENGINEERING|DEVELOPMENT|SAFETY|QUALITY))|\\bCHIEF PRODUCT OFFICER\\b",
       soc = "^(11|13|15|17)-"),
  list(role = "program_manager",
       pattern = "\\b(PROGRAM|PROGRAMME|PROJECT) (MANAGER|MANAGERS|MANAGEMENT|LEAD|LEADER|COORDINATOR|DIRECTOR|SPECIALIST|OFFICER|CONTROLLER)\\b|\\bSCRUM MASTER\\b|\\bDELIVERY (MANAGER|LEAD)\\b|\\bTPM\\b|\\bPMO\\b|\\bAGILE COACH\\b|\\bRELEASE TRAIN ENGINEER\\b",
       soc = "^(11-1|11-2|11-3|11-9041|11-9199|13-|15-|17-)"),
  list(role = "ux_designer",
       pattern = "\\bUX\\b|\\bUI / UX\\b|USER EXPERIENCE|INTERACTION DESIGN|\\bUI DESIGN|USER INTERFACE DESIGN|\\bUSER RESEARCH|EXPERIENCE DESIGN|SERVICE DESIGNER|CONTENT DESIGNER|VISUAL DESIGNER",
       soc = "^(11|13|15|17-2112|19-3|27)"),
  list(role = "ux_designer",
       pattern = "\\bPRODUCT DESIGN(ER|ERS)?\\b",
       soc = "^(11|13|15|27-1024|27-1029)"),
  list(role = "data_engineer",
       pattern = "\\bDATA (ENGINEER|ENGINEERS|ENGINEERING|ARCHITECT|ARCHITECTURE|PLATFORM|PIPELINE|INTEGRATION|WAREHOUSE|WAREHOUSING|INFRASTRUCTURE|MODELER|MODELLER|OPS)\\b|\\bETL\\b|\\bELT\\b|\\bBIG DATA\\b|\\bANALYTICS ENGINEER|\\bDATABRICKS\\b|\\bSNOWFLAKE\\b|\\bHADOOP\\b|\\bSPARK\\b|\\bINFORMATICA\\b|\\bDATA ?STAGE\\b|\\bTALEND\\b|\\bAB INITIO\\b",
       soc = SOC_TECH),
  list(role = "data_scientist",
       pattern = "\\bDATA SCIEN|\\bDATA SCI\\b|\\bDECISION SCIENTIST\\b",
       soc = "^(11|13|15|19)-"),
  list(role = "ml_engineer",
       pattern = "MACHINE LEARNING|DEEP LEARNING|\\bML\\b|\\bMLE\\b|\\bMLOPS\\b|\\bAI\\b|\\bA I\\b|ARTIFICIAL INTELLIGENCE|COMPUTER VISION|\\bNLP\\b|NATURAL LANGUAGE|\\bLLMS?\\b|GENERATIVE|\\bGEN ?AI\\b|APPLIED SCIENTIST|\\bAPPLIED SCIENCES?\\b|REINFORCEMENT LEARNING|SPEECH RECOGNITION|\\bPERCEPTION\\b",
       soc = "^(15-|11-3021|17-2061)"),
  list(role = "data_analyst",
       pattern = "\\bDATA (ANALYST|ANALYSTS|ANALYSIS|ANALYTICS|SPECIALIST|VISUALIZATION|VISUALISATION)\\b|BUSINESS INTELLIGENCE|\\bBI (ANALYST|ENGINEER|DEVELOPER|CONSULTANT|ARCHITECT|LEAD|MANAGER|SPECIALIST)\\b|\\bANALYTICS\\b|\\bREPORTING ANALYST|\\bINSIGHTS? ANALYST|\\bPOWER BI\\b|\\bTABLEAU\\b|\\bMIS ANALYST",
       soc = "^(11|13|15)-"),
  list(role = "devops",
       pattern = "\\bDEV ?OPS\\b|\\bDEVSECOPS\\b|SITE RELIABILITY|\\bSRE\\b|\\bCLOUD (ENGINEER|ENGINEERS|DEVELOPER|OPERATIONS|INFRASTRUCTURE|ADMINISTRATOR|SPECIALIST|SUPPORT|PLATFORM)\\b|\\bPLATFORM ENGINEER|\\bINFRASTRUCTURE ENGINEER|\\bPRODUCTION ENGINEER|\\bRELIABILITY ENGINEER\\b|\\bKUBERNETES\\b|\\b(AWS|AZURE|GCP) (ENGINEER|DEVELOPER|CONSULTANT|ADMINISTRATOR)\\b|\\bBUILD (AND|&) RELEASE\\b|\\bRELEASE ENGINEER|\\bBUILD ENGINEER|\\bCI / CD\\b",
       soc = SOC_TECH),
  list(role = "security",
       pattern = "\\bSECURITY\\b|\\bCYBER|PENETRATION|\\bPEN ?TEST|\\bINFOSEC\\b|\\bSOC ANALYST\\b|VULNERABILITY|IDENTITY (AND|&) ACCESS|\\bIAM\\b|\\bGRC\\b|\\bTHREAT\\b|\\bFORENSIC|\\bCISO\\b",
       soc = "^(15-|11-3021|13-1111|13-1199)"),
  list(role = "qa",
       pattern = "\\bQA\\b|\\bQ A\\b|QUALITY ASSURANCE|\\bQUALITY ENGINEER|\\bTEST\\b|\\bTESTER\\b|\\bTESTERS\\b|\\bTESTING\\b|\\bSDET\\b|\\bIN TEST\\b|\\bQE\\b|\\bAUTOMATION (ENGINEER|TESTER|ANALYST|LEAD|ARCHITECT)\\b|\\bVALIDATION ENGINEER",
       soc = SOC_COMPUTER),
  list(role = "mobile",
       pattern = "\\bIOS\\b|\\bANDROID\\b|\\bMOBILE (DEVELOPER|ENGINEER|APPLICATION|APPLICATIONS|APP|APPS|SOFTWARE|DEVELOPMENT)\\b|\\bSWIFT\\b|\\bKOTLIN\\b|\\bREACT NATIVE\\b|\\bFLUTTER\\b|\\bXAMARIN\\b",
       soc = SOC_COMPUTER),
  list(role = "frontend",
       pattern = "\\bFRONT ?END\\b|\\bUI (DEVELOPER|ENGINEER|ARCHITECT|LEAD)\\b|\\bWEB (DEVELOPER|DEVELOPERS|ENGINEER|PROGRAMMER|APPLICATION DEVELOPER|DEVELOPMENT)\\b|\\bREACT(JS| JS)?\\b|\\bANGULAR(JS)?\\b|\\bVUE(JS)?\\b|JAVASCRIPT|TYPESCRIPT",
       soc = SOC_COMPUTER),
  list(role = "embedded",
       pattern = "EMBEDDED|FIRMWARE|\\bRTOS\\b|DEVICE DRIVER|\\bBSP\\b|\\bKERNEL\\b",
       soc = "^(15-|17-2061|17-2071|17-2072)"),
  list(role = "dba",
       pattern = "DATABASE (ADMINISTRATOR|ADMINISTRATORS|ADMIN|ADMINISTRATION)|\\bDBA\\b",
       soc = SOC_COMPUTER),
  list(role = "network_sysadmin",
       pattern = "\\bNETWORK (ENGINEER|ENGINEERS|ADMINISTRATOR|ADMIN|ANALYST|SPECIALIST|TECHNICIAN|SUPPORT|OPERATIONS)\\b|\\bSYSTEMS? (ADMINISTRATOR|ADMINISTRATORS|ADMIN|ADMINISTRATION)\\b|\\bSYSADMIN\\b|\\bHELP ?DESK\\b|\\bSERVICE DESK\\b|\\bDESKTOP SUPPORT\\b|\\bIT SUPPORT\\b|\\bTECHNICAL SUPPORT\\b|\\bSUPPORT (ENGINEER|SPECIALIST|ANALYST|TECHNICIAN)\\b|\\bNOC\\b|\\bINFRASTRUCTURE (ANALYST|SPECIALIST|ADMINISTRATOR|CONSULTANT)\\b|\\b(WINDOWS|LINUX|UNIX) (ADMINISTRATOR|ADMIN|ENGINEER)\\b|\\bVMWARE\\b|\\bCITRIX\\b",
       soc = SOC_COMPUTER),
  list(role = "it_architect",
       pattern = "\\bARCHITECT\\b|\\bARCHITECTS\\b",
       soc = SOC_TECH),
  list(role = "business_analyst",
       pattern = "\\bBUSINESS (ANALYST|ANALYSTS|ANALYSIS)\\b",
       soc = "^(11|13|15)-"),
  list(role = "systems_analyst",
       pattern = "\\bSYSTEMS? ANALYST|\\bPROGRAMMER ANALYST|\\bTECHNICAL ANALYST|\\bFUNCTIONAL (CONSULTANT|ANALYST|LEAD)\\b|\\bTECHNO ?FUNCTIONAL\\b|\\bSAP\\b|\\bERP\\b|\\bPACKAGED? (CONSULTANT|APP)|\\bORACLE (CONSULTANT|FUNCTIONAL|APPS|EBS|FUSION|CLOUD)\\b|\\bSALESFORCE\\b|\\bWORKDAY\\b|\\bSERVICENOW\\b|\\bPEOPLESOFT\\b|\\bSIEBEL\\b|\\bPEGA\\b|\\bGUIDEWIRE\\b|\\bMAINFRAME\\b|\\bCRM\\b|\\bEDI\\b|\\bAPPLICATION (ANALYST|SUPPORT|CONSULTANT)\\b|\\b(IT|TECHNOLOGY|SYSTEMS?|SOLUTIONS?|IMPLEMENTATION|TECHNICAL) CONSULTANT\\b",
       soc = SOC_COMPUTER),
  list(role = "swe",
       pattern = "^ENGINEERING MANAGER\\b|\\b(MANAGER|DIRECTOR|VP|VICE PRESIDENT|HEAD) (OF )?ENGINEERING\\b|\\bENGINEERING (MANAGER|DIRECTOR|LEAD)\\b",
       soc = SOC_TECH),
  list(role = "swe",
       pattern = "SOFTWARE|DEVELOPER|PROGRAMMER|PROGRAMMING|\\bSDE\\b|\\bSWE\\b|FULL ?STACK|BACK ?END|\\bAPPLICATION (ENGINEER|DEVELOPER|DEVELOPMENT)\\b|\\bJAVA\\b|PYTHON|\\bDOTNET\\b|CPLUSPLUS|CSHARP|\\bGOLANG\\b|\\bRUBY\\b|\\bSCALA\\b|\\bNODE ?JS\\b|\\bPHP\\b|\\bBLOCKCHAIN\\b|\\bGAME (DEVELOPER|PROGRAMMER|ENGINEER)\\b",
       soc = "^(15-|11-3021|11-9041|17-2061|17-2071|17-2072)"),
  list(role = "quant",
       pattern = "\\bQUANT\\b|\\bQUANTS\\b|\\bQUANTITATIVE (ANALYST|RESEARCHER|RESEARCH|DEVELOPER|STRATEGIST|TRADER|MODELER|ASSOCIATE|ANALYTICS)\\b|\\bRISK (ANALYST|MODELER|MODELLER|MODELING|MANAGER|ASSOCIATE|SPECIALIST|OFFICER)\\b|\\bMODEL (RISK|VALIDATION)\\b|\\bACTUAR|\\bTRADER\\b|\\bTRADING\\b|\\bSTRUCTURER\\b",
       soc = "^(13-2|15-2|11-3031|41-3031)"),
  list(role = "accountant",
       pattern = "ACCOUNTANT|ACCOUNTING|AUDITOR|\\bAUDIT\\b|\\bTAX\\b|\\bCPA\\b|CONTROLLER|BOOKKEEP|PAYROLL|ACCOUNTS (PAYABLE|RECEIVABLE)",
       soc = "^(13-1|13-2|11-3031|43-3)"),
  list(role = "financial_analyst",
       pattern = "\\bFINANCIAL (ANALYST|ANALYSIS|PLANNING)\\b|\\bFP & A\\b|\\bINVESTMENT (ANALYST|BANKING|ASSOCIATE|BANKER|MANAGER)\\b|\\bEQUITY RESEARCH\\b|\\bCREDIT ANALYST\\b|\\bPORTFOLIO (ANALYST|MANAGER)\\b|\\bVALUATION\\b|\\bTREASURY\\b|\\bCORPORATE FINANCE\\b|\\bPRIVATE EQUITY\\b|\\bASSET MANAGEMENT\\b|\\bWEALTH MANAGEMENT\\b|\\bUNDERWRIT",
       soc = "^(11|13|41-3031)"),
  list(role = "marketing",
       pattern = "MARKETING|MARKET RESEARCH|\\bBRAND\\b|\\bSEO\\b|\\bSEM\\b|\\bGROWTH (MANAGER|MARKETER|LEAD|ANALYST)\\b|\\bCOMMUNICATIONS? (SPECIALIST|MANAGER|COORDINATOR|DIRECTOR|ASSOCIATE|LEAD)\\b|PUBLIC RELATIONS|SOCIAL MEDIA|\\bCONTENT (STRATEGIST|MARKETING|WRITER|CREATOR|MANAGER|SPECIALIST)\\b|COPYWRITER|ADVERTISING|\\bMEDIA (PLANNER|BUYER|SPECIALIST)\\b|DEMAND GENERATION",
       soc = "^(11-2|13-1|27-3|41-)"),
  list(role = "sales",
       pattern = "\\bSALES\\b|ACCOUNT EXECUTIVE|ACCOUNT MANAGER|BUSINESS DEVELOPMENT|CUSTOMER SUCCESS|CLIENT SUCCESS|\\bPRE ?SALES\\b|\\bSOLUTIONS? ENGINEER\\b|\\bPARTNERSHIPS? (MANAGER|LEAD|DIRECTOR)\\b|\\bKEY ACCOUNT\\b|\\bCHANNEL (MANAGER|SALES)\\b",
       soc = "^(11-1|11-2|13-1|41-)"),
  list(role = "hr",
       pattern = "HUMAN RESOURCES|\\bHR\\b|\\bHRBP\\b|RECRUIT|\\bTALENT (ACQUISITION|PARTNER|MANAGEMENT|DEVELOPMENT)\\b|\\bPEOPLE (PARTNER|OPERATIONS|BUSINESS PARTNER)\\b|COMPENSATION|\\bBENEFITS\\b|LEARNING (AND|&) DEVELOPMENT|\\bTRAINING (SPECIALIST|MANAGER|COORDINATOR)\\b",
       soc = "^(11-3|13-1|43-4161)"),
  list(role = "operations",
       pattern = "SUPPLY CHAIN|LOGISTIC|PROCUREMENT|PURCHASING|SOURCING|\\bBUYER\\b|INVENTORY|DEMAND PLANN|SUPPLY PLANN|\\bMATERIALS? (PLANNER|MANAGER)\\b|\\bOPERATIONS (ANALYST|MANAGER|SPECIALIST|ASSOCIATE|COORDINATOR|LEAD|DIRECTOR)\\b|\\bBUSINESS OPERATIONS\\b|FULFILLMENT|PROCESS IMPROVEMENT|CONTINUOUS IMPROVEMENT|SIX SIGMA",
       soc = "^(11-1|11-3|11-9199|13-1)"),
  list(role = "consultant",
       pattern = "MANAGEMENT CONSULT|\\bSTRATEGY (CONSULTANT|ANALYST|MANAGER|ASSOCIATE|DIRECTOR)\\b|\\bCORPORATE STRATEGY\\b|\\bSTRATEGIC PLANN|\\bM & A\\b|MERGERS",
       soc = "^(11-|13-1)")
)

# Ordered SOC fallback rules (regex on normalised "NN-NNNN.NN" codes).
SOC_RULES <- data.table::fread(text = "
pattern|role
^15-1252|swe
^15-1251|swe
^15-113[23]|swe
^15-1131|swe
^15-1295|swe
^15-1299\\.07|swe
^15-1254|frontend
^15-1134|frontend
^15-1253|qa
^15-1199\\.01|qa
^15-2051\\.0[12]|data_analyst
^15-1199\\.08|data_analyst
^15-2051|data_scientist
^15-1243|data_engineer
^15-1199\\.0[67]|data_engineer
^15-1221|research_scientist
^15-1111|research_scientist
^15-20[234]1|quant_stats
^15-209|quant_stats
^15-2011|quant
^15-1299\\.08|it_architect
^15-1199\\.02|it_architect
^15-1241|it_architect
^15-1143|it_architect
^15-1211|systems_analyst
^15-1121|systems_analyst
^15-1212|security
^15-1122|security
^15-1299\\.0[456]|security
^15-1244|network_sysadmin
^15-1142|network_sysadmin
^15-123[12]|network_sysadmin
^15-115[12]|network_sysadmin
^15-1299\\.01|network_sysadmin
^15-1199\\.03|network_sysadmin
^15-1242|dba
^15-1141|dba
^15-1299\\.09|program_manager
^15-1199\\.09|program_manager
^15-1199\\.10|marketing
^15-1255|ux_designer
^15-1|it_other
^15-2|quant_stats
^11-3021|it_manager
^13-1082|program_manager
^17-207[12]|electrical
^17-2061|electrical
^17-2199\\.0[67]|electrical
^17-2141|mechanical
^17-2011|mechanical
^17-2199\\.0[58]|mechanical
^17-2112|industrial_mfg
^17-2111|industrial_mfg
^17-2199\\.04|industrial_mfg
^11-3051|industrial_mfg
^17-2051|civil
^17-2081|civil
^11-9021|civil
^17-1011|architect_building
^17-1012|architect_building
^17-|other_engineering
^11-9041|eng_manager
^13-2099\\.0[12]|quant
^13-2054|quant
^13-2011|accountant
^13-208|accountant
^43-303|accountant
^13-2|financial_analyst
^11-3031|financial_analyst
^41-3031|financial_analyst
^13-1111|consultant
^13-1161|marketing
^13-107|hr
^13-114|hr
^13-115|hr
^11-311|hr
^11-312|hr
^11-313|hr
^43-4161|hr
^13-1|operations
^11-307|operations
^11-306|operations
^11-301|operations
^11-1|general_mgmt
^11-2021|marketing
^11-201|marketing
^11-203|marketing
^11-2022|sales
^41-|sales
^27-3091|other
^27-3|marketing
^27-1|designer
^27-4|designer
^29-12|physician
^29-106|physician
^29-102|physician
^29-114|nurse
^29-115|nurse
^29-116|nurse
^29-117|nurse
^29-2061|nurse
^29-|allied_health
^31-|allied_health
^11-9111|health_admin
^21-|counselor_social
^11-9121|life_scientist
^19-1|life_scientist
^19-402|life_scientist
^19-2|physical_scientist
^19-406|social_scientist
^19-40[3-9]|physical_scientist
^19-3|social_scientist
^19-|life_scientist
^25-1|professor
^25-|teacher
^11-903|teacher
^23-|legal
^11-|general_mgmt
", sep = "|")

#' Classify job titles into roles.
#'
#' @param title_norm Normalised job titles (see normalize_title()).
#' @param soc Normalised SOC codes ("NN-NNNN.NN").
#' @return A list with `role_id` and `method` ("title", "soc" or "none").
classify_role <- function(title_norm, soc) {
  n <- length(title_norm)
  role <- rep(NA_character_, n)
  method <- rep(NA_character_, n)
  soc[is.na(soc)] <- ""
  title_norm[is.na(title_norm)] <- ""

  for (rule in TITLE_RULES) {
    open <- which(is.na(role))
    if (!length(open)) break
    ok <- open[grepl(rule$soc, soc[open], perl = TRUE)]
    if (!length(ok)) next
    hit <- ok[grepl(rule$pattern, title_norm[ok], perl = TRUE)]
    role[hit] <- rule$role
    method[hit] <- "title"
  }
  for (i in seq_len(nrow(SOC_RULES))) {
    open <- which(is.na(role))
    if (!length(open)) break
    hit <- open[grepl(SOC_RULES$pattern[i], soc[open], perl = TRUE)]
    role[hit] <- SOC_RULES$role[i]
    method[hit] <- "soc"
  }
  none <- is.na(role)
  role[none] <- "other"
  method[none] <- "none"
  list(role_id = role, method = method)
}

#' Validate that every role referenced by the rules exists.
validate_taxonomy <- function() {
  used <- unique(c(vapply(TITLE_RULES, `[[`, "", "role"), SOC_RULES$role))
  bad <- setdiff(used, ROLES$role_id)
  if (length(bad)) stop("Unknown roles in rules: ", paste(bad, collapse = ", "))
  bad_fam <- setdiff(ROLES$family_id, FAMILIES$family_id)
  if (length(bad_fam)) stop("Unknown families: ", paste(bad_fam, collapse = ", "))
  invisible(TRUE)
}
