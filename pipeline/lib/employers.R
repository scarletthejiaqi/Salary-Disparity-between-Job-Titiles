# -----------------------------------------------------------------------------
# Employer name normalisation (pure functions, unit-tested).
# -----------------------------------------------------------------------------

# Only legal-entity suffixes: words such as "AMERICA" or "GROUP" are part of
# real names ("Bank of America") and are kept.
.LEGAL_SUFFIXES <- c(
  "INCORPORATED", "INC", "CORPORATION", "CORP", "COMPANY", "CO", "LLC", "L L C",
  "LLP", "L L P", "LP", "L P", "LTD", "LIMITED", "PLC", "PC", "P C", "PLLC",
  "PA", "NA", "N A"
)

#' Build a matching key for an employer name: upper-case, strip "d/b/a ..."
#' and punctuation, drop trailing legal suffixes ("INC", "LLC", ...).
employer_key <- function(x) {
  k <- toupper(as.character(x))
  k <- iconv(k, from = "UTF-8", to = "ASCII//TRANSLIT", sub = " ")
  k[is.na(k)] <- ""
  k <- sub("\\b(D/B/A|DBA|D B A)\\b.*$", "", k, perl = TRUE)
  k <- gsub("&", " AND ", k, fixed = TRUE)
  k <- gsub("[^A-Z0-9 ]", " ", k, perl = TRUE)
  k <- gsub("\\s+", " ", trimws(k), perl = TRUE)
  k <- sub("^THE ", "", k, perl = TRUE)
  suffix_rx <- paste0("( (", paste(.LEGAL_SUFFIXES, collapse = "|"), "))+$")
  repeat {
    k2 <- sub(suffix_rx, "", k, perl = TRUE)
    if (identical(k2, k)) break
    k <- k2
  }
  trimws(k)
}

# Parent-company aliases for the largest sponsors, so that e.g. "Amazon.com
# Services LLC" and "Amazon Web Services, Inc." are reported together.
# Patterns are matched against employer_key() output.
EMPLOYER_ALIASES <- data.table::fread(text = "
pattern|parent
^AMAZON|Amazon
^A2Z DEVELOPMENT|Amazon
^GOOGLE|Google
^META PLATFORMS|Meta
^FACEBOOK|Meta
^MICROSOFT|Microsoft
^APPLE$|Apple
^APPLE INC|Apple
^DELOITTE|Deloitte
^ERNST AND YOUNG|EY
^EY |EY
^TATA CONSULTANCY|Tata Consultancy Services
^INFOSYS|Infosys
^COGNIZANT|Cognizant
^ACCENTURE|Accenture
^IBM|IBM
^INTERNATIONAL BUSINESS MACHINES|IBM
^INTEL$|Intel
^INTEL CORP|Intel
^ORACLE|Oracle
^JPMORGAN|JPMorgan Chase
^JP MORGAN|JPMorgan Chase
^J P MORGAN|JPMorgan Chase
^WAL MART|Walmart
^WALMART|Walmart
^GOLDMAN SACHS|Goldman Sachs
^CAPGEMINI|Capgemini
^HCL AMERICA|HCLTech
^HCL TECHNOLOGIES|HCLTech
^WIPRO|Wipro
^LTIMINDTREE|LTIMindtree
^LARSEN AND TOUBRO INFOTECH|LTIMindtree
^MINDTREE|LTIMindtree
^TECH MAHINDRA|Tech Mahindra
^SALESFORCE|Salesforce
^NVIDIA|NVIDIA
^QUALCOMM|Qualcomm
^CISCO|Cisco
^ADOBE|Adobe
^BYTEDANCE|ByteDance / TikTok
^TIKTOK|ByteDance / TikTok
^UBER TECHNOLOGIES|Uber
^PRICEWATERHOUSECOOPERS|PwC
^PWC|PwC
^KPMG|KPMG
^MPHASIS|Mphasis
^CITIBANK|Citi
^CITIGROUP|Citi
^CITICORP|Citi
^BANK OF AMERICA|Bank of America
^FIDELITY|Fidelity Investments
^TESLA|Tesla
^MORGAN STANLEY|Morgan Stanley
^CAPITAL ONE|Capital One
^WELLS FARGO|Wells Fargo
^MCKINSEY|McKinsey & Company
^BOSTON CONSULTING GROUP|Boston Consulting Group
^LINKEDIN|LinkedIn
^NETFLIX|Netflix
^AIRBNB|Airbnb
^STRIPE|Stripe
^PAYPAL|PayPal
^VMWARE|VMware
^BROADCOM|Broadcom
^MICRON|Micron
^ADVANCED MICRO DEVICES|AMD
^SAMSUNG|Samsung
^INTUIT|Intuit
^SERVICENOW|ServiceNow
^WORKDAY|Workday
^DATABRICKS|Databricks
^SNOWFLAKE|Snowflake
^OPENAI|OpenAI
^ANTHROPIC|Anthropic
^ERICSSON|Ericsson
^SAP AMERICA|SAP
^SAP LABS|SAP
^DELL|Dell
^HEWLETT PACKARD ENTERPRISE|Hewlett Packard Enterprise
^HP INC|HP
^LTI$|LTIMindtree
^VIRTUSA|Virtusa
^HEXAWARE|Hexaware
^ZS ASSOCIATES|ZS Associates
^EPAM|EPAM Systems
^PERSISTENT SYSTEMS|Persistent Systems
^L AND T TECHNOLOGY|L&T Technology Services
^NTT DATA|NTT DATA
^COMPUNNEL|Compunnel
^SIEMENS|Siemens
^GENERAL MOTORS|General Motors
^FORD MOTOR|Ford
^JOHNSON AND JOHNSON|Johnson & Johnson
^PFIZER|Pfizer
^MERCK|Merck
^ELI LILLY|Eli Lilly
^AMGEN|Amgen
^GENENTECH|Genentech
^MAYO CLINIC|Mayo Clinic
^AMERICAN EXPRESS|American Express
^VISA$|Visa
^VISA USA|Visa
^VISA U S A|Visa
^MASTERCARD|Mastercard
^BLOOMBERG|Bloomberg
^BLACKROCK|BlackRock
^CHARLES SCHWAB|Charles Schwab
^UNITEDHEALTH|UnitedHealth Group
^OPTUM|UnitedHealth Group
^CVS|CVS Health
^WALGREEN|Walgreens
^TARGET CORP|Target
^HOME DEPOT|Home Depot
^LOWE S|Lowe's
^COSTCO|Costco
^CHEWY|Chewy
^EXPEDIA|Expedia
^BOOKING|Booking Holdings
^DOORDASH|DoorDash
^LYFT|Lyft
^PINTEREST|Pinterest
^SNAP INC|Snap
^SNAP$|Snap
^X CORP|X (Twitter)
^TWITTER|X (Twitter)
^SPOTIFY|Spotify
^ROBINHOOD|Robinhood
^COINBASE|Coinbase
^BLOCK INC|Block
^SQUARE INC|Block
^PALANTIR|Palantir
^ROBERT BOSCH|Bosch
^BOSCH|Bosch
^TOYOTA|Toyota
^HONDA|Honda
^RIVIAN|Rivian
^LUCID|Lucid
^SPACE EXPLORATION|SpaceX
^SPACEX|SpaceX
^BLUE ORIGIN|Blue Origin
^LOCKHEED|Lockheed Martin
^BOEING|Boeing
^NORTHROP|Northrop Grumman
^RAYTHEON|RTX
^RTX|RTX
^GE |GE
^GENERAL ELECTRIC|GE
^HONEYWELL|Honeywell
^ARM INC|Arm
^MARVELL|Marvell
^ANALOG DEVICES|Analog Devices
^TEXAS INSTRUMENTS|Texas Instruments
^APPLIED MATERIALS|Applied Materials
^LAM RESEARCH|Lam Research
^KLA|KLA
^SYNOPSYS|Synopsys
^CADENCE DESIGN|Cadence
^WESTERN DIGITAL|Western Digital
^SEAGATE|Seagate
^JUNIPER|Juniper Networks
^ARISTA|Arista Networks
^PALO ALTO NETWORKS|Palo Alto Networks
^CROWDSTRIKE|CrowdStrike
^ZOOM|Zoom
^DROPBOX|Dropbox
^ATLASSIAN|Atlassian
^SHOPIFY|Shopify
^EBAY|eBay
^ROKU|Roku
^ELECTRONIC ARTS|Electronic Arts
^ACTIVISION|Activision Blizzard
^EXL SERVICE|EXL
^GENPACT|Genpact
^SYNTEL|Atos Syntel
^IGATE|Capgemini
^KFORCE|Kforce
^RANDSTAD|Randstad
^ROBERT HALF|Robert Half
", sep = "|")

#' Map employer keys to a reporting name: a curated parent company when one
#' matches, otherwise NA (callers fall back to the most common raw spelling).
employer_parent <- function(key) {
  out <- rep(NA_character_, length(key))
  for (i in seq_len(nrow(EMPLOYER_ALIASES))) {
    open <- is.na(out)
    if (!any(open)) break
    hit <- open & grepl(EMPLOYER_ALIASES$pattern[i], key, perl = TRUE)
    out[hit] <- EMPLOYER_ALIASES$parent[i]
  }
  out
}
