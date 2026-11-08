test_that("taxonomy tables are internally consistent", {
  expect_true(validate_taxonomy())
  expect_false(anyDuplicated(ROLES$role_id) > 0)
  expect_true(all(ROLES$family_id %in% FAMILIES$family_id))
})

test_that("software and data roles are split by job title", {
  cases <- data.frame(
    title = c("Software Engineer", "Senior Data Engineer", "Machine Learning Engineer",
              "Data Scientist II", "Data Analyst", "Business Intelligence Engineer II",
              "DevOps Engineer", "Site Reliability Engineer", "iOS Developer",
              "Front End Developer", "QA Automation Engineer", "Embedded Software Engineer",
              "Applied Scientist II", "Solution Architect", "Business Analyst",
              "Business Systems Analyst", "SAP ABAP Consultant", "Network Engineer",
              "Database Administrator", "Security Engineer", "Product Manager",
              "Senior Technical Program Manager", "Scrum Master", "UX Designer",
              "Software Engineering Manager", "Engineering Manager"),
    soc = c("15-1252.00", "15-1252.00", "15-1252.00", "15-2051.00", "15-2051.00", "15-2051.01",
            "15-1252.00", "15-1252.00", "15-1252.00", "15-1252.00", "15-1253.00", "15-1252.00",
            "15-1221.00", "15-1299.08", "15-2051.01", "15-1211.00", "15-1211.00", "15-1244.00",
            "15-1242.00", "15-1212.00", "15-1299.09", "11-3021.00", "15-1299.09", "15-1255.00",
            "11-3021.00", "11-3021.00"),
    expected = c("swe", "data_engineer", "ml_engineer", "data_scientist", "data_analyst",
                 "data_analyst", "devops", "devops", "mobile", "frontend", "qa", "embedded",
                 "ml_engineer", "it_architect", "business_analyst", "systems_analyst",
                 "systems_analyst", "network_sysadmin", "dba", "security", "product_manager",
                 "program_manager", "program_manager", "ux_designer", "swe", "swe"),
    stringsAsFactors = FALSE
  )
  expect_equal(classify_titles(cases$title, cases$soc), cases$expected)
})

test_that("non-tech roles fall back to SOC codes", {
  cases <- data.frame(
    title = c("Assistant Professor", "Registered Nurse", "Hospitalist", "Physical Therapist",
              "Financial Analyst", "Accountant", "Quantitative Analyst", "Management Analyst",
              "Marketing Specialist", "Mechanical Engineer", "Electrical Engineer",
              "Civil Engineer", "Postdoctoral Fellow", "Chemist", "Economist", "Lawyer",
              "HR Specialist", "Graphic Designer", "Architect", "Elementary School Teacher",
              "Logistics Analyst", "Sales Engineer", "Chief Executive Officer", "Actuary",
              "Mystery job"),
    soc = c("25-1071.00", "29-1141.00", "29-1229.02", "29-1123.00", "13-2051.00",
            "13-2011.00", "13-2099.01", "13-1111.00", "13-1161.00", "17-2141.00",
            "17-2071.00", "17-2051.00", "19-1042.00", "19-2031.00", "19-3011.00",
            "23-1011.00", "13-1071.00", "27-1024.00", "17-1011.00", "25-2021.00",
            "13-1081.02", "41-9031.00", "11-1011.00", "15-2011.00", "53-3032.00"),
    expected = c("professor", "nurse", "physician", "allied_health", "financial_analyst",
                 "accountant", "quant", "consultant", "marketing", "mechanical", "electrical",
                 "civil", "life_scientist", "physical_scientist", "social_scientist", "legal",
                 "hr", "designer", "architect_building", "teacher", "operations", "sales",
                 "general_mgmt", "quant", "other"),
    stringsAsFactors = FALSE
  )
  expect_equal(classify_titles(cases$title, cases$soc), cases$expected)
})

test_that("2010 SOC codes used in FY2019 files map to the same roles", {
  expect_equal(classify_titles(c("Programmer Analyst", "Developer", "Web Developer", "Statistician"),
                               c("15-1121.00", "15-1132.00", "15-1134.00", "15-2041.00")),
               c("systems_analyst", "swe", "frontend", "quant_stats"))
})

test_that("title rules respect SOC constraints", {
  # 'Quality engineer' is QA only for computer occupations
  expect_equal(classify_titles(c("Quality Engineer", "Quality Engineer"), c("15-1253.00", "17-2112.00")),
               c("qa", "industrial_mfg"))
  # Plain 'Architect' depends on the occupation code
  expect_equal(classify_titles(c("Architect", "Architect"), c("15-1299.08", "17-1011.00")),
               c("it_architect", "architect_building"))
})
