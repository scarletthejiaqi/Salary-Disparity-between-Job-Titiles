test_that("normalize_title expands abbreviations and strips job codes", {
  expect_equal(normalize_title("Sr. Software Dev. Engineer - II (Backend)"),
               "SENIOR SOFTWARE DEV ENGINEER II BACKEND")
  expect_equal(normalize_title("MANAGER JC50 - COMPUTER SYSTEMS ENGINEERS/ARCHITECTS"),
               "MANAGER COMPUTER SYSTEMS ENGINEERS / ARCHITECTS")
  expect_equal(normalize_title("Information Security Analysts - KBGFJG222324-13"),
               "INFORMATION SECURITY ANALYSTS")
  expect_equal(normalize_title(".NET Developer"), "DOTNET DEVELOPER")
  expect_equal(normalize_title("UI/UX Designer"), "UI / UX DESIGNER")
})

test_that("display_title restores readable casing", {
  expect_equal(display_title("SENIOR SOFTWARE ENGINEER II"), "Senior Software Engineer II")
  expect_equal(display_title("DOTNET DEVELOPER"), ".NET Developer")
  expect_equal(display_title("IOS DEVELOPER"), "iOS Developer")
})

test_that("normalize_soc standardises code formats", {
  expect_equal(normalize_soc(c("15-1252.00", "15-1132", "151132", "15-1299.8", "n/a")),
               c("15-1252.00", "15-1132.00", "15-1132.00", "15-1299.08", NA))
})

test_that("employer_key collapses legal suffixes and punctuation", {
  expect_equal(employer_key(c("TATA CONSULTANCY SERVICES LIMITED", "Tata Consultancy Services Limited")),
               c("TATA CONSULTANCY SERVICES", "TATA CONSULTANCY SERVICES"))
  expect_equal(employer_key("Meta Platforms, Inc"), "META PLATFORMS")
  expect_equal(employer_key("Fidelity Technology Group, LLC d/b/a Fidelity Investments"),
               "FIDELITY TECHNOLOGY GROUP")
  expect_equal(employer_key("Bank of America, N.A."), "BANK OF AMERICA")
})

test_that("employer_parent groups legal entities of the same company", {
  keys <- employer_key(c("Amazon.com Services LLC", "Amazon Web Services, Inc.", "Google LLC",
                         "HCL AMERICA INC", "Bank of America N.A.", "Tiny Startup LLC"))
  expect_equal(employer_parent(keys),
               c("Amazon", "Amazon", "Google", "HCLTech", "Bank of America", NA))
})
