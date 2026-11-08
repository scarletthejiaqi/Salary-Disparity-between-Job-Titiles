seniority_of <- function(x) classify_seniority(normalize_title(x))

test_that("explicit seniority keywords are detected", {
  titles <- c("Software Engineer Intern", "Junior Data Analyst", "Associate Software Engineer",
              "Software Engineer", "Senior Software Engineer", "Sr. Data Scientist",
              "Staff Software Engineer", "Principal Engineer", "Lead Data Engineer",
              "Engineering Manager", "Senior Manager", "Director, Software Engineering",
              "Vice President", "Head of Data", "Chief Technology Officer")
  expect_equal(seniority_of(titles),
               c("entry", "entry", "entry", "mid", "senior", "senior", "lead", "lead", "lead",
                 "manager", "manager", "director", "director", "director", "director"))
})

test_that("numeric job levels are mapped", {
  expect_equal(seniority_of(c("Software Development Engineer I", "Software Dev Engineer II",
                              "Data Engineer III", "Software Engineer IV",
                              "Technology Consultant 2", "Techno Functional Consultant L3")),
               c("entry", "mid", "senior", "lead", "mid", "senior"))
})

test_that("individual-contributor 'manager' roles are not people managers", {
  expect_equal(seniority_of(c("Product Manager", "Senior Product Manager", "Project Manager",
                              "Technical Program Manager III", "Account Manager",
                              "Group Product Manager", "Director of Product Management",
                              "Manager III, Software Dev")),
               c("mid", "senior", "mid", "senior", "mid", "manager", "director", "manager"))
})

test_that("director-adjacent titles are classified as managers", {
  expect_equal(seniority_of(c("Associate Director", "Assistant Vice President", "AVP, Risk",
                              "Managing Director", "General Manager")),
               c("manager", "manager", "manager", "director", "director"))
})

test_that("academic and medical career stages are handled", {
  expect_equal(seniority_of(c("Postdoctoral Research Associate", "Research Assistant",
                              "Assistant Professor", "Associate Professor", "Professor",
                              "Resident Physician", "Clinical Fellow", "Technical Fellow",
                              "Research Assistant Professor")),
               c("entry", "entry", "mid", "senior", "lead", "entry", "entry", "lead", "mid"))
})

test_that("'staff' prefixes depend on the profession", {
  expect_equal(seniority_of(c("Staff Accountant", "Audit Staff", "Staff Nurse",
                              "Member of Technical Staff", "Senior Member of Technical Staff",
                              "Consulting Member of Technical Staff")),
               c("entry", "entry", "mid", "mid", "senior", "lead"))
})

test_that("job codes are not mistaken for levels", {
  expect_equal(seniority_of("Information Security Analysts - KBGFJG219733-5"), "mid")
})

test_that("DOL wage levels are mapped", {
  expect_equal(dol_wage_level(c("I", "II", "III", "IV", "N/A", NA)),
               c("dol_1", "dol_2", "dol_3", "dol_4", NA, NA))
})
