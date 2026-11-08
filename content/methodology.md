## Methodology

### What this dashboard measures

This dashboard tracks **what employers offer for professional jobs in the U.S.** and how that has changed since 2019, broken down by role, seniority, employer and location, together with **market-wide hiring demand**.

| Question | Source | Coverage |
|---|---|---|
| What does a role pay, by seniority, employer and place? | U.S. Department of Labor (DOL), Office of Foreign Labor Certification: **Labor Condition Application (LCA) disclosure data** for H-1B, H-1B1 and E-3 positions | Filings received Jan 2019 onward, every quarter DOL publishes |
| Are employers posting more or fewer jobs? | **Indeed Hiring Lab** Job Postings Index by occupational sector | Daily since Feb 1, 2020 (shown as monthly averages) |
| Are advertised wages still rising across the whole market? | **Indeed Hiring Lab** Wage Tracker by occupational sector | Monthly since 2019 |
| How many openings are there in the economy? | BLS **JOLTS** total nonfarm job openings (via FRED) | Monthly |
| What is a dollar worth over time? | BLS **CPI-U** (via FRED) | Monthly |

### The salary data: DOL Labor Condition Applications

Before an employer can hire someone on an H-1B (or H-1B1 / E-3) visa, it must file a Labor Condition Application with DOL. Every application states the **job title, occupation (SOC code), worksite, the wage the employer will pay, and the prevailing wage level** for that occupation and area. DOL publishes all decided applications each quarter. These are real, legally attested job offers, which makes them one of the very few public sources of salary by job title that goes back years.

**Definitions used here**

- **Filing**: one certified application (DOL status *Certified* or *Certified – Withdrawn*), counted once even if it covers several positions. Some DOL releases are cumulative (for example, the FY2021 Q3 file repeats Q1 and Q2), and an application can reappear when its status changes, so every case number is kept once, using its most recent determination.
- **Offered salary**: the lower end of the wage range on the application, converted to an annual amount (hourly × 2,080, weekly × 52, bi-weekly × 26, monthly × 12). This is **base pay only**. Bonuses, equity and benefits are not included.
- **Period**: the date DOL received the application. The latest year is **partial** (DOL's newest release runs through June), so the dashboard compares it with the *same months* of the previous year rather than with a full year.
- **New hires only**: filings for new employment or for a change of employer, as opposed to extensions or amendments for current employees.
- **Full-time only**: part-time positions are excluded.
- **Sanity checks**: annual offers outside $25,000 to $1,000,000, or more than 10% below the stated prevailing wage (which DOL does not allow), are treated as data-entry errors and dropped. The table below shows how many records each step removes.

**Statistics.** Medians and percentiles are computed directly from individual filings. Any figure based on **fewer than 30 filings is not shown**. *Inflation-adjusted* figures restate each period in constant 2025 dollars using CPI-U averaged over the same months. BLS did not publish CPI for October 2025, after the federal government shutdown, so averages for that period use the months available.

### Role families and roles

Each filing is assigned to one of **52 roles in 14 families**:

1. **Job-title rules come first.** Ordered keyword rules separate roles that share an occupation code. For example, *Data Engineer*, *ML Engineer*, *DevOps/SRE* and *Software Engineer* are all usually filed as SOC 15-1252 "Software Developers". Each rule only applies to compatible occupation codes, so a "Quality Engineer" is QA in software but Industrial/Manufacturing in a factory.
2. **Occupation codes are the fallback.** Both the 2018 SOC system and the 2010 system used in older (FY2019) files are mapped.

The taxonomy table below lists every role with its most common job titles and how often a title rule decided the role.

### Seniority

There are two independent views of seniority:

- **From job titles.** Keywords define six levels: *Entry / Junior* (intern, junior, associate, graduate, postdoc, resident, "level I"); *Mid / Unspecified* ("II", or no keyword); *Senior* ("senior", "III", associate professor); *Lead / Staff / Principal* (lead, staff, principal, "IV/V", full professor); *Manager*; *Director & Executive* (director, VP, head of, chief, partner). Individual-contributor roles whose titles contain "manager" (*Product Manager*, *Project Manager*, *Account Manager*) are not treated as people managers.
- **From DOL wage levels.** Employers must choose one of four prevailing-wage levels based on the job's requirements: **Level I** (entry), **II** (qualified), **III** (experienced), **IV** (fully competent). This is an official, consistent classification, but the employer picks it.

The check table at the bottom compares the two views. Many titles carry no seniority keyword, so *Mid / Unspecified* mixes several real levels.

### Important caveats

- **Who is covered.** LCA filings describe positions employers intend to fill with H-1B, H-1B1 or E-3 workers. They are concentrated in technology, engineering, finance, healthcare, research and education, and at employers that sponsor visas. They are a strong gauge of pay for professional roles at those employers, not of every U.S. job. That is why the market-wide Indeed series sit alongside them.
- **A filing is not a hire.** Some certified applications are never used, and one person can appear in several filings (e.g. an amendment after a move).
- **Seasonality.** Most new H-1B "cap" filings are submitted in spring, after the annual registration lottery, and many are for new graduates. Use the half-year view and same-months comparisons for the latest year.
- **Policy changes in 2025–2026.** A September 2025 presidential proclamation added a $100,000 payment for certain new H-1B petitions, mainly for workers outside the U.S., and has been challenged in court. A DHS rule effective February 2026 weights the H-1B lottery by wage level, so Level IV registrations get more chances than Level I. A DOL proposal to raise prevailing-wage levels was pending. These can shift both the volume and the seniority mix of 2026 filings, so read 2026 changes with care.
- **Pay is not adjusted for cost of living**, and remote-work status is not recorded on filings.

### Market-wide context (Indeed Hiring Lab, BLS)

- **Job Postings Index**: the seasonally adjusted number of job postings on Indeed relative to February 1, 2020 (= 100), by Indeed's occupational sectors. Each role family is matched to the closest sectors (listed in the taxonomy table).
- **Wage Tracker**: year-over-year growth in advertised wages for the same job titles, as a 3-month average. Not seasonally adjusted.
- **JOLTS**: total job openings on the last business day of the month, seasonally adjusted.

### Sources & licences

- U.S. Department of Labor, Office of Foreign Labor Certification, [Performance Data: LCA disclosure files](https://www.dol.gov/agencies/eta/foreign-labor/performance). Public domain.
- Indeed Hiring Lab, [Job Postings Tracker](https://github.com/hiring-lab/job_postings_tracker) and [Wage Tracker](https://github.com/hiring-lab/indeed-wage-tracker). Licensed **CC BY 4.0**; source: Indeed Hiring Lab.
- U.S. Bureau of Labor Statistics, JOLTS (JTSJOL) and CPI-U (CPIAUCSL), retrieved from [FRED](https://fred.stlouisfed.org/), Federal Reserve Bank of St. Louis.

Everything is rebuilt from these sources by a reproducible pipeline (`make data`); see the project README.
