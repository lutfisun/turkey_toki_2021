# On Data Sources and Methods

We utilized six main Data Sets in our research: TOKI projects, population, general (parliamentary) election results, local (mayoral) election results, cabinet members, nightlights.

## 1. TOKI Projects: output: temp/toki_catwide.csv

-   Source: <https://www.toki.gov.tr/proje-tipine-gore-uygulamalar>

Operation order and purpose:

1.  scrape_toki.py -output: raw/toki_projects_raw.xlsx

    -   scrape projects from TOKI website browsing by category on June 19th 2020.
    -   output: test.xlsx
    -   edited the output in excel to add project category (written as separater/subheader in the test output) to its corresponding rows.
    -   Some projects have missing year, but the row it is located can imply what year bidding took place. Or the firm's website may contain that information. So, we filled those years using based on the row of the observation (among other 2014 projects for example) and other online information about that project.
    -   Named the result: toki_projects_raw.xlsx

2.  toki_project_names.R -output: temp/toki_names1234.xlsx

    -   reads the scraped data toki_projects_raw.xlsx and stores first four words of project
    -   It is often the case the first three-four words of the project name contains the district name. So, this code is to make it easy to match each project with its district.

3.  Excel -output: temp/toki_districts.csv

    -   Manually go through projects to find their district and its name in the project year. District names change and project name sometimes fails to reflect that.
    -   Some project names do not mention the district at all, but the project site can be located on google maps. Sometimes the project can be found in news sources where its distric or neighborhood (hence the district is implied) is mentioned.
    -   Eliminate projects that span multiple provinces
    -   Replace special turkish characters with characters in english (latin) alphabet.
    -   Convert the approximate monetary cost of project (winning bid) to dollars using exhange rates at the end of each year.

4.  toki_clean_wide.R -output: temp/toki_catwide.csv

    -   Converts the data from long to wide (with respect to project category)
    -   Saves project dollar amounts for each category in a separate variable coded as dols\_\*\*\* (replace \*\*\* with project category)
    -   This is to have a single observation for each district in a given year.

## 2. Population output: population_district.csv

-   We were able to find 2000 census data with population for each district and 2007-2019 data (after Turkey transitioned from census to address based population counting system).

-   Excel

    -   For, the years 2002-2005, we interpolated population using 2000 and 2006 observations. Similarly, we estimated 2020 data using a linear trend based on previous observations.
    -   output: population_district.csv

## 3. General Elections -output: gen_elections.csv

-   Excel

    -   

    -   output: gen_elections.csv

## 4. Local Elections

## 5. Cabinet Members

-   Sources

    -   <https://www.akparti.org.tr/ak-kadro/hukumetler/hukumet/>

    -   <https://www.tbmm.gov.tr/>

-   output: cabin_merge/long_cabinet_origin.dta and cabin_merge/long_cabinet_represents.dta

## 6. Nightlights

## Merging

-   merge_all.do

    -   yoyoyo
