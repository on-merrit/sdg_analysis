library(tidyverse)
library(vroom)

con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "data/processed/sdg_mag.db")

DBI::dbListTables(con)

author_meta <- vroom("data/processed/sdg_author_data.csv")

copy_to(con, author_meta, "author_metadata",
        temporary = FALSE,
        indexes = list(
          "authorid", "year_first_paper", "lastknownaffiliationid"
        )
)


rm(author_meta)
gc()

author_paper_affil <- vroom("data/processed/sdg_author_paper_affil.csv")

copy_to(con, author_paper_affil, "author_paper_affil",
        temporary = FALSE,
        indexes = list(
          "authorid", "paperid", "affiliationid"
        )
)
rm(author_paper_affil)
gc()


# papers table
paper_cols <- cols(
        journalid = col_double(),
        year = col_double(),
        doi = col_character(),
        paperid = col_double(),
        fieldofstudyid = col_double(),
        fos_displayname = col_character(),
        fos_normalizedname = col_character(),
        doctype = col_character(),
        papertitle = col_character(),
        originaltitle = col_character(),
        booktitle = col_character(),
        date = col_date(format = ""),
        publisher = col_character(),
        referencecount = col_double(),
        citationcount = col_double(),
        originalvenue = col_character(),
        familyid = col_double(),
        is_oa = col_logical(),
        oa_status = col_character(),
        is_funded = col_logical(),
        mean_citations = col_double(),
        citations_norm = col_double()
)
papers_collated <- vroom("data/processed/sdg_papers_collated.csv",
                         col_types = paper_cols)
# there are still some parsing errors in later columns, but disregard them for
# now
copy_to(con, papers_collated, "papers",
        temporary = FALSE,
        indexes = list("doi", "fieldofstudyid", "oa_status", "journalid",
                       "year", "paperid", "is_oa", "is_funded")
)
rm(papers_collated)
gc()


# funders table
funder_cols <- cols(
        doi = col_character(),
        funded_project_title = col_character(),
        fundingStream = col_character(),
        funding_jurisdiction = col_character(),
        funder_name = col_character(),
        funder_shortname = col_character(),
        is_funded = col_logical(),
        paperid = col_double()
)

funders_table <- vroom("data/processed/openaire_funders_injoin_w_sdg.csv",
                       col_types = funder_cols)
copy_to(con, funders_table, "funded_projects",
        temporary = FALSE,
        indexes = list("doi", "paperid", "is_funded")
)


# list everything
DBI::dbListTables(con)
