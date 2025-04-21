library("devtools")
build(args=c('--no-build-vignettes'))
install()
library("testthat")

# Helper function to check for string equality ignoring white space difference
expect_equal_ignore_spaces <- function(string1, string2) {
  string1 <- gsub("([;()'+-/|*\n])", " \\1 ", string1)
  string2 <- gsub("([;()'+-/|*\n])", " \\1 ", string2)
  string1 <- gsub(" +", " ", string1)
  string2 <- gsub(" +", " ", string2)
  expect_equivalent(string1, string2)
}

# TEST FOR OHDSI WEBAPI CONCEPT_HIERARCHY SQL SCRIPTS
# E.G Endpoint: WebAPI/ddl/results?dialect=<dialect>&schema=<achilles_results_schema>&vocabSchema=<vocabSchema>
testthat::test_that("OHDSI WEBAPI CONCEPT_HIERARCHY SQL SCRIPTS ", {
  suppressWarnings({
    sql <- SqlRender::translate(sql = "insert into @results_schema.heracles_analysis (analysis_id,analysis_name,stratum_1_name,stratum_2_name,stratum_3_name,stratum_4_name,stratum_5_name,analysis_type) select    0 as analysis_id, CAST('Source name' as VARCHAR(255)) as analysis_name, CAST(NULL AS VARCHAR(255)) as stratum_1_name, CAST(NULL AS VARCHAR(255)) as stratum_2_name, CAST(NULL AS VARCHAR(255)) as stratum_3_name, CAST(NULL AS VARCHAR(255)) as stratum_4_name, CAST(NULL AS VARCHAR(255)) as stratum_5_name, CAST('PERSON' as VARCHAR(255)) as analysis_type union all select    1 as analysis_id, CAST('Number of persons' as VARCHAR(255)) as analysis_name, CAST(NULL AS VARCHAR(255)) as stratum_1_name, CAST(NULL AS VARCHAR(255)) as stratum_2_name, CAST(NULL AS VARCHAR(255)) as stratum_3_name, CAST(NULL AS VARCHAR(255)) as stratum_4_name, CAST(NULL AS VARCHAR(255)) as stratum_5_name, CAST('PERSON' as VARCHAR(255)) as analysis_type union all select    4 as analysis_id, CAST('Number of persons by race' as VARCHAR(255)) as analysis_name, CAST('race_concept_id' as VARCHAR(255)) as stratum_1_name, CAST(NULL AS VARCHAR(255)) as stratum_2_name, CAST(NULL AS VARCHAR(255)) as stratum_3_name, CAST(NULL AS VARCHAR(255)) as stratum_4_name, CAST(NULL AS VARCHAR(255)) as stratum_5_name, CAST('PERSON' as VARCHAR(255)) as analysis_type ;", targetDialect = "hana")  
    expect_equal_ignore_spaces(sql, "INSERT INTO @results_schema.heracles_analysis (analysis_id,analysis_name,stratum_1_name,stratum_2_name,stratum_3_name,stratum_4_name,stratum_5_name,analysis_type) SELECT 0 as analysis_id, CAST('Source name' as VARCHAR(255)) as analysis_name, CAST(NULL AS VARCHAR(255)) as stratum_1_name, CAST(NULL AS VARCHAR(255)) as stratum_2_name, CAST(NULL AS VARCHAR(255)) as stratum_3_name, CAST(NULL AS VARCHAR(255)) as stratum_4_name, CAST(NULL AS VARCHAR(255)) as stratum_5_name, CAST('PERSON' as VARCHAR(255)) as analysis_type  FROM DUMMY UNION ALL SELECT 1 as analysis_id, CAST('Number of persons' as VARCHAR(255)) as analysis_name, CAST(NULL AS VARCHAR(255)) as stratum_1_name, CAST(NULL AS VARCHAR(255)) as stratum_2_name, CAST(NULL AS VARCHAR(255)) as stratum_3_name, CAST(NULL AS VARCHAR(255)) as stratum_4_name, CAST(NULL AS VARCHAR(255)) as stratum_5_name, CAST('PERSON' as VARCHAR(255)) as analysis_type  FROM DUMMY  UNION all select    4 as analysis_id, CAST('Number of persons by race' as VARCHAR(255)) as analysis_name, CAST('race_concept_id' as VARCHAR(255)) as stratum_1_name, CAST(NULL AS VARCHAR(255)) as stratum_2_name, CAST(NULL AS VARCHAR(255)) as stratum_3_name, CAST(NULL AS VARCHAR(255)) as stratum_4_name, CAST(NULL AS VARCHAR(255)) as stratum_5_name, CAST('PERSON' as VARCHAR(255)) as analysis_type  FROM DUMMY;")
  })
})

testthat::test_that("OHDSI WEBAPI CONCEPT_HIERARCHY SQL SCRIPTS ", {
  suppressWarnings({
    sql <- SqlRender::translate(sql = "select digits.n  into #digits FROM ( select 0 as n union all select 1 union all select 2 union all select 3 union all select 4 union all select 5 union all select 6 union all select 7 union all select 8 union all select 9 ) digits;", targetDialect = "hana")
    expect_equal_ignore_spaces(sql, "CREATE LOCAL TEMPORARY TABLE #digits AS ( \n SELECT \n digits . n \n FROM \n ( SELECT 0 AS n FROM DUMMY UNION all SELECT 1 FROM DUMMY UNION all SELECT 2 FROM DUMMY UNION all SELECT 3 FROM DUMMY UNION all SELECT 4 FROM DUMMY UNION all SELECT 5 FROM DUMMY UNION all SELECT 6 FROM DUMMY UNION all SELECT 7 FROM DUMMY UNION all SELECT 8 FROM DUMMY UNION all select 9 FROM DUMMY ) digits ) ;")
  })
})

testthat::test_that("OHDSI WEBAPI CONCEPT_HIERARCHY SQL SCRIPTS ", {
  suppressWarnings({
    sql <- SqlRender::translate(sql = "SELECT y1.n + (10*y10.n) + (100*y100.n) + (1000*y1000.n) AS d_years, mths.n as d_months into #generate_dates FROM #digits y1, #digits y10, (select 0 n union all select 1 union all select 9) y100, (select 1 n union all select 2) y1000, (select 1 n union all select 2 union all select 3 union all select 4 union all select 5 union all select 6 union all select 7 union all select 8 union all select 9 union all select 10 union all select 11 union all select 12) mths where y1.n + (10*y10.n) + (100*y100.n) + (1000*y1000.n) >= 1900 and y1.n + (10*y10.n) + (100*y100.n) + (1000*y1000.n) < 2100 ;", targetDialect = "hana")
    expect_equal_ignore_spaces(sql, "CREATE LOCAL TEMPORARY TABLE #generate_dates AS( \n SELECT \n y1.n + (10*y10.n) + (100*y100.n) + (1000*y1000.n) AS d_years, mths.n as d_months \n FROM \n #digits y1, #digits y10, (SELECT 0 n  FROM DUMMY  UNION all SELECT 1   FROM DUMMY  UNION all select 9 FROM DUMMY) y100, (SELECT 1 n  FROM DUMMY UNION all select 2 FROM DUMMY) y1000, (SELECT 1 n  FROM DUMMY  UNION all SELECT 2   FROM DUMMY  UNION all SELECT 3   FROM DUMMY  UNION all SELECT 4   FROM DUMMY  UNION all SELECT 5   FROM DUMMY  UNION all SELECT 6   FROM DUMMY  UNION all SELECT 7   FROM DUMMY  UNION all SELECT 8   FROM DUMMY  UNION all SELECT 9   FROM DUMMY  UNION all SELECT 10   FROM DUMMY  UNION all SELECT 11   FROM DUMMY  UNION all select 12 FROM DUMMY) mths WHERE y1.n + (10*y10.n) + (100*y100.n) + (1000*y1000.n) >= 1900 and y1.n + (10*y10.n) + (100*y100.n) + (1000*y1000.n) < 2100 );")
    })
})
testthat::test_that("OHDSI COHORT_GENERATOR SQL", {
  suppressWarnings({
    sql <- SqlRender::translate(sql = "UPDATE STATISTICS #Codesets;", targetDialect = "hana")
    expect_equal_ignore_spaces(sql, "-- STATISTICS not supported on tempoary tables")
    })
})

testthat::test_that("OHDSI COHORT DIAGNOSTICS SQL", {
  suppressWarnings({
    sql <- SqlRender::translate(sql = "DROP TABLE IF EXISTS #Temp;", targetDialect = "hana")
    expect_equal_ignore_spaces(sql, "DO BEGIN IF EXISTS (SELECT * FROM (SELECT SCHEMA_NAME || '.' || TABLE_NAME AS combined_name, SCHEMA_NAME, TABLE_NAME FROM tables) WHERE combined_name=UPPER('#Temp')) THEN DROP TABLE #Temp; END IF; END;")

     sql <- SqlRender::translate(sql = "LEN(DIAGNOSTICS)", targetDialect = "hana")
    expect_equal_ignore_spaces(sql, "LENGTH(DIAGNOSTICS)")
    })
})