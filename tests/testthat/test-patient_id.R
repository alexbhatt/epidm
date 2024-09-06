library(testthat)
library(epidm)

# S1: NHS + DOB
test_that("Stage 1 linkage assigns correct IDs", {


  pd <- data.frame(
    nhs_number = c(9435817777, 9435817777, 9435773982, 9435797881, 9435797881, 9435754422, 9435754422, 9435802508),
    local_patient_identifier = c('HS45202', 'HS45202', 'KR2535', 'NEW001', 'IG12067', 'IG12067', 'IG12067', 'UK8734'),
    patient_birth_date = as.Date(c('1962-06-14', '1962-06-14', '1927-06-24', '1938-10-05', '1938-10-05', '1930-01-01', '1930-02-01', '1989-01-01')),
    sex = c('Male', 'Male', 'Male', 'Female', 'Female', 'Female', 'Female', 'Male'),
    surname = c('RODA', 'RODA', 'LINTON', 'WILLIAMS', 'WILLIAMS', 'WILLIAMS', 'WILLIAMS', 'JAMILETH'),
    forename = c('TYLER', 'TYLER', 'KASHIEF', 'JANE', 'JAMILETH', 'JAMILETH', 'JAMILETH', 'WILLIAMS'),
    organism_species_name = c('STAPHYLOCOCCUS AUREUS', 'STAPHYLOCOCCUS AUREUS', 'STAPHYLOCOCCUS AUREUS', 'E. coli', 'E. coli', 'KLEBSIELLA PNEUMONIAE', 'KLEBSIELLA PNEUMONIAE', 'E. coli'),
    specimen_date = as.Date(c('2023-08-26', '2023-08-27', '2023-02-25', '2024-01-01', '2024-02-02', '2024-03-03', '2024-04-03', '2024-01-01')),
    specimen_type = c('BLOOD', 'BLOOD', 'BLOOD', 'URINE', 'URINE', 'BLOOD', 'BLOOD', 'URINE'),
    lab_code = c('ES3851', 'ES3851', 'CU5997', 'LAB1001', 'LAB1002', 'LAB1003', 'LAB1003', 'LAB1001'),
    local_authority_name = c('Cheshire West and Chester', 'Cheshire West and Chester', 'Plymouth', 'Worthing', 'Worthing', 'Worthing', 'Worthing', 'Worthing'),
    local_authority_code = c('E06000050', 'E06000050', 'E06000026', 'E07000229', 'E07000229', 'E07000229', 'E07000229', 'E07000229'),
    postcode = c('CW6 9TX', 'CW6 9TX', 'PL7 1LU', 'BN14 9EP', 'BN14 9EP', 'BN14 9EP', 'BN14 9EP', 'BN14 9EP')
  )

  result <- uk_patient_id(
    data = pd,
    id = list(
      nhs_number = 'nhs_number',
      hospital_number = 'local_patient_identifier',
      date_of_birth = 'patient_birth_date',
      sex_mfu = 'sex',
      forename = 'forename',
      surname = 'surname',
      postcode = 'postcode'
    ),
    .sortOrder = 'specimen_date',
    .forceCopy = TRUE,
    .useStages = 1
  )[]

  # Define the expected ID column
  expected_id <- c(1, 2, 2, 4, 4, 5, 7, 8)

  # Check if the result matches the expected values
  expect_equal(result$id, expected_id)
})

# S2: HOS + DOB
test_that("Stage 2 linkage assigns correct IDs", {


  pd <- data.frame(
    nhs_number = c(9435817777, 9435817777, 9435773982, 9435797881, 9435797881, 9435754422, 9435754422, 9435802508),
    local_patient_identifier = c('HS45202', 'HS45202', 'KR2535', 'NEW001', 'IG12067', 'IG12067', 'IG12067', 'UK8734'),
    patient_birth_date = as.Date(c('1962-06-14', '1962-06-14', '1927-06-24', '1938-10-05', '1938-10-05', '1930-01-01', '1930-02-01', '1989-01-01')),
    sex = c('Male', 'Male', 'Male', 'Female', 'Female', 'Female', 'Female', 'Male'),
    surname = c('RODA', 'RODA', 'LINTON', 'WILLIAMS', 'WILLIAMS', 'WILLIAMS', 'WILLIAMS', 'JAMILETH'),
    forename = c('TYLER', 'TYLER', 'KASHIEF', 'JANE', 'JAMILETH', 'JAMILETH', 'JAMILETH', 'WILLIAMS'),
    organism_species_name = c('STAPHYLOCOCCUS AUREUS', 'STAPHYLOCOCCUS AUREUS', 'STAPHYLOCOCCUS AUREUS', 'E. coli', 'E. coli', 'KLEBSIELLA PNEUMONIAE', 'KLEBSIELLA PNEUMONIAE', 'E. coli'),
    specimen_date = as.Date(c('2023-08-26', '2023-08-27', '2023-02-25', '2024-01-01', '2024-02-02', '2024-03-03', '2024-04-03', '2024-01-01')),
    specimen_type = c('BLOOD', 'BLOOD', 'BLOOD', 'URINE', 'URINE', 'BLOOD', 'BLOOD', 'URINE'),
    lab_code = c('ES3851', 'ES3851', 'CU5997', 'LAB1001', 'LAB1002', 'LAB1003', 'LAB1003', 'LAB1001'),
    local_authority_name = c('Cheshire West and Chester', 'Cheshire West and Chester', 'Plymouth', 'Worthing', 'Worthing', 'Worthing', 'Worthing', 'Worthing'),
    local_authority_code = c('E06000050', 'E06000050', 'E06000026', 'E07000229', 'E07000229', 'E07000229', 'E07000229', 'E07000229'),
    postcode = c('CW6 9TX', 'CW6 9TX', 'PL7 1LU', 'BN14 9EP', 'BN14 9EP', 'BN14 9EP', 'BN14 9EP', 'BN14 9EP')
  )

  result <- uk_patient_id(
    data = pd,
    id = list(
      nhs_number = 'nhs_number',
      hospital_number = 'local_patient_identifier',
      date_of_birth = 'patient_birth_date',
      sex_mfu = 'sex',
      forename = 'forename',
      surname = 'surname',
      postcode = 'postcode'
    ),
    .sortOrder = 'specimen_date',
    .forceCopy = TRUE,
    .useStages = 2
  )[]

  # Define the expected ID column
  expected_id <- c(1, 2, 2, 4, 5, 6, 7, 8)

  # Check if the result matches the expected values
  expect_equal(result$id, expected_id)
})

# S3: NHS + HOS
test_that("Stage 3 linkage assigns correct IDs", {


  pd <- data.frame(
    nhs_number = c(9435817777, 9435817777, 9435773982, 9435797881, 9435797881, 9435754422, 9435754422, 9435802508),
    local_patient_identifier = c('HS45202', 'HS45202', 'KR2535', 'NEW001', 'IG12067', 'IG12067', 'IG12067', 'UK8734'),
    patient_birth_date = as.Date(c('1962-06-14', '1962-06-14', '1927-06-24', '1938-10-05', '1938-10-05', '1930-01-01', '1930-02-01', '1989-01-01')),
    sex = c('Male', 'Male', 'Male', 'Female', 'Female', 'Female', 'Female', 'Male'),
    surname = c('RODA', 'RODA', 'LINTON', 'WILLIAMS', 'WILLIAMS', 'WILLIAMS', 'WILLIAMS', 'JAMILETH'),
    forename = c('TYLER', 'TYLER', 'KASHIEF', 'JANE', 'JAMILETH', 'JAMILETH', 'JAMILETH', 'WILLIAMS'),
    organism_species_name = c('STAPHYLOCOCCUS AUREUS', 'STAPHYLOCOCCUS AUREUS', 'STAPHYLOCOCCUS AUREUS', 'E. coli', 'E. coli', 'KLEBSIELLA PNEUMONIAE', 'KLEBSIELLA PNEUMONIAE', 'E. coli'),
    specimen_date = as.Date(c('2023-08-26', '2023-08-27', '2023-02-25', '2024-01-01', '2024-02-02', '2024-03-03', '2024-04-03', '2024-01-01')),
    specimen_type = c('BLOOD', 'BLOOD', 'BLOOD', 'URINE', 'URINE', 'BLOOD', 'BLOOD', 'URINE'),
    lab_code = c('ES3851', 'ES3851', 'CU5997', 'LAB1001', 'LAB1002', 'LAB1003', 'LAB1003', 'LAB1001'),
    local_authority_name = c('Cheshire West and Chester', 'Cheshire West and Chester', 'Plymouth', 'Worthing', 'Worthing', 'Worthing', 'Worthing', 'Worthing'),
    local_authority_code = c('E06000050', 'E06000050', 'E06000026', 'E07000229', 'E07000229', 'E07000229', 'E07000229', 'E07000229'),
    postcode = c('CW6 9TX', 'CW6 9TX', 'PL7 1LU', 'BN14 9EP', 'BN14 9EP', 'BN14 9EP', 'BN14 9EP', 'BN14 9EP')
  )

  result <- uk_patient_id(
    data = pd,
    id = list(
      nhs_number = 'nhs_number',
      hospital_number = 'local_patient_identifier',
      date_of_birth = 'patient_birth_date',
      sex_mfu = 'sex',
      forename = 'forename',
      surname = 'surname',
      postcode = 'postcode'
    ),
    .sortOrder = 'specimen_date',
    .forceCopy = TRUE,
    .useStages = 3
  )[]

  # Define the expected ID column
  expected_id <- c(1, 2, 2, 4, 5, 6, 7, 7)

  # Check if the result matches the expected values
  expect_equal(result$id, expected_id)
})


# S4: NHS + NAME
test_that("Stage 4 linkage assigns correct IDs", {


  pd <- data.frame(
    nhs_number = c(9435817777, 9435817777, 9435773982, 9435797881, 9435797881, 9435754422, 9435754422, 9435802508),
    local_patient_identifier = c('HS45202', 'HS45202', 'KR2535', 'NEW001', 'IG12067', 'IG12067', 'IG12067', 'UK8734'),
    patient_birth_date = as.Date(c('1962-06-14', '1962-06-14', '1927-06-24', '1938-10-05', '1938-10-05', '1930-01-01', '1930-02-01', '1989-01-01')),
    sex = c('Male', 'Male', 'Male', 'Female', 'Female', 'Female', 'Female', 'Male'),
    surname = c('RODA', 'RODA', 'LINTON', 'WILLIAMS', 'WILLIAMS', 'WILLIAMS', 'WILLIAMS', 'JAMILETH'),
    forename = c('TYLER', 'TYLER', 'KASHIEF', 'JAMILETH', 'JAMILETH', 'JAMILETH', 'JAMILETH', 'WILLIAMS'),
    organism_species_name = c('STAPHYLOCOCCUS AUREUS', 'STAPHYLOCOCCUS AUREUS', 'STAPHYLOCOCCUS AUREUS', 'E. coli', 'E. coli', 'KLEBSIELLA PNEUMONIAE', 'KLEBSIELLA PNEUMONIAE', 'E. coli'),
    specimen_date = as.Date(c('2023-08-26', '2023-08-27', '2023-02-25', '2024-01-01', '2024-02-02', '2024-03-03', '2024-04-03', '2024-01-01')),
    specimen_type = c('BLOOD', 'BLOOD', 'BLOOD', 'URINE', 'URINE', 'BLOOD', 'BLOOD', 'URINE'),
    lab_code = c('ES3851', 'ES3851', 'CU5997', 'LAB1001', 'LAB1002', 'LAB1003', 'LAB1003', 'LAB1001'),
    local_authority_name = c('Cheshire West and Chester', 'Cheshire West and Chester', 'Plymouth', 'Worthing', 'Worthing', 'Worthing', 'Worthing', 'Worthing'),
    local_authority_code = c('E06000050', 'E06000050', 'E06000026', 'E07000229', 'E07000229', 'E07000229', 'E07000229', 'E07000229'),
    postcode = c('CW6 9TX', 'CW6 9TX', 'PL7 1LU', 'BN14 9EP', 'BN14 9EP', 'BN14 9EP', 'BN14 9EP', 'BN14 9EP')
  )

  result <- uk_patient_id(
    data = pd,
    id = list(
      nhs_number = 'nhs_number',
      hospital_number = 'local_patient_identifier',
      date_of_birth = 'patient_birth_date',
      sex_mfu = 'sex',
      forename = 'forename',
      surname = 'surname',
      postcode = 'postcode'
    ),
    .sortOrder = 'specimen_date',
    .forceCopy = TRUE,
    .useStages = 4
  )[]

  # Define the expected ID column
  expected_id <- c(1, 2, 2, 4, 4, 5, 7, 7)

  # Check if the result matches the expected values
  expect_equal(result$id, expected_id)
})

# S5: HOS + NAME
test_that("Stage 5 linkage assigns correct IDs", {


  pd <- data.frame(
    nhs_number = c(9435817777, 9435817777, 9435773982, 9435797881, 9435797881, 9435754422, 9435754422, 9435802508),
    local_patient_identifier = c('HS45202', 'HS45202', 'KR2535', 'NEW001', 'IG12067', 'IG12067', 'IG12067', 'UK8734'),
    patient_birth_date = as.Date(c('1962-06-14', '1962-06-14', '1927-06-24', '1938-10-05', '1938-10-05', '1930-01-01', '1930-02-01', '1989-01-01')),
    sex = c('Male', 'Male', 'Male', 'Female', 'Female', 'Female', 'Female', 'Male'),
    surname = c('RODA', 'RODA', 'LINTON', 'WILLIAMS', 'WILLIAMS', 'WILLIAMS', 'WILLIAMS', 'JAMILETH'),
    forename = c('TYLER', 'TYLER', 'KASHIEF', 'JAMILETH', 'JAMILETH', 'JAMILETH', 'JAMILETH', 'WILLIAMS'),
    organism_species_name = c('STAPHYLOCOCCUS AUREUS', 'STAPHYLOCOCCUS AUREUS', 'STAPHYLOCOCCUS AUREUS', 'E. coli', 'E. coli', 'KLEBSIELLA PNEUMONIAE', 'KLEBSIELLA PNEUMONIAE', 'E. coli'),
    specimen_date = as.Date(c('2023-08-26', '2023-08-27', '2023-02-25', '2024-01-01', '2024-02-02', '2024-03-03', '2024-04-03', '2024-01-01')),
    specimen_type = c('BLOOD', 'BLOOD', 'BLOOD', 'URINE', 'URINE', 'BLOOD', 'BLOOD', 'URINE'),
    lab_code = c('ES3851', 'ES3851', 'CU5997', 'LAB1001', 'LAB1002', 'LAB1003', 'LAB1003', 'LAB1001'),
    local_authority_name = c('Cheshire West and Chester', 'Cheshire West and Chester', 'Plymouth', 'Worthing', 'Worthing', 'Worthing', 'Worthing', 'Worthing'),
    local_authority_code = c('E06000050', 'E06000050', 'E06000026', 'E07000229', 'E07000229', 'E07000229', 'E07000229', 'E07000229'),
    postcode = c('CW6 9TX', 'CW6 9TX', 'PL7 1LU', 'BN14 9EP', 'BN14 9EP', 'BN14 9EP', 'BN14 9EP', 'BN14 9EP')
  )

  result <- uk_patient_id(
    data = pd,
    id = list(
      nhs_number = 'nhs_number',
      hospital_number = 'local_patient_identifier',
      date_of_birth = 'patient_birth_date',
      sex_mfu = 'sex',
      forename = 'forename',
      surname = 'surname',
      postcode = 'postcode'
    ),
    .sortOrder = 'specimen_date',
    .forceCopy = TRUE,
    .useStages = 5
  )[]

  # Define the expected ID column
  expected_id <- c(1, 2, 2, 4, 5, 6, 6, 6)

  # Check if the result matches the expected values
  expect_equal(result$id, expected_id)
})

# S6: DOB + SURNAME  >>>> not producing expected output
test_that("Stage 6 linkage assigns correct IDs", {


  pd <- data.frame(
    nhs_number = c(9435817777, 9435817777, 9435773982, 9435797881, 9435797881, 9435754422, 9435754422, 9435802508),
    local_patient_identifier = c('HS45202', 'HS45202', 'KR2535', 'NEW001', 'IG12067', 'IG12067', 'IG12067', 'UK8734'),
    patient_birth_date = as.Date(c('1962-06-14', '1962-06-14', '1927-06-24', '1938-10-05', '1938-10-05', '1930-01-01', '1930-02-01', '1989-01-01')),
    sex = c('Male', 'Male', 'Male', 'Female', 'Female', 'Female', 'Female', 'Male'),
    surname = c('RODA', 'RODA', 'LINTON', 'WILLIAMS', 'WILLIAMS', 'WILLIAMS', 'WILLIAMS', 'JAMILETH'),
    forename = c('TYLER', 'TYLER', 'KASHIEF', 'JAMILETH', 'JAMILETH', 'JAMILETH', 'JAMILETH', 'WILLIAMS'),
    organism_species_name = c('STAPHYLOCOCCUS AUREUS', 'STAPHYLOCOCCUS AUREUS', 'STAPHYLOCOCCUS AUREUS', 'E. coli', 'E. coli', 'KLEBSIELLA PNEUMONIAE', 'KLEBSIELLA PNEUMONIAE', 'E. coli'),
    specimen_date = as.Date(c('2023-08-26', '2023-08-27', '2023-02-25', '2024-01-01', '2024-02-02', '2024-03-03', '2024-04-03', '2024-01-01')),
    specimen_type = c('BLOOD', 'BLOOD', 'BLOOD', 'URINE', 'URINE', 'BLOOD', 'BLOOD', 'URINE'),
    lab_code = c('ES3851', 'ES3851', 'CU5997', 'LAB1001', 'LAB1002', 'LAB1003', 'LAB1003', 'LAB1001'),
    local_authority_name = c('Cheshire West and Chester', 'Cheshire West and Chester', 'Plymouth', 'Worthing', 'Worthing', 'Worthing', 'Worthing', 'Worthing'),
    local_authority_code = c('E06000050', 'E06000050', 'E06000026', 'E07000229', 'E07000229', 'E07000229', 'E07000229', 'E07000229'),
    postcode = c('CW6 9TX', 'CW6 9TX', 'PL7 1LU', 'BN14 9EP', 'BN14 9EP', 'BN14 9EP', 'BN14 9EP', 'BN14 9EP')
  )

  result <- uk_patient_id(
    data = pd,
    id = list(
      nhs_number = 'nhs_number',
      hospital_number = 'local_patient_identifier',
      date_of_birth = 'patient_birth_date',
      sex_mfu = 'sex',
      forename = 'forename',
      surname = 'surname',
      postcode = 'postcode'
    ),
    .sortOrder = 'specimen_date',
    .forceCopy = TRUE,
    .useStages = 6
  )[]

  # Define the expected ID column
  expected_id <- c(1, 2, 2, 4, 5, 6, 6, 6)

  # Check if the result matches the expected values
  expect_equal(result$id, expected_id)
})

## S7: SEX + FULL NAME
test_that("Stage 7 linkage assigns correct IDs", {


  pd <- data.frame(
    nhs_number = c(9435817777, 9435817777, 9435773982, 9435797881, 9435797881, 9435754422, 9435754422, 9435802508),
    local_patient_identifier = c('HS45202', 'HS45202', 'KR2535', 'NEW001', 'IG12067', 'IG12067', 'IG12067', 'UK8734'),
    patient_birth_date = as.Date(c('1962-06-14', '1962-06-14', '1927-06-24', '1938-10-05', '1938-10-05', '1930-01-01', '1930-02-01', '1989-01-01')),
    sex = c('Male', 'Male', 'Male', 'Female', 'Female', 'Female', 'Female', 'Male'),
    surname = c('RODA', 'RODA', 'LINTON', 'WILLIAMS', 'WILLIAMS', 'WILLIAMS', 'WILLIAMS', 'JAMILETH'),
    forename = c('TYLER', 'TYLER', 'KASHIEF', 'JAMILETH', 'JAMILETH', 'JAMILETH', 'JAMILETH', 'WILLIAMS'),
    organism_species_name = c('STAPHYLOCOCCUS AUREUS', 'STAPHYLOCOCCUS AUREUS', 'STAPHYLOCOCCUS AUREUS', 'E. coli', 'E. coli', 'KLEBSIELLA PNEUMONIAE', 'KLEBSIELLA PNEUMONIAE', 'E. coli'),
    specimen_date = as.Date(c('2023-08-26', '2023-08-27', '2023-02-25', '2024-01-01', '2024-02-02', '2024-03-03', '2024-04-03', '2024-01-01')),
    specimen_type = c('BLOOD', 'BLOOD', 'BLOOD', 'URINE', 'URINE', 'BLOOD', 'BLOOD', 'URINE'),
    lab_code = c('ES3851', 'ES3851', 'CU5997', 'LAB1001', 'LAB1002', 'LAB1003', 'LAB1003', 'LAB1001'),
    local_authority_name = c('Cheshire West and Chester', 'Cheshire West and Chester', 'Plymouth', 'Worthing', 'Worthing', 'Worthing', 'Worthing', 'Worthing'),
    local_authority_code = c('E06000050', 'E06000050', 'E06000026', 'E07000229', 'E07000229', 'E07000229', 'E07000229', 'E07000229'),
    postcode = c('CW6 9TX', 'CW6 9TX', 'PL7 1LU', 'BN14 9EP', 'BN14 9EP', 'BN14 9EP', 'BN14 9EP', 'BN14 9EP')
  )

  result <- uk_patient_id(
    data = pd,
    id = list(
      nhs_number = 'nhs_number',
      hospital_number = 'local_patient_identifier',
      date_of_birth = 'patient_birth_date',
      sex_mfu = 'sex',
      forename = 'forename',
      surname = 'surname',
      postcode = 'postcode'
    ),
    .sortOrder = 'specimen_date',
    .forceCopy = TRUE,
    .useStages = 7
  )[]

  # Define the expected ID column
  expected_id <- c(1, 2, 2, 4, 4, 4, 4, 5)

  # Check if the result matches the expected values
  expect_equal(result$id, expected_id)
})

