/***********************************************
NHS Digital: Emergency Care Data Set
Base SQL query for a standard ECDS feed
Data is submitted daily by NHS Trusts

Values
::DBN:: The database you storing the tables
::TBLVIEW:: The table or view containing ECDS

Recommend
WHERE clause
eg. WHERE ECDS.ArrivalDate >= '2021-01-01'

or
INNER JOIN [::DBN2::].[dbo].[::PIITBL::] 
	ON [::PIITBL::].NHSNumber = ECDS.NhsNumber

JOIN METHOD can be combined with UNION
to combine multiple identifiers
***********************************************/

SELECT DISTINCT
	ECDS.IdentifierNumber,
	ECDS.ReceivedDate,
	ECDS.OrganisationCodeOfProvider,
	ECDS.PatientPathwayIdentifier,
	ECDS.LocalPatientIdentifier,
	ECDS.NhsNumber,
	ECDS.PatientBirthDate,
	ECDS.PostcodeOfUsualAddress,
	ECDS.PatientStatedGenderCode,
	ECDS.ArrivalDate,
	ECDS.DepartureDate,
	ECDS.EmergencyCareStatusCode,
	ECDS.EmergencyCareDestinationCode,
	ECDS.EmergencyCareChiefComplaintCode
FROM [::DBN::].[dbo].[::TBLVIEW::] ECDS

