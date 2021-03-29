/***********************************************
NHS Digital: Emergency Care Data Set
Base SQL query for a standard ECDS feed
Data is submitted daily by NHS Trusts

Values
::DATABASE::
::ECDS_VIEW::

Recommend
WHERE clause
eg. WHERE ECDS.ArrivalDate >= '2021-01-01'

or
INNER JOIN [::DATABASE2::].[dbo].[::PIITABLE::] 
	ON [::PIITABLE].NHSNumber = ECDS.NhsNumber

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
FROM [::DATABASE::].[dbo].[::ECDS_VIEW::] ECDS

