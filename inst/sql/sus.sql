/***********************************************
NHS Digital: Secondary Use Service
Base query for the SUS data set
Data is submitted by the 21st of each month
for the preceeding months data.

Values
::DBN:: The database you storing the tables
::TBLVIEW:: The table or view containing SUS
::PIITBL:: The table contiaining PII for linking

Recommend
WHERE clause
eg. WHERE SUS.[Episode End Date] >= '2021-01-01'

or
INNER JOIN [::DBN2::].[dbo].[::PIITABLE::]
        ON [::PIITBL::].NHSNumber = SUS.NhsNumber

	JOIN METHOD can be combined with UNION
	to combine multiple identifiers

***********************************************/
SELECT DISTINCT
	SUS.[NHS Number]
	,SUS.[Local Patient Identifier]
	,SUS.[Birth Date]
	,SUS.[Gender Code]
	,SUS.[Episode Age]
	,SUS.[Ethnic Category Code]
	,SUS.[Postcode of usual Address]
	,SUS.[Marital Status]
	,SUS.[Org Code Local Patient Identifier]
	,SUS.[Hospital Provider Spell No]
	,SUS.[Patient Classification]
	,SUS.[Source of Admission (Hospital Provider Spell)]
	,SUS.[ADMINISTRATIVE CATEGORY (ON ADMISSION)]
	,SUS.[Admission Method (Hospital Provider Spell)]
	,SUS.[Discharge Destination (Hospital Provider Spell)]
	,SUS.[Discharge Method (Hospital Provider Spell)]
	,SUS.[Start Date (Hospital Provider Spell)]
	,SUS.[End Date (Hospital Provider Spell)]
	,SUS.[Episode Number]
	,SUS.[Episode Start Date]
	,SUS.[Episode End Date]
	,SUS.[Organisation Code (Code of Provider)]
	,SUS.[Provider Site Code]
	,SUS.[Organisation Code (Code of Commissioner)]
	,SUS.[Organisation Code (Sender)]
	,SUS.[Site code of Treatment (at start of episode)]
	,SUS.[Main Specialty Code]
	,SUS.[Treatment Function Code]
	,SUS.[Referring Organisation Code]
	,SUS.[Episode Duration]
	,SUS.[Episode Duration Grouper]
	,SUS.[Report Period Start Date]
	,SUS.[Report Period End Date]
	,SUS.[Primary Diagnosis Code]
	,SUS.[Secondary Diagnosis Code 1]
	,SUS.[Secondary Diagnosis Code 2]
	,SUS.[Secondary Diagnosis Code 3]
	,SUS.[Secondary Diagnosis Code 4]
	,SUS.[Secondary Diagnosis Code 5]
	,SUS.[Secondary Diagnosis Code 6]
	,SUS.[Secondary Diagnosis Code 7]
	,SUS.[Secondary Diagnosis Code 8]
	,SUS.[Secondary Diagnosis Code 9]
	,SUS.[Secondary Diagnosis Code 10]
	,SUS.[Secondary Diagnosis Code 11]
	,SUS.[Secondary Diagnosis Code 12]
	,SUS.[Secondary Diagnosis Code 13]
	,SUS.[Secondary Diagnosis Code 14]
	,SUS.[Secondary Diagnosis Code 15]
	,SUS.[Secondary Diagnosis Code 16]
	,SUS.[Secondary Diagnosis Code 17]
	,SUS.[Secondary Diagnosis Code 18]
	,SUS.[Secondary Diagnosis Code 19]
	,SUS.[Secondary Diagnosis Code 20]
	,SUS.[Secondary Diagnosis Code 21]
	,SUS.[Secondary Diagnosis Code 22]
	,SUS.[Secondary Diagnosis Code 23]
	,SUS.[Secondary Diagnosis Code 24]
	,SUS.[Primary Procedure Code]
	,SUS.[Primary Procedure Date]
	,SUS.[Secondary Procedure Code 1]
	,SUS.[Secondary Procedure Date 1]
	,SUS.[Secondary Procedure Code 2]
	,SUS.[Secondary Procedure Date 2]
	,SUS.[Secondary Procedure Code 3]
	,SUS.[Secondary Procedure Date 3]
	,SUS.[Secondary Procedure Code 4]
	,SUS.[Secondary Procedure Date 4]
	,SUS.[Secondary Procedure Code 5]
	,SUS.[Secondary Procedure Date 5]
	,SUS.[Secondary Procedure Code 6]
	,SUS.[Secondary Procedure Date 6]
	,SUS.[Secondary Procedure Code 7]
	,SUS.[Secondary Procedure Date 7]
	,SUS.[Secondary Procedure Code 8]
	,SUS.[Secondary Procedure Date 8]
	,SUS.[Secondary Procedure Code 9]
	,SUS.[Secondary Procedure Date 9]
	,SUS.[Secondary Procedure Code 10]
	,SUS.[Secondary Procedure Date 10]
	,SUS.[Secondary Procedure Code 11]
	,SUS.[Secondary Procedure Date 11]
	,SUS.[Secondary Procedure Code 12]
	,SUS.[Secondary Procedure Date 12]
	,SUS.[UploadDate]
FROM [::DBN::].[dbo].[::TBLVIEW::] SUS 
---- INNER JOIN [::DBN2::].[dbo].[::PIITABLE::] ON [::PIITABLE].NHSNumber = SUS.NhsNumber
---- WHERE	SUS.[End Date (Hospital Provider Spell)] >= '20191201'
