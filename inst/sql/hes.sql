/***********************************************
NHS Digital: Hospital Episode Statistics
Base query for the HES data set
Data is submitted by the quarterly.

Note: data may be in flat table; or separated
into the base, with OPCS and ICD10 as skinny tables.
We are given both long and wide format options.
Join as appropriate for analysis.

Values
::DBN:: The database you storing the tables
::PIITBL:: The table or view containing HES PII

Recommend
WHERE clause
eg. WHERE HES.[ADMIDATE_DV] >= '2021-01-01'

or
INNER JOIN [::DBN2::].[dbo].[::PIITABLE::]
        ON [::PIITBL::].NHSNumber = SUS.NhsNumber

	JOIN METHOD can be combined with UNION
	to combine multiple identifiers

***********************************************/
SELECT DISTINCT
	apc.[FYEAR]
	,apc.[EPIKEY]
	,apc.[ENCRYPTED_HESID]
	,apc.[ADMIAGE]
	,apc.[SEX]
	,apc.[ETHNOS]
	,apc.[IMD04]
	,apc.[ADMIDATE_DV]
	,apc.[ADMIMETH]
	,apc.[ADMINCAT]
	,apc.[ADMISORC]
	,apc.[ADMISTAT]
	,apc.[DISDATE_DV]
	,apc.[DISDEST]
	,apc.[DISMETH]
	,apc.[SPELBGIN]
	,apc.[SPELDUR]
	,apc.[SPELEND]
	,apc.[PROCODE]
	,apc.[PROCODE3]
	,apc.[PROVSPNOPS]
	,apc.[EPIDUR]
	,apc.[EPIEND]
	,apc.[EPIEND_DV]
	,apc.[EPIORDER]
	,apc.[EPISTART]
	,apc.[EPISTART_DV]
	,apc.[EPISTAT]
	,apc.[EPITYPE]
	,apc.[MAINSPEF]
	,apc.[TRETSPEF]
	,apc.[FAE]
	,apc.[FAE_EMERGENCY]
	,apc.[FCE]
	,apc.[FDE]
	,diag.*
	,oper.*
FROM [HES_APC].[dbo].[vtHES_APC] apc
INNER JOIN [HES_APC].[dbo].[vtHES_APC_DIAG] diag ON apc.FYEAR=diag.FYEAR AND apc.EPIKEY=diag.EPIKEY
INNER JOIN [HES_APC].[dbo].[vtHES_APC_OPERTN] oper ON apc.FYEAR=oper.FYEAR AND apc.EPIKEY=oper.EPIKEY
INNER JOIN [::DBN::].[dbo].[::PIITBL::] pid ON apc.FYEAR=pid.FYEAR AND apc.EPIKEY=pid.EPIKEY
WHERE apc.[ADMIDATE_DV] >= '2020-01-01'


