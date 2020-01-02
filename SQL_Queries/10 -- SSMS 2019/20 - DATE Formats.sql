USE AdventureWorks2017
GO

SELECT BusinessEntityID, HireDate,
	FORMAT(HireDate, 'dddd, MMMM dd, yyyy') AS FormattedDate,
	FORMAT(HireDate, 'd-MMM') AS FormattedDate2
FROM HumanResources.Employee;
	