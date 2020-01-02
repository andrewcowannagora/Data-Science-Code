USE AdventureWorks2017
GO

SELECT a.BusinessEntityID, a.FirstName, a.LastName, b.PhoneNumber
FROM Person.Person AS a
INNER JOIN Person.PersonPhone AS b
	ON a.BusinessEntityID = b.BusinessEntityID;