USE AdventureWorks2017;
GO

SELECT BusinessEntityID, FirstName, LastName,
	(SELECT JobTitle
	FROM HumanResources.Employee
	WHERE BusinessEntityID = MyPeople.BusinessEntityID) AS JobTitle
FROM Person.Person AS MyPeople
WHERE EXISTS (SELECT JobTitle
	FROM HumanResources.Employee
	WHERE BusinessEntityID = MyPeople.BusinessEntityID);

-- may need correlated subquery based on database setup