USE AdventureWorks2017
GO

SELECT Person.BusinessEntityID, Person.PersonType, Person.FirstName, Person.LastName, Employee.JobTitle
FROM Person.Person
LEFT JOIN HumanResources.Employee
ON Person.BusinessEntityID = Employee.BusinessEntityID;