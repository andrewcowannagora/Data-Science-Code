USE AdventureWorks2017
GO

SELECT FirstName, LastName,
	CONCAT(FirstName, ' ', MiddleName, ' ', LastName) AS FullName,
	CONCAT_WS(' ', FirstName, MiddleName, LastName) AS WithSeperators
FROM Person.Person;
