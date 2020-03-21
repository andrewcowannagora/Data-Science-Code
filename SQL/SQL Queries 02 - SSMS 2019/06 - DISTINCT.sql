USE AdventureWorks2017
GO

SELECT DISTINCT City, StateProvinceID
FROM Person.Address
ORDER BY City;