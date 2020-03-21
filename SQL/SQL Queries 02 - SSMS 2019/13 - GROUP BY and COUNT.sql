USE AdventureWorks2017
GO

SELECT City, StateProvinceID, COUNT(*) AS CountofAddresses
FROM Person.Address
GROUP BY City, StateProvinceID
ORDER BY CountofAddresses DESC;