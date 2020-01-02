USE AdventureWorks2017
GO

SELECT Name, ProductNumber, 'AdventureWorks' AS Manuacturer, ListPrice,
	ListPrice * 0.85 AS SalePrice
FROM Production.Product
WHERE ListPrice > 0;