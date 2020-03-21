USE AdventureWorks2017
GO

SELECT Name, TaxRate
FROM Sales.SalesTaxRate
WHERE (TaxRate >= 7) AND (TaxRate <= 10);