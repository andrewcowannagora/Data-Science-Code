USE AdventureWorks2017
GO

SELECT TOP 5 WITH TIES TaxRate, Name
FROM Sales.SalesTaxRate
ORDER BY TaxRate;