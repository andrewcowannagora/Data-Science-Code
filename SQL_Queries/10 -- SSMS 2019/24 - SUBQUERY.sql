USE AdventureWorks2017;
GO

SELECT BusinessEntityID, SalesYTD,

	(SELECT MAX(SalesYTD) 
	FROM Sales.SalesPerson) AS HighestSalesYTD,

	(SELECT MAX(SalesYTD) 
	FROM Sales.SalesPerson) - SalesYTD AS SalesGap

FROM Sales.SalesPerson
ORDER BY SalesYTD DESC;
