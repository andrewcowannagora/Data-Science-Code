USE AdventureWorks2017
GO

SELECT BusinessEntityID, SalesYTD,
	IIF(SalesYTD > 2000000, 'Met', 'Has Not Met') AS Status
FROM Sales.SalesPerson;


SELECT IIF(SalesYTD > 2000000, 'Met', 'Has Not Met') AS Status,
	COUNT(*)
FROM Sales.SalesPerson
GROUP BY IIF(SalesYTD > 2000000, 'Met', 'Has Not Met');