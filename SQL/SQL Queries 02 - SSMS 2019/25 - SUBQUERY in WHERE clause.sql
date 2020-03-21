USE AdventureWorks2017;
GO

SELECT SalesOrderID, SUM(LineTotal) AS OrderTotal
FROM Sales.SalesOrderDetail
GROUP BY SalesOrderID
HAVING SUM(LineTotal) > 

	(SELECT AVG(ResultTable.MyValues) AS AverageValue
	FROM (SELECT SUM(LineTotal) AS MyValues
	FROM Sales.SalesOrderDetail
	GROUP BY SalesOrderID) AS ResultTable);