USE AdventureWorks2017;
GO

SELECT ProductLine, AVG(ListPrice) AS AvgPrice
FROM Production.Product
WHERE ProductLine IS NOT NULL
GROUP BY ProductLine;

-- Pivot Results

SELECT 'Avg List Price' AS 'Product Line',
       M,R,S,T
FROM (SELECT ProductLine, ListPrice
		FROM Production.Product) AS SourceData
PIVOT (AVG(ListPrice) FOR ProductLine IN (M,R,S,T)) AS PivotTable;