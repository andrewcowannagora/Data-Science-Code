USE AdventureWorks2017
GO

SELECT Color, COUNT(*) AS NumProd
FROM Production.Product
WHERE Color IS NOT NULL
GROUP BY Color
HAVING COUNT(*) > 25;