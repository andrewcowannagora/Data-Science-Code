USE AdventureWorks2017
GO

SELECT SalesOrderDetail.ProductID, Product.Name, SUM(SalesOrderDetail.OrderQty) AS TotalQtySold
FROM Sales.SalesOrderDetail
INNER JOIN Production.Product
ON Sales.SalesOrderDetail.ProductID = Production.Product.ProductID
GROUP BY SalesOrderDetail.ProductID, Product.Name
ORDER BY TotalQtySold DESC;