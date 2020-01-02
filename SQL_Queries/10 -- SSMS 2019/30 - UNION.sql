USE AdventureWorks2017;
GO

--Union
SELECT ProductCategoryID, NULL AS ProductSubcategoryID, Name
FROM Production.ProductCategory

UNION

SELECT ProductCategoryID, ProductSubcategoryID, Name
FROM Production.ProductSubcategory;

