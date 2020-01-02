USE AdventureWorks2017;
GO

SELECT ProductID
FROM Production.ProductProductPhoto

INTERSECT

SELECT ProductID
FROM Production.ProductReview;

-- Join Version
SELECT DISTINCT a.ProductID
FROM Production.ProductProductPhoto AS a
	INNER JOIN Production.ProductReview AS b
	ON a.ProductID = b.ProductID;