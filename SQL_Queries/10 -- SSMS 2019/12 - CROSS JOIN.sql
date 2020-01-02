USE AdventureWorks2017
GO

SELECT A.Name, B.Name
FROM HumanResources.Department AS A
CROSS JOIN HumanResources.Department AS B
WHERE A.Name <> B.Name;