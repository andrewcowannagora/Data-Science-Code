USE AdventureWorks2017;
GO

DECLARE @VarColor VARCHAR(20) = 'Blue';

SELECT ProductID, Name, ProductNumber, Color, ListPrice
FROM Production.Product
WHERE Color = @VarColor;