USE AdventureWorks2017;
GO

DECLARE @Counter INT = 1;
DECLARE @Product INT = 710;

WHILE @Counter <=3
BEGIN
	SELECT ProductID, Name, ProductNumber, Color, ListPrice
	FROM Production.Product
	WHERE ProductID = @Product;

	SET @Counter = @Counter + 1
	SET @Product = @Product + 10

END
