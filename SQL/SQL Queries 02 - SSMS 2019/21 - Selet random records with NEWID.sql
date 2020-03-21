USE AdventureWorks2017
GO

SELECT TOP 10 WorkOrderID, NEWID() AS NewID
FROM Production.WorkOrder
ORDER BY NewID;
	