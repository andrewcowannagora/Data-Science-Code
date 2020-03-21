USE AdventureWorks2017
GO

SELECT WorkOrderID, ScrappedQty, ScrapReasonID
FROM Production.WorkOrder
WHERE ScrapReasonID IS NOT NULL;