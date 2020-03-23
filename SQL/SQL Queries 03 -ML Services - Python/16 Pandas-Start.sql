-- Order summary query
SELECT Orders.OrderID, SUM(OrderLines.UnitPrice*OrderLines.Quantity) AS OrderTotal
FROM Sales.Orders INNER JOIN Sales.OrderLines ON Orders.OrderID = OrderLines.OrderID
GROUP BY Orders.OrderID
ORDER BY Orders.OrderID;

-- Python script template
EXECUTE sp_execute_external_script
@language = N'Python',
@script = N'

',
@input_data_1 = N'