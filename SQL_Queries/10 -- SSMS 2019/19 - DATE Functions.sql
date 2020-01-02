USE AdventureWorks2017
GO

SELECT BusinessEntityID, HireDate,
	YEAR(HireDate) AS HireYear,
	MONTH(HireDate) AS HireMonth,
	DAY(HireDate) AS HireDay
FROM HumanResources.Employee;


SELECT YEAR(HireDate), COUNT(*) AS NewHires
FROM HumanResources.Employee
GROUP BY YEAR(HireDate);

SELECT GETDATE()
SELECT GETUTCDATE()

SELECT BusinessEntityID, HireDate,
	DATEDIFF(day, HireDate, GETDATE()) AS DaysSinceHire
FROM HumanResources.Employee;

SELECT BusinessEntityID, HireDate,
	DATEDIFF(year, HireDate, GETDATE()) AS YearsSinceHire,
	DATEADD(year, 10, HireDate) AS AnniversaryDate
FROM HumanResources.Employee;