USE AdventureWorks2017
GO

SELECT *
FROM HumanResources.Department
WHERE GroupName IN ('Research and Development', 'Quality Assurance');