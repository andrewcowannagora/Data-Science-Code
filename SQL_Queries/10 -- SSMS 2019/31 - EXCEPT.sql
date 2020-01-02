USE AdventureWorks2017;
GO

-- Except - Distict Rows from first query, not present in second, Union but no duplicates
SELECT BusinessEntityID
FROM Person.Person
WHERE PersonType <> 'EM'

EXCEPT

SELECT BusinessEntityID
FROM Sales.PersonCreditCard

-- Join version
SELECT Person.BusinessEntityID
FROM Person.Person
	LEFT JOIN Sales.PersonCreditCard
	ON Person.BusinessEntityID = PersonCreditCard.BusinessEntityID
WHERE Person.PersonType <> 'EM' AND PersonCreditCard.CreditCardID IS NULL;

