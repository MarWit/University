# Zadanie 1

```sql
SELECT DISTINCT a.City FROM SalesLT.SalesOrderHeader as h, SalesLT.Address as a WHERE h.ShipToAddressID=a.AddressID ORDER BY a.City
```

# Zadanie 2

```sql
SELECT pm.Name, COUNT(pm.ProductModelID) as "Count" FROM SalesLT.ProductModel as pm, SalesLT.Product as p WHERE pm.ProductModelID=p.ProductModelID GROUP BY pm.Name HAVING COUNT(pm.ProductModelID) > 1
```

# Zadanie 3

```sql
SELECT a.City, COUNT(c.CustomerID) as "# of clients", COUNT(distinct c.SalesPerson) as "# of sales person" FROM SalesLT.Address as a, SalesLT.Customer as c,
SalesLT.CustomerAddress as ca WHERE ca.AddressID = a.AddressID and ca.CustomerID = c.CustomerID GROUP BY a.City
```

# Zadanie 4

```sql
SELECT pc.Name, p.Name FROM SalesLT.Product as p, SalesLT.ProductCategory as pc WHERE pc.ParentProductCategoryID is NULL and p.ProductCategoryID = pc.ProductCategoryID -- ????

SELECT pc.Name, p.Name
FROM
	(SELECT DISTINCT ParentProductCategoryID FROM SalesLT.ProductCategory WHERE ParentProductCategoryID IS NOT NULL) as t,
	SalesLT.Product as p INNER JOIN
	SalesLT.ProductCategory as pc ON p.ProductCategoryID = pc.ProductCategoryID
WHERE t.ParentProductCategoryID = p.ProductCategoryID
```

# Zadanie 5

```sql
SELECT c.LastName, c.FirstName, SUM(od.UnitPriceDiscount) as "Discount" FROM SalesLT.Customer as c INNER JOIN SalesLT.SalesOrderHeader as oh ON c.CustomerID = oh.CustomerID INNER JOIN SalesLT.SalesOrderDetail as od ON oh.SalesOrderID = od.SalesOrderID GROUP BY c.CustomerID, c.FirstName, c.LastName
```

# Zadanie 6

```sql
WITH t(sum_) as (SELECT SUM(od.UnitPrice) as sum_ FROM SalesLT.SalesOrderHeader as oh INNER JOIN SalesLT.SalesOrderDetail as od ON oh.SalesOrderID = od.SalesOrderID GROUP BY oh.CustomerID)
SELECT c.LastName, c.FirstName, SUM(od.UnitPrice) as "Money spent" FROM SalesLT.Customer as c INNER JOIN SalesLT.SalesOrderHeader as oh ON c.CustomerID = oh.CustomerID INNER JOIN SalesLT.SalesOrderDetail as od ON oh.SalesOrderID = od.SalesOrderID, t
GROUP BY c.CustomerID, c.FirstName, c.LastName
HAVING SUM(od.UnitPrice) > AVG(t.sum_)
```

# Zadanie 7

```sql
CREATE TABLE SalesLT.OrdersToProcess( SalesOrderID INT, Delayed BIT, FOREIGN KEY (SalesOrderID) REFERENCES SalesLT.SalesOrderHeader(SalesOrderID))

MERGE INTO SalesLT.OrdersToProcess as tp
USING (SELECT SalesOrderID, ShipDate, (CASE WHEN DueDate > GETDATE() and ShipDate is null THEN 1 ELSE 0 END) as OrderDelayed FROM SalesLT.SalesOrderHeader) as oh
ON (tp.SalesOrderID = oh.SalesOrderID)
WHEN MATCHED AND oh.ShipDate is not null THEN
	DELETE
WHEN MATCHED AND tp.Delayed <> oh.OrderDelayed THEN
	UPDATE SET tp.Delayed = oh.OrderDelayed
WHEN NOT MATCHED THEN
	INSERT (SalesOrderID, Delayed) VALUES (oh.SalesOrderID, oh.OrderDelayed);
```

# Zadanie 8

```sql
ALTER TABLE SalesLT.Customer ADD CreditCardNumber VARCHAR(19) NOT NULL DEFAULT '0000-0000-0000-0000'
```

# Zadanie 9

```sql
UPDATE SalesLT.SalesOrderHeader SET CreditCardApprovalCode='abc' WHERE SalesOrderID=71774
UPDATE SalesLT.SalesOrderHeader SET CreditCardApprovalCode='abc' WHERE SalesOrderID=71846
UPDATE SalesLT.SalesOrderHeader SET CreditCardApprovalCode='abc' WHERE SalesOrderID=71946
SELECT * FROM SalesLT.SalesOrderHeader WHERE CreditCardApprovalCode is NOT NULL
```

# Zadanie 10

```sql
CREATE TABLE SalesLT.Test(id int IDENTITY(1000,10))
CREATE TABLE SalesLT.Test2(id int IDENTITY(42, 42))

INSERT SalesLT.Test DEFAULT VALUES
INSERT SalesLT.Test DEFAULT VALUES
INSERT SalesLT.Test DEFAULT VALUES
INSERT SalesLT.Test2 DEFAULT VALUES
INSERT SalesLT.Test2 DEFAULT VALUES
INSERT SalesLT.Test2 DEFAULT VALUES


SELECT @@IDENTITY
SELECT IDENT_CURRENT('SalesLT.Test')
SELECT IDENT_CURRENT('SalesLT.Test2')
```
