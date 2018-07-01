# Zadanie 1

```sql
declare c_towary cursor for SELECT t.ID, c.Cena, k.Waluta, k.CenaPLN 
	FROM Towary t 
	CROSS JOIN Kursy k 
	JOIN Ceny c ON t.ID = c.TowarID 
	WHERE c.Waluta = 'PLN' and k.Waluta <> 'PLN'
	
open c_towary
declare @idx INT
declare @pln FLOAT
declare @waluta char(3)
declare @przelicznik float

fetch next from c_towary into @idx, @pln, @waluta, @przelicznik
while ( @@fetch_status=0 )
BEGIN
	IF EXISTS (SELECT * FROM Ceny WHERE TowarID=@idx and Waluta=@waluta)
	BEGIN
		UPDATE Ceny SET Cena=(@pln / @przelicznik) WHERE TowarID=@idx and Waluta=@waluta
	END
	ELSE
	BEGIN
		INSERT Ceny VALUES (@idx, @waluta, (@pln / @przelicznik))
	END
	
	fetch next from c_towary into @idx, @pln, @waluta, @przelicznik
END 
close c_towary
deallocate c_towary

MERGE Ceny
USING (SELECT * from Kursy) as k
ON (Ceny.Waluta = k.Waluta)
WHEN NOT MATCHED BY SOURCE
	THEN DELETE
```

# Zadanie 2

```sql
CREATE TABLE Employees(id int identity, SalaryGross float)
CREATE TABLE SalaryHistory(id int identity, EmployeeID int, year int, month int, SalaryNet float, SalaryGross float)

CREATE TABLE SalaryLog(EmployeeID int, months int)

CREATE PROCEDURE calculateSalary( @month int ) AS
BEGIN
    DECLARE cur CURSOR FOR (SELECT EmployeeID, SUM(SalaryGross) as sum_, COUNT(SalaryGross) as cnt_ FROM SalaryHistory WHERE year = YEAR(GETDATE()) and month < @month GROUP BY EmployeeID)
    DECLARE @Eidx int
    DECLARE @SalaryGross FLOAT
    DECLARE @SalaryGrossCount int
    DECLARE @SalaryGrossSum FLOAT
    DECLARE @percent FLOAT

    FETCH NEXT FROM cur INTO @Eidx, @SalaryGrossSum, @SalaryGrossCount

    WHILE (@@fetch_status = 0)
    BEGIN
        IF @SalaryGrossCount + 1 < @month
        BEGIN
            IF EXISTS(SELECT EmployeeID FROM SalaryLog WHERE EmployeeID = @Eidx)
                UPDATE SalaryLog SET months = @month - @SalaryGrossCount + 1 WHERE EmployeeID = @Eidx
            ELSE
                INSERT SalaryLog VALUES (@Eidx, @month - @SalaryGrossCount + 1)

            FETCH NEXT FROM cur INTO @Eidx, @SalaryGrossSum, @SalaryGrossCount
            CONTINUE
        END

        SELECT @SalaryGross = SalaryGross FROM Employees WHERE id = @Eidx
        IF @SalaryGross + @SalaryGrossSum > 85528.0
            SET @percent = 0.32
        ELSE
            SET @percent = 0.18

        INSERT INTO SalaryHistory(EmployeeID, year, month, SalaryNet, SalaryGross) VALUES
            (@Eidx, YEAR(GETDATE()), @month, @SalaryGross * @percent, @SalaryGross * (1 - @percent) )

        FETCH NEXT FROM cur INTO @Eidx, @SalaryGrossSum, @SalaryGrossCount
    END

    CLOSE cur
    DEALLOCATE cur
END
```

# Zadanie 3

```sql
CREATE TABLE Bufor(ID int IDENTITY PRIMARY KEY, AdresUrl varchar(200), OstatnieWejscie DATETIME)
CREATE TABLE Historia(ID int IDENTITY PRIMARY KEY, AdresUrl varchar(200), OstatnieWejscie DATETIME)
CREATE TABLE Parametry(nazwa varchar(100) PRIMARY KEY, wartosc varchar(100))
INSERT Parametry VALUES ('max_cache', '2')

CREATE TRIGGER tr_insert_bufor ON Bufor INSTEAD OF INSERT
AS BEGIN
	declare @website varchar(200)
	declare @new_date datetime
	SELECT @website = AdresUrl, @new_date = OstatnieWejscie FROM inserted
	
	IF EXISTS(SELECT * FROM Bufor WHERE AdresUrl = @website)
	BEGIN
		UPDATE Bufor SET OstatnieWejscie=@new_date WHERE AdresUrl = @website
		RETURN
	END 
	
	declare @max_cache INT
	set @max_cache = (select CAST(wartosc as int) FROM Parametry WHERE nazwa = 'max_cache')
	
	IF (SELECT COUNT(*) FROM Bufor) = @max_cache
	BEGIN
		declare @idx int
		declare @old_website varchar(200)
		declare @old_date DATETIME
		SELECT TOP 1 @idx = ID, @old_website = AdresUrl, @old_date = OstatnieWejscie FROM Bufor ORDER BY OstatnieWejscie ASC
		DELETE FROM Bufor WHERE ID=@idx
		INSERT INTO Historia (AdresUrl, OstatnieWejscie) VALUES (@old_website, @old_date)
	END

	INSERT INTO Bufor (AdresUrl, OstatnieWejscie) VALUES (@website, @new_date)
END 

CREATE TRIGGER tr_insert_historia ON Historia INSTEAD OF INSERT
AS BEGIN
	declare @website varchar(200)
	declare @new_date datetime
	SELECT @website = AdresUrl, @new_date = OstatnieWejscie FROM inserted
	
	IF EXISTS(SELECT * FROM Historia WHERE AdresUrl = @website)
	BEGIN
		UPDATE Historia SET OstatnieWejscie=@new_date WHERE AdresUrl = @website
	END
	ELSE
	BEGIN
		INSERT INTO Historia (AdresUrl, OstatnieWejscie) VALUES (@website, @new_date)
	END 
END 
```
