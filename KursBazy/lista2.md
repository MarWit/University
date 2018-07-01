# Zadanie 1

```sql
CREATE FUNCTION SomeFunction (@days int)
RETURNS TABLE
AS
RETURN
(
	SELECT c.PESEL, COUNT(*) LiczbaEgzemplarzy
	FROM Czytelnik c INNER JOIN Wypozyczenie w ON c.Czytelnik_ID = w.Czytelnik_ID
	WHERE w.Liczba_Dni > @days
	GROUP BY c.PESEL
)
```

# Zadanie 2

```sql
DROP PROCEDURE IF EXISTS randomFill
CREATE PROCEDURE randomFill (@n int)
AS
BEGIN
    DECLARE @liczba_imion int
    DECLARE @liczba_nazwisk int

    SET @liczba_imion = (SELECT COUNT(*) FROM imiona)
    SET @liczba_nazwisk = (SELECT COUNT(*) FROM nazwiska)

    IF (2 * @n > @liczba_imion * @liczba_nazwisk )
    BEGIN
        THROW 510000, 'n is too big', 1
        RETURN
    END

	DECLARE @imie varchar(30)
	DECLARE @nazwisko varchar(30)
	
	TRUNCATE TABLE dane
	
	WHILE @n > 0
	BEGIN
		SET @imie = (SELECT TOP 1 imie FROM imiona ORDER BY NEWID())
		SET @nazwisko = (SELECT TOP 1 nazwisko FROM nazwiska ORDER BY NEWID())
		
		IF EXISTS (SELECT * FROM dane WHERE imie = @imie AND nazwisko = @nazwisko)
			CONTINUE
		ELSE
		BEGIN
			SET @n = @n - 1
			INSERT dane VALUES (@imie, @nazwisko)
		END 
	END 
END 
```

# Zadanie 3

```sql
CREATE PROCEDURE newReader ( @pesel char(11), @nazwisko varchar(30), @miasto varchar(30), @data_urodzenia date, @ostatnie_wypozyczenie date = NULL )
AS
BEGIN
IF( LEN(@pesel) <> 11 or (ISNUMERIC(@pesel) <> 1) )
BEGIN
	THROW 510000, 'PESEL has invalid format!', 1
	RETURN
END
IF( UNICODE(LOWER(SUBSTRING(@nazwisko,1,1))) = UNICODE(SUBSTRING(@nazwisko,1,1)) )
BEGIN
	THROW 520000, 'Last name should start with capital letter!', 1
	RETURN
END
IF(
	(CAST( SUBSTRING(@pesel,1,2) as int ) <> (YEAR(@data_urodzenia) % 100)) OR
	(CAST( SUBSTRING(@pesel,3,2) as int ) <> MONTH(@data_urodzenia)) OR
	(CAST( SUBSTRING(@pesel,5,2) as int ) <> DAY(@data_urodzenia))
)
BEGIN
	THROW 530000, 'Birth date and pesel does not match!', 1
	RETURN
END
INSERT INTO Czytelnik(PESEL, Nazwisko, Miasto, Data_Urodzenia, Ostatnie_Wypozyczenie)
VALUES (@pesel, @nazwisko, @miasto, @data_urodzenia, @ostatnie_wypozyczenie)
END
```

# Zadanie 4

```sql
CREATE TYPE CzytelnikTableType AS TABLE ( Czytelnik_ID int )

CREATE PROCEDURE listaCzytelnikow @list CzytelnikTableType READONLY
AS
SELECT c.Czytelnik_ID, SUM(w.Liczba_Dni) Suma_dni
FROM Wypozyczenie w INNER JOIN list ON w.Czytelnik_ID = list.Czytelnik_ID
GROUP BY c.Czytelnik_ID
```

# Zadanie 5

```sql
CREATE PROCEDURE szukajKsiazki (@Tytul varchar(300) = NULL, @Autor varchar(200) = NULL, @RokWydania int = NULL)
AS
BEGIN
DECLARE @QUERY NVARCHAR(4000)
DECLARE @COLUMNS NVARCHAR(4000)
DECLARE @PARAMS NVARCHAR(4000)
DECLARE @GROUPIN NVARCHAR(4000)
SET @QUERY = N' FROM Ksiazka k JOIN Egzemplarz e ON k.Ksiazka_ID = e.Ksiazka_ID WHERE 1 = 1'
SET @COLUMNS = N'COUNT(*) Ilosc'
SET @PARAMS = '@TytulParam varchar(300), @AutorParam varchar(200), @RokWydaniaParam int'
SET @GROUPIN = ''
IF (@Tytul IS NOT NULL)
BEGIN
	SET @QUERY = @QUERY + N' AND k.Tytul = @TytulParam'
    SET @COLUMNS = @COLUMNS + N', k.Tytul'
    SET @GROUPIN = N'k.Tytul'
END
IF (@Autor IS NOT NULL)
BEGIN
	SELECT @QUERY = @QUERY + N' AND k.Autor = @AutorParam'
    SET @COLUMNS = @COLUMNS + N', k.Autor'
    IF (LEN(@GROUPIN) > 0)
    	SET @GROUPIN = @GROUPIN + N', k.Autor'
    ELSE
    	SET @GROUPIN = N'k.Autor'
END
IF (@RokWydania IS NOT NULL)
BEGIN
	SELECT @QUERY = @QUERY + N' AND k.Rok_Wydania = @RokWydaniaParam'
    SET @COLUMNS = @COLUMNS + N', k.Rok_Wydania'
    IF (LEN(@GROUPIN) > 0)
    	SET @GROUPIN = @GROUPIN + N', k.Rok_Wydania'
    ELSE
    	SET @GROUPIN = N'k.Rok_Wydania'
END
IF (LEN(@GROUPIN) > 0)
	SET @GROUPIN = N' GROUP BY ' + @GROUPIN
SET @QUERY = N'SELECT ' + @COLUMNS + @QUERY + @GROUPIN
EXEC sp_executeSQL 
	@QUERY, 
	@PARAMS,
	@TytulParam = @Tytul,
	@AutorParam = @Autor,
	@RokWydaniaParam = @RokWydania
END
```
