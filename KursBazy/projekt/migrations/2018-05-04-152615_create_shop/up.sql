CREATE TABLE shop (
    id SERIAL PRIMARY KEY,
    name VARCHAR(80) NOT NULL,
    city VARCHAR(60) NOT NULL,
    zipcode CHAR(10) NOT NULL,
    address1 VARCHAR(60) NOT NULL,
    address2 VARCHAR(60)
)
