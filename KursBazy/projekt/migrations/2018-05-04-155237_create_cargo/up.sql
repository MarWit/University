CREATE TABLE cargo (
    id SERIAL PRIMARY KEY,
    shop_id INT NOT NULL REFERENCES shop(id),
    delivery_id INT NOT NULL REFERENCES delivery(id),
    full_name VARCHAR(80) NOT NULL,
    city VARCHAR(60) NOT NULL,
    zipcode CHAR(10) NOT NULL,
    address1 VARCHAR(60) NOT NULL,
    address2 VARCHAR(60)
)
