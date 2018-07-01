CREATE TYPE DeliveryStatus AS ENUM (
    'UNKNOWN',
    'BOOKED',
    'READY',
    'ONBOARD',
    'DELIVERED'
);

CREATE TABLE delivery (
    id SERIAL PRIMARY KEY,
    status DeliveryStatus NOT NULL,
    features TEXT NOT NULL DEFAULT '',
    last_location INT NOT NULL,
    last_update DATE NOT NULL,
    size_x FLOAT,
    size_y FLOAT,
    size_z FLOAT,
    weight FLOAT
)
