-- +goose Up
ALTER TABLE heroes
    ALTER COLUMN flask_charges SET DEFAULT 8,
    ADD CONSTRAINT heroes_flask_charges_max CHECK (flask_charges <= 8);

-- +goose Down
ALTER TABLE heroes
    ALTER COLUMN flask_charges SET DEFAULT 1,
    DROP CONSTRAINT heroes_flask_charges_max;
