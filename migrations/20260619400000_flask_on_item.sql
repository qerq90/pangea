-- +goose Up
ALTER TABLE items
    ADD COLUMN flask_effect JSONB     NULL,
    ADD COLUMN charges      INT       NULL,
    ADD COLUMN max_charges  INT       NULL;

ALTER TABLE heroes
    DROP COLUMN IF EXISTS flask_charges;

ALTER TABLE heroes
    DROP CONSTRAINT IF EXISTS heroes_flask_charges_max;

-- +goose Down
ALTER TABLE items
    DROP COLUMN IF EXISTS flask_effect,
    DROP COLUMN IF EXISTS charges,
    DROP COLUMN IF EXISTS max_charges;

ALTER TABLE heroes
    ADD COLUMN flask_charges INT NOT NULL DEFAULT 8;
