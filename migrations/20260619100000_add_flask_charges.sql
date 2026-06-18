-- +goose Up
ALTER TABLE heroes ADD COLUMN flask_charges INT NOT NULL DEFAULT 1;

-- +goose Down
ALTER TABLE heroes DROP COLUMN flask_charges;
