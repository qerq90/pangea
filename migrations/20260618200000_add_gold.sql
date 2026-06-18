-- +goose Up
ALTER TABLE heroes ADD COLUMN gold BIGINT NOT NULL DEFAULT 0;

-- +goose Down
ALTER TABLE heroes DROP COLUMN gold;
