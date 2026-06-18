-- +goose Up
ALTER TABLE heroes ADD COLUMN trauma_until BIGINT;

-- +goose Down
ALTER TABLE heroes DROP COLUMN trauma_until;
