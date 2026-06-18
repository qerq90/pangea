-- +goose Up
ALTER TABLE heroes ADD COLUMN trauma_name VARCHAR;

-- +goose Down
ALTER TABLE heroes DROP COLUMN trauma_name;
