-- +goose Up
-- +goose StatementBegin
ALTER TABLE heroes ADD COLUMN trauma_names TEXT NOT NULL DEFAULT '';
UPDATE heroes SET trauma_names = COALESCE(trauma_name, '');
ALTER TABLE heroes DROP COLUMN trauma_name;
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
ALTER TABLE heroes ADD COLUMN trauma_name VARCHAR;
UPDATE heroes SET trauma_name = NULLIF(trauma_names, '');
ALTER TABLE heroes DROP COLUMN trauma_names;
-- +goose StatementEnd
