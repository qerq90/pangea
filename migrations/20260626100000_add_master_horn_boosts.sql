-- +goose Up
-- +goose StatementBegin
ALTER TABLE heroes ADD COLUMN master_horn_boosts JSONB NOT NULL DEFAULT '{}'::jsonb;
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
ALTER TABLE heroes DROP COLUMN master_horn_boosts;
-- +goose StatementEnd
