-- +goose Up
-- +goose StatementBegin
ALTER TABLE heroes ADD COLUMN lore_data JSONB;
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
ALTER TABLE heroes DROP COLUMN lore_data;
-- +goose StatementEnd
