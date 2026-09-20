-- +goose Up
-- +goose StatementBegin
ALTER TABLE heroes ADD COLUMN rune_data JSONB NOT NULL DEFAULT '{}'::jsonb;
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
ALTER TABLE heroes DROP COLUMN rune_data;
-- +goose StatementEnd
