-- +goose Up
-- +goose StatementBegin
ALTER TABLE heroes ADD COLUMN quest_data JSONB;
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
ALTER TABLE heroes DROP COLUMN quest_data;
-- +goose StatementEnd
