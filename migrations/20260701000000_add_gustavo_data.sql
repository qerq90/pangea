-- +goose Up
-- +goose StatementBegin
ALTER TABLE heroes ADD COLUMN gustavo_data JSONB;
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
ALTER TABLE heroes DROP COLUMN gustavo_data;
-- +goose StatementEnd
