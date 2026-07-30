-- +goose Up
-- +goose StatementBegin
ALTER TABLE heroes ADD COLUMN azat_data JSONB;
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
ALTER TABLE heroes DROP COLUMN azat_data;
-- +goose StatementEnd
