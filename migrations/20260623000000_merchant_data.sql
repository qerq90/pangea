-- +goose Up
-- +goose StatementBegin
ALTER TABLE heroes ADD COLUMN merchant_data JSONB;
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
ALTER TABLE heroes DROP COLUMN merchant_data;
-- +goose StatementEnd
