-- +goose Up
-- +goose StatementBegin
ALTER TABLE heroes ADD COLUMN card_seller_data JSONB;
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
ALTER TABLE heroes DROP COLUMN card_seller_data;
-- +goose StatementEnd
