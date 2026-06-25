-- +goose Up
-- +goose StatementBegin
ALTER TABLE heroes ADD COLUMN return_state TEXT;
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
ALTER TABLE heroes DROP COLUMN return_state;
-- +goose StatementEnd
