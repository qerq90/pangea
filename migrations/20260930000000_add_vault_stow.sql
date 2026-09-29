-- +goose Up
-- +goose StatementBegin
ALTER TABLE heroes ADD COLUMN vault_stow JSONB NOT NULL DEFAULT '{}'::jsonb;
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
ALTER TABLE heroes DROP COLUMN vault_stow;
-- +goose StatementEnd
