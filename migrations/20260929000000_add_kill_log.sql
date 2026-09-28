-- +goose Up
-- +goose StatementBegin
ALTER TABLE heroes ADD COLUMN kill_log JSONB NOT NULL DEFAULT '{}'::jsonb;
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
ALTER TABLE heroes DROP COLUMN kill_log;
-- +goose StatementEnd
