-- +goose Up
-- +goose StatementBegin
ALTER TABLE heroes ADD COLUMN daily_quests JSONB NOT NULL DEFAULT '{}'::jsonb;
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
ALTER TABLE heroes DROP COLUMN daily_quests;
-- +goose StatementEnd
