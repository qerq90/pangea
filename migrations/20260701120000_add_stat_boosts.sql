-- +goose Up
-- +goose StatementBegin
ALTER TABLE heroes ADD COLUMN stat_boosts JSONB NOT NULL DEFAULT '{"boosts":[]}'::jsonb;
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
ALTER TABLE heroes DROP COLUMN stat_boosts;
-- +goose StatementEnd
