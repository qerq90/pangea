-- +goose Up
-- +goose StatementBegin
ALTER TABLE heroes ADD COLUMN guild_reputation BIGINT NOT NULL DEFAULT 0;
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
ALTER TABLE heroes DROP COLUMN guild_reputation;
-- +goose StatementEnd
