-- +goose Up
-- +goose StatementBegin
ALTER TABLE heroes ADD COLUMN kills BIGINT NOT NULL DEFAULT 0;
ALTER TABLE heroes ADD COLUMN achievements TEXT NOT NULL DEFAULT '';
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
ALTER TABLE heroes DROP COLUMN achievements;
ALTER TABLE heroes DROP COLUMN kills;
-- +goose StatementEnd
