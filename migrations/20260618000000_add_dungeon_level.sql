-- +goose Up
-- +goose StatementBegin
ALTER TABLE heroes ADD COLUMN dungeon_level INT NOT NULL DEFAULT 1;
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
ALTER TABLE heroes DROP COLUMN dungeon_level;
-- +goose StatementEnd
