-- +goose Up
-- +goose StatementBegin
ALTER TABLE heroes ADD COLUMN max_dungeon_level INT NOT NULL DEFAULT 1;
-- максимально доступный этаж: открывается победой над «Отмеченным тьмой» на текущем этаже
UPDATE heroes SET max_dungeon_level = dungeon_level WHERE dungeon_level > max_dungeon_level;
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
ALTER TABLE heroes DROP COLUMN max_dungeon_level;
-- +goose StatementEnd
