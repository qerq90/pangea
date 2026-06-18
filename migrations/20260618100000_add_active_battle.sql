-- +goose Up
-- +goose StatementBegin
ALTER TABLE heroes ADD COLUMN active_battle JSONB;
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
ALTER TABLE heroes DROP COLUMN active_battle;
-- +goose StatementEnd
