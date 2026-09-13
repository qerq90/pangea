-- +goose Up
-- +goose StatementBegin
ALTER TABLE heroes ADD COLUMN npc_quests JSONB;
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
ALTER TABLE heroes DROP COLUMN npc_quests;
-- +goose StatementEnd
