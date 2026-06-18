-- +goose Up
-- +goose StatementBegin
ALTER TABLE heroes ADD COLUMN scene_data JSONB;
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
ALTER TABLE heroes DROP COLUMN scene_data;
-- +goose StatementEnd
