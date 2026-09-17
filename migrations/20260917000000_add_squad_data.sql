-- +goose Up
-- +goose StatementBegin
ALTER TABLE heroes ADD COLUMN squad_data JSONB NOT NULL DEFAULT '{"heroPos":1,"allies":[],"away":{}}'::jsonb;
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
ALTER TABLE heroes DROP COLUMN squad_data;
-- +goose StatementEnd
