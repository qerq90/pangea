-- +goose Up
-- +goose StatementBegin
ALTER TABLE heroes ADD COLUMN weapon_dust JSONB NOT NULL DEFAULT '{"layers":[],"penalty":false}'::jsonb;
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
ALTER TABLE heroes DROP COLUMN weapon_dust;
-- +goose StatementEnd
