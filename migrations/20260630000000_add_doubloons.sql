-- +goose Up
-- +goose StatementBegin
ALTER TABLE heroes ADD COLUMN doubloons BIGINT NOT NULL DEFAULT 0;
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
ALTER TABLE heroes DROP COLUMN doubloons;
-- +goose StatementEnd
