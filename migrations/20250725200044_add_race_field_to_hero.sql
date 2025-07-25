-- +goose Up
-- +goose StatementBegin
ALTER TABLE heroes
ADD COLUMN race VARCHAR(255) DEFAULT 'human';
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
SELECT 'down SQL query';
-- +goose StatementEnd
