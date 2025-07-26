-- +goose Up
-- +goose StatementBegin
ALTER TABLE heroes
ALTER COLUMN race SET DEFAULT 'Человек';

UPDATE heroes
SET race = 'Человек';
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
SELECT 'down SQL query';
-- +goose StatementEnd
