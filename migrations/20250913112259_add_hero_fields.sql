-- +goose Up
-- +goose StatementBegin
ALTER TABLE heroes
ADD COLUMN exp INT DEFAULT 0;

ALTER TABLE heroes
ADD COLUMN upgrade_points INT DEFAULT 0;
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
SELECT 'down SQL query';
-- +goose StatementEnd
