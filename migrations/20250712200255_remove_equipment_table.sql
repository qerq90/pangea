-- +goose Up
-- +goose StatementBegin
DROP TABLE items;
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
SELECT 'down SQL query';
-- +goose StatementEnd
