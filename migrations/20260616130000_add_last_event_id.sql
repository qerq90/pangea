-- +goose Up
-- +goose StatementBegin
ALTER TABLE users ADD COLUMN last_event_id BIGINT;
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
ALTER TABLE users DROP COLUMN last_event_id;
-- +goose StatementEnd
