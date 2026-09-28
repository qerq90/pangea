-- +goose Up
-- +goose StatementBegin
ALTER TABLE users ADD COLUMN last_seen_at TIMESTAMP;
-- +goose StatementEnd
-- +goose StatementBegin
CREATE INDEX idx_users_last_seen ON users (last_seen_at);
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
DROP INDEX IF EXISTS idx_users_last_seen;
-- +goose StatementEnd
-- +goose StatementBegin
ALTER TABLE users DROP COLUMN last_seen_at;
-- +goose StatementEnd
