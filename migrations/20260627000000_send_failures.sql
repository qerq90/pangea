-- +goose Up
-- +goose StatementBegin
CREATE TABLE send_failures(
  id            BIGSERIAL PRIMARY KEY,
  user_id       BIGINT,
  vk_id         TEXT NOT NULL,
  message_text  TEXT NOT NULL,
  keyboard_json TEXT,
  error_code    INT,
  error_message TEXT NOT NULL,
  attempts      INT  NOT NULL,
  created_at    BIGINT NOT NULL
);
-- +goose StatementEnd
-- +goose StatementBegin
CREATE INDEX idx_send_failures_user ON send_failures(user_id, created_at);
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
DROP TABLE send_failures;
-- +goose StatementEnd
