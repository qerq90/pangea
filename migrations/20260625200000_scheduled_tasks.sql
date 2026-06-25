-- +goose Up
-- +goose StatementBegin
CREATE TABLE scheduled_tasks(
  id             BIGSERIAL PRIMARY KEY,
  user_id        BIGINT NOT NULL,
  fire_at        BIGINT NOT NULL,
  kind           TEXT   NOT NULL,
  expected_state TEXT   NOT NULL,
  action         TEXT   NOT NULL,
  status         TEXT   NOT NULL DEFAULT 'Pending',
  attempts       INT    NOT NULL DEFAULT 0,
  created_at     BIGINT NOT NULL
);
-- +goose StatementEnd
-- +goose StatementBegin
CREATE INDEX idx_scheduled_due ON scheduled_tasks(status, fire_at);
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
DROP TABLE scheduled_tasks;
-- +goose StatementEnd
