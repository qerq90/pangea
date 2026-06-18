-- +goose Up
-- +goose StatementBegin
DROP TABLE IF EXISTS events;

CREATE TABLE event_log (
    id          BIGSERIAL,
    user_id     BIGINT       NOT NULL,
    event_type  VARCHAR(64)  NOT NULL,
    payload     JSONB        NOT NULL,
    trace_id    UUID,
    occurred_at TIMESTAMP    NOT NULL DEFAULT now(),
    PRIMARY KEY (id, occurred_at)
) PARTITION BY RANGE (occurred_at);

CREATE TABLE event_log_default  PARTITION OF event_log DEFAULT;
CREATE TABLE event_log_2026_06  PARTITION OF event_log FOR VALUES FROM ('2026-06-01') TO ('2026-07-01');
CREATE TABLE event_log_2026_07  PARTITION OF event_log FOR VALUES FROM ('2026-07-01') TO ('2026-08-01');
CREATE TABLE event_log_2026_08  PARTITION OF event_log FOR VALUES FROM ('2026-08-01') TO ('2026-09-01');
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
DROP TABLE IF EXISTS event_log;
-- +goose StatementEnd
