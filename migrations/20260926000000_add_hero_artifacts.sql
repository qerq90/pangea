-- +goose Up
-- +goose StatementBegin
-- Ларец Азата и Живая сумка: сборные артефакты из Лавки Фета. `tier` 0 — не
-- куплен, 1..4 — куплен и улучшен (каждая ступень даёт ещё мест).
CREATE TABLE hero_artifacts (
    hero_id        BIGINT PRIMARY KEY REFERENCES heroes (id) ON DELETE CASCADE,
    casket_tier    INT   NOT NULL DEFAULT 0,
    casket_charges INT   NOT NULL DEFAULT 0,
    casket_items   JSONB NOT NULL DEFAULT '{"data":[]}'::jsonb,
    bag_tier       INT   NOT NULL DEFAULT 0,
    bag_charges    INT   NOT NULL DEFAULT 0,
    bag_items      JSONB NOT NULL DEFAULT '{"data":[]}'::jsonb
);
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
DROP TABLE hero_artifacts;
-- +goose StatementEnd
