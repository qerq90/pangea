-- +goose Up
-- +goose StatementBegin
-- Сборные артефакты из Лавки Фета: Ларец Азата, Живая сумка и Миниатюрный
-- шкаф. `tier` 0 — не куплен, 1..4 — куплен и улучшен (каждая ступень даёт ещё
-- мест). Зарядов у шкафа нет: магии в нём тоже нет.
CREATE TABLE hero_artifacts (
    hero_id        BIGINT PRIMARY KEY REFERENCES heroes (id) ON DELETE CASCADE,
    casket_tier    INT   NOT NULL DEFAULT 0,
    casket_charges INT   NOT NULL DEFAULT 0,
    casket_items   JSONB NOT NULL DEFAULT '{"data":[]}'::jsonb,
    bag_tier       INT   NOT NULL DEFAULT 0,
    bag_charges    INT   NOT NULL DEFAULT 0,
    bag_items      JSONB NOT NULL DEFAULT '{"data":[]}'::jsonb,
    wardrobe_tier  INT   NOT NULL DEFAULT 0,
    wardrobe_items JSONB NOT NULL DEFAULT '{"data":[]}'::jsonb
);
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
DROP TABLE hero_artifacts;
-- +goose StatementEnd
