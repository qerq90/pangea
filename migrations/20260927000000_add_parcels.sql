-- +goose Up
-- +goose StatementBegin
-- Посылки: то, что один искатель передал другому. Вещь лежит здесь, пока
-- получатель не окажется в игре — тогда она сама ложится в его банковскую
-- ячейку, а если та не приняла, ждёт его на почте в Торговом доме.
CREATE TABLE parcels (
    id        BIGSERIAL   PRIMARY KEY,
    hero_id   BIGINT      NOT NULL REFERENCES heroes (id) ON DELETE CASCADE,
    from_name VARCHAR(255) NOT NULL,
    item      JSONB       NOT NULL,
    sent_at   BIGINT      NOT NULL
);

CREATE INDEX parcels_hero_idx ON parcels (hero_id);
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
DROP TABLE parcels;
-- +goose StatementEnd
