-- +goose Up
-- +goose StatementBegin
CREATE TABLE items (
    id            BIGSERIAL PRIMARY KEY,
    hero_id       BIGINT NOT NULL REFERENCES heroes(id),
    name          VARCHAR NOT NULL,
    lvl           BIGINT NOT NULL,
    rarity        VARCHAR NOT NULL,
    item_type     VARCHAR NOT NULL,
    attack        BIGINT NOT NULL DEFAULT 0,
    accuracy      BIGINT NOT NULL DEFAULT 0,
    energy        BIGINT NOT NULL DEFAULT 0,
    armor         BIGINT NOT NULL DEFAULT 0,
    defence       BIGINT NOT NULL DEFAULT 0,
    evasion       BIGINT NOT NULL DEFAULT 0,
    created_at    TIMESTAMP NOT NULL DEFAULT NOW()
);
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
DROP TABLE IF EXISTS items;
-- +goose StatementEnd
