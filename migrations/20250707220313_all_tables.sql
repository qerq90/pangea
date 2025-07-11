-- +goose Up
-- +goose StatementBegin
CREATE TABLE users (
    id BIGINT PRIMARY KEY,
    vk_id VARCHAR(255) NOT NULL,
    telegram_id VARCHAR(255) NOT NULL
);

CREATE TABLE heroes (
    id BIGINT PRIMARY KEY,
    user_id BIGINT NOT NULL,
    state VARCHAR(255) NOT NULL,
    base_stats JSONB NOT NULL,
    fight_stats JSONB NOT NULL,
    equipment JSONB NOT NULL
);

CREATE TABLE monsters (
    id BIGINT PRIMARY KEY,
    lvl BIGINT NOT NULL,
    race VARCHAR(50) NOT NULL,
    rarity VARCHAR(50) NOT NULL,
    fight_stats JSONB NOT NULL
);

CREATE TABLE items (
    id BIGINT PRIMARY KEY,
    item_type VARCHAR(50) NOT NULL,
    attack BIGINT NOT NULL,
    accuracy BIGINT NOT NULL,
    armor BIGINT NOT NULL,
    defence BIGINT NOT NULL,
    evasion BIGINT NOT NULL
);
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
SELECT 'down SQL query';
-- +goose StatementEnd
