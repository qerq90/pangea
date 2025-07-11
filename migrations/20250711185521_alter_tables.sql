-- +goose Up
-- +goose StatementBegin
DROP TABLE IF EXISTS heroes;
DROP TABLE IF EXISTS monsters;

-- Создание таблицы heroes
CREATE TABLE heroes (
    id BIGSERIAL PRIMARY KEY,
    user_id BIGINT NOT NULL,
    state VARCHAR(255) NOT NULL,
    base_stats JSONB NOT NULL,
    fight_stats JSONB NOT NULL,
    equipment JSONB NOT NULL
);

-- Создание таблицы monsters
CREATE TABLE monsters (
    id BIGSERIAL PRIMARY KEY,
    lvl BIGINT NOT NULL,
    race VARCHAR(50) NOT NULL,
    rarity VARCHAR(50) NOT NULL,
    fight_stats JSONB NOT NULL
);
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
SELECT 'down SQL query';
-- +goose StatementEnd
