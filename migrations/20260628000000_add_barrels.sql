-- +goose Up
-- +goose StatementBegin
CREATE TABLE barrels (
    id BIGSERIAL PRIMARY KEY,
    hero_id BIGINT NOT NULL UNIQUE,
    items JSONB NOT NULL DEFAULT '{"data":[]}'::jsonb,
    gold BIGINT NOT NULL DEFAULT 0
);
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
DROP TABLE barrels;
-- +goose StatementEnd
