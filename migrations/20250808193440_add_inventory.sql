-- +goose Up
-- +goose StatementBegin
CREATE TABLE inventories (
    id BIGSERIAL PRIMARY KEY,
    hero_id BIGINT NOT NULL,
    max_items INT NOT NULL,
    items JSONB NOT NULL
);
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
SELECT 'down SQL query';
-- +goose StatementEnd
