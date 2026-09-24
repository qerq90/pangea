-- +goose Up
-- +goose StatementBegin
CREATE TABLE bank_vaults (
    id      BIGSERIAL PRIMARY KEY,
    hero_id BIGINT  NOT NULL UNIQUE REFERENCES heroes (id) ON DELETE CASCADE,
    cells   INT     NOT NULL DEFAULT 0,
    items   JSONB   NOT NULL DEFAULT '{"data":[]}'::jsonb,
    silver  BIGINT  NOT NULL DEFAULT 0
);
-- +goose StatementEnd

-- +goose Down
-- +goose StatementBegin
DROP TABLE bank_vaults;
-- +goose StatementEnd
